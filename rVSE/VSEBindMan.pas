unit VSEBindMan;

interface

uses
  Windows, AvL, avlUtils, VSECore
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TBindEvents=(beDown, beUp);
  TBindEvent=class(TCoreEvent)
  protected
    FName: string;
    FEvType: TBindEvents;
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; const Name: string; EvType: TBindEvents);
    property Name: string read FName;
    property EvType: TBindEvents read FEvType;
  end;
  TBindingRec=record
    Name, Description: string; //Name and description of binding
    Key: Byte; //Default key
  end;
  TBinding=record
    Name, Description: string;
    Key, DefKey: Byte;
  end;
  TBindings = array of TBinding;
  TBindMan=class(TModule)
  private
    FBindings: TBindings;
    FScrollStateClicks: Integer;
    FScrollStateUp: Boolean;
    {$IFDEF VSE_CONSOLE}FCmdBindings: array of record Key: Byte; Cmd: string; end; {$ENDIF}
    function KeyEvent(Key: Integer; Event: TKeyEvents): Boolean;
    function GetActive(Name: string): Boolean;
    function FindBinding(Name: string; NoLogLost: Boolean = false): Integer;
    {$IFDEF VSE_CONSOLE}
    function BindHandler(Sender: TObject; Args: array of const): Boolean;
    function BindCmdHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
    procedure Update; override;
    procedure OnEvent(var Event: TCoreEvent); override;
    procedure AddBinding(const Name, Description: string; Key: Byte); //Add binding Name with default key Key
    procedure AddBindings(const Bindings: array of TBindingRec); //Add several bindings
    function  GetBindKeyName(const BindName: string): string; //Get name of binded to BindName key
    procedure ResetKeys; //Reset all keys to default
    procedure SaveBindings; //Save bindings to ini
    property  Active[Name: string]: Boolean read GetActive; default; //True if binded key pressed
    property  Bindings: TBindings read FBindings; //Raw bindings info array
  end;

function KeyToStr(Key: Integer): string; //Get name for key code
function ProcessKeyTags(const S: string): string; //Replaces tags $BindName$ by binded key name, $$ by $

var
  BindMan: TBindMan; //Global variable for accessing to Bindings Manager

const
  SSectionBindings = 'Bindings';
  VK_XBUTTON4=5; //Mouse button 4
  VK_XBUTTON5=6; //Mouse button 5
  VK_MWHEELUP=VK_F23; //Mouse wheel up
  VK_MWHEELDOWN=VK_F24; //Mouse wheel down
  MBtnMap: array[mbLeft..mbX2] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5);
  BindEventNames: array[TBindEvents] of string = ('beDown', 'beUp');

implementation

const
  TagDelim='$';

var
  KeyNames: array[0..255] of string;

procedure InitKeyNames;
var
  i: Integer;
begin
  for i:=0 to 255 do
  begin
    SetLength(KeyNames[i], 101);
    SetLength(KeyNames[i], GetKeyNameText(MapVirtualKey(i, 0) shl 16, @KeyNames[i][1], 100));
  end;
  KeyNames[0]:='---';
  KeyNames[VK_LBUTTON]:='Left MB';
  KeyNames[VK_RBUTTON]:='Right MB';
  KeyNames[VK_MBUTTON]:='Middle MB';
  KeyNames[VK_XBUTTON4]:='MB 4';
  KeyNames[VK_XBUTTON5]:='MB 5';
  KeyNames[VK_MWHEELUP]:='Wheel Up';
  KeyNames[VK_MWHEELDOWN]:='Wheel Down';
  KeyNames[$03]:='Cancel';
  KeyNames[$0C]:='Clear';
  KeyNames[$13]:='Pause';
  KeyNames[$20]:='Space';
  KeyNames[$21]:='Page Up';
  KeyNames[$22]:='Page Down';
  KeyNames[$23]:='End';
  KeyNames[$24]:='Home';
  KeyNames[$25]:='Left';
  KeyNames[$26]:='Up';
  KeyNames[$27]:='Right';
  KeyNames[$28]:='Down';
  KeyNames[$29]:='Select';
  KeyNames[$2D]:='Insert';
  KeyNames[$2E]:='Delete';
  KeyNames[$5B]:='Left Win';
  KeyNames[$5C]:='Right Win';
  KeyNames[$5D]:='Apps';
  KeyNames[$6F]:='Num /';
  KeyNames[$90]:='Num Lock';
end;

function KeyToStr(Key: Integer): string;
begin
  Result:=KeyNames[Key];
  if Result='' then Result:='VK #'+IntToStr(Key);
end;

function StrToKey(KeyName: string): Integer;
var
  i: Integer;
begin
  Result:=0;
  if KeyName='' then Exit;
  if Copy(KeyName, 1, 4)='VK #' then Delete(KeyName, 1, 3);
  if KeyName[1]<>'#' then
  begin
    for i:=0 to 255 do
      if SameText(KeyName, KeyNames[i]) then
      begin
        Result:=i;
        Exit;
      end;
  end
    else Result:=StrToInt(Copy(KeyName, 2, 3));
end;

function ProcessKeyTags(const S: string): string;
var
  CurPos, Idx: Integer;
begin
  Result:='';
  CurPos:=0;
  Idx:=Pos(TagDelim, S);
  while Idx>0 do
  begin
    Result:=Result+Copy(S, CurPos+1, Idx-CurPos-1);
    if Idx=Length(S) then Break;
    if S[Idx+1]=TagDelim then
    begin
      Result:=Result+TagDelim;
      CurPos:=Idx+1;
    end
    else begin
      CurPos:=PosEx(TagDelim, S, Idx+1);
      if CurPos=0 then Exit;
      Result:=Result+BindMan.GetBindKeyName(Copy(S, Idx+1, CurPos-Idx-1));
    end;
    Idx:=PosEx(TagDelim, S, CurPos+1);
  end;
  Result:=Result+Copy(S, CurPos+1, MaxInt);
end;

{ TBindEvent }

constructor TBindEvent.Create(Sender: TObject; const Name: string; EvType: TBindEvents);
begin
  inherited Create(Sender);
  FName := Name;
  FEvType := EvType;
end;

{$IFDEF VSE_LOG}function TBindEvent.GetDump: string;
begin
  Result := Format('%s(Name=%s Ev=%s)', [string(ClassName), FName, BindEventNames[FEvType]]);
end;{$ENDIF}

{ TBindMan }

constructor TBindMan.Create;
begin
  inherited Create;
  BindMan:=Self;
  {$IFDEF VSE_CONSOLE}
  Console['bind name=s ?key=s']:=BindHandler;
  Console['bindcmd key=s ?cmd=s*']:=BindCmdHandler;
  {$ENDIF}
end;

destructor TBindMan.Destroy;
begin
  BindMan:=nil;
  SaveBindings;
  Finalize(FBindings);
  inherited Destroy;
end;

class function TBindMan.Name: string;
begin
  Result:='BindMan';
end;

function TBindMan.FindBinding(Name: string; NoLogLost: Boolean = false): Integer;
var
  i: Integer;
begin
  for i:=0 to High(FBindings) do
    if SameText(FBindings[i].Name, Name) then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
  {$IFDEF VSE_LOG}if not NoLogLost then Log(llWarning, 'BindMan: Bind "'+Name+'" not found');{$ENDIF}
end;

function TBindMan.GetActive(Name: string): Boolean;
var
  i: Integer;
begin
  Result:=false;
  i:=FindBinding(Name);
  if i>-1 then
    with FBindings[i] do
    begin
      if Key in [VK_MWHEELUP, VK_MWHEELDOWN] then
      begin
        if (FScrollStateClicks>0) and (((Key=VK_MWHEELUP) and FScrollStateUp) or
                                      ((Key=VK_MWHEELDOWN) and not FScrollStateUp))
          then Result:=true;
      end
      else
        Result:=Core.KeyPressed[Key];
    end;
end;

procedure TBindMan.ResetKeys;
var
  i: Integer;
begin
  for i:=0 to High(FBindings) do
    with FBindings[i] do
      Key:=DefKey;
end;

procedure TBindMan.AddBinding(const Name, Description: string; Key: Byte);
var
  Idx: Integer;
begin
  Idx:=FindBinding(Name, true);
  if Idx<0 then
  begin
    SetLength(FBindings, Length(FBindings)+1);
    Idx:=High(FBindings);
  end;
  FBindings[Idx].Name:=Name;
  FBindings[Idx].Description:=Description;
  FBindings[Idx].DefKey:=Key;
  with FBindings[Idx] do
    if Settings.Str[SSectionBindings, Name]<>'' then
      Key:=Settings.Int[SSectionBindings, Name]
    else
      Key:=DefKey;
end;

procedure TBindMan.AddBindings(const Bindings: array of TBindingRec);
var
  i: Integer;
begin
  for i:=0 to High(Bindings) do
    with Bindings[i] do
      AddBinding(Name, Description, Key);
end;

function TBindMan.GetBindKeyName(const BindName: string): string;
var
  i: Integer;
begin
  i:=FindBinding(BindName);
  if i>-1
    then Result:=KeyToStr(FBindings[i].Key)
    else Result:=''; 
end;

procedure TBindMan.Update;
begin
  if FScrollStateClicks>0 then Dec(FScrollStateClicks);
end;

procedure TBindMan.OnEvent(var Event: TCoreEvent);
const
  EvMap: array[meDown..meUp] of TKeyEvents = (keDown, keUp);
var
  Key, Btn: Integer;
begin
  if Event is TMouseEvent then
    with Event as TMouseEvent do
    begin
      if EvType in [meDown, meUp] then
        KeyEvent(MBtnMap[Button], EvMap[EvType]);
      if EvType=meWheel then
      begin
        Btn := Abs(Button);
        if Button >= 0
          then Key := VK_MWHEELUP
        else
          Key := VK_MWHEELDOWN;
        FScrollStateClicks := Btn + 1;
        FScrollStateUp := Key = VK_MWHEELUP;
        while Btn > 0 do
        begin
          KeyEvent(Key, keDown);
          KeyEvent(Key, keUp);
          Dec(Btn);
        end;
      end;
    end
  else if Event is TKeyEvent then
    with Event as TKeyEvent do
    begin
      if KeyEvent(Key, EvType) then
        FreeAndNil(Event);
    end
  else if (Event is TSysNotify) and ((Event as TSysNotify).Notify = snStateChanged) then
    FScrollStateClicks := 0
  else inherited;
end;

function TBindMan.KeyEvent(Key: Integer; Event: TKeyEvents): Boolean;
const
  EvMap: array[keDown..keUp] of TBindEvents = (beDown, beUp);
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to High(FBindings) do
    if FBindings[i].Key=Key then
    begin
      Core.SendEvent(TBindEvent.Create(Self, FBindings[i].Name, EvMap[Event]), [erState]);
      Result:=true;
      Break;
    end;
  {$IFDEF VSE_CONSOLE}
  if Event=keUp then
    for i:=0 to High(FCmdBindings) do
      if FCmdBindings[i].Key=Key then
      begin
        Console.Execute(FCmdBindings[i].Cmd);
        Result:=true;
        Break;
      end;
  {$ENDIF}
end;

procedure TBindMan.SaveBindings;
var
  i: Integer;
begin
  for i:=0 to High(FBindings) do
    Settings.Int[SSectionBindings, FBindings[i].Name]:=FBindings[i].Key;
end;

{$IFDEF VSE_CONSOLE}
function TBindMan.BindHandler(Sender: TObject; Args: array of const): Boolean;
var
  Binding, Key: Integer;
begin
  Result:=false;
  Binding:=FindBinding(string(Args[1].VAnsiString));
  if Binding=-1 then Exit;
  Result:=true;
  if Length(Args)>2 then
  begin
    Key:=StrToKey(string(Args[2].VAnsiString));
    if Key=0 then
    begin
      Console.WriteLn('Unknown key name: '+string(Args[2].VAnsiString)+PostfixError);
      Result:=false;
      Exit;
    end;
    FBindings[Binding].Key:=Key;
  end
    else Console.WriteLn(FBindings[Binding].Name+' binded to key '+KeyToStr(FBindings[Binding].Key));
end;

function TBindMan.BindCmdHandler(Sender: TObject; Args: array of const): Boolean;

  function FindBinding(Key: Byte): Integer;
  begin
    for Result:=0 to High(FCmdBindings) do
      if FCmdBindings[Result].Key=Key then Exit;
    Result:=-1;
  end;

var
  Key: Byte;
  Binding: Integer;
begin
  Result:=false;
  Key:=StrToKey(string(Args[1].VAnsiString));
  if Key<>0 then
  begin
    Binding:=FindBinding(Key);
    if Length(Args)>2 then
    begin
      if Binding=-1 then Binding:=FindBinding(0);
      if Binding=-1 then
      begin
        SetLength(FCmdBindings, Length(FCmdBindings)+1);
        Binding:=High(FCmdBindings);
      end;
      FCmdBindings[Binding].Key:=Key;
      FCmdBindings[Binding].Cmd:=string(Args[2].VAnsiString);
    end
      else if Binding<>-1 then
        FCmdBindings[Binding].Key:=0;
  end
    else Console.WriteLn('Unknown key name: '+string(Args[1].VAnsiString)+PostfixError);
end;
{$ENDIF}

initialization
  InitKeyNames;
  RegisterModule(TBindMan);

end.