unit VSEBindMan;

interface

uses
  Windows, AvL, avlUtils, VSECore, VSEGUI, VSEMemPak
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TBindEvent=(beNone, beDown, beUp);
  PEventQueue=^TEventQueue;
  TEventQueue=record
    Next: PEventQueue;
    Event: TBindEvent;
    Age: Byte;
  end;
  TBindingRec=record
    Name, Description: string; //Name and description of binding
    Key: Byte; //Default key
  end;
  TBinding=record
    Name, Description: string;
    Key, DefKey: Byte;
    Events: PEventQueue;
  end;
  TBindings = array of TBinding;
  TBindMan=class(TModule)
  private
    FBindings: TBindings;
    FQueuePool: array of TEventQueue;
    FScrollStateClicks: Integer;
    FScrollStateUp: Boolean;
    {$IFDEF VSE_CONSOLE}FCmdBindings: array of record Key: Byte; Cmd: string; end; {$ENDIF}
    procedure ResetEvents;
    function GetBindActive(Name: string): Boolean;
    function FindBinding(Name: string; NoLogLost: Boolean = false): Integer;
    function NewEvent(Event_: TBindEvent): PEventQueue;
    {$IFDEF VSE_CONSOLE}
    function BindHandler(Sender: TObject; Args: array of const): Boolean;
    function BindCmdHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    class function Name: string; override; //internally used
    procedure Update; override; //internally used
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;//internally used
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override; //internally used
    function  SysNotify(Notify: TSysNotify): Boolean; override; //internally used
    procedure AddBinding(const Name_, Description_: string; Key_: Byte); //Add binding Name with default key Key
    procedure AddBindings(const Bindings: array of TBindingRec); //Add several bindings
    function  GetBindKeyName(const BindName: string): string; //Get name of binded to BindName key
    function  GetBindEvent(const Name: string): TBindEvent; //Get oldest event from queue for binding, returns beNone if no events
    procedure ResetKeys; //Reset all keys to default
    procedure SaveBindings; //Save bindings to ini
    property  BindActive[Name: string]: Boolean read GetBindActive; //True if binded key pressed, mouse wheel up/down cannot be pressed, only events
    property  Bindings: TBindings read FBindings; //Raw bindings info array
  end;

function KeyToStr(Key: Integer): string; //Get name for key code
function ProcessKeyTags(const S: string): string; //Replaces tags $BindName$ by bind BindMane key name, $$ by $

var
  BindMan: TBindMan; //Global variable for accessing to Bindings Manager

const
  SSectionBindings = 'Bindings';
  VK_XBUTTON4=5; //Mouse button 4
  VK_XBUTTON5=6; //Mouse button 5
  VK_MWHEELUP=VK_F23; //Mouse wheel up
  VK_MWHEELDOWN=VK_F24; //Mouse wheel down
  MBtnMap: array[mbLeft..mbX2] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5);

implementation

uses
  VSERender2D;

const
  MaxEventAge=5;
  DeadEvent=255;
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

{ TBindMan }

constructor TBindMan.Create;
var
  i: Integer;
begin
  inherited Create;
  BindMan:=Self;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['bind name=s ?key=s']:=BindHandler;
  Console.OnCommand['bindcmd key=s ?cmd=s*']:=BindCmdHandler;
  {$ENDIF}
  SetLength(FQueuePool, 512*MaxEventAge);
  for i:=0 to High(FQueuePool) do
    FQueuePool[i].Age:=DeadEvent;
end;

destructor TBindMan.Destroy;
begin
  BindMan:=nil;
  SaveBindings;
  Finalize(FBindings);
  Finalize(FQueuePool);
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

function TBindMan.GetBindActive(Name: string): Boolean;
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
        {$IFDEF VSE_CONSOLE}if not Console.Active or (Key in [VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5]) then{$ENDIF}
          Result:=Core.KeyPressed[Key];
    end;
end;

function TBindMan.GetBindEvent(const Name: string): TBindEvent;
var
  i: Integer;
begin
  Result:=beNone;
  i:=FindBinding(Name);
  if (i>-1) and Assigned(FBindings[i].Events) then
    with FBindings[i] do
    begin
      Result:=Events^.Event;
      Events^.Age:=DeadEvent;
      Events:=Events^.Next;
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

procedure TBindMan.AddBinding(const Name_, Description_: string; Key_: Byte);
var
  Idx: Integer;
begin
  Idx:=FindBinding(Name_, true);
  if Idx<0 then
  begin
    SetLength(FBindings, Length(FBindings)+1);
    Idx:=High(FBindings);
  end;
  with FBindings[Idx] do
  begin
    Name:=Name_;
    Description:=Description_;
    DefKey:=Key_;
    if Settings.Str[SSectionBindings, Name]<>'' then
      Key:=Settings.Int[SSectionBindings, Name]
    else
      Key:=DefKey;
    Events:=nil;
  end;
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
var
  i: Integer;
  Event: PEventQueue;
begin
  if FScrollStateClicks>0 then Dec(FScrollStateClicks);
  for i:=0 to High(FBindings) do
    with FBindings[i] do
    begin
      while Assigned(Events) and (Events^.Age>=MaxEventAge) do
      begin
        Events^.Age:=DeadEvent;
        Events:=Events^.Next;
      end;
      Event:=Events;
      while Assigned(Event) do
      begin
        Inc(Event^.Age);
        Event:=Event^.Next;
      end;
    end;
end;

function TBindMan.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if Notify=snStateChanged then
    ResetEvents;
end;

procedure TBindMan.KeyEvent(Key: Integer; Event: TKeyEvent);
const
  EvMap: array[keDown..keUp] of TBindEvent = (beDown, beUp);
var
  i: Integer;
  Ev: PEventQueue;
begin
  for i:=0 to High(FBindings) do
    if FBindings[i].Key=Key then
      with FBindings[i] do
      begin
        if Assigned(Events) then
        begin
          Ev:=Events;
          while Assigned(Ev^.Next) do Ev:=Ev^.Next;
          Ev^.Next:=NewEvent(EvMap[Event]);
        end
          else Events:=NewEvent(EvMap[Event]);
        Break;
      end;
  {$IFDEF VSE_CONSOLE}
  if Event=keUp then
    for i:=0 to High(FCmdBindings) do
      if FCmdBindings[i].Key=Key then
      begin
        Console.Execute(FCmdBindings[i].Cmd);
        Break;
      end;
  {$ENDIF}
end;

procedure TBindMan.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
const
  EvMap: array[meDown..meUp] of TKeyEvent = (keDown, keUp);
var
  Key: Integer;
begin
  if Event in [meDown, meUp] then KeyEvent(MBtnMap[Button], EvMap[Event]);
  if Event=meWheel then
  begin
    if Button>=0
      then Key:=VK_MWHEELUP
    else begin
      Key:=VK_MWHEELDOWN;
      Button:=-Button;
    end;
    FScrollStateClicks:=Button+1;
    FScrollStateUp:=Key=VK_MWHEELUP;
    while Button>0 do
    begin
      KeyEvent(Key, keDown);
      KeyEvent(Key, keUp);
      Dec(Button);
    end;
  end;
end;

function TBindMan.NewEvent(Event_: TBindEvent): PEventQueue;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FQueuePool) do
    if FQueuePool[i].Age=DeadEvent then
    begin
      Result:=@FQueuePool[i];
      with Result^ do
      begin
        Event:=Event_;
        Next:=nil;
        Age:=0;
      end;
      Exit;
    end;
  {$IFDEF VSE_LOG}Log(llError, 'BindMan: Event pool overflow');{$ENDIF}
end;

procedure TBindMan.ResetEvents;
var
  i: Integer;
begin
  FScrollStateClicks:=0;
  for i:=0 to High(FBindings) do
    with FBindings[i] do
      while Assigned(Events) do
      begin
        Events^.Age:=DeadEvent;
        Events:=Events^.Next;
      end;
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