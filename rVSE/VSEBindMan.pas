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
  TBindMan=class(TModule)
  private
    FBindings: array of TBinding;
    FQueuePool: array of TEventQueue;
    FScrollStateClicks: Integer;
    FScrollStateUp: Boolean;
    {$IFDEF VSE_CONSOLE}FCmdBindings: array of record Key: Byte; Cmd: string; end; {$ENDIF}
    procedure ResetEvents;
    function GetBindActive(Name: string): Boolean;
    function FindBinding(Name: string; NoLogLost: Boolean = false): Integer;
    function NewEvent(Event_: TBindEvent): PEventQueue;
    procedure SaveBindings;
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
    property  BindActive[Name: string]: Boolean read GetBindActive; //True if binded key pressed, mouse wheel up/down cannot be pressed, only events
  end;
  TBindManCfgForm=class(TGUIForm) // Keys configuration form
  private
    FLabels, FButtons: array of Integer;
    FPageLabel, FPage, FPages, FActive: Integer;
    FOnClose: TOnEvent;
    procedure ChangePage(Btn: PBtn);
    procedure KeyBtnClick(Btn: PBtn);
    procedure CloseClick(Btn: PBtn);
    procedure DefaultClick(Btn: PBtn);
    procedure SetKey(Key: Integer);
  public
    constructor Create(X, Y, Width, Height: Integer; const DefaultCapt, CloseCapt: string);
    destructor Destroy; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure Refresh;
    property  OnClose: TOnEvent read FOnClose write FOnClose; //Triggered at click on 'close' button
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

implementation

uses
  VSERender2D;

const
  MBtnMap: array[mbLeft..mbX2] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5);
  MaxEventAge=5;
  DeadEvent=255;
  PageLabel='%d/%d';
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

{ TBindManCfgForm }

constructor TBindManCfgForm.Create(X, Y, Width, Height: Integer; const DefaultCapt, CloseCapt: string);
var
  Btn: TBtn;
  Lbl: TLbl;
  BHeight, i: Integer;
begin
  inherited Create(X, Y, Width, Height);
  FActive:=-1;
  BHeight:=Render2D.TextHeight(GetFont)+10;
  SetLength(FLabels, Min(Length(BindMan.FBindings), (Height-20-Render2D.TextHeight(GetFont)) div (BHeight+10)));
  SetLength(FButtons, Length(FLabels));
  FPages:=High(BindMan.FBindings) div Max(Length(FLabels), 1);
  with Btn do
  begin
    Type_:=btPush;
    X:=2*FWidth div 3;
    Width:=FWidth-X-10;
    Height:=BHeight;
    OnClick:=KeyBtnClick;
    Enabled:=true;
  end;
  with Lbl do
  begin
    X:=10;
    Width:=Btn.X-20;
    Align:=laLeft;
    Color:=0;
  end;
  for i:=0 to High(FLabels) do
  begin
    with Btn do
    begin
      Y:=20+Render2D.TextHeight(GetFont)+i*(BHeight+10);
      Tag:=i;
    end;
    Lbl.Y:=25+Render2D.TextHeight(GetFont)+i*(BHeight+10);
    FButtons[i]:=AddButton(Btn);
    FLabels[i]:=AddLabel(Lbl);
  end;
  Refresh;
  if FPages>0 then
  begin
    FPageLabel:=CreateSelect(Self, 10, Height-40, Min(Width div 2, Width-280), 30, ChangePage, '<', '>');
    Self.Lbl[FPageLabel]^.Caption:=Format(PageLabel, [FPage+1, FPages+1]);
  end;
  with Btn do
  begin
    Width:=120;
    Y:=FHeight-40;
    X:=FWidth-260;
    Caption:=DefaultCapt;
    OnClick:=DefaultClick;
    AddButton(Btn);
    X:=FWidth-130;
    Caption:=CloseCapt;
    OnClick:=CloseClick;
    AddButton(Btn);
  end;
end;

procedure TBindManCfgForm.ChangePage(Btn: PBtn);
begin
  FPage:=Min(FPages, Max(0, FPage+Btn^.Tag));
  Lbl[FPageLabel].Caption:=Format(PageLabel, [FPage+1, FPages+1]);
  Refresh;
end;

procedure TBindManCfgForm.Refresh;
var
  i: Integer;
begin
  for i:=0 to High(FLabels) do
  begin
    if FPage*Length(FLabels)+i>High(BindMan.FBindings) then
    begin
      Lbl[FLabels[i]]^.Caption:='';
      with Button[FButtons[i]]^ do
      begin
        Caption:='';
        Enabled:=false;
      end;
      Continue;
    end;
    Lbl[FLabels[i]]^.Caption:=BindMan.FBindings[FPage*Length(FLabels)+i].Description;
    with Button[FButtons[i]]^ do
    begin
      Caption:=KeyToStr(BindMan.FBindings[FPage*Length(FLabels)+i].Key);
      Enabled:=true;
    end;
  end;
end;

procedure TBindManCfgForm.KeyBtnClick(Btn: PBtn);
begin
  FActive:=FPage*Length(FLabels)+Btn^.Tag;
  Btn^.Caption:='???';
end;

procedure TBindManCfgForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if FActive>-1 then
  begin
    if Event<>keUp then Exit;
    if Key=VK_ESCAPE then
    begin
      FActive:=-1;
      Refresh;
    end
      else if Key=VK_BACK
        then SetKey(0)
        else SetKey(Key);
  end
  else begin
    if (Event=keUp) and (Key=VK_ESCAPE)
      then CloseClick(nil)
      else inherited KeyEvent(Key, Event);
  end;
end;

procedure TBindManCfgForm.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if FActive>-1 then
  begin
    if not (Event in [meDown, meWheel]) then Exit;
    if Event=meWheel then
    begin
      if Button>0
        then SetKey(VK_MWHEELUP)
        else SetKey(VK_MWHEELDOWN);
    end
      else SetKey(MBtnMap[Button]);
  end
    else inherited MouseEvent(Button, Event, X, Y);
end;

procedure TBindManCfgForm.CloseClick(Btn: PBtn);
begin
  BindMan.SaveBindings;
  if Assigned(FOnClose) then FOnClose(Self);
  Close;
end;

destructor TBindManCfgForm.Destroy;
begin
  Finalize(FLabels);
  Finalize(FButtons);
  inherited Destroy;
end;

procedure TBindManCfgForm.DefaultClick(Btn: PBtn);
begin
  Settings.EraseSection(SSectionBindings);
  BindMan.ResetKeys;
  Refresh;
end;

procedure TBindManCfgForm.SetKey(Key: Integer);
var
  i: Integer;
begin
  if Key in [VK_SNAPSHOT] then Exit;
  for i:=0 to High(BindMan.FBindings) do
    if (i<>FActive) and (BindMan.FBindings[i].Key=Key)
      then BindMan.FBindings[i].Key:=BindMan.FBindings[FActive].Key;
  BindMan.FBindings[FActive].Key:=Key;
  FActive:=-1;
  Refresh;
end;

initialization
  InitKeyNames;
  RegisterModule(TBindMan);

end.