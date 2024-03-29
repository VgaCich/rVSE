unit VSECore;

//TODO: edit all log messages

interface

uses
  Windows, Messages, MMSystem, AvL, avlMath, avlUtils, OpenGL, oglExtensions,
  VSEOpenGLExt, VSEImageCodec {$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  //Events
  TCoreEvent = class // Base event class
  protected
    FSender: TObject;
    {$IFDEF VSE_LOG}function GetDump: string; virtual;{$ENDIF}
  public
    constructor Create(Sender: TObject = nil);
    property Sender: TObject read FSender; //Event sender
    {$IFDEF VSE_LOG}property Dump: string read GetDump;{$ENDIF}
  end;
  TMouseEvents=(meDown, meUp, meMove, meWheel); //Mouse events: button pressed, button release, mouse moving, mouse wheel
  TMouseEvent = class(TCoreEvent) //Mouse event
  protected
    FButton: Integer;
    FEvType: TMouseEvents;
    FCursor: TPoint;
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; Button: Integer; EvType: TMouseEvents; Cursor: TPoint);
    property Button: Integer read FButton; //Mouse button number or wheel click if Event=meWheel
    property EvType: TMouseEvents read FEvType; //Event type
    property Cursor: TPoint read FCursor; //Mouse cursor coordinates or cursor coordinates delta if Core.MouseCapture=true
  end;
  TKeyEvents=(keDown, keUp); //Keyboard events: key pressed, key released
  TKeyEvent = class(TCoreEvent) //Keyboard event
  protected
    FKey: Integer;
    FEvType: TKeyEvents;
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; Key: Integer; EvType: TKeyEvents);
    property Key: Integer read FKey; //Virtual key code
    property EvType: TKeyEvents read FEvType; //Event type
  end;
  TCharEvent = class(TCoreEvent) //Keyboard character event
  protected
    FChr: Char;
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; Chr: Char);
    property Chr: Char read FChr;
  end;
  TSysNotifies=(  //System notifies:
    snMinimized, //Application minimized
    snMaximized, //Application maximized
    snUpdateOverload, //Update Overload Detection triggered
    snPause, //Engine paused
    snResume, //Engine resumed
    snResolutionChanged, //Resolution changed
    snStateChanged, //State changed
    snLogSysInfo //Write system info to log
  );
  TSysNotify = class(TCoreEvent) //System notify
  protected
    FNotify: TSysNotifies;
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; Notify: TSysNotifies);
    property Notify: TSysNotifies read FNotify; //Notification code
  end;
  PStream = ^TStream;
  TGetFileEvent = class(TCoreEvent)
  protected
    FFileName: string;
    FResult: PStream;
    procedure SetResult(Result: TStream);
    {$IFDEF VSE_LOG}function GetDump: string; override;{$ENDIF}
  public
    constructor Create(Sender: TObject; const FileName: string; Result: PStream);
    property FileName: string read FFileName;
    property Result: TStream write SetResult;
  end;

  TCoreModule=class
  public
    procedure Draw; virtual; //Draw handler
    procedure Update; virtual; //Update handler
    procedure OnEvent(var Event: TCoreEvent); virtual; //Events handler; FreeAndNil event to stop furhter dispatching
  end;
  TGameState = class(TCoreModule) //Base state class
  protected
    function GetName: string; virtual; abstract; //Must return state name
  public
    procedure OnEvent(var Event: TCoreEvent); override;
    function  Activate: Cardinal; virtual; //Activate handler (triggered on switching to state), must return updates interval
    procedure Deactivate; virtual; //Deactiovate handler (triggered on switching from state)
    property  Name: string read GetName; //State name
  end;
  TModule = class(TCoreModule) //Base engine module class
  public
    constructor Create; virtual;
    class function Name: string; virtual; abstract; //Must return module name
  end;
  CModule=class of TModule;

  TStopState=( //Engine stop codes
    StopNormal, //Engine stopped normally
    StopDefault, //Engine stopped by something other than StopEngine
    StopNeedRestart, //Engine needs restart
    //Critical errors
    StopInitError, //Cannot initialize engine
    StopInternalError, //Internal engine error
    StopUserException, //Engine stopped due to unhandled exception in user code
    StopDisplayModeError, //Engine stopped due to error when setting display mode
    StopUserError //Engine stopped by user code due to error
  );
  TEventReceiver=(erModule, erState);
  TEventReceivers=set of TEventReceiver;
  TCore=class
  private
    FHandle: THandle;
    FDC: HDC;
    FRC: HGLRC;
    FResolutionX: Cardinal;
    FResolutionY: Cardinal;
    FRefreshRate: Cardinal;
    FColorDepth: Cardinal;
    FFramesCount, FFPS, FFPSTimer, FPreviousUpdate, FUpdInt, FUpdOverloadCount, FUpdOverloadThreshold: Cardinal;
    FHPETFreq: Int64;
    FStates: array of TGameState;
    FModules: array of TModule;
    FState, FSwitchTo: Cardinal;
    FCurState: TGameState;
    FPrevStateName: string;
    FFullscreen, FNeedSwitch, FMinimized, FPaused, FMouseCapture, FInhibitUpdate: Boolean;
    FKeyState: TKeyboardState;
    FSavedMousePos: TPoint;
    procedure SetFullscreen(Value: Boolean);
    function  GetCaption: string;
    procedure SetCaption(const Value: string);
    function  GetVSync: Boolean;
    procedure SetVSync(Value: Boolean);
    procedure SetState(Value: Cardinal);
    function  GetKeyPressed(Index: Byte): Boolean;
    procedure SetMouseCapture(Value: Boolean);
    function  GetMouseCursor: TPoint;
    function  GetTime: Cardinal;
    procedure ResetMouse;
    {$IFDEF VSE_CONSOLE}
    function QuitHandler(Sender: TObject; Args: array of const): Boolean;
    function StateHandler(Sender: TObject; Args: array of const): Boolean;
    function ResolutionHandler(Sender: TObject; Args: array of const): Boolean;
    function FullscreenHandler(Sender: TObject; Args: array of const): Boolean;
    function VSyncHandler(Sender: TObject; Args: array of const): Boolean;
    function ScreenshotHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  protected
    procedure StartEngine;
    procedure SaveSettings;
    procedure Update;
    procedure Resume;
    procedure MouseEvent(Button: Integer; Event: TMouseEvents; X, Y: Integer);
    procedure KeyEvent(Key: Integer; Event: TKeyEvents);
    procedure CharEvent(C: Char);
  public
    constructor Create(WndHandle: THandle); //internally used
    destructor Destroy; override; //internally used
    procedure StopEngine(StopState: TStopState = StopNormal); //Stop engine with stop code StopState and quit
    procedure SendEvent(Event: TCoreEvent; Receivers: TEventReceivers = [erModule, erState]); //Send event
    {State manager}
    function  AddState(State: TGameState): Cardinal; //Add state object, returns state index
    function  ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean; //Replace state at index OrigState with state object NewState; returns true if success
    procedure DeleteState(State: Cardinal); //Delete state, may change indices of other states
    procedure SwitchState(NewState: Cardinal); overload; //Switch to state by state index
    procedure SwitchState(const NewStateName: string); overload; //Switch to state by state name
    function  StateExists(State: Cardinal): Boolean; //Returns true if exists state with supplied index
    function  GetState(State: Cardinal): TGameState; //Returns state object by index
    function  FindState(const Name: string): Cardinal; //Returns state index by state name or InvalidState if state not found
    {Misc.}
    function  KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean; //Returns true if Key pressed, but no more often then Rate; KeyVar - counter for rate limiting
    procedure SetResolution(ResolutionX, ResolutionY, RefreshRate: Cardinal; Fullscreen: Boolean; CanReset: Boolean = true);  //Set resolution ResX*ResY@Refresh; CanReset: return to previous resolution if fail
    procedure MakeScreenshot(Name: string; Format: TImageFormat; Numerate: Boolean = true); //Makes screenshot in exe folder; Name: screentshot file name; Format: screenshot file format; Numerate: append counter to name
    procedure ResetUpdateTimer; //Reset update timer and clear pending updates
    function GetFile(const FileName: string): TStream; //Get file as stream
    function GetFileText(const FileName: string): TStringList; //Get text file as TStringList
    ///
    property Caption: string read GetCaption write SetCaption;
    property Handle: THandle read FHandle; //Engine window handle
    property DC: HDC read FDC; //Engine window GDI device context
    property RC: HGLRC read FRC; //Engine window OpenGL rendering context
    property ResolutionX: Cardinal read FResolutionX; //Horizontal resolution of viewport
    property ResolutionY: Cardinal read FResolutionY; //Vertical resolution of viewport
    property RefreshRate: Cardinal read FRefreshRate; //Screen refresh rate, fullscreen only
    property ColorDepth: Cardinal read FColorDepth write FColorDepth; //Color depth, applied after engine restart
    property Fullscreen: Boolean read FFullscreen write SetFullscreen; //Fullscreen mode
    property VSync: Boolean read GetVSync write SetVSync; //Vertical synchronization
    property Minimized: Boolean read FMinimized; //Engine window minimized
    property Paused: Boolean read FPaused write FPaused; //Engine paused; do not write
    property KeyPressed[Index: Byte]: Boolean read GetKeyPressed; //True if Key pressed
    property MouseCapture: Boolean read FMouseCapture write SetMouseCapture; //Mouse capture mode
    property MouseCursor: TPoint read GetMouseCursor; //Mouse cursor coordinates relative to engine window
    property Time: Cardinal read GetTime; //Current time in ms
    property State: Cardinal read FState write SetState; //Current state index
    property CurState: TGameState read FCurState; //Current state object
    property PrevStateName: string read FPrevStateName; //Name of previous state
    property InhibitUpdate: Boolean read FInhibitUpdate write FInhibitUpdate; //Inhibit next State.Update
    property FPS: Cardinal read FFPS; //Current FPS
    property UpdateInterval: Cardinal read FUpdInt write FUpdInt; //Current state updates interval
    property UpdateOverloadThreshold: Cardinal read FUpdOverloadThreshold write FUpdOverloadThreshold; //Update Overload Detection threshold, overloaded update cycles before triggering
  end;

  TSettings=class
  private
    FFirstRun: Boolean;
    FIni: TIniFile;
    function GetBool(const Section, Name: string): Boolean;
    function GetInt(const Section, Name: string): Integer;
    function GetStr(const Section, Name: string): string;
    procedure SetBool(const Section, Name: string; const Value: Boolean);
    procedure SetInt(const Section, Name: string; const Value: Integer);
    procedure SetStr(const Section, Name: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReloadInitSettings; //Reload InitSettings from ini
    function ReadSection(const Section: string): TStringList; //Read section contents to TStringList
    procedure EraseSection(const Section: string); //Erase section
    property FirstRun: Boolean read FFirstRun; //True if ini file wasn't exist at time of engine's start
    property Bool[const Section, Name: string]: Boolean read GetBool write SetBool; //Read/write Boolean value
    property Int[const Section, Name: string]: Integer read GetInt write SetInt; //Read/write Integer value
    property Str[const Section, Name: string]: string read GetStr write SetStr; //Read/write String value
  end;

  TInitStates=procedure;
  TInitSettings=record
    InitStates: TInitStates; //Init states procedure pointer
    Caption: string; //Engine window caption
    Version: string; //Application version
    DataDir: string; //Data directory (absolute path with trailing backslash)
    ResolutionX: Integer; //Horizontal resolution
    ResolutionY: Integer; //Vertical resolution
    RefreshRate: Integer; //Screen refresh rate, fullscreen only
    ColorDepth: Integer; //Color depth
    Fullscreen: Boolean; //Fullscreen mode
    VSync: Boolean; //Vertical synchronization
  end;

function VSEStart: TStopState;  //Start engine, returns engine stop code
procedure LogException(Comment: string); //Writes current exception info to log, followed by Comment. Call only in except block
procedure RegisterModule(Module: CModule); //Register engine module

var
  Core: TCore; //Global variable for accessing to Engine Core
  Settings: TSettings; //Interface to engine's ini file
  InitSettings: TInitSettings = ( //Engine pre-init settings
    InitStates: nil;
    Caption: '';
    Version: '';
    DataDir: '';
    ResolutionX: 640;
    ResolutionY: 480;
    RefreshRate: 0;
    ColorDepth: 32;
    Fullscreen: false;
    VSync: true;
  );

const
  UM_STOPENGINE = WM_USER;
  InvalidState = $FFFFFFFF; //Non-existing state index
  StopCodeNames: array[TStopState] of string =
    ('Normal', 'Default', 'Need Restart', 'Init Error', 'Internal Error', 'User Exception', 'Display Mode Error', 'User Error');
  SysNotifyNames: array[TSysNotifies] of string =
    ('snMinimized', 'snMaximized', 'snUpdateOverload', 'snPause', 'snResume', 'snResolutionChanged', 'snStateChanged', 'snLogSysInfo');
  MouseEventNames: array[TMouseEvents] of string =
    ('meDown', 'meUp', 'meMove', 'meWheel');
  KeyEventNames: array[TKeyEvents] of string =
    ('keDown', 'keUp');
  VSECaptVer = 'reduced VS Engine 1.0';
  SSectionSettings = 'Settings';
  mbLeft = 1; //Left mouse button
  mbRight = 2; //Right mouse button
  mbMiddle = 3; //Middle mouse button
  mbX1 = 4; //Fourth mouse button
  mbX2 = 5; //Fifth mouse button

implementation

{$IFDEF VSE_CONSOLE}
uses
  VSEConsole;
{$ENDIF}

const
  WindowedWindowStyle = WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  MinResolutionX = 640;
  MinResolutionY = 480;
  DefaultOverloadThreshold = 8;
  WndClassName: PChar = 'VSENGINE';
  WM_XBUTTONDOWN=$20B;
  WM_XBUTTONUP=$20C;
  SNameColorDepth = 'ColorDepth';
  SNameFullscreen = 'Fullscreen';
  SNameRefreshRate = 'RefreshRate';
  SNameResolutionY = 'ResolutionY';
  SNameResolutionX = 'ResolutionX';
  SNameVSync = 'VSync';

var
  Mutex: Integer=0;
  VSEStopState: TStopState=StopNormal;
  Modules: array of CModule;
  {$IFNDEF VSE_NOSYSINFO}SysInfoLogged: Boolean = false;{$ENDIF}

procedure LogErrorAndShowMessage(Msg: string);
begin
  {$IFDEF VSE_LOG}Log(llError, Msg);{$ENDIF}
  MessageBox(0, PChar(Msg), PChar(InitSettings.Caption), MB_ICONERROR);
end;

procedure LogException(Comment: string);
begin
  Comment:=Format('Exception "%s" at $%s with message "%s" %s', [string(ExceptObject.ClassName), IntToHex(Cardinal(ExceptAddr), 8), Exception(ExceptObject).Message, Comment]);
  {$IFDEF VSE_LOG}Log(llError, Comment);{$ENDIF}
  {$IFNDEF VSE_DEBUG}MessageBox(0, PChar(Comment), PChar(InitSettings.Caption), MB_ICONERROR);{$ENDIF}
end;

procedure UpdateFPS(uID, uMsg, dwUser, dw1, dw2: Cardinal); stdcall;
begin
  Core.FFPS:=Core.FFramesCount;
  Core.FFramesCount:=0;
end;

{ TCoreEvent }

constructor TCoreEvent.Create(Sender: TObject);
begin
  inherited Create;
  FSender := Sender;
end;

{$IFDEF VSE_LOG}
function TCoreEvent.GetDump: string;
begin
  Result := ClassName;
end;
{$ENDIF}

{ TMouseEvent }

constructor TMouseEvent.Create(Sender: TObject; Button: Integer; EvType: TMouseEvents; Cursor: TPoint);
begin
  inherited Create(Sender);
  FButton := Button;
  FEvType := EvType;
  FCursor := Cursor;
end;

{$IFDEF VSE_LOG}
function TMouseEvent.GetDump: string;
begin
  Result := Format('%s(Btn=%d Ev=%s X=%d Y=%d)', [string(ClassName), FButton, MouseEventNames[FEvType], Cursor.X, Cursor.Y]);
end;
{$ENDIF}

{ TKeyEvent }

constructor TKeyEvent.Create(Sender: TObject; Key: Integer; EvType: TKeyEvents);
begin
  inherited Create(Sender);
  FKey := Key;
  FEvType := EvType;
end;

{$IFDEF VSE_LOG}
function TKeyEvent.GetDump: string;
begin
  Result := Format('%s(Key=%d Ev=%s)', [string(ClassName), FKey, KeyEventNames[FEvType]]);
end;
{$ENDIF}

{ TCharEvent }

constructor TCharEvent.Create(Sender: TObject; Chr: Char);
begin
  inherited Create(Sender);
  FChr := Chr;
end;

{$IFDEF VSE_LOG}
function TCharEvent.GetDump: string;
begin
  Result := Format('%s(Chr=0x%02x)', [string(ClassName), Ord(FChr)]);
end;
{$ENDIF}

{ TSysNotify }

constructor TSysNotify.Create(Sender: TObject; Notify: TSysNotifies);
begin
  inherited Create(Sender);
  FNotify := Notify;
end;

{$IFDEF VSE_LOG}
function TSysNotify.GetDump: string;
begin
  Result := Format('%s(Notify=%s)', [string(ClassName), SysNotifyNames[FNotify]]);
end;
{$ENDIF}

{ TGetFileEvent }

constructor TGetFileEvent.Create(Sender: TObject; const FileName: string; Result: PStream);
begin
  inherited Create(Sender);
  FFileName := FileName;
  FResult := Result;
end;

procedure TGetFileEvent.SetResult(Result: TStream);
begin
  if not Assigned(FResult) or not Assigned(Result) then Exit;
  if Assigned(FResult^) then
    FreeAndNil(FResult^);
  FResult^ := Result;
end;

{$IFDEF VSE_LOG}
function TGetFileEvent.GetDump: string;
begin
  Result := Format('%s(File="%s")', [string(ClassName), FFileName]);
end;
{$ENDIF}

{ TCoreModule }

procedure TCoreModule.Draw;
begin

end;

procedure TCoreModule.Update;
begin

end;

procedure TCoreModule.OnEvent(var Event: TCoreEvent);
begin

end;

{ TGameState }

procedure TGameState.OnEvent(var Event: TCoreEvent);
begin
  inherited;
  if (Event is TSysNotify) then
    case (Event as TSysNotify).Notify of
      snUpdateOverload: Core.ResetUpdateTimer; //Override to implement your own UpdateOverload handler
      snMinimized: Core.Paused := true; //Override to disable core pausing on minimizing window
    end;
end;

function  TGameState.Activate: Cardinal;
begin
  Result := 50;
end;

procedure TGameState.Deactivate;
begin

end;

{ TModule }

constructor TModule.Create;
begin
  inherited;
end;

{ TCore }

constructor TCore.Create(WndHandle: THandle);
begin
  inherited Create;
  FPaused:=true;
  FMinimized:=false;
  FNeedSwitch:=false;
  FState:=InvalidState;
  FStates:=nil;
  FModules:=nil;
  FCurState:=nil;
  FHandle:=WndHandle;
  FFullscreen:=false;
  FFPS:=0;
  FFramesCount:=0;
  {$IFDEF VSE_LOG}if not{$ENDIF}
  QueryPerformanceFrequency(FHPETFreq)
  {$IFDEF VSE_LOG}then Log(llWarning, 'HPET not available, using GTC'){$ENDIF};
  FPreviousUpdate:=0;
  FUpdOverloadCount:=0;
  FUpdOverloadThreshold:=DefaultOverloadThreshold;
end;

destructor TCore.Destroy;
var
  i: Integer;
  Name: string;
begin
  SaveSettings;
  for i:=0 to High(FStates) do
  try
    Name:=FStates[i].Name;
    FAN(FStates[i]);
  except
    LogException('in state '+Name+'.Free');
  end;
  Finalize(FStates);
  for i:=High(FModules) downto 0 do
  try
    if not Assigned(FModules[i]) then Continue;
    Name:=FModules[i].Name;
    {$IFDEF VSE_LOG}Log(llInfo, 'Finalizing module '+Name);{$ENDIF}
    FAN(FModules[i]);
  except
    LogException('in module '+Name+'.Free');
  end;
  Finalize(FModules);
  {$IFDEF VSE_CONSOLE}FAN(Console);{$ENDIF}
  if FFullscreen then gleGoBack;
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  if FDC>0 then ReleaseDC(FHandle, FDC);
  timeKillEvent(FFPSTimer);
  inherited Destroy;
end;

//Protected - interacting with WndProc

procedure TCore.StartEngine;
var
  i: Integer;
begin
  //FFullscreen:=InitSettings.Fullscreen;
  if InitSettings.ResolutionX<MinResolutionX then InitSettings.ResolutionX:=MinResolutionX;
  if InitSettings.ResolutionY<MinResolutionY then InitSettings.ResolutionY:=MinResolutionY;
  if InitSettings.RefreshRate=0 then InitSettings.RefreshRate:=gleGetCurrentResolution.RefreshRate;
  FColorDepth:=InitSettings.ColorDepth;
  FDC:=GetDC(FHandle);
  FRC:=gleSetPix(FDC, FColorDepth);
  if FRC=0 then raise Exception.Create('Unable to set rendering context');
  {$IFDEF VSE_CONSOLE}
  Console:=TConsole.Create;
  Console['quit']:=QuitHandler;
  Console['state ?state=s']:=StateHandler;
  Console['resolution ?resx=i640:65536 ?resy=i480:65536 ?refr=i0']:=ResolutionHandler;
  Console['fullscreen ?val=eoff:on']:=FullscreenHandler;
  Console['vsync ?val=eoff:on']:=VSyncHandler;
  Console['screenshot ?name=s ?fmt=ebmp:jpg:gif:png:tif']:=ScreenshotHandler;
  {$ENDIF}
  SetLength(FModules, Length(Modules));
  for i:=0 to High(Modules) do
  try
    {$IFDEF VSE_LOG}Log(llInfo, 'Initializing module '+Modules[i].Name);{$ENDIF}
    FModules[i]:=Modules[i].Create;
    if not Assigned(FModules[i]) then raise Exception.Create('Can''t initialize module '+Modules[i].Name);
  except
    LogException('in module '+Modules[i].Name+'.Create');
    Core.StopEngine(StopInitError);
  end;
  {$IF Defined(VSE_LOG) and not Defined(VSE_NOSYSINFO)}
  if not SysInfoLogged then
  begin
    SendEvent(TSysNotify.Create(Self, snLogSysInfo), [erModule]);
    SysInfoLogged := true;
  end;
  {$IFEND}
  SetResolution(InitSettings.ResolutionX, InitSettings.ResolutionY, InitSettings.RefreshRate, InitSettings.Fullscreen, false);
  VSync:=InitSettings.VSync;
  {$IFDEF VSE_LOG}Log(llInfo, 'States initialization');{$ENDIF}
  if not Assigned(InitSettings.InitStates) then raise Exception.Create('InitStates() are NULL');
  try
    InitSettings.InitStates;
    {$IFDEF VSE_LOG}Log(llInfo, 'States initialized');{$ENDIF}
  except
    LogException('in InitStates');
    Core.StopEngine(StopUserException);
  end;
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glHint(GL_SHADE_MODEL, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  FFPSTimer:=timeSetEvent(1000, 0, @UpdateFPS, 0, TIME_PERIODIC);
  {$IFDEF VSE_CONSOLE}
  {$IFDEF VSE_LOG}Log(llInfo, 'Executing autoexec.cfg');{$ENDIF}
  Console.Execute('if "exist autoexec.cfg" exec autoexec.cfg');
  {$ENDIF}
  FPaused:=false;
end;

procedure TCore.SaveSettings;
begin
  Settings.Int[SSectionSettings, SNameResolutionX]:=FResolutionX;
  Settings.Int[SSectionSettings, SNameResolutionY]:=FResolutionY;
  Settings.Int[SSectionSettings, SNameRefreshRate]:=FRefreshRate;
  Settings.Int[SSectionSettings, SNameColorDepth]:=FColorDepth;
  Settings.Bool[SSectionSettings, SNameFullscreen]:=Fullscreen;
  Settings.Bool[SSectionSettings, SNameVSync]:=VSync;
end;

procedure TCore.Update;
var
  T, UpdTime: Cardinal;
  i: Integer;
  Cursor: TPoint;
begin
  try
    GetKeyboardState(FKeyState);
    {$IFDEF VSE_CONSOLE}Console.Update;{$ENDIF}
    for i:=0 to High(FModules) do
    try
      FModules[i].Update;
    except
      LogException(Format('in module %s.Update', [FModules[i].Name]));
      {$IFNDEF VSE_DEBUG}StopEngine(StopInternalError);{$ENDIF}
    end;
    if FPaused then Exit;
    if GetForegroundWindow<>FHandle then
    begin
      {$IFDEF VSE_LOG}if not FMinimized then Log(llInfo, 'Window minimized');{$ENDIF}
      FMinimized:=True;
      if FFullscreen then
      begin
        gleGoBack;
        SendMessage(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
      end;
      SendEvent(TSysNotify.Create(Self, snMinimized));
      if FPaused then
      begin
        if FNeedSwitch
          then State:=FSwitchTo;
        SendEvent(TSysNotify.Create(Self, snPause));
        if not FFullscreen
          then SendMessage(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
    end;
    if FNeedSwitch
      then State:=FSwitchTo;
    if FPreviousUpdate=0 then FPreviousUpdate:=Time;
    T:=Time-FPreviousUpdate; //TODO: one update per draw if FUpdInt=0
    FPreviousUpdate:=FPreviousUpdate+T-(T mod FUpdInt);
    if FCurState<>nil then
    begin
      if FMouseCapture and not FMinimized then
      begin
        Cursor:=MouseCursor;
        Cursor.X:=Cursor.X-FResolutionX div 2;
        Cursor.Y:=Cursor.Y-FResolutionY div 2;
        ResetMouse;
        if not FInhibitUpdate then
          SendEvent(TMouseEvent.Create(Self, 0, meMove, Cursor), [erState]);
      end;
      if not FInhibitUpdate then
        for i:=1 to T div FUpdInt do
        begin
          UpdTime:=Time;
          try
            FCurState.Update;
          except
            LogException('in state '+FCurState.Name+'.Update');
            {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
          end;
          if Time-UpdTime>FUpdInt then
          begin
            Inc(FUpdOverloadCount);
            if FUpdOverloadCount>FUpdOverloadThreshold then
            begin
              SendEvent(TSysNotify.Create(Self, snUpdateOverload), [erState]);
              {$IFDEF VSE_LOG}Log(llWarning, 'Update overload in state "'+FCurState.Name+'"');{$ENDIF}
            end;
          end
            else if FUpdOverloadCount>0 then Dec(FUpdOverloadCount);
        end;
      FInhibitUpdate:=false;
      try
        FCurState.Draw;
      except
        LogException('in state '+FCurState.Name+'.Draw');
        {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
      end;
    end;
    for i:=High(FModules) downto 0 do
    try
      FModules[i].Draw;
    except
      LogException(Format('in module %s.Draw', [FModules[i].Name]));
      {$IFNDEF VSE_DEBUG}StopEngine(StopInternalError);{$ENDIF}
    end;
    SwapBuffers(FDC);
    Inc(FFramesCount);
  except
    LogException('in TCore.Update');
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.Resume;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Window maximized');{$ENDIF}
  try
    if FFullscreen then gleGoFullscreen(ResolutionX, ResolutionY, RefreshRate, FColorDepth);
    FMinimized:=false;
    if FPaused then ResetUpdateTimer;
    FPaused:=false;
    SendEvent(TSysNotify.Create(Self, snResume));
    SendEvent(TSysNotify.Create(Self, snMaximized));
  except
    LogException('in TCore.Resume');
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.MouseEvent(Button: Integer; Event: TMouseEvents; X, Y: Integer);
var
  Pos: TPoint;
begin
  try
    Pos:=Point(X, Y);
    if (Event=meWheel) and not Fullscreen then
      ScreenToClient(Handle, Pos);
    if Event=meDown
      then SetCapture(FHandle)
      else if Event=meUp
        then ReleaseCapture;
    if (FMouseCapture and (Event=meMove)) or FPaused then Exit;
    SendEvent(TMouseEvent.Create(Self, Button, Event, Pos));
  except
    LogException(Format('in TCore.MouseEvent(%d, %s, %d, %d)', [Button, MouseEventNames[Event], X, Y]));
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.KeyEvent(Key: Integer; Event: TKeyEvents);
begin
  try
    if FPaused then Exit;
    {$IFDEF VSE_ESC_EXIT}
    if (Key=VK_ESCAPE) and (Event=keUp) then
    begin
      StopEngine;
      Exit;
    end;
    {$ENDIF}
    {$IFDEF VSE_USE_SNAPSHOT_KEY}
    if (Key=VK_SNAPSHOT) and (Event=keUp) then
    begin
      MakeScreenshot(ChangeFileExt(ExtractFileName(ExeName), '')+'_screenshot', ifPNG);
      Exit;
    end;
    {$ENDIF}
    SendEvent(TKeyEvent.Create(Self, Key, Event));
  except
    LogException(Format('in TCore.KeyEvent(%d, %s)', [Key, KeyEventNames[Event]]));
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.CharEvent(C: Char);
begin
  try
    if FPaused then Exit;
    SendEvent(TCharEvent.Create(Self, C));
  except
    LogException('in TCore.CharEvent(#'+IntToStr(Ord(C))+')');
    StopEngine(StopInternalError);
  end;
end;

//Public

procedure TCore.StopEngine(StopState: TStopState);
begin
  {$IFDEF VSE_LOG}LogF(llInfo, 'Stopping engine with code %d (%s)', [Ord(StopState), StopCodeNames[StopState]]);{$ENDIF}
  VSEStopState:=StopState;
  PostMessage(Handle, UM_STOPENGINE, 0, 0);
end;

procedure TCore.SendEvent(Event: TCoreEvent; Receivers: TEventReceivers);
var
  i: Integer;
  EventDump: string;
begin
  if not Assigned(Event) then Exit;
  try
    {$IFDEF VSE_LOG}EventDump := Event.Dump;{$ENDIF}
    if erModule in Receivers then
      for i:=0 to High(FModules) do
      try
        if Assigned(Event) then
          FModules[i].OnEvent(Event)
        else Break;
      except
        LogException(Format('in module %s.Event(%s)', [FModules[i].Name, EventDump]));
        {$IFNDEF VSE_DEBUG}StopEngine(StopInternalError);{$ENDIF}
      end;
    if (erState in Receivers) and Assigned(Event) and Assigned(FCurState) then
    try
      FCurState.OnEvent(Event);
    except
      LogException(Format('in state %s.Event(%s)', [FCurState.Name, EventDump]));
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
  finally
    FreeAndNil(Event);
  end;
end;

function TCore.AddState(State: TGameState): Cardinal;
begin
  Result:=Length(FStates);
  SetLength(FStates, Result+1);
  FStates[Result]:=State;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Added state #%d %s', [Result, State.Name]);{$ENDIF}
end;

function TCore.ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
begin
  Result:=true;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Replacing state #%d with %s', [OrigState, NewState.Name]);{$ENDIF}
  if OrigState<Length(FStates)
    then FStates[OrigState]:=NewState
    else Result:=false;
end;

procedure TCore.DeleteState(State: Cardinal);
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Deleting state #'+IntToStr(State));{$ENDIF}
  if State<Length(FStates) then
  begin
    if State<High(FStates)
      then Move(FStates[State+1], FStates[State], (Length(FStates)-State-1)*SizeOf(TGameState));
    SetLength(FStates, Length(FStates)-1);
  end;
end;

procedure TCore.SwitchState(NewState: Cardinal);
begin
  if NewState = InvalidState then Exit;
  FSwitchTo:=NewState;
  FNeedSwitch:=true;
end;

procedure TCore.SwitchState(const NewStateName: string);
begin
  SwitchState(FindState(NewStateName));
end;

function TCore.StateExists(State: Cardinal): Boolean;
begin
  Result:=(State<Length(FStates)) and Assigned(FStates[State]);
end;

function TCore.GetState(State: Cardinal): TGameState;
begin
  if State<Length(FStates)
    then Result:=FStates[State]
    else Result:=nil;
end;

function TCore.FindState(const Name: string): Cardinal;
var
  i: Cardinal;
begin
  Result:=InvalidState;
  for i:=0 to High(FStates) do
    if Assigned(FStates[i]) and (FStates[i].Name=Name) then
    begin
      Result:=i;
      Exit;
    end;
end;

function TCore.KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean;
var
  T: Cardinal;
begin
  Result:=false;
  if KeyPressed[Key] then
  begin
    T:=Time;
    if (T>KeyVar+Rate) or (KeyVar=0) then
    begin
      Result:=true;
      KeyVar:=T;
    end;
  end
    else KeyVar:=0;
end;

procedure TCore.SetResolution(ResolutionX, ResolutionY, RefreshRate: Cardinal; Fullscreen: Boolean; CanReset: Boolean = true);
var
  OldResX, OldResY, OldRefresh: Cardinal;
  R: TRect;
begin
  //TODO: ���������� �����
  OldResX:=FResolutionX;
  OldResY:=FResolutionY;
  OldRefresh:=FRefreshRate;
  FResolutionX:=ResolutionX;
  FResolutionY:=ResolutionY;
  FRefreshRate:=RefreshRate;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Set resolution %dx%d@%d', [ResolutionX, ResolutionY, RefreshRate]);{$ENDIF}
  Self.Fullscreen:=Fullscreen;
  SendMessage(FHandle, WM_SIZE, 0, ResolutionY shl 16 + ResolutionX);
  if FFullscreen then
  begin
    if not gleGoFullscreen(ResolutionX, ResolutionY, RefreshRate, FColorDepth) then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Unable to set resolution %dx%d@%d', [ResolutionX, ResolutionY, RefreshRate]);{$ENDIF}
      if CanReset
        then SetResolution(OldResX, OldResY, OldRefresh, false)
        else begin
          MessageBox(FHandle, 'Unable to set resolution! Choose lower resolution or refresh rate', PChar(InitSettings.Caption), MB_ICONERROR);
          StopEngine(StopDisplayModeError);
        end;
    end;
  end
  else begin
    GetWindowRect(FHandle, R);
    SetWindowPos(FHandle, 0, (Screen.Width-R.Right+R.Left) div 2, (Screen.Height-R.Bottom+R.Top) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
  SendEvent(TSysNotify.Create(Self, snResolutionChanged));
end;

procedure TCore.MakeScreenshot(Name: string; Format: TImageFormat; Numerate: Boolean = true);
var
  Image: TImage;
  i, Quality: Integer;
begin
  if Numerate then
  begin
    for i:=0 to 99 do
      if (i=99) or not FileExists(ExePath+Name+IntToStrLZ(i, 2)+ImageFormatExtension[Format]) then
      begin
        Name:=ExePath+Name+IntToStrLZ(i, 2)+ImageFormatExtension[Format];
        Break;
      end;
  end
    else Name:=ExePath+Name+ImageFormatExtension[Format];
  Image:=TImage.Create(FResolutionX, FResolutionY, pfBGR24bit, Ceil(FResolutionX*3/4)*4);
  try
    glPixelStore(GL_PACK_ALIGNMENT, 4);
    glReadPixels(0, 0, FResolutionX, FResolutionY, GL_BGR, GL_UNSIGNED_BYTE, Image.Pixels);
    if Format = ifJPEG
      then Quality := 95
      else Quality := 0;
    Image.Save(Name, Format, Quality);
    {$IFDEF VSE_LOG}Log(llInfo, 'Screenshot saved to "'+Name+'"'){$ENDIF};
  finally
    Image.Free;
  end;
end;

procedure TCore.ResetUpdateTimer;
begin
  FPreviousUpdate:=Time;
end;

function TCore.GetFile(const FileName: string): TStream;
begin
  Result:=nil;
  if FileExists(InitSettings.DataDir+FileName) then
    Result:=TFileStream.Create(InitSettings.DataDir+FileName, fmOpenRead or fmShareDenyWrite)
  else SendEvent(TGetFileEvent.Create(Self, FileName, @Result), [erModule]);
end;

function TCore.GetFileText(const FileName: string): TStringList;
var
  F: TStream;
begin
  Result:=nil;
  F:=GetFile(FileName);
  if Assigned(F) then
  try
    Result:=TStringList.Create;
    Result.LoadFromStream(F);
  finally
    FAN(F);
  end;
end;

//Private

procedure TCore.SetFullscreen(Value: Boolean);
begin
  //TODO: ���������� �����
  if FFullscreen=Value then Exit;
  FFullscreen:=Value;
  if Value
    then FFullscreen:=gleGoFullscreen(FResolutionX, FResolutionY, FRefreshRate, FColorDepth)
    else gleGoBack;
  {$IFDEF VSE_LOG}if FFullscreen<>Value then Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');{$ENDIF}
  if FFullscreen then
  begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW);
    SetWindowLong(FHandle, GWL_STYLE, Integer(WS_POPUP) or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
    SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
  end
  else begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_WINDOWEDGE);
    SetWindowLong(FHandle, GWL_STYLE, WindowedWindowStyle);
    SetWindowPos(FHandle, HWND_NOTOPMOST, (Screen.Width-FResolutionX) div 2, (Screen.Height-FResolutionY) div 2, 0, 0, SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
  end;
end;

function TCore.GetCaption: string;
begin
  SetLength(Result, GetWindowTextLength(FHandle));
  SetLength(Result, GetWindowText(FHandle, PChar(Result), Length(Result)));
end;

procedure TCore.SetCaption(const Value: string);
begin
  SetWindowText(FHandle, PChar(Value));
end;

function TCore.GetVSync: Boolean;
begin
  Result:=false;
  if WGL_EXT_swap_control then Result:=wglGetSwapIntervalEXT<>0;
end;

procedure TCore.SetVSync(Value: Boolean);
const
  VSync: array[Boolean] of Integer=(0, 1);
begin
  if WGL_EXT_swap_control then wglSwapIntervalEXT(VSync[Value]);
end;

procedure TCore.SetState(Value: Cardinal);
begin
  {$IFDEF VSE_LOG}if StateExists(FState) and StateExists(Value)
    then LogF(llInfo, 'Switch state from %s to %s', [FStates[FState].Name, FStates[Value].Name])
    else if StateExists(Value)
      then Log(llInfo, 'Switch state to '+FStates[Value].Name);{$ENDIF}
  FNeedSwitch:=false;
  if (FState=Value) or (Value>High(FStates)) then Exit;
  if FCurState<>nil then
  begin
    try
      FCurState.Deactivate;
    except
      LogException('in state '+FCurState.Name+'.Deactivate');
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
    FPrevStateName:=FCurState.Name;
  end;
  SendEvent(TSysNotify.Create(Self, snStateChanged), [erModule]);
  FState:=Value;
  FCurState:=FStates[Value];
  try
    FUpdInt:=FCurState.Activate;
  except
    if FUpdInt<=1 then FUpdInt:=50;
    LogException('in state '+FCurState.Name+'.Activate');
    {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
  end;
end;

function TCore.GetKeyPressed(Index: Byte): Boolean;
begin
  Result:=(FKeyState[Index]>127) and not FMinimized;
end;

procedure TCore.SetMouseCapture(Value: Boolean);
begin
  if Value=FMouseCapture then Exit;
  FMouseCapture:=Value;
  if Value then
  begin
    GetCursorPos(FSavedMousePos);
    ResetMouse;
    SetCapture(FHandle);
    ShowCursor(false);
  end
  else begin
    ReleaseCapture;
    with FSavedMousePos do
      SetCursorPos(X, Y);
    ShowCursor(true);
  end;
end;

function TCore.GetMouseCursor: TPoint;
begin
  GetCursorPos(Result);
  ScreenToClient(FHandle, Result);
end;

function TCore.GetTime: Cardinal;
var
  T: Int64;
begin
  if QueryPerformanceCounter(T) then
    Result := 1000 * (T div FHPETFreq) + (1000 * (T mod FHPETFreq)) div FHPETFreq
  else
    Result := GetTickCount;
end;

procedure TCore.ResetMouse;
var
  Cur: TPoint;
begin
  Cur := Point(FResolutionX div 2, FResolutionY div 2);
  ClientToScreen(FHandle, Cur);
  SetCursorPos(Cur.X, Cur.Y);
end;

{$IFDEF VSE_CONSOLE}
const
  BoolState: array[Boolean] of string = ('off', 'on');

function TCore.FullscreenHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1
    then Fullscreen:=Boolean(Args[1].VInteger)
    else Console.WriteLn('Fullscreen: '+BoolState[Fullscreen]);
  Result:=true;
end;

function TCore.QuitHandler(Sender: TObject; Args: array of const): Boolean;
begin
  StopEngine;
  Result:=true;
end;

function TCore.ResolutionHandler(Sender: TObject; Args: array of const): Boolean;
begin
  Result:=true;
  case Length(Args) of
    1: Console.WriteLn(Format('Resolution: %dx%d@%d', [ResolutionX, ResolutionY, RefreshRate]));
    3: SetResolution(Args[1].VInteger, Args[2].VInteger, RefreshRate, Fullscreen);
    4: SetResolution(Args[1].VInteger, Args[2].VInteger, Args[3].VInteger, Fullscreen);
    else begin
      Console.WriteLn('Invalid arguments' + PostfixError);
      Result:=false;
    end;
  end;
end;

function TCore.ScreenshotHandler(Sender: TObject; Args: array of const): Boolean;
begin
  case Length(Args) of
    1: MakeScreenshot(ChangeFileExt(ExtractFileName(ExeName), '')+'_screenshot', ifPNG);
    2: MakeScreenshot(string(Args[1].VAnsiString), ifPNG, false);
    else MakeScreenshot(string(Args[1].VAnsiString), TImageFormat(Args[2].VInteger), false);
  end;
  Result:=true;
end;

function TCore.StateHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1
    then SwitchState(string(Args[1].VAnsiString))
    else Console.WriteLn('Current state: '+CurState.Name);
  Result:=true;
end;

function TCore.VSyncHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1
    then VSync:=Boolean(Args[1].VInteger)
    else Console.WriteLn('VSync: '+BoolState[VSync]);
  Result:=true;
end;
{$ENDIF}

{TSettings}

constructor TSettings.Create;
var
  IniName: string;
begin
  inherited;
  IniName:=ChangeFileExt(FullExeName, '.ini');
  FFirstRun:=not FileExists(IniName);
  InitSettings.DataDir:=ExePath+'data\';
  {$IFNDEF VSE_NO_INI}
  FIni:=TIniFile.Create(IniName);
  if not FFirstRun then
    ReloadInitSettings;
  {$ENDIF}
end;

destructor TSettings.Destroy;
begin
  FAN(FIni);
  inherited;
end;

procedure TSettings.ReloadInitSettings;
begin
  {$IFNDEF VSE_NO_INI}
  with InitSettings do
  begin
    {$IFDEF VSE_LOG}Log(llInfo, 'Loading settings from ini file');{$ENDIF}
    ResolutionX:=FIni.ReadInteger(SSectionSettings, SNameResolutionX, ResolutionX);
    ResolutionY:=FIni.ReadInteger(SSectionSettings, SNameResolutionY, ResolutionY);
    RefreshRate:=FIni.ReadInteger(SSectionSettings, SNameRefreshRate, RefreshRate);
    ColorDepth:=FIni.ReadInteger(SSectionSettings, SNameColorDepth, ColorDepth);
    Fullscreen:=FIni.ReadBool(SSectionSettings, SNameFullscreen, Fullscreen);
    VSync:=FIni.ReadBool(SSectionSettings, SNameVSync, VSync);
  end;
  {$ENDIF}
end;

procedure TSettings.EraseSection(const Section: string);
begin
  if Assigned(FIni) then FIni.EraseSection(Section);
end;

function TSettings.GetBool(const Section, Name: string): Boolean;
begin
  if Assigned(FIni) then
    Result:=FIni.ReadBool(Section, Name, false)
  else
    Result:=false;
end;

function TSettings.GetInt(const Section, Name: string): Integer;
begin
  if Assigned(FIni) then
    Result:=FIni.ReadInteger(Section, Name, 0)
  else
    Result:=0;
end;

function TSettings.GetStr(const Section, Name: string): string;
begin
  if Assigned(FIni) then
    Result:=FIni.ReadString(Section, Name, '')
  else
    Result:='';
end;

function TSettings.ReadSection(const Section: string): TStringList;
begin
  Result:=TStringList.Create;
  if Assigned(FIni) then FIni.ReadSectionValues(Section, Result);
end;

procedure TSettings.SetBool(const Section, Name: string; const Value: Boolean);
begin
  if Assigned(FIni) then FIni.WriteBool(Section, Name, Value);
end;

procedure TSettings.SetInt(const Section, Name: string; const Value: Integer);
begin
  if Assigned(FIni) then FIni.WriteInteger(Section, Name, Value);
end;

procedure TSettings.SetStr(const Section, Name: string; const Value: string);
begin
  if Assigned(FIni) then FIni.WriteString(Section, Name, Value);
end;

{Procedures}

function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WndWidth, WndHeight: Word;
begin
  Result:=0;
  case (Msg) of
    WM_ACTIVATE:
      begin
        if not Assigned(Core) then
        begin
          {$IFDEF VSE_LOG}Log(llInfo, 'Creating engine');
          {$IFDEF VSE_DEBUG}Log(llInfo, 'Debug mode');{$ENDIF}{$ENDIF}
          try
            Core:=TCore.Create(hWnd);
            Core.StartEngine;
            {$IFDEF VSE_LOG}Log(llInfo, 'Engine created');{$ENDIF}
          except
            LogException('while initializing engine');
            VSEStopState:=StopInitError;
            SendMessage(hWnd, UM_STOPENGINE, 0, 0);
          end;
        end;
      end;
    WM_KEYUP: Core.KeyEvent(wParam, keUp);
    WM_KEYDOWN: Core.KeyEvent(wParam, keDown);
    WM_CHAR: Core.CharEvent(Chr(wParam));
    WM_MOUSEMOVE: Core.MouseEvent(0, meMove, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONDOWN: Core.MouseEvent(mbLeft, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONUP: Core.MouseEvent(mbLeft, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONDOWN: Core.MouseEvent(mbRight, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONUP: Core.MouseEvent(mbRight, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONDOWN: Core.MouseEvent(mbMiddle, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONUP: Core.MouseEvent(mbMiddle, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONDOWN: Core.MouseEvent(3+HiWord(wParam), meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONUP: Core.MouseEvent(3+HiWord(wParam), meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MOUSEWHEEL: Core.MouseEvent(SmallInt(HiWord(wParam)) div 120, meWheel, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    {$IFDEF VSE_USE_ALT_ENTER}
    WM_SYSKEYUP: if Assigned(Core) and (wParam=VK_RETURN) and (lParam and (1 shl 29) <> 0) then
      Core.Fullscreen:=not Core.Fullscreen;
    {$ENDIF}
    WM_DESTROY:
      begin
        {$IFDEF VSE_LOG}Log(llInfo, 'Destroying engine');{$ENDIF}
        try
          FAN(Core);
          {$IFDEF VSE_LOG}Log(llInfo, 'Engine destroyed');{$ENDIF}
        except
          LogException('while destroying engine');
          VSEStopState:=StopInternalError;
        end;
        PostQuitMessage(Integer(VSEStopState));
        Result:=0;
      end;
    WM_QUERYENDSESSION:
      begin
        {$IFDEF VSE_LOG}Log(llInfo, 'Received WM_QUERYENDSESSION');{$ENDIF}
        Core.StopEngine;
        Result:=1;
      end;
    WM_SIZE:
      try
        if Assigned(Core) then begin
          if Core.Fullscreen then
          begin
            WndWidth:=Core.ResolutionX;
            WndHeight:=Core.ResolutionY;
          end
          else begin
            WndWidth:=Core.ResolutionX+GetSystemMetrics(SM_CXDLGFRAME)*2;
            WndHeight:=Core.ResolutionY+GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYDLGFRAME)*2;
          end;
          gleResizeWnd(Core.ResolutionX, Core.ResolutionY);
          if Core.Fullscreen
            then SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, WndWidth, WndHeight, 0)
            else SetWindowPos(hWnd, HWND_TOP, 0, 0, WndWidth, WndHeight, SWP_NOMOVE or SWP_FRAMECHANGED);
        end;
        Result:=0;
      except
        LogException('while resizing window');
        {$IFNDEF VSE_DEBUG}Core.StopEngine(StopInternalError);{$ENDIF}
      end;
    UM_STOPENGINE: DestroyWindow(hWnd);  
    else
      Result:=DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;

function IsRunning(const ID: string): Boolean;
begin
  Result:=false;
  if Mutex<>0 then Exit;
  Mutex:=CreateMutex(nil, true, PChar(ID));
  if GetLastError=ERROR_ALREADY_EXISTS then Result:=true;
end;

function VSEStart: TStopState;
var
  WndClass: TWndClass;
  Handle: THandle;
  Msg: TMsg;
  Fin: Boolean;
begin
  Result:=StopInitError;
  if IsRunning(InitSettings.Caption) then Exit;
  VSEStopState:=StopDefault;
  {$IFDEF VSE_LOG}
  LogRaw(llInfo, '');
  Log(llInfo, InitSettings.Caption+' '+InitSettings.Version+' started');
  Log(llInfo, VSECaptVer);
  {$ENDIF}
  Set8087CW($133F);
  Fin:=false;
  ZeroMemory(@WndClass, SizeOf(WndClass));
  with WndClass do
  begin
    style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
    lpfnWndProc:=@WndProc;
    hInstance:=SysInit.hInstance;
    hCursor:=LoadCursor(0, IDC_ARROW);
    hIcon:=LoadIcon(hInstance, 'MAINICON');
    lpszClassName:=WndClassName;
    hbrBackground:=0;
  end;    
  if Windows.RegisterClass(WndClass)=0 then
  begin
    LogErrorAndShowMessage('Failed to register the window class');
    Exit;
  end;
  Handle:=CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE, WndClassName, PChar(InitSettings.Caption),
    WindowedWindowStyle, 0, 0, 800, 600, 0, 0, hInstance, nil);
  if Handle=0 then
  begin
    LogErrorAndShowMessage('Unable to create window');
    Exit;
  end;
  SendMessage(Handle, WM_SETICON, ICON_SMALL, LoadIcon(hInstance, IDI_APPLICATION));
  SendMessage(Handle, WM_SETICON, ICON_BIG, LoadIcon(hInstance, IDI_APPLICATION));
  ShowWindow(Handle, SW_SHOW);
  try
    while not Fin do
    begin
      if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
      begin
        if (Msg.message=WM_QUIT)
          then Fin:=true
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end
      else
        if Core<>nil then
        begin
          if Core.Minimized and (GetForegroundWindow=Handle)
            then Core.Resume;
          if Core.Paused
            then Sleep(50)
            else Core.Update;
        end;
    end;
    Result:=TStopState(Msg.wParam);
  except
    LogException('in main loop');
    Result:=StopInternalError;
  end;
  {$IFDEF VSE_LOG}
  if Result<>StopNormal then
  begin
    LogF(llInfo, 'Engine stopped with error code %d (%s)', [Ord(Result), StopCodeNames[Result]]);
  end;
  {$ENDIF}
  UnregisterClass(WndClassName, hInstance);
end;

procedure RegisterModule(Module: CModule);
begin
  SetLength(Modules, Length(Modules)+1);
  Modules[High(Modules)]:=Module;
end;

initialization
  Settings:=TSettings.Create;

finalization
  Finalize(Modules);
  FAN(Settings);
  ReleaseMutex(Mutex);

end.
