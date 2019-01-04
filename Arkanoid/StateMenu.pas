unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, VSEGUI, StateStart, StateGame;

type
  TStateMenu=class;
  TMainMenu=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FResumeButton: Integer;
    procedure GameClick(Btn: PBtn);
    procedure OptionsClick(Btn: PBtn);
    procedure CredsClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu);
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TDiffMenu = class(TGUIForm)
  protected
    FParent: TStateMenu;
    procedure BtnClick(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu);
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TOptions=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FLResolution, FLRefreshRate, FLColorDepth, FLMouseSens, FLCacheSize,
      FCFullscreen, FCVSync, FCEnableBGM, FBToggleCache, FBClearCache,
      FCurrentResolution, FCurrentRefreshRate, FColorDepth, FMouseSens: Integer;
    FResolutions: TResolutions;
    procedure DrawForm(State: TBtnState); override;
    procedure ResClick(Btn: PBtn);
    procedure RefrClick(Btn: PBtn);
    procedure DepthClick(Btn: PBtn);
    procedure SensClick(Btn: PBtn);
    procedure ToggleCache(Btn: PBtn);
    procedure ClearCache(Btn: PBtn);
    procedure OKClick(Btn: PBtn);
    procedure CancelClick(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ReadOptions;
  end;
  TTextView=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FText: TStringList;
    procedure DrawForm(State: TBtnState); override;
    procedure Close(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu; const Caption, TextFile: string);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TStateMenu=class(TGameState)
  private
    FMainMenu: TMainMenu;
    FDiffMenu: TDiffMenu;
    FOptions: TOptions;
    FTextView: TTextView;
    FCurFrm: TGUIForm;
    FStart: TStateStart;
    FGame: TStateGame;
    {$IFDEF VSE_CONSOLE}
    function MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
  end;

const
  UIFont = 'Tahoma';
  UIFontSize = 12;
  GameTitle = 'Arkanoid 64k';
  GameVer = '1.0';

var
  BgTex: Cardinal;

implementation

uses VSESound, VSETexMan, VSERender2D, VSEImageCodec
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{TMainMenu}

var
  MainMenuItems: array[0..4] of TMenuItem = (
    (Caption: 'New game'; Tag: 1),
    (Caption: 'Resume'; Tag: 0),
    (Caption: 'Options'; Tag: 0),
    (Caption: 'Credits'; Tag: 0),
    (Caption: 'Exit'; Tag: 0));

constructor TMainMenu.Create(Parent: TStateMenu);
begin
  inherited Create(300, 150, 200, 300);
  FParent:=Parent;
  FCaption:=GameTitle;
  MainMenuItems[0].OnClick:=GameClick;
  MainMenuItems[1].OnClick:=GameClick;
  MainMenuItems[2].OnClick:=OptionsClick;
  MainMenuItems[3].OnClick:=CredsClick;
  MainMenuItems[4].OnClick:=ExitClick;
  FResumeButton:=CreateMenu(Self, 30, 50, 140, 30, 20, MainMenuItems)[1];
  Button[FResumeButton].Enabled:=false;
end;

procedure TMainMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp)
    then if Self.Button[FResumeButton].Enabled
      then Core.SwitchState('Game')
      else Core.StopEngine
    else inherited KeyEvent(Key, Event);
end;

procedure TMainMenu.ResumeEnable(Enable: Boolean);
begin
  Button[FResumeButton].Enabled:=Enable;
end;

procedure TMainMenu.GameClick(Btn: PBtn);
begin
  if Btn.Tag=1 then
  begin
    FParent.FCurFrm:=FParent.FDiffMenu;
  end
    else Core.SwitchState('Game');
end;

procedure TMainMenu.OptionsClick(Btn: PBtn);
begin
  FParent.FOptions.ReadOptions;
  FParent.FCurFrm:=FParent.FOptions;
end;

procedure TMainMenu.CredsClick(Btn: PBtn);
begin
  if not Assigned(FParent.FTextView) then
    FParent.FTextView:=TTextView.Create(FParent, Btn.Caption, 'Creds.txt');
  FParent.FCurFrm:=FParent.FTextView;
end;

procedure TMainMenu.ExitClick(Btn: PBtn);
begin
  Core.StopEngine;
end;

{TDiffMenu}

var
  DiffMenuItems: array[0..2] of TMenuItem = (
    (Caption: 'Easy'; Tag: 5),
    (Caption: 'Normal'; Tag: 3),
    (Caption: 'Hard'; Tag: 1));

constructor TDiffMenu.Create(Parent: TStateMenu);
var
  i: Integer;
begin
  inherited Create(300, 200, 200, 200);
  FParent:=Parent;
  FCaption:='Difficulty';
  for i:=Low(DiffMenuItems) to High(DiffMenuItems) do
    DiffMenuItems[i].OnClick:=BtnClick;
  CreateMenu(Self, 30, 50, 140, 30, 20, DiffMenuItems);
end;

procedure TDiffMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp)
    then FParent.FCurFrm:=FParent.FMainMenu
    else inherited KeyEvent(Key, Event);
end;

procedure TDiffMenu.BtnClick(Btn: PBtn);
begin
  FParent.FGame.NewGame(Btn.Tag);
  Core.SwitchState('Game');
  FParent.FCurFrm:=FParent.FMainMenu;
end;

{TOptions}

const
  CacheState: array[Boolean] of string = ('Enable cache', 'Disable cache');
  SCacheSize='Cache: ';

constructor TOptions.Create(Parent: TStateMenu);
var
  Btn: TBtn;
  Lbl: TLbl;
begin
  inherited Create(200, 130, 400, 350);
  FParent:=Parent;
  FCaption:='Options';
  FResolutions:=gleGetResolutions;
  FLResolution:=CreateSelect(Self, 10, 60, 190, 20, ResClick, '-', '+');
  FLRefreshRate:=CreateSelect(Self, 10, 110, 190, 20, RefrClick, '-', '+');
  FLColorDepth:=CreateSelect(Self, 10, 160, 190, 20, DepthClick, '-', '+');
  FLMouseSens:=CreateSelect(Self, 10, 210, 190, 20, SensClick, '-', '+');
  with Btn do
  begin
    Enabled:=true;
    X:=220;
    Y:=60;
    Width:=160;
    Height:=30;
    Tag:=0;
    Type_:=btPush;
    OnClick:=ToggleCache;
    FBToggleCache:=AddButton(Btn);
    Y:=100;
    Caption:='Clear cache';
    OnClick:=ClearCache;
    FBClearCache:=AddButton(Btn);
    X:=10;
    Y:=250;
    Width:=200;
    Height:=20;
    Type_:=btCheck;
    OnClick:=nil;
    Caption:='Fullscreen';
    FCFullscreen:=AddButton(Btn);
    Y:=280;
    Caption:='V. Sync';
    FCVSync:=AddButton(Btn);
    X:=220;
    Y:=250;
    Width:=160;
    Caption:='Music';
    FCEnableBGM:=AddButton(Btn);
    X:=140;
    Y:=310;
    Width:=120;
    Height:=30;
    Type_:=btPush;
    Caption:='OK';
    OnClick:=OKClick;
    AddButton(Btn);
    X:=270;
    Caption:='Cancel';
    OnClick:=CancelClick;
    AddButton(Btn);
  end;
  with Lbl do
  begin
    Align:=laCenter;
    Color:=0;
    X:=10;
    Y:=38;
    Width:=190;
    Caption:='Resolution';
    AddLabel(Lbl);
    Y:=88;
    Caption:='Refresh rate';
    AddLabel(Lbl);
    Y:=138;
    Caption:='Color depth';
    AddLabel(Lbl);
    Y:=188;
    Caption:='Mouse sensivity';
    AddLabel(Lbl);
    X:=220;
    Y:=38;
    Width:=160;
    Align:=laLeft;
    Caption:='';
    FLCacheSize:=AddLabel(Lbl);
  end;
end;

destructor TOptions.Destroy;
begin
  Finalize(FResolutions);
  inherited Destroy;
end;

procedure TOptions.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp)
    then FParent.FCurFrm:=FParent.FMainMenu
    else inherited KeyEvent(Key, Event);
end;

procedure TOptions.ReadOptions;
var
  i: Integer;
begin
  Button[FCFullscreen].Checked:=Core.Fullscreen;
  Button[FCVSync].Checked:=Core.VSync;
  Button[FCEnableBGM].Checked:=Sound.EnableBGM;
  FColorDepth:=Core.ColorDepth;
  FMouseSens:=FParent.FGame.MouseSens;
  FCurrentResolution:=-1;
  for i:=0 to High(FResolutions) do
    if (FResolutions[i].Width=Core.ResolutionX) and (FResolutions[i].Height=Core.ResolutionY) then FCurrentResolution:=i;
  if FCurrentResolution=-1 then
  begin
    FCurrentResolution:=Length(FResolutions);
    SetLength(FResolutions, FCurrentResolution+1);
    with FResolutions[FCurrentResolution] do
    begin
      Width:=Core.ResolutionX;
      Height:=Core.ResolutionY;
      SetLength(RefreshRates, 1);
      RefreshRates[0]:=Core.RefreshRate;
    end;
  end;
  FCurrentRefreshRate:=-1;
  for i:=0 to High(FResolutions[FCurrentResolution].RefreshRates) do
    if FResolutions[FCurrentResolution].RefreshRates[i]=Core.RefreshRate then FCurrentRefreshRate:=i;
  if FCurrentRefreshRate=-1 then
  begin
    FCurrentRefreshRate:=Length(FResolutions[FCurrentResolution].RefreshRates);
    SetLength(FResolutions[FCurrentResolution].RefreshRates, FCurrentRefreshRate+1);
    FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]:=Core.RefreshRate;
  end;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.DrawForm(State: TBtnState);
begin
  Lbl[FLResolution].Caption:=Format('%dx%d', [FResolutions[FCurrentResolution].Width, FResolutions[FCurrentResolution].Height]);
  Lbl[FLRefreshRate].Caption:=IntToStr(FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]);
  Lbl[FLColorDepth].Caption:=IntToStr(FColorDepth);
  Lbl[FLMouseSens].Caption:=IntToStr(FMouseSens);
  inherited;
end;

procedure TOptions.ResClick(Btn: PBtn);
begin
  FCurrentResolution:=Max(0, Min(FCurrentResolution+Btn.Tag, High(FResolutions)));
  FCurrentRefreshRate:=0;
end;

procedure TOptions.RefrClick(Btn: PBtn);
begin
  FCurrentRefreshRate:=Max(0, Min(FCurrentRefreshRate+Btn.Tag, High(FResolutions[FCurrentResolution].RefreshRates)));
end;

procedure TOptions.DepthClick(Btn: PBtn);
const
  Depth: array[-1..1] of Integer = (16, 0, 32);
begin
  FColorDepth:=Depth[Btn.Tag];
end;

procedure TOptions.SensClick(Btn: PBtn);
begin
  FMouseSens:=Max(1, Min(FMouseSens+Btn.Tag, 10));
end;

procedure TOptions.ToggleCache(Btn: PBtn);
begin
  if UseCache then
  begin
    UseCache:=false;
    DeleteDir(CacheDir);
  end
  else begin
    UseCache:=true;
    CreateDir(CacheDir);
  end;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.ClearCache(Btn: PBtn);
begin
  FParent.FStart.ClearCache;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
end;

procedure TOptions.OKClick(Btn: PBtn);
begin
  Core.SetResolution(FResolutions[FCurrentResolution].Width,
                     FResolutions[FCurrentResolution].Height,
                     FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate],
                     Button[FCFullscreen].Checked, true);
  Core.VSync:=Button[FCVSync].Checked;
  Core.ColorDepth:=FColorDepth;
  Sound.EnableBGM:=Button[FCEnableBGM].Checked;
  FParent.FGame.MouseSens:=FMouseSens;
  FParent.FCurFrm:=FParent.FMainMenu;
end;

procedure TOptions.CancelClick(Btn: PBtn);
begin
  FParent.FCurFrm:=FParent.FMainMenu;
end;

{TTextView}

constructor TTextView.Create(Parent: TStateMenu; const Caption, TextFile: string);
var
  Btn: TBtn;
begin
  inherited Create(80, 60, 640, 480);
  FParent:=Parent;
  FCaption:=Caption;
  FText:=Core.GetFileText(TextFile);
  with Btn do
  begin
    Type_:=btPush;
    X:=535;
    Y:=445;
    Width:=100;
    Height:=30;
    Enabled:=true;
    Caption:='Close';
    OnClick:=Close;
    AddButton(Btn);
  end;
end;

destructor TTextView.Destroy;
begin
  FAN(FText);
  inherited Destroy;
end;

procedure TTextView.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Event=keDown) and (Key=VK_ESCAPE) then Close(nil);
  inherited KeyEvent(Key, Event);
end;

procedure TTextView.DrawForm(State: TBtnState);
var
  i, Left: Integer;
  S: string;
begin
  inherited;
  gleColor(clText);
  for i:=0 to Min(FText.Count-1, 24) do
  begin
    S:=FText[i];
    Left:=10;
    if (S<>'') and (S[1]=#9) then
    begin
      S:=Copy(S, 2, MaxInt);
      Inc(Left, 310-Render2D.TextWidth(Font, S) div 2);
    end;
    Render2D.TextOut(Font, Left, 35+16*i, S);
  end;
end;

procedure TTextView.Close(Btn: PBtn);
begin
  FParent.FCurFrm:=FParent.FMainMenu;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['menubg file=s']:=MenuBgHandler;
  {$ENDIF}
  SetGUIFont(UIFont, UIFontSize, true);
  FMainMenu:=TMainMenu.Create(Self);
  FDiffMenu:=TDiffMenu.Create(Self);
  FOptions:=TOptions.Create(Self);
  FCurFrm:=FMainMenu;
  FStart:=TStateStart(Core.GetState(Core.FindState('Start')));
  FGame:=TStateGame(Core.GetState(Core.FindState('Game')));
end;

destructor TStateMenu.Destroy;
begin
  FAN(FMainMenu);
  FAN(FOptions);
  FAN(FTextView);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if FGame.CanResumeGame then FGame.Draw
    else if BgTex<>0 then
    begin
      TexMan.Bind(BgTex);
      Render2D.Enter;
      gleColor(clWhite);
      with Render2D.VSBounds do
        Render2D.DrawRect(Left, Top, Right - Left, Bottom - Top, 0, 0, 1, 1);
      Render2D.Leave;
      TexMan.Unbind;
    end;
  FCurFrm.Draw;
end;

procedure TStateMenu.Update;
begin
  inherited;
  if not FCurFrm.Movable then FCurFrm.Movable:=true;
  FCurFrm.Update;
end;

function TStateMenu.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  FMainMenu.ResumeEnable(FGame.CanResumeGame);
end;

procedure TStateMenu.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  inherited;
  FCurFrm.MouseEvent(Button, Event, X, Y);
end;

procedure TStateMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  inherited;
  FCurFrm.KeyEvent(Key, Event);
end;

procedure TStateMenu.CharEvent(C: Char);
begin
  inherited;
  FCurFrm.CharEvent(C);
end;

function TStateMenu.GetName: string;
begin
  Result:='Menu';
end;

function TStateMenu.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if Notify=snConsoleActive then Result:=true;
end;

{$IFDEF VSE_CONSOLE}
function TStateMenu.MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
var
  Image: TImage;
begin
  Result:=false;
  if FileExists(ExePath + string(Args[1].VAnsiString)) then
  begin
    Image:=TImage.Create;
    try
      Image.Load(ExePath + string(Args[1].VAnsiString));
      BgTex:=TexMan.AddTexture('MenuBg', Image, true, false);
    finally
      Image.Free;
    end;
    Result:=true;
  end
  {$IFDEF VSE_LOG}else LogF(llError, 'StateMenu.MenuBg: file "%s" not found', [string(Args[1].VAnsiString)]){$ENDIF};
end;
{$ENDIF}

end.
