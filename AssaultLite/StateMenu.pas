unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore,
  VSEGUI, VSEFormManager, VSEForms, StateStart, StateLoad, StateGame;

type
  TStateMenu=class;
  TMainMenu=class(TAlignedForm)
  protected
    FResumeButton: Integer;
    procedure GameClick(Btn: PBtn);
    procedure OptionsClick(Btn: PBtn);
    procedure TextClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  public
    constructor Create;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TOptions=class(TOptionsForm)
  protected
    FLCacheSize, FCEnableBGM, FBToggleCache, FBClearCache: Integer;
    procedure ToggleCache(Btn: PBtn);
    procedure ClearCache(Btn: PBtn);
    procedure KeyConfig(Btn: PBtn);
    procedure OKClick(Btn: PBtn); override;
  public
    constructor Create;
  end;
  TStateMenu=class(TGameState)
  private
    FFormsSet: TGUIFormsSet;
    FGame: TStateGame;
    FBgTex: Cardinal;
    {$IFDEF VSE_CONSOLE}
    function MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
  end;

const
  SIDMenu = 'Menu';
  UIFont = 'Tahoma';

implementation

uses VSESound, VSETexMan, VSERender2D, VSEImageCodec
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{TMainMenu}

const
  IDKeyConfig = 'KeyConfig';
  IDMainMenu = 'MainMenu';
  IDOptions = 'Options';
  IDTextView = 'TextView';
  TextFiles: array[0..1] of string = ('Creds.txt', 'Help.txt');

var
  MainMenuItems: array[0..5] of TMenuItem = (
    (Caption: 'New game'; Tag: 1),
    (Caption: 'Resume'; Tag: 0),
    (Caption: 'Options'; Tag: 0),
    (Caption: 'Credits'; Tag: 0),
    (Caption: 'Help'; Tag: 1),
    (Caption: 'Exit'; Tag: 0));

constructor TMainMenu.Create;
begin
  inherited Create(0, 0, 200, 350);
  Alignment:=[faCenter, faMiddle];
  FCaption:='Assault Lite';
  MainMenuItems[0].OnClick:=GameClick;
  MainMenuItems[1].OnClick:=GameClick;
  MainMenuItems[2].OnClick:=OptionsClick;
  MainMenuItems[3].OnClick:=TextClick;
  MainMenuItems[4].OnClick:=TextClick;
  MainMenuItems[5].OnClick:=ExitClick;
  FResumeButton:=CreateMenu(Self, 30, 50, 140, 30, 20, MainMenuItems)[1];
  Button[FResumeButton].Enabled:=false;
end;

procedure TMainMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp) then
    if Self.Button[FResumeButton].Enabled then
       Core.SwitchState(SIDGame)
    else
      Core.StopEngine
  else
    inherited KeyEvent(Key, Event);
end;

procedure TMainMenu.ResumeEnable(Enable: Boolean);
begin
  Button[FResumeButton].Enabled:=Enable;
end;

procedure TMainMenu.GameClick(Btn: PBtn);
begin
  if Btn.Tag=1 then
  begin
    (Core.GetState(Core.FindState(SIDLoad)) as TStateLoad).LevelName:='L0';
    Core.SwitchState(SIDLoad);
  end
    else Core.SwitchState(SIDGame);
end;

procedure TMainMenu.OptionsClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDOptions, TOptions.Create, Name);
  FormManager.Show(IDOptions);
end;

procedure TMainMenu.TextClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDTextView, TTextView.Create(640, 480, Btn.Caption, 'Close', Core.GetFileText(TextFiles[Btn.Tag])), Name);
  FormManager.Show(IDTextView);
end;

procedure TMainMenu.ExitClick(Btn: PBtn);
begin
  Core.StopEngine;
end;                 

{TOptions}

const
  CacheState: array[Boolean] of string = ('Enable cache', 'Disable cache');
  SCacheSize='Cache: ';

constructor TOptions.Create;
var
  Btn: TBtn;
  Lbl: TLbl;
begin
  inherited Create(400, 350);
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
    Y:=150;
    Caption:='Controls';
    OnClick:=KeyConfig;
    AddButton(Btn);
    X:=220;
    Y:=200;
    Width:=160;
    Height:=20;
    Type_:=btCheck;
    OnClick:=nil;
    Caption:='Music';
    FCEnableBGM:=AddButton(Btn);
  end;
  with Lbl do
  begin
    Color:=0;
    X:=220;
    Y:=38;
    Width:=160;
    Align:=laLeft;
    Caption:='';
    FLCacheSize:=AddLabel(Lbl);
  end;
  Button[FCEnableBGM].Checked:=Sound.EnableBGM;
  Self.Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.ToggleCache(Btn: PBtn);
begin
  (Core.GetState(Core.FindState(SIDStart)) as TStateStart).SetCache(not UseCache);
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.ClearCache(Btn: PBtn);
begin
  (Core.GetState(Core.FindState(SIDStart)) as TStateStart).ClearCache;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
end;

procedure TOptions.KeyConfig(Btn: PBtn);
begin
  FParentSet.AddForm(IDKeyConfig, TBindManCfgForm.Create(400, 350, 'Default', 'OK'), Name);
  FormManager.Show(IDKeyConfig);
end;

procedure TOptions.OKClick(Btn: PBtn);
begin
  Sound.EnableBGM:=Button[FCEnableBGM].Checked;
  inherited;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['menubg file=s']:=MenuBgHandler;
  {$ENDIF}
  FFormsSet:=TGUIFormsSet.Create;
  FFormsSet.AddForm(IDMainMenu, TMainMenu.Create);
  FGame:=TStateGame(Core.GetState(Core.FindState('Game')));
end;

destructor TStateMenu.Destroy;
begin
  FAN(FFormsSet);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if FGame.CanResumeGame then FGame.Draw
    else if FBgTex<>0 then
    begin
      TexMan.Bind(FBgTex);
      Render2D.Enter;
      gleColor(clWhite);
      with Render2D.VSBounds do
        Render2D.DrawRect(Left, Top, Right - Left, Bottom - Top, 0, 0, 1, 1);
      Render2D.Leave;
      TexMan.Unbind;
    end;
end;

function TStateMenu.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  FormManager.FormsSet := FFormsSet;
  (FormManager[IDMainMenu] as TMainMenu).ResumeEnable(FGame.CanResumeGame);
end;

procedure TStateMenu.Deactivate;
begin
  FormManager.FormsSet:=nil;
end;

function TStateMenu.GetName: string;
begin
  Result:=SIDMenu;
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
      FBgTex:=TexMan.AddTexture('MenuBg', Image, true, false);
    finally
      Image.Free;
    end;
    Result:=true;
  end
  {$IFDEF VSE_LOG}else LogF(llError, 'StateMenu.MenuBg: file "%s" not found', [string(Args[1].VAnsiString)]){$ENDIF};
end;
{$ENDIF}

end.
