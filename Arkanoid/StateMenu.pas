unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, VSEGUI, VSEFormManager, VSEForms, StateStart, StateGame, StateGameEnd;

type
  TStateMenu=class;
  TMainMenu=class(TAlignedForm)
  protected
    FResumeButton: Integer;
    procedure GameClick(Btn: PBtn);
    procedure ScoresClick(Btn: PBtn);
    procedure OptionsClick(Btn: PBtn);
    procedure CredsClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  public
    constructor Create;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TDiffMenu=class(TAlignedForm)
  protected
    procedure BtnClick(Btn: PBtn);
  public
    constructor Create;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TScores = class(TAlignedForm)
  protected
    FScores: array[0..ScoresCount-1] of Integer;
    procedure ClearClick(Btn: PBtn);
    procedure CloseClick(Btn: PBtn);
    procedure DrawForm(State: TBtnState); override;
  public
    constructor Create;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TOptions=class(TOptionsForm)
  protected
    FLCacheSize, FLMouseSens, FCEnableBGM, FBToggleCache, FBClearCache, FMouseSens: Integer;
    procedure DrawForm(State: TBtnState); override;
    procedure SensClick(Btn: PBtn);
    procedure ToggleCache(Btn: PBtn);
    procedure ClearCache(Btn: PBtn);
    procedure OKClick(Btn: PBtn); override;
  public
    constructor Create;
  end;
  TStateMenu=class(TGameState)
  private
    FFormsSet: TGUIFormsSet;
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
  UIFontSize = 12;
  GameTitle = 'Arkanoid 64k';
  GameVer = '1.1';

procedure DrawBackground;
function BackgroundLoaded: Boolean;

implementation

uses VSESound, VSETexMan, VSERender2D, VSEImageCodec
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

var
  BgTex: Cardinal;

procedure DrawBackground;
begin
  if BgTex <> 0 then
  begin
    TexMan.Bind(BgTex);
    Render2D.Enter;
    gleColor(clWhite);
    with Render2D, Render2D.VSBounds do
      DrawRect(Left, Top, Right - Left, Bottom - Top, 0, 0, 1, 1);
    Render2D.Leave;
    TexMan.Unbind;
  end;
end;

function BackgroundLoaded: Boolean;
begin
  Result := BgTex <> 0;
end;

const
  IDDiffMenu = 'DiffMenu';
  IDMainMenu = 'MainMenu';
  IDOptions = 'Options';
  IDScores = 'Scores';
  IDTextView = 'TextView';

{TMainMenu}

var
  MainMenuItems: array[0..5] of TMenuItem = (
    (Caption: 'New game'; Tag: 1),
    (Caption: 'Resume'; Tag: 0),
    (Caption: 'Scores'; Tag: 0),
    (Caption: 'Options'; Tag: 0),
    (Caption: 'Credits'; Tag: 0),
    (Caption: 'Exit'; Tag: 0));

constructor TMainMenu.Create;
begin
  inherited Create(0, 0, 200, 350);
  Alignment:=[faCenter, faMiddle];
  FCaption:=GameTitle;
  MainMenuItems[0].OnClick:=GameClick;
  MainMenuItems[1].OnClick:=GameClick;
  MainMenuItems[2].OnClick:=ScoresClick;
  MainMenuItems[3].OnClick:=OptionsClick;
  MainMenuItems[4].OnClick:=CredsClick;
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
    FParentSet.AddForm(IDDiffMenu, TDiffMenu.Create, Name);
    FormManager.Show(IDDiffMenu);
  end
  else
    Core.SwitchState(SIDGame);
end;

procedure TMainMenu.ScoresClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDScores, TScores.Create, Name);
  FormManager.Show(IDScores);
end; 

procedure TMainMenu.OptionsClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDOptions, TOptions.Create, Name);
  FormManager.Show(IDOptions);
end;

procedure TMainMenu.CredsClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDTextView, TTextView.Create(640, 480, Btn.Caption, 'Close', Core.GetFileText('Creds.txt')), Name);
  FormManager.Show(IDTextView);
end;

procedure TMainMenu.ExitClick(Btn: PBtn);
begin
  Core.StopEngine;
end;

{TDiffMenu}

var
  DiffMenuItems: array[0..3] of TMenuItem = (
    (Caption: 'Easy'; Tag: 5),
    (Caption: 'Normal'; Tag: 3),
    (Caption: 'Hard'; Tag: 1),
    (Caption: 'Back'; Tag: 0));

constructor TDiffMenu.Create;
var
  i: Integer;
begin
  inherited Create(0, 0, 200, 250);
  Alignment:=[faCenter, faMiddle];
  FCaption:='Difficulty';
  for i:=Low(DiffMenuItems) to High(DiffMenuItems) do
    DiffMenuItems[i].OnClick:=BtnClick;
  CreateMenu(Self, 30, 50, 140, 30, 20, DiffMenuItems);
end;

procedure TDiffMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp) then
    Close
  else
    inherited KeyEvent(Key, Event);
end;

procedure TDiffMenu.BtnClick(Btn: PBtn);
begin
  if Btn.Tag > 0 then
  begin
    (Core.GetState(Core.FindState(SIDGame)) as TStateGame).NewGame(Btn.Tag);
    Core.SwitchState(SIDGame);
  end;
  Close;
end;

{ TScores }

constructor TScores.Create;
var
  Btn: TBtn;
  i: Integer;
begin
  inherited Create(0, 0, 200, 300);
  Alignment:=[faCenter, faMiddle];
  FCaption:='Scores';
  with Btn do
  begin
    Enabled:=true;
    X:=30;
    Y:=200;
    Width:=140;
    Height:=30;
    Type_:=btPush;
    Caption := 'Clear';
    OnClick := ClearClick;
    AddButton(Btn);
    Y := 250;
    Caption:='Close';
    OnClick:=CloseClick;
    AddButton(Btn);
  end;
  for i := 0 to High(FScores) do
    FScores[i] := Settings.Int[SScores, IntToStr(i)];
end;

procedure TScores.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp) then
    Close
  else
    inherited KeyEvent(Key, Event);
end;

procedure TScores.DrawForm(State: TBtnState);
var
  i, LHeight: Integer;
begin
  inherited;
  LHeight := Render2D.TextHeight(Font) + 10;
  gleColor(clText);
  for i:= 0 to High(FScores) do
    Render2D.TextOut(Font, 30, 40 + i * LHeight, IntToStr(i + 1) + ': ' + IntToStr(FScores[i]));
end;

procedure TScores.ClearClick(Btn: PBtn);
begin
  Settings.EraseSection(SScores);
  Close;
end;

procedure TScores.CloseClick(Btn: PBtn);
begin
  Close;
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
  inherited Create(400, 300);
  FLMouseSens := CreateSelect(Self, 220, 160, 160, 20, SensClick, '-', '+');
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
    Align:=laCenter;
    Caption:='';
    FLCacheSize:=AddLabel(Lbl);
    Y := 138;
    Caption:='Mouse sensivity';
    AddLabel(Lbl);
  end;
  Button[FCEnableBGM].Checked:=Sound.EnableBGM;
  Self.Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
  FMouseSens:=(Core.GetState(Core.FindState(SIDGame)) as TStateGame).MouseSens;
end;

procedure TOptions.DrawForm(State: TBtnState);
begin
  Lbl[FLMouseSens].Caption := IntToStr(FMouseSens);
  inherited;
end;

procedure TOptions.SensClick(Btn: PBtn);
begin
  FMouseSens:=Max(1, Min(FMouseSens+Btn.Tag, 10));
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

procedure TOptions.OKClick(Btn: PBtn);
begin
  Sound.EnableBGM:=Button[FCEnableBGM].Checked;
  (Core.GetState(Core.FindState(SIDGame)) as TStateGame).MouseSens:=FMouseSens;
  inherited;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['menubg file=s']:=MenuBgHandler;
  {$ENDIF}
  SetGUIFont(UIFont, UIFontSize, true);
  FFormsSet:=TGUIFormsSet.Create;
  FFormsSet.AddForm(IDMainMenu, TMainMenu.Create);
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
  with Core.GetState(Core.FindState(SIDGame)) as TStateGame do
    if CanResumeGame then
      Draw
  else
    DrawBackground;
end;

function TStateMenu.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  FormManager.FormsSet:=FFormsSet;
  (FormManager[IDMainMenu] as TMainMenu).ResumeEnable((Core.GetState(Core.FindState(SIDGame)) as TStateGame).CanResumeGame);
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
