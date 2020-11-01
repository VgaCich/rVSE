unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore,
  VSEGUI, VSEFormManager, VSEBindMan, VSEForms, StateGame, GameData;

type
  TStateMenu = class;
  TMainMenu = class(TAlignedForm)
  protected
    FResumeButton: Integer;
    procedure GameClick(Btn: PBtn);
    procedure OptionsClick(Btn: PBtn);
    procedure TextClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  public
    constructor Create;
    function KeyEvent(Key: Integer; Event: TKeyEvents): Boolean; override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TOptions = class(TOptionsForm)
  protected
    FLQuality: Integer;
    FQuality: TGraphicsQuality;
    procedure DrawForm(State: TBtnState); override;
    procedure QualityClick(Btn: PBtn);
    procedure KeyConfig(Btn: PBtn);
    procedure OKClick(Btn: PBtn); override;
  public
    constructor Create;
  end;
  TStateMenu = class(TGameState)
  private
    FFormsSet: TGUIFormsSet;
    FGame: TStateGame;
    {$IFDEF VSE_CONSOLE}function MenuBgHandler(Sender: TObject; Args: array of const): Boolean;{$ENDIF}
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function Activate: Cardinal; override;
    procedure Deactivate; override;
  end;

const
  SIDMenu = 'Menu';

procedure DrawBackground;
function GetTexFileName(const Name: string): string;

implementation

uses
  VSETexMan, VSERender2D, VSEImageCodec
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  IDKeyConfig = 'KeyConfig';
  IDMainMenu = 'MainMenu';
  IDOptions = 'Options';
  IDTextView = 'TextView';
  TextFiles: array[0..1] of string = ('Help.txt', 'Creds.txt');

var
  BgTex: Cardinal;

procedure DrawBackground;
begin
  if BgTex <> 0 then
  begin
    TexMan.Bind(BgTex);
    Render2D.Enter;
    gleColor(clWhite);
    with Render2D do
      DrawRect(0, 0, VSWidth, VSHeight, 0, 0, 1, 1);
    Render2D.Leave;
    TexMan.Unbind;
  end;
end;

function GetTexFileName(const Name: string): string;
var
  Quality: TGraphicsQuality;
  i: Integer;
begin
  Result := Name;
  Quality := TGraphicsQuality(Settings.Int[SSectionSettings, SGraphicsQuality]);
  for i := 0 to High(TexFolders[Quality]) do
    with TexFolders[Quality][i] do
      if SameText(Folder, Copy(Name, 1, Length(Folder))) then
      begin
        Insert(Postfix, Result, Length(Folder) + 1);
        Break;
      end;
end;

{TMainMenu}

var
  MainMenuItems: array[0..5] of TMenuItem = (
    (Caption: 'Новая игра'; Tag: 1),
    (Caption: 'Продолжить'; Tag: 0),
    (Caption: 'Настройки'; Tag: 0),
    (Caption: 'Справка'; Tag: 0),
    (Caption: 'Титры'; Tag: 1),
    (Caption: 'Выход'; Tag: 0));

constructor TMainMenu.Create;
begin
  inherited Create(300, 130, 200, 350);
  FCaption := GameTitle;
  MainMenuItems[0].OnClick := GameClick;
  MainMenuItems[1].OnClick := GameClick;
  MainMenuItems[2].OnClick := OptionsClick;
  MainMenuItems[3].OnClick := TextClick;
  MainMenuItems[4].OnClick := TextClick;
  MainMenuItems[5].OnClick := ExitClick;
  FResumeButton := CreateMenu(Self, 30, 50, 140, 30, 20, MainMenuItems)[1];
  Button[FResumeButton].Enabled := false;
end;

function TMainMenu.KeyEvent(Key: Integer; Event: TKeyEvents): Boolean;
begin
  Result := false;
  if (Key=VK_ESCAPE) and (Event=keUp) then
  begin
    if Self.Button[FResumeButton].Enabled then
       Core.SwitchState(SIDGame)
    else
      Core.StopEngine;
    Result := true;
  end
  else
    Result := inherited KeyEvent(Key, Event);
end;

procedure TMainMenu.ResumeEnable(Enable: Boolean);
begin
  Button[FResumeButton].Enabled := Enable;
end;

procedure TMainMenu.GameClick(Btn: PBtn);
begin
  if Btn.Tag = 1 then
    (Core.GetState(Core.FindState(SIDGame)) as TStateGame).NewGame;
  Core.SwitchState(SIDGame);
end;

procedure TMainMenu.OptionsClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDOptions, TOptions.Create, Name);
  FormManager.Show(IDOptions);
end;

procedure TMainMenu.TextClick(Btn: PBtn);
begin
  FParentSet.AddForm(IDTextView, TTextView.Create(640, 480, Btn.Caption, 'Закрыть', Core.GetFileText(TextFiles[Btn.Tag])), Name);
  FormManager.Show(IDTextView);
end;

procedure TMainMenu.ExitClick(Btn: PBtn);
begin
  Core.StopEngine;
end;                 

{TOptions}

constructor TOptions.Create;
var
  Btn: TBtn;
  Lbl: TLbl;
begin
  inherited Create(400, 350);
  FCaption := 'Настройки';
  with Button[FCFullscreen]^ do
  begin
    X := 220;
    Y := 110;
    Caption := 'Полный экран';
  end;
  with Button[FCVSync]^ do
  begin
    X := 220;
    Y := 140;
    Caption := 'Верт. синхр.';
  end;
  Button[FBOK].Caption := 'ОК';
  Button[FBCancel].Caption := 'Отмена';
  Self.Lbl[FLResolutionCap].Caption := 'Разрешение экрана';
  Self.Lbl[FLRefreshRateCap].Caption := 'Частота обновления';
  Self.Lbl[FLColorDepthCap].Caption := 'Глубина цвета';
  FRestartMessage := 'Выбранные настройки требуют перезапуска игры. Перезапустить сейчас?';
  FRestartYes := 'Да';
  FRestartNo := 'Нет';
  FLQuality := CreateSelect(Self, 10, 210, 190, 20, QualityClick, '-', '+');
  with Btn do
  begin
    Enabled := true;
    X := 220;
    Y := 60;
    Width := 160;
    Height := 30;
    Tag := 0;
    Type_ := btPush;
    Caption := 'Управление';
    OnClick := KeyConfig;
    AddButton(Btn);
  end;
  with Lbl do
  begin
    Align := laCenter;
    Color := 0;
    X := 10;
    Y := 188;
    Width := 190;
    Caption := 'Качество графики';
    AddLabel(Lbl);
  end;
  FQuality := TGraphicsQuality(Settings.Int[SSectionSettings, SGraphicsQuality]);
end;

procedure TOptions.DrawForm(State: TBtnState);
begin
  Lbl[FLQuality].Caption := GraphicsQualityName[FQuality];
  inherited;
end;

procedure TOptions.QualityClick(Btn: PBtn);
begin
  FQuality := TGraphicsQuality(Max(Integer(Low(TGraphicsQuality)), Min(Integer(FQuality) + Btn.Tag, Integer(High(TGraphicsQuality)))));
end;

procedure TOptions.KeyConfig(Btn: PBtn);
begin
  (FParentSet.AddForm(IDKeyConfig, TBindManCfgForm.Create(400, 350, 'Сброс', 'OK'), Name) as TBindManCfgForm).Caption := 'Управление';
  FormManager.Show(IDKeyConfig);
end;

procedure TOptions.OKClick(Btn: PBtn);
begin
  FNeedRestart := FQuality <> TGraphicsQuality(Settings.Int[SSectionSettings, SGraphicsQuality]);
  Settings.Int[SSectionSettings, SGraphicsQuality] := Integer(FQuality);
  inherited;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}Console['menubg file=s'] := MenuBgHandler;{$ENDIF}
  FFormsSet := TGUIFormsSet.Create;
  FFormsSet.AddForm(IDMainMenu, TMainMenu.Create);
  FGame := TStateGame(Core.GetState(Core.FindState(SIDGame)));
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
  if FGame.CanResumeGame then
    FGame.Draw
  else
    DrawBackground;
end;

function TStateMenu.Activate: Cardinal;
begin
  Result := inherited Activate;
  glClearColor(0, 0, 0, 1);
  FormManager.FormsSet := FFormsSet;
  with FormManager[IDMainMenu] as TMainMenu do
  begin
    ResumeEnable(FGame.CanResumeGame);
    if FGame.CanResumeGame then
    begin
      Left := (800 - Width) div 2;
      Top := (600 - Height) div 2;
    end
    else begin
      Left := 50;
      Top := 220;
    end;
  end;
end;

procedure TStateMenu.Deactivate;
begin
  FormManager.FormsSet := nil;
end;

function TStateMenu.GetName: string;
begin
  Result := SIDMenu;
end;

{$IFDEF VSE_CONSOLE}
function TStateMenu.MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
var
  Data: TStream;
begin
  Result := false;
  Data := Core.GetFile(string(Args[1].VAnsiString));
  if Assigned(Data) then
  begin
    BgTex := TexMan.AddTexture('MenuBg', Data, true, false, true);
    Result := true;
  end
  {$IFDEF VSE_LOG}  else
    LogF(llError, 'StateMenu.MenuBg: file "%s" not found', [string(Args[1].VAnsiString)]){$ENDIF};
end;
{$ENDIF}

end.

