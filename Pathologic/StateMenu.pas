unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore,
  VSEGUI, VSEFormManager, VSEBindMan, StateStart, StateGame;

type
  TGraphicsQuality = (gqMin, gqMed, gqFull);
  TOnMessageBox = procedure(Sender: TObject; BtnNum: Integer) of object;
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
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TOptions = class(TAlignedForm)
  protected
    FLResolution, FLRefreshRate, FLColorDepth, FLQuality, FCFullscreen, FCVSync,
      FCurrentResolution, FCurrentRefreshRate, FColorDepth: Integer;
    FResolutions: TResolutions;
    FQuality: TGraphicsQuality;
    procedure DrawForm(State: TBtnState); override;
    procedure ResClick(Btn: PBtn);
    procedure RefrClick(Btn: PBtn);
    procedure DepthClick(Btn: PBtn);
    procedure QualityClick(Btn: PBtn);
    procedure KeyConfig(Btn: PBtn);
    procedure OKClick(Btn: PBtn);
    procedure CancelClick(Btn: PBtn);
    procedure RestartClick(Sender: TObject; BtnNum: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TMessageBox = class(TAlignedForm)
  private
    FHandler: TOnMessageBox; 
    procedure Click(Btn: PBtn);
  public
    constructor Create(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox = nil);
  end;
  TTextView = class(TAlignedForm)
  protected
    FCurPage, FPages, FLPage: Integer;
    FText: TStringList;
    procedure DrawForm(State: TBtnState); override;
    procedure ChangePage(Btn: PBtn);
    procedure Close(Btn: PBtn);
  public
    constructor Create(const Caption, TextFile: string);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TStateMenu = class(TGameState)
  private
    FFormsSet: TGUIFormsSet;
    FStart: TStateStart;
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
    function SysNotify(Notify: TSysNotify): Boolean; override;
  end;

const
  SIDMenu = 'Menu';
  UIFont = 'Tahoma';
  UIFontSize = 12;
  GameTitle = '���. ������';
  SGraphicsQuality = 'Quality';

procedure DrawBackground;
function GetTexFileName(const Name: string): string;

implementation

uses
  VSETexMan, VSERender2D, VSEMemPak, VSEImageCodec
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  IDKeyConfig = 'KeyConfig';
  IDMainMenu = 'MainMenu';
  IDOptions = 'Options';
  IDTextView = 'TextView';
  TextFiles: array[0..1] of string = ('Help.txt', 'Creds.txt');
  GraphicsQualityName: array[TGraphicsQuality] of string = ('�����������', '�������', '������������');
  TexFolders: array[TGraphicsQuality] of array[0..4] of record Folder, Postfix: string end = (
    (
      (Folder: 'Cards'; Postfix: '.min'),
      (Folder: 'Chars'; Postfix: '.min'),
      (Folder: 'Chips'; Postfix: '.min'),
      (Folder: 'Docs'; Postfix: '.min'),
      (Folder: 'Map'; Postfix: '.min')
    ),
    (
      (Folder: 'Cards'; Postfix: '.med'),
      (Folder: 'Chars'; Postfix: '.full'),
      (Folder: 'Chips'; Postfix: '.full'),
      (Folder: 'Docs'; Postfix: '.full'),
      (Folder: 'Map'; Postfix: '.med')
    ),
    (
      (Folder: 'Cards'; Postfix: '.full'),
      (Folder: 'Chars'; Postfix: '.full'),
      (Folder: 'Chips'; Postfix: '.full'),
      (Folder: 'Docs'; Postfix: '.full'),
      (Folder: 'Map'; Postfix: '.full')
    ));

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
    (Caption: '����� ����'; Tag: 1),
    (Caption: '����������'; Tag: 0),
    (Caption: '���������'; Tag: 0),
    (Caption: '�������'; Tag: 0),
    (Caption: '�����'; Tag: 1),
    (Caption: '�����'; Tag: 0));

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

procedure TMainMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key = VK_ESCAPE) and (Event = keUp) then
    if Self.Button[FResumeButton].Enabled then
      Core.SwitchState(SIDGame)
    else
      Core.StopEngine
  else
    inherited KeyEvent(Key, Event);
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
  FParentSet.AddForm(IDTextView, TTextView.Create(Btn.Caption, TextFiles[Btn.Tag]), Name);
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
  i: Integer;
begin
  inherited Create(0, 0, 400, 350);
  Alignment := [faCenter, faMiddle];
  FCaption := '���������';
  FResolutions := gleGetResolutions;
  FLResolution := CreateSelect(Self, 10, 60, 190, 20, ResClick, '-', '+');
  FLRefreshRate := CreateSelect(Self, 10, 110, 190, 20, RefrClick, '-', '+');
  FLColorDepth := CreateSelect(Self, 10, 160, 190, 20, DepthClick, '-', '+');
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
    Caption := '����������';
    OnClick := KeyConfig;
    AddButton(Btn);
    Y := 110;
    Height := 20;
    Type_ := btCheck;
    OnClick := nil;
    Caption := '������ �����';
    Checked := Core.Fullscreen;
    FCFullscreen := AddButton(Btn);
    Y := 140;
    Caption := '����. �����.';
    Checked := Core.VSync;
    FCVSync := AddButton(Btn);
    X := 140;
    Y := 310;
    Width := 120;
    Height := 30;
    Type_ := btPush;
    Caption := 'OK';
    OnClick := OKClick;
    AddButton(Btn);
    X := 270;
    Caption := '������';
    OnClick := CancelClick;
    AddButton(Btn);
  end;
  with Lbl do
  begin
    Align := laCenter;
    Color := 0;
    X := 10;
    Y := 38;
    Width := 190;
    Caption := '���������� ������';
    AddLabel(Lbl);
    Y := 88;
    Caption := '������� ����������';
    AddLabel(Lbl);
    Y := 138;
    Caption := '������� �����';
    AddLabel(Lbl);
    Y := 188;
    Caption := '�������� �������';
    AddLabel(Lbl);
  end;
  FColorDepth := Core.ColorDepth;
  FQuality := TGraphicsQuality(Settings.Int[SSectionSettings, SGraphicsQuality]);
  FCurrentResolution := -1;
  for i := 0 to High(FResolutions) do
    if (FResolutions[i].Width = Core.ResolutionX) and (FResolutions[i].Height = Core.ResolutionY) then
      FCurrentResolution := i;
  if FCurrentResolution = -1 then
  begin
    FCurrentResolution := Length(FResolutions);
    SetLength(FResolutions, FCurrentResolution + 1);
    with FResolutions[FCurrentResolution] do
    begin
      Width := Core.ResolutionX;
      Height := Core.ResolutionY;
      SetLength(RefreshRates, 1);
      RefreshRates[0] := Core.RefreshRate;
    end;
  end;
  FCurrentRefreshRate := -1;
  for i := 0 to High(FResolutions[FCurrentResolution].RefreshRates) do
    if FResolutions[FCurrentResolution].RefreshRates[i] = Core.RefreshRate then
      FCurrentRefreshRate := i;
  if FCurrentRefreshRate = -1 then
  begin
    FCurrentRefreshRate := Length(FResolutions[FCurrentResolution].RefreshRates);
    SetLength(FResolutions[FCurrentResolution].RefreshRates, FCurrentRefreshRate + 1);
    FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate] := Core.RefreshRate;
  end;
end;

destructor TOptions.Destroy;
begin
  Finalize(FResolutions);
  inherited Destroy;
end;

procedure TOptions.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key = VK_ESCAPE) and (Event = keUp) then
    CancelClick(nil)
  else
    inherited KeyEvent(Key, Event);
end;

procedure TOptions.DrawForm(State: TBtnState);
begin
  Lbl[FLResolution].Caption := Format('%dx%d', [FResolutions[FCurrentResolution].Width, FResolutions[FCurrentResolution].Height]);
  Lbl[FLRefreshRate].Caption := IntToStr(FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]);
  Lbl[FLColorDepth].Caption := IntToStr(FColorDepth);
  Lbl[FLQuality].Caption := GraphicsQualityName[FQuality];
  inherited;
end;

procedure TOptions.ResClick(Btn: PBtn);
begin
  FCurrentResolution := Max(0, Min(FCurrentResolution + Btn.Tag, High(FResolutions)));
  FCurrentRefreshRate := 0;
end;

procedure TOptions.RefrClick(Btn: PBtn);
begin
  FCurrentRefreshRate := Max(0, Min(FCurrentRefreshRate + Btn.Tag, High(FResolutions[FCurrentResolution].RefreshRates)));
end;

procedure TOptions.DepthClick(Btn: PBtn);
const
  Depth: array[-1..1] of Integer = (16, 0, 32);
begin
  FColorDepth := Depth[Btn.Tag];
end;

procedure TOptions.QualityClick(Btn: PBtn);
begin
  FQuality := TGraphicsQuality(Max(Integer(Low(TGraphicsQuality)), Min(Integer(FQuality) + Btn.Tag, Integer(High(TGraphicsQuality)))));
end;

procedure TOptions.KeyConfig(Btn: PBtn);
begin
  with FParentSet.AddForm(IDKeyConfig, TBindManCfgForm.Create(200, 130, 400, 350, '�����', 'OK'), Name) as TBindManCfgForm do
  begin
    Movable := true;
    Caption := '����������';
  end;
  FormManager.Show(IDKeyConfig);
end;

procedure TOptions.OKClick(Btn: PBtn);
const
  IDRestartBox = 'RestartBox';
var
  NeedRestart: Boolean;
begin
  NeedRestart := (Core.ColorDepth <> FColorDepth) or
    (FQuality <> TGraphicsQuality(Settings.Int[SSectionSettings, SGraphicsQuality]));
  with FResolutions[FCurrentResolution] do
    if (Core.ResolutionX <> Width) or (Core.ResolutionY <> Height) or (Core.RefreshRate <> RefreshRates[FCurrentRefreshRate]) then
      Core.SetResolution(Width, Height, RefreshRates[FCurrentRefreshRate], Button[FCFullscreen].Checked, true)
    else
      Core.Fullscreen := Button[FCFullscreen].Checked;
  Core.VSync := Button[FCVSync].Checked;
  Core.ColorDepth := FColorDepth;
  Settings.Int[SSectionSettings, SGraphicsQuality] := Integer(FQuality);
  if NeedRestart then
    FormManager.Show(FParentSet.AddForm(IDRestartBox, TMessageBox.Create(Caption, '��������� ��������� ������� �����������. ������������� ����?', ['��', '���'], RestartClick), Name).Name)
  else
    Close;
end;

procedure TOptions.CancelClick(Btn: PBtn);
begin
  Close;
end;

procedure TOptions.RestartClick(Sender: TObject; BtnNum: Integer);
begin
  if BtnNum = 0 then
    Core.StopEngine(StopNeedRestart);
  Close;
end;

{TTextView}

constructor TTextView.Create(const Caption, TextFile: string);
var
  Line: Integer;
  Src, Dst: string;
  Btn: TBtn;
begin
  inherited Create(0, 0, 640, 480);
  Alignment := [faCenter, faMiddle];
  FCaption := Caption;
  FText := GetFileText(TextFile);
  Line := 0;
  while Line < FText.Count do
  begin
    Src := ProcessKeyTags(FText[Line]);
    Dst := '';
    while (Src <> '') and (Render2D.TextWidth(Font, Src) > 620) do
    begin
      Dst := Src[Length(Src)] + Dst;
      Delete(Src, Length(Src), 1);
    end;
    FText[Line] := Src;
    if Dst <> '' then
      FText.Insert(Line + 1, Dst);
    Inc(Line);
  end;
  FPages := (FText.Count - 1) div 25;
  FCurPage := 0;
  with Btn do
  begin
    Type_ := btPush;
    X := 535;
    Y := 445;
    Width := 100;
    Height := 30;
    Enabled := true;
    Caption := '�������';
    OnClick := Close;
    AddButton(Btn);
  end;
  if FPages > 0 then
    FLPage := CreateSelect(Self, 5, 445, 315, 30, ChangePage, '<', '>');
end;

destructor TTextView.Destroy;
begin
  FAN(FText);
  inherited Destroy;
end;

procedure TTextView.KeyEvent(Key: Integer; Event: TKeyEvent);
var
  Btn: TBtn;
begin
  if Event = keDown then
    case Key of
      VK_LEFT:
        begin
          Btn.Tag := -1;
          ChangePage(@Btn);
        end;
      VK_RIGHT:
        begin
          Btn.Tag := 1;
          ChangePage(@Btn);
        end;
    end
  else if Key = VK_ESCAPE then
    Close(nil);
  inherited KeyEvent(Key, Event);
end;

procedure TTextView.DrawForm(State: TBtnState);
var
  i, Left: Integer;
  S: string;
begin
  if FPages > 0 then
    Lbl[FLPage].Caption := Format('%d/%d', [FCurPage + 1, FPages + 1]);
  inherited;
  gleColor(clText);
  for i := 0 to 24 do
    if 25 * FCurPage + i < FText.Count then
    begin
      S := FText[25 * FCurPage + i];
      Left := 10;
      if (S <> '') and (S[1] = #9) then
      begin
        S := Copy(S, 2, MaxInt);
        Inc(Left, 310 - Render2D.TextWidth(Font, S) div 2);
      end;
      Render2D.TextOut(Font, Left, 35 + 16 * i, S);
    end;
end;

procedure TTextView.ChangePage(Btn: PBtn);
begin
  FCurPage := Max(0, Min(FCurPage + Btn.Tag, FPages));
end;

procedure TTextView.Close(Btn: PBtn);
begin
  inherited Close;
end;

{TMessageBox}

constructor TMessageBox.Create(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox);
const
  BtnWidth = 75;
  WndHeight = 90;
var
  i, WndWidth: Integer;
  Lbl: TLbl;
  Btn: TBtn;
begin
  WndWidth := Min(Max(Render2D.TextWidth(GUIFont, Prompt) + 25, Length(Buttons) * (BtnWidth + 10) + 10), Render2D.VSWidth);
  inherited Create(0, 0, WndWidth, WndHeight);
  Alignment := [faCenter, faMiddle];
  FCaption := Caption;
  FHandler := Handler;
  with Lbl do
  begin
    Align := laCenter;
    Color := 0;
    X := 10;
    Y := 30;
    Width := WndWidth - 20;
    Caption := Prompt;
  end;
  AddLabel(Lbl);
  WndWidth := Max(0, WndWidth - Length(Buttons) * BtnWidth - High(Buttons) * 10) div 2;
  with Btn do
  begin
    Type_ := btPush;
    Y := 55;
    Width := BtnWidth;
    Height := 25;
    OnClick := Click;
    Enabled := true;
    for i := 0 to High(Buttons) do
    begin
      X := WndWidth + i * (BtnWidth + 10);
      Tag := i;
      Caption := Buttons[i];
      AddButton(Btn);
    end;
  end;
end;

procedure TMessageBox.Click(Btn: PBtn);
begin
  if Assigned(FHandler) then
    FHandler(Self, Btn.Tag);
  Close;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}Console.OnCommand['menubg file=s'] := MenuBgHandler;{$ENDIF}
  FFormsSet := TGUIFormsSet.Create;
  FFormsSet.AddForm(IDMainMenu, TMainMenu.Create);
  //FFormsSet.AddForm(IDOptions, TOptions.Create, IDMainMenu);
  FStart := TStateStart(Core.GetState(Core.FindState(SIDStart)));
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

function TStateMenu.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result := inherited SysNotify(Notify);
  if Notify = snConsoleActive then
    Result := true;
end;

{$IFDEF VSE_CONSOLE}
function TStateMenu.MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
var
  Data: TStream;
begin
  Result := false;
  Data := GetFile(string(Args[1].VAnsiString));
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

