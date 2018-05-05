unit StateGame;

interface

uses
  Windows, AvL, avlUtils, avlEventBus, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSECore, VSECamera, VSEGUI, Scene, Game;

type
  TStateGame=class(TGameState)
  private
    {$IFDEF VSE_DEBUG}
    FFont: Cardinal;
    FShowDebugInfo: Boolean;
    {$ENDIF}
    FFormsSet: TGUIFormsSet;
    FCamera: TCamera;
    FScene: TScene;
    FGame: TGame;
    FMouse3D: TVector3D;
    FOnMouseEvent: Integer;
    function GetCanResumeGame: Boolean;
    procedure PlayerActionsChanged(Sender: TObject; const Args: array of const);
    procedure ActivePlayerChanged(Sender: TObject; const Args: array of const);
  protected
    function  GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
    procedure NewGame;
    property CanResumeGame: Boolean read GetCanResumeGame;
    property Mouse3D: TVector3D read FMouse3D;
  end;

const
  SIDGame = 'Game';


implementation

uses VSERender2D, VSETexMan, VSEMemPak, VSEBindMan, VSEFormManager
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF},
  StateMenu, GameForms;

const
  Bindings: array[0..3] of TBindingRec = (
    (Name: 'CamFwd'; Description: 'Камера вперед'; Key: Ord('W')),
    (Name: 'CamBwd'; Description: 'Камера назад'; Key: Ord('S')),
    (Name: 'CamLeft'; Description: 'Камера влево'; Key: Ord('A')),
    (Name: 'CamRight'; Description: 'Камера вправо'; Key: Ord('D'))
  );

constructor TStateGame.Create;
begin
  inherited Create;
  FOnMouseEvent := EventBus.RegisterEvent(GameOnMouseEvent);
  FFormsSet := TGUIFormsSet.Create;
  {$IFDEF VSE_DEBUG}
  FFormsSet.AddForm(IDLogPoints, TLogPointsForm.Create, '');
  FFormsSet.Visible[IDLogPoints] := false;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['debuginfo ?show=eoff:on'] := Console.GetConVarHandler(FShowDebugInfo, cvBool);
  Console.OnCommand['logpoints ?show=eoff:on'] := Console.GetConVarHandler(FFormsSet.FindForm(IDLogPoints).Visible, cvBool);
  {$ENDIF}
  FShowDebugInfo := true;
  FFont := Render2D.CreateFont('Courier New', 10);
  {$ENDIF}
  BindMan.AddBindings(Bindings);
  FCamera := TCamera.Create;
end;

destructor TStateGame.Destroy;
begin
  EventBus.RemoveListeners([PlayerActionsChanged, ActivePlayerChanged]);
  FAN(FGame);
  FAN(FScene);
  FAN(FCamera);
  FAN(FFormsSet);
  inherited Destroy;
end;

procedure TStateGame.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  DrawBackground;
  glePerspectiveMatrix2(60, Core.ResolutionX, Core.ResolutionY, 1, 1000);
  FCamera.Apply;
  FScene.Draw;
  with Core.MouseCursor do
    FMouse3D := gleScreenTo3D(X, Y, true);
  {$IFDEF VSE_DEBUG}
  if FormManager.Visible[IDLogPoints] then
    (FormManager[IDLogPoints] as TLogPointsForm).DrawPoints;
  if FShowDebugInfo then
  begin
    Render2D.Enter;
    gleColor(clWhite);
    with Render2D.VSBounds do
    begin
      Render2D.TextOut(FFont, Left + 5, Top + 5, 'FPS: ' + IntToStr(Core.FPS));
      with FCamera.Eye do
        Render2D.TextOut(FFont, Left + 5, Top + 25, Format('Pos=%s, %s, %s',
          [FloatToStr2(X, 4, 2), FloatToStr2(Y, 4, 2), FloatToStr2(Z, 4, 2)]));
      with FCamera do
        Render2D.TextOut(FFont, Left + 5, Top + 45, Format('Angles=%s, %s',
          [FloatToStr2(Angle.X, 4, 2), FloatToStr2(Angle.Y, 4, 2)]));
      with FMouse3D do
        Render2D.TextOut(FFont, Left + 5, Top + 65, Format('Mouse=%s, %s, %s',
         [FloatToStr2(X, 4, 2), FloatToStr2(Y, 4, 2), FloatToStr2(Z, 4, 2)]));
    end;
    Render2D.Leave;
  end;
  {$ENDIF}
end;

procedure TStateGame.Update;
const
  Move: array[Boolean, Boolean] of Single = ((0, -1), (1, 0));
begin
  inherited; //TODO: Move by screen edges?
  FCamera.Move(Vector2D(Move[BindMan.BindActive['CamLeft'], BindMan.BindActive['CamRight']],
    Move[BindMan.BindActive['CamFwd'], BindMan.BindActive['CamBwd']]), MapBounds.Min, MapBounds.Max);
  if Assigned(FGame) then
  begin
    EventBus.SendEvent(FOnMouseEvent, FGame, [Integer(meMove), @FMouse3D]);
    FGame.Update;
    FScene.Update;
  end;
end;

function TStateGame.Activate: Cardinal;

  procedure SetMovable(Self: TObject; Form: TGUIForm);
  begin
    Form.Movable := true;
  end;

begin
  inherited Activate;
  Result := 20;
  glClearColor(0, 0, 0, 1);
  glClearStencil(0);
  FFormsSet.IterateForms(TOnForm(MakeMethod(@SetMovable)));
  FormManager.FormsSet := FFormsSet;
  Draw;
  Core.ResetUpdateTimer;
  //TODO: FGame.Resume;
end;

procedure TStateGame.Deactivate;
begin
  inherited;
  FormManager.FormsSet := nil;
  //TODO: FGame.Pause;
end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  inherited;
  if not Core.MouseCapture and FormManager.MouseBusy(X, Y) then Exit;
  case Event of
    meDown: if Button in [mbRight, mbMiddle] then
        Core.MouseCapture := true
      else if Assigned(FGame) and (Button = mbLeft) then
        EventBus.SendEvent(FOnMouseEvent, FGame, [Integer(meDown), @FMouse3D]);
    meUp: if Button in [mbRight, mbMiddle] then
        Core.MouseCapture := false
      else if Assigned(FGame) and (Button = mbLeft) then
        EventBus.SendEvent(FOnMouseEvent, FGame, [Integer(meUp), @FMouse3D]);
    meMove: if Core.MouseCapture then //TODO: Panning & Rotating speed to Options
      with FCamera do
        if Core.KeyPressed[VK_RBUTTON] then
          Angle := Vector2D(Max(90.0, Min(Angle.X + X / 10, 270.0)),
                            Max(-89.9, Min(Angle.Y - Y / 10, -35.0)))
        else
          Move(Vector2D(0.1 * X, 0.1 * Y), MapBounds.Min, MapBounds.Max);
    meWheel: FCamera.Height := Max(10.0, Min(FCamera.Height - Button, 75.0));
  end;
end;

procedure TStateGame.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  inherited;
  if Event = keUp then
  begin
    case Key of
      VK_ESCAPE: Core.SwitchState(SIDMenu);
    end;
  end;
end;

function TStateGame.SysNotify(Notify: TSysNotify): Boolean;

  procedure RealignForm(Self: TObject; Form: TGUIForm);
  begin
    if Form is TAlignedForm then
      (Form as TAlignedForm).Align;
  end;

begin
  Result := inherited SysNotify(Notify);
  case Notify of
    snMinimize: Core.SwitchState(SIDMenu);
    snConsoleActive: Result := true;
    snResolutionChanged: FFormsSet.IterateForms(TOnForm(MakeMethod(@RealignForm)));
  end;
end;

procedure TStateGame.NewGame;
begin
  FCamera.Eye := Vector3D(0, 50, 45);
  FCamera.Angle := Vector2D(180, -60);
  FAN(FGame);
  FAN(FScene);
  FScene := TScene.Create;
  FGame := TGame.Create(FScene);
  EventBus.AddListener(PlayerOnActionsChanged, PlayerActionsChanged);
  EventBus.AddListener(GameOnActivePlayerChanged, ActivePlayerChanged);
  ActivePlayerChanged(FGame, [TObject(nil), FGame.ActivePlayer]);
  {$IFDEF VSE_DEBUG}
  FFormsSet[IDPlayerSelect].Free;
  FFormsSet.AddForm(IDPlayerSelect, TPlayerSelectForm.Create(FGame));
  {$ENDIF}
end;

function TStateGame.GetName: string;
begin
  Result := SIDGame;
end;

function TStateGame.GetCanResumeGame: Boolean;
begin
  Result := Assigned(FGame);
end;

procedure TStateGame.PlayerActionsChanged(Sender: TObject; const Args: array of const);
var
  i: Integer;
begin
  if not Assigned(FGame) or (Sender <> FGame.ActivePlayer) then Exit;
  for i := 0 to High(FGame.ActivePlayer.AvailActions) do
    with FGame.ActivePlayer.AvailActions[i] do
      if Assigned(Form) and not Assigned(FFormsSet.FindForm(Form)) then
        FFormsSet.AddForm(ClassName, Form, '');
end;

procedure TStateGame.ActivePlayerChanged(Sender: TObject; const Args: array of const);
var
  i: Integer;
begin
  Assert((Length(Args) = 2) and (Args[0].VType = vtObject) and (Args[1].VType = vtObject));
  if Assigned(Args[0].VObject) then
    with Args[0].VObject as TPlayer do
      for i := 0 to High(AvailActions) do
        with AvailActions[i] do
          if Assigned(Form) then
            FFormsSet.RemoveForm(Form);
  PlayerActionsChanged(Args[1].VObject, []);
end;

end.
