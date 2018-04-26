unit StateGame;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, VSECamera, Scene, Game;

type
  TStateGame=class(TGameState)
  private
    FFont: Cardinal;
    {$IFDEF VSE_DEBUG}FShowDebugInfo: Boolean;{$ENDIF}
    FCamera: TCamera;
    FScene: TScene;
    FGame: TGame;
    FMouse3D: TVector3D;
    function GetCanResumeGame: Boolean;
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
  end;

const
  SIDGame = 'Game';

implementation

uses VSERender2D, VSETexMan, VSEMemPak, VSEBindMan
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF},
  StateMenu;

const
  Bindings: array[0..3] of TBindingRec = (
    (Name: 'CamFwd'; Description: '������ ������'; Key: Ord('W')),
    (Name: 'CamBwd'; Description: '������ �����'; Key: Ord('S')),
    (Name: 'CamLeft'; Description: '������ �����'; Key: Ord('A')),
    (Name: 'CamRight'; Description: '������ ������'; Key: Ord('D'))
  );

constructor TStateGame.Create;
begin
  inherited Create;
  Randomize;
  {$IFDEF VSE_DEBUG}
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['debuginfo ?val=eoff:on'] := Console.GetConVarHandler(FShowDebugInfo, cvBool);
  {$ENDIF}
  FShowDebugInfo := true;
  {$ENDIF}
  BindMan.AddBindings(Bindings);
  FFont := Render2D.CreateFont('Courier New', 10);
  FCamera := TCamera.Create;
end;

destructor TStateGame.Destroy;
begin
  FAN(FGame);
  FAN(FScene);
  FAN(FCamera);
  inherited Destroy;
end;

procedure TStateGame.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  DrawBackground;
  glePerspectiveMatrix(60, Core.ResolutionX, Core.ResolutionY);
  FCamera.Apply;
  FScene.Draw;
  with Core.MouseCursor do
    FMouse3D := gleScreenTo3D(X, Y, true);
  {$IFDEF VSE_DEBUG}
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
  FGame.Update;
  FScene.Update;
end;

function TStateGame.Activate: Cardinal;
begin
  inherited Activate;
  Result := 20;
  glClearColor(0, 0, 0, 1);
  glClearStencil(0);
  Draw;
  Core.ResetUpdateTimer;
  //TODO: FGame.Resume;
end;

procedure TStateGame.Deactivate;
begin
  inherited;
  //TODO: FGame.Pause;
end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  inherited;
  case Event of
    meDown: if Button in [mbRight, mbMiddle] then
      Core.MouseCapture := true;
    meUp: if Button in [mbRight, mbMiddle] then
      Core.MouseCapture := false;
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
begin
  Result := inherited SysNotify(Notify);
  case Notify of
    snMinimize: Core.SwitchState(SIDMenu);
    snConsoleActive: Result := true;
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
end;

function TStateGame.GetName: string;
begin
  Result := SIDGame;
end;

function TStateGame.GetCanResumeGame: Boolean;
begin
  Result := Assigned(FGame);
end;

end.