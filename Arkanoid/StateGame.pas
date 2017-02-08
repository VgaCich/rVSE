unit StateGame;

interface

uses
  Windows, MMSystem, AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSECore, VSEPrimitiveModel;

type
  TStateGame = class;
  TGameObject = class
  private
    FX, FY, FWidth, FHeight: Single;
  protected
    FModel: TPriModel;
    FParent: TStateGame;
  public
    constructor Create(Parent: TStateGame);
    destructor Destroy; override;
    procedure Draw; virtual;
    procedure Update; virtual;
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
  end;
  TBrick = class(TGameObject)
  private
    FHealth: Integer;
  public
    constructor Create(Parent: TStateGame);
    procedure Draw; override;
    procedure Hit;
    function GetRect: TRect;
    property Health: Integer read FHealth;
  end;
  TPaddle = class(TGameObject)
  private
    FOldX, FSpeed: Single;
  public
    constructor Create(Parent: TStateGame);
    procedure Update; override;
    property Speed: Single read FSpeed;
  end;
  TWall = class(TGameObject)
  private
    FShift: Single;
  public
    constructor Create(Parent: TStateGame);
    procedure Draw; override;
  end;
  TBall = class(TGameObject)
  private
    FLaunched: Boolean;
    FOldX, FOldY: Single;
    FPaddle: TPaddle;
    FLives: Integer;
  public
    constructor Create(Parent: TStateGame);
    procedure Update; override;
    procedure Launch;
    procedure Reset;
    property Launched: Boolean read FLaunched;
    property Lives: Integer read FLives;
  end;
  TStateGame=class(TGameState)
  private
    FFont: Cardinal;
    FMouseSens, FLevel, FScore: Integer;
    FBall: TBall;
    FPaddle: TPaddle;
    FWall: TWall;
    FBricks: array of TBrick;
    function GetCanResumeGame: Boolean;
    procedure StartLevel;
  protected
    function  GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure ClearBricks;
    procedure Deactivate; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
    procedure NewGame(Lives: Integer);
    property CanResumeGame: Boolean read GetCanResumeGame;
    property MouseSens: Integer read FMouseSens write FMouseSens;
    property Level: Integer read FLevel;
    property Score: Integer read FScore write FScore;
  end;

implementation

uses
  VSERender2D, VSEMemPak, VSESound, VSETexMan, VSEGUI, VSECollisionCheck,
  {$IFDEF VSE_CONSOLE}VSEConsole,{$ENDIF}{$IFDEF VSE_LOG}VSELog,{$ENDIF}
  StateMenu, StateGameEnd;

const
  SSectionMouse = 'Mouse';
  SMouseSens = 'Sensivity';
  BoardWidth = 15.0;
  BoardHeight = 12.0;
  BallSize = 1.0;
  BallSpeed = 0.5;
  PaddleLevel = -10.0;
  PaddleWidth = 5.0;
  PaddleHeight = 0.3;
  BrickWidth = 4.0;
  BrickHeight = 1.5;

function Sign(X: Single): Integer;
begin
  if X >= 0 then
    Result := 1
  else
    Result := -1;
end;

function Intersects(X11, Y11, X12, Y12, X21, Y21, X22, Y22: Integer): Boolean;

  function Area(X1, Y1, X2, Y2, X3, Y3: Integer): Integer;
  begin
    Result := (X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1);
  end;

  procedure Swap(var A, B: Integer);
  var
    T: Integer;
  begin
    T := A;
    A := B;
    B := T;
  end;

  function Intersect(A, B, C, D: Integer): Boolean;
  begin
    if A > B then Swap(A, B);
    if C > D then Swap(C, D);
    Result := Max(A, C) <= Min(B, D);
  end;

begin
  Result := Intersect(X11, X12, X21, X22) and
            Intersect(Y11, Y12, Y21, Y22) and
            (Area(X11, Y11, X12, Y12, X21, Y21) * Area(X11, Y11, X12, Y12, X22, Y22) <= 0) and
            (Area(X21, Y21, X22, Y22, X11, Y11) * Area(X21, Y21, X22, Y22, X12, Y12) <= 0);
end;

{ TStateGame }

constructor TStateGame.Create;
begin
  inherited Create;
  FFont := Render2D.CreateFont(UIFont, 20);
  FMouseSens := Max(0, Min(Settings.Int[SSectionMouse, SMouseSens], 10));
  if FMouseSens = 0 then
    FMouseSens := 5;
end;

destructor TStateGame.Destroy;
begin
  Settings.Int[SSectionMouse, SMouseSens] := FMouseSens;
  ClearBricks;
  FreeAndNil(FBall);
  FreeAndNil(FPaddle);
  FreeAndNil(FWall);
  inherited Destroy;
end;

procedure TStateGame.Draw;
const
  LightPos: array[0..3] of GLfloat = (0, 5, 10, 1);
  LightAmbi: array[0..3] of GLfloat = (0.1, 0.1, 0.1, 1);
  LightDiff: array[0..3] of GLfloat = (1, 1, 1, 1);
var
  i: Integer;
  S: string;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if BgTex <> 0 then
  begin
    Render2D.Enter;
    gleColor(clWhite);
    TexMan.Bind(BgTex);
    gleColor(clWhite);
    with Render2D.VSBounds do
      Render2D.DrawRect(Left, Top, Right - Left, Bottom - Top, 0, 0, 1, 1);
    TexMan.Unbind;
    Render2D.Leave;
  end;
  glePerspectiveMatrix(60, Core.ResolutionX, Core.ResolutionY);
  glMatrixMode(GL_PROJECTION);
  glTranslate(0, 0, -22);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glEnable(GL_DEPTH_TEST);
  glColor(1.0, 1.0, 1.0);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmbi);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  FWall.Draw;
  FPaddle.Draw;
  FBall.Draw;
  for i := 0 to High(FBricks) do
  FBricks[i].Draw;
  Render2D.Enter;
  gleColor(clText);
  with Render2D.VSBounds do
  begin
    i := Round(Bottom - Render2D.TextHeight(FFont) - 5);
    Render2D.TextOut(FFont, Left + 5, i, 'Level: ' + IntToStr(Level));
    S := 'Score: ' + IntToStr(Score);
    Render2D.TextOut(FFont, (Left + Right - Render2D.TextWidth(FFont, S)) / 2, i, S);
    S := 'Lives: ' + IntToStr(FBall.Lives);
    Render2D.TextOut(FFont, Right - Render2D.TextWidth(FFont, S) - 5, i, S);
  end;
  Render2D.Leave;
end;

procedure TStateGame.Update;
var
  i: Integer;
  BricksLeft: Boolean;
begin
  inherited;
  FWall.Update;
  FPaddle.Update;
  FBall.Update;
  BricksLeft := false;
  for i := 0 to High(FBricks) do
  begin
    FBricks[i].Update;
    if FBricks[i].Health > 0 then
      BricksLeft := true;
  end;
  if not BricksLeft then
  begin
    Inc(FScore, 100 * FLevel);
    Inc(FLevel);
    if Level mod 5 = 0 then
      Inc(FBall.FLives);
    StartLevel;
  end
  else if FBall.Lives <= 0 then
    Core.SwitchState('GameEnd');
end;

function TStateGame.Activate: Cardinal;
begin
  inherited Activate;
  Result := 20;
  glClearColor(0, 0, 0, 1);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_DEPTH_TEST);
  Core.MouseCapture := true;
  Sound.PlayMusic('Music.xm');
end;

procedure TStateGame.Deactivate;
begin
  inherited;
  Core.MouseCapture := false;
  Sound.StopMusic;
end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  inherited;
  if Event = meMove then
  begin
    FPaddle.X := Max(-(BoardWidth - FPaddle.Width / 2), Min(FPaddle.X + Power(1.25, FMouseSens) * X / 200, BoardWidth - FPaddle.Width / 2));
    if not FBall.Launched then
      FBall.X := FPaddle.X;
  end;
  if not FBall.Launched and (Event = meDown) and (Button = mbLeft) then
    FBall.Launch;
end;

procedure TStateGame.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  inherited;
  if Event = keUp then
  begin
    case Key of
      VK_ESCAPE: Core.SwitchState('Menu');
    end;
  end;
end;

function TStateGame.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result := inherited SysNotify(Notify);
  if Notify = snMinimize then
    Core.SwitchState('Menu');
end;

function TStateGame.GetName: string;
begin
  Result := 'Game';
end;

procedure TStateGame.ClearBricks;
var
  i: Integer;
begin
  for i := 0 to High(FBricks) do
    FBricks[i].Free;
  SetLength(FBricks, 0);
end;

procedure TStateGame.NewGame(Lives: Integer);
begin
  if not Assigned(FWall) then
    FWall := TWall.Create(Self);
  if not Assigned(FPaddle) then
    FPaddle := TPaddle.Create(Self);
  if not Assigned(FBall) then
  begin
    FBall := TBall.Create(Self);
    {$IFDEF VSE_CONSOLE}
    Console.OnCommand['lives ?num=i'] := Console.GetConVarHandler(FBall.FLives, cvInt);
    {$ENDIF}
  end;
  FLevel := 1;
  FScore := 0;
  FPaddle.Y := PaddleLevel - FPaddle.Height / 2;
  FBall.FLives := Lives;
  StartLevel;
end;

function TStateGame.GetCanResumeGame: Boolean;
begin
  Result := Length(FBricks) <> 0;
end;

procedure TStateGame.StartLevel;
var
  i: Integer;
begin
  ClearBricks;
  SetLength(FBricks, 20);
  for i := 0 to High(FBricks) do
  begin
    FBricks[i] := TBrick.Create(Self);
    FBricks[i].X := (i mod 5 - 2) * 5;
    FBricks[i].Y := (i div 5) * 2 + 3;
  end;
  FPaddle.X := 0;
  FBall.Reset;
end;

{ TGameObject }

constructor TGameObject.Create(Parent: TStateGame);
begin
  FParent := Parent;
end;

destructor TGameObject.Destroy;
begin
  FreeAndNil(FModel);
  inherited;
end;

procedure TGameObject.Draw;
begin
  glPushMatrix;
  glTranslate(FX, FY, 0);
  FModel.Draw;
  glPopMatrix;
end;

procedure TGameObject.Update;
begin

end;

{ TBall }

constructor TBall.Create(Parent: TStateGame);
begin
  inherited;
  FPaddle := FParent.FPaddle;
  FModel := TPriModel.Create('Ball.vpm');
  FWidth := BallSize;
  FHeight := BallSize;
end;

procedure TBall.Launch;
begin
  FLaunched := true;
  PlaySound(PChar(SND_ALIAS_SYSTEMASTERISK), 0, SND_ALIAS_ID or SND_ASYNC);
  FOldX := X;
  FOldY := Y;
  X := X + (Random(2) - 0.5) * BallSpeed + FPaddle.Speed / 5;
  Y := Y + BallSpeed;
end;

procedure TBall.Reset;
begin
  FLaunched := false;
  X := FPaddle.X;
  Y := FPaddle.Y + FPaddle.Height / 2 + Height / 2;
end;

procedure TBall.Update;
var
  DeltaX, DeltaY: Single;
  Rect: TRect;
  i, iX, iY, iOX, iOY: Integer;
begin
  inherited;
  DeltaX := X - FOldX;
  DeltaY := Y - FOldY;
  if FLaunched then
  begin
    X := X + DeltaX;
    if (X > BoardWidth) or (X < -BoardWidth) then
    begin
      X := Sign(X) * 2 * BoardWidth - X;
      DeltaX := -DeltaX;
      PlaySound(PChar(SND_ALIAS_SYSTEMQUESTION), 0, SND_ALIAS_ID or SND_ASYNC);
    end;
    Y := Y + DeltaY;
    if Y > BoardHeight then
    begin
      Y := 2 * BoardHeight - Y;
      DeltaY := -DeltaY;
      PlaySound(PChar(SND_ALIAS_SYSTEMQUESTION), 0, SND_ALIAS_ID or SND_ASYNC);
    end
    else if (Y < PaddleLevel) and (Y - DeltaY >= PaddleLevel) and
            (Abs(X - FPaddle.X) < FPaddle.Width / 2) then
    begin
      Y := 2 * PaddleLevel - Y;
      DeltaX := DeltaX + FPaddle.Speed / 8;
      DeltaY := -DeltaY;
      PlaySound(PChar(SND_ALIAS_SYSTEMQUESTION), 0, SND_ALIAS_ID or SND_ASYNC);
    end
    else if Y < -BoardHeight then
    begin
      PlaySound(PChar(SND_ALIAS_SYSTEMHAND), 0, SND_ALIAS_ID or SND_ASYNC);
      Dec(FLives);
      if Lives > 0 then
        FParent.Score := FParent.Score - 250;
      Reset;
    end;
    for i := 0 to High(FParent.FBricks) do
    begin
      Rect := FParent.FBricks[i].GetRect;
      iX := Round(100 * X);
      iY := Round(100 * Y);
      iOX := Round(100 * FOldX);
      iOY := Round(100 * FOldY);
      if (FParent.FBricks[i].Health > 0) and PointInRect(Point(iX, iY), Rect) then
      begin
        FParent.FBricks[i].Hit;
        if Intersects(iOX, iOY, iX, iY, Rect.Left, Rect.Top, Rect.Right, Rect.Top) or
           Intersects(iOX, iOY, iX, iY, Rect.Left, Rect.Bottom, Rect.Right, Rect.Bottom) then
          DeltaY := -DeltaY;
        if Intersects(iOX, iOY, iX, iY, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom) or
           Intersects(iOX, iOY, iX, iY, Rect.Right, Rect.Top, Rect.Right, Rect.Bottom) then
          DeltaX := -DeltaX;
      end;
    end;
  end;
  FOldX := X - DeltaX;
  FOldY := Y - DeltaY;
end;

{ TBrick }

constructor TBrick.Create(Parent: TStateGame);
begin
  inherited;
  FModel := TPriModel.Create('Brick.vpm');
  FHealth := Random(Parent.Level) + 1;
  if FHealth <= 2 then
    FModel.Materials[1].Texture := TexMan.GetTex('Grass');
  FWidth := BrickWidth;
  FHeight := BrickHeight;
end;

procedure TBrick.Draw;
begin
  if FHealth > 0 then
    inherited;
end;

function TBrick.GetRect: TRect;
begin
  Result := Rect(Round(100 * (X - Width / 2) - 50 * BallSize),
                 Round(100 * (Y - Height / 2) - 50 * BallSize),
                 Round(100 * (X + Width / 2) + 50 * BallSize),
                 Round(100 * (Y + Height / 2) + 50 * BallSize));
end;

procedure TBrick.Hit;
begin
  Dec(FHealth);
  FParent.Score := FParent.Score + 5;
  if FHealth <= 0 then
  begin
    FParent.Score := FParent.Score + 20;
    PlaySound(PChar(SND_ALIAS_SYSTEMEXCLAMATION), 0, SND_ALIAS_ID or SND_ASYNC)
  end
  else
    PlaySound(PChar(SND_ALIAS_SYSTEMQUESTION), 0, SND_ALIAS_ID or SND_ASYNC)
end;

{ TPaddle }

constructor TPaddle.Create(Parent: TStateGame);
begin
  inherited;
  FModel := TPriModel.Create('Paddle.vpm');
  FWidth := PaddleWidth;
  FHeight := PaddleHeight;
end;

procedure TPaddle.Update;
begin
  inherited;
  FSpeed := X - FOldX;
  FOldX := X;
end;

{ TWall }

const
  ObjBack = $4B434142;
  ObjWall = $4C4C4157;

constructor TWall.Create(Parent: TStateGame);
begin
  inherited;
  FModel := TPriModel.Create('Wall.vpm');
  FWidth := BoardWidth * 2;
  FHeight := BoardHeight * 2;
end;

procedure TWall.Draw;
begin
  glPushMatrix;
  glTranslate(FX, FY, 0);
  FModel.Objects[ObjWall].Visible := false;
  FModel.Objects[ObjBack].Visible := true;
  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glLoadIdentity;
  if BGTex <> 0 then
    FModel.Materials[FModel.Objects[ObjBack].MaterialID].Diffuse := $80FFFFFF;
  FShift := FShift + 0.015 / Core.FPS;
  if FShift > 1 then
    FShift := FShift - 1;
  glTranslate(FShift, 0, 0);
  glMatrixMode(GL_MODELVIEW);
  FModel.Draw;
  glMatrixMode(GL_TEXTURE);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  FModel.Objects[ObjWall].Visible := true;
  FModel.Objects[ObjBack].Visible := false;
  FModel.Draw;
  glPopMatrix;
end;

end.
