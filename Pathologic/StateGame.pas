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
    FFreeList: TList;
    FFormsSet: TGUIFormsSet;
    FCamera: TCamera;
    FScene: TScene;
    FGame: TGame;
    FMouse3D: TVector3D;
    FOnMouseEvent: Integer;
    function GetCanResumeGame: Boolean;
    procedure FreeList;
    procedure FreeListAdd(Sender: TObject; const Args: array of const);
    procedure FreeListRemove(Sender: TObject; const Args: array of const);
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
    function MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean; override;
    function KeyEvent(Key: Integer; Event: TKeyEvent): Boolean; override;
    function SysNotify(Notify: TSysNotify): Boolean; override;
    procedure NewGame;
    property CanResumeGame: Boolean read GetCanResumeGame;
    property Mouse3D: TVector3D read FMouse3D;
  end;

const
  SIDGame = 'Game';

implementation

uses VSERender2D, VSETexMan, VSEFormManager, VSEBindMan, VSECollisionCheck
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF},
  StateMenu, GameForms, GameObjects, GameData;

const
  MoveBorder = 25;

constructor TStateGame.Create;
begin
  inherited Create;
  FOnMouseEvent := EventBus.RegisterEvent(GameOnMouseEvent);
  FFreeList := TList.Create;
  EventBus.AddListener(EventBus.RegisterEvent(GameObjects.FreeListAdd), FreeListAdd);
  EventBus.AddListener(EventBus.RegisterEvent(GameObjects.FreeListRemove), FreeListRemove);
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
  EventBus.RemoveListeners([FreeListAdd, FreeListRemove, PlayerActionsChanged, ActivePlayerChanged]);
  FreeList;
  FAN(FCamera);
  FAN(FFormsSet);
  FAN(FFreeList);
  inherited Destroy;
end;

procedure TStateGame.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  DrawBackground;
  glePerspectiveMatrix2(60, Core.ResolutionX, Core.ResolutionY, 1, 500);
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
  inherited;
  with Core do
    FCamera.Move(Vector2D(
      Move[
        BindMan.BindActive[BindCamLeft] or PointInRect(MouseCursor, Rect(0, 0, MoveBorder, ResolutionY)),
        BindMan.BindActive[BindCamRight] or PointInRect(MouseCursor, Rect(ResolutionX - MoveBorder, 0, ResolutionX, ResolutionY))],
      Move[
        BindMan.BindActive[BindCamFwd] or PointInRect(MouseCursor, Rect(0, 0, ResolutionX, MoveBorder)),
        BindMan.BindActive[BindCamBwd] or PointInRect(MouseCursor, Rect(0, ResolutionY - MoveBorder, ResolutionX, ResolutionY))]),
      MapBounds.Min, MapBounds.Max);
  if Assigned(FGame) then
  begin
    EventBus.SendEvent(FOnMouseEvent, FGame, [Integer(meMove), @FMouse3D]);
    FGame.Update;
    FScene.Update;
  end;
end;

function TStateGame.Activate: Cardinal;
begin
  inherited Activate;
  Result := 20;
  glClearColor(0, 0, 0, 1);
  glClearStencil(0);
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

function TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean;
begin
  Result := inherited MouseEvent(Button, Event, X, Y);
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

function TStateGame.KeyEvent(Key: Integer; Event: TKeyEvent): Boolean;
begin
  Result := inherited KeyEvent(Key, Event);
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
  if Notify = snMinimize then
    Core.SwitchState(SIDMenu);
end;

procedure TStateGame.NewGame;
begin
  FreeList;
  FCamera.Eye := Vector3D(0, 50, 45);
  FCamera.Angle := Vector2D(180, -60);
  FScene := TScene.Create;
  EventBus.SendEvent(GameObjects.FreeListAdd, FScene, []);
  FGame := TGame.Create(FScene);
  EventBus.SendEvent(GameObjects.FreeListAdd, FGame, []);
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

procedure TStateGame.FreeList;
var
  Item: TObject;
begin
  while FFreeList.Count > 0 do
  begin
    Item := TObject(FFreeList.Last);
    FFreeList.Remove(Item);
    Item.Free;
  end;
end;

procedure TStateGame.FreeListAdd(Sender: TObject; const Args: array of const);
begin
  Assert(Assigned(Sender) and (Length(Args) = 0));
  if FFreeList.IndexOf(Sender) < 0 then
    FFreeList.Add(Sender);
end;

procedure TStateGame.FreeListRemove(Sender: TObject; const Args: array of const);
begin
  Assert(Assigned(Sender) and (Length(Args) = 0));
  FFreeList.Remove(Sender);
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
