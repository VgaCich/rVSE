unit Scene;

interface

uses
  AvL, avlUtils, avlEventBus, avlMath, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, VSEPrimitiveModel, GameObjects;

type
  TTitleMsgStatus = record
    Msg: string;
    Pos, Delta: Single;
    Time: Cardinal;
  end;
  TScene = class
  private
    FMap: TPriModel;
    FObjects: TGameObjectsArray;
    FObjectAtMouse: TGameObject;
    FTitleFont, FInfoFont: Cardinal;
    FTitleMsgs: array[0..2] of TTitleMsgStatus;
    FQuarterHlColor: TColor;
    FOnQuarterHighlight: Integer;
    procedure AddObject(Sender: TObject; const Args: array of const);
    procedure RemoveObject(Sender: TObject; const Args: array of const);
    procedure ShowTitleMessage(Sender: TObject; const Args: array of const);
    procedure SetQuarterHlColor(Sender: TObject; const Args: array of const);
    {$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
    function ClearSceneHandler(Sender: TObject; Args: array of const): Boolean;
    {$IFEND}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Update;
    function ObjectAt(X, Y: Integer): TGameObject;
    property Objects: TGameObjectsArray read FObjects;
    property ObjectAtMouse: TGameObject read FObjectAtMouse;
  end;

const
  SceneAddObject = 'Scene.AddObject'; //<In> Object
  SceneRemoveObject = 'Scene.RemoveObject'; //<In> Object
  SceneShowTitleMessage = 'Scene.ShowTitleMessage'; //<In> Message, Level, [Player]
  SceneSetQuarterHlColor = 'Scene.SetQuarterHlColor'; //<In> Color
  SceneOnQuarterHighlight = 'Scene.OnQuarterHighlight'; //<Out> Quarter

implementation

uses VSERender2D, VSETexMan, VSEFormManager, StateMenu, StateGame, Game, GameData
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  TitleMsgMoveTime = 15;
  TitleMsgShowTime = 100;
  TitleMsgPos = 100;
  StencilIsMap = 1;
  StencilObjShift = 2;

{ TScene }

constructor TScene.Create;
begin
  inherited;
  FMap := TPriModel.Create(Core.GetFile('Models\Map.vpm'), true);
  FObjects := TGameObjectsArray.Create;
  FTitleFont := Render2D.CreateFont(UIFont, 20, true);
  FInfoFont := Render2D.CreateFont(UIFont, 12, true);
  EventBus.AddListener(EventBus.RegisterEvent(SceneAddObject), AddObject);
  EventBus.AddListener(EventBus.RegisterEvent(SceneRemoveObject), RemoveObject);
  EventBus.AddListener(EventBus.RegisterEvent(SceneShowTitleMessage), ShowTitleMessage);
  EventBus.AddListener(EventBus.RegisterEvent(SceneSetQuarterHlColor), SetQuarterHlColor);
  FOnQuarterHighlight := EventBus.RegisterEvent(SceneOnQuarterHighlight);
  {$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
  Console.OnCommand['clearscene'] := ClearSceneHandler;
  {$IFEND}
end;

destructor TScene.Destroy;
begin
  EventBus.RemoveListeners([AddObject, RemoveObject, ShowTitleMessage, SetQuarterHlColor]);
  FAN(FObjects);
  FAN(FMap);
  inherited;
end;

procedure TScene.Draw;
const
  LightPos: array[0..3] of GLfloat = (0, 50, 50, 1);
  LightAmbi: array[0..3] of GLfloat = (0.5, 0.5, 0.5, 1);
  LightDiff: array[0..3] of GLfloat = (1, 1, 1, 1);
var
  i, H: Integer;
begin
  glPushAttrib(GL_ENABLE_BIT);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_STENCIL_TEST);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmbi);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glStencilFunc(GL_ALWAYS, StencilIsMap, $FF);
  glColor(1.0, 1.0, 1.0);
  FMap.Draw;
  for i := 0 to FObjects.Count - 1 do
  begin
    glStencilFunc(GL_ALWAYS, StencilObjShift + i, $FF);
    FObjects[i].Draw;
  end;
  with Core.MouseCursor do
    if (Core.CurState is TStateGame) and not (Core.MouseCapture or FormManager.MouseOnForm(X, Y)) then
      FObjectAtMouse := ObjectAt(X, Y)
    else
      FObjectAtMouse := nil;
  if Assigned(FObjectAtMouse) and (FObjectAtMouse is TQuarter) then
  begin
    FQuarterHlColor := clBlack;
    EventBus.SendEvent(FOnQuarterHighlight, Self, [FObjectAtMouse]);
    (FObjectAtMouse as TQuarter).DrawHighlight(FQuarterHlColor);
  end;
  glPopAttrib;
  Render2D.Enter;
  if Assigned(FObjectAtMouse) and (FObjectAtMouse is TCharacter) then
    with Render2D.MapCursor(Core.MouseCursor), FObjectAtMouse as TCharacter do
    begin
      H := Render2D.TextHeight(FInfoFont);
      gleColor($80000000);
      with Render2D.VSBounds do
        Render2D.DrawRect(X, Y - H, Render2D.TextWidth(FInfoFont, Profile.RuName) + 4, H);
      gleColor(clWhite);
      Render2D.TextOut(FInfoFont, X + 2, Y - H, Profile.RuName);
    end;
  H := Render2D.TextHeight(FTitleFont);
  for i := 0 to High(FTitleMsgs) do
    if FTitleMsgs[i].Msg <> '' then
      with FTitleMsgs[i] do
      begin
        gleColor($80000000);
        with Render2D.VSBounds do
          Render2D.DrawRect(Left, TitleMsgPos + i * (H + 5), Right - Left, H);
        gleColor(clWhite);
        Render2D.TextOut(FTitleFont, Pos, TitleMsgPos + i * (H + 5), Msg);
      end;
  Render2D.Leave;
end;

procedure TScene.Update;
var
  i: Integer;
begin
  for i := 0 to FObjects.Count - 1 do
    FObjects[i].Update; //TODO: Replace with UpdateAnim?
  for i := 0 to High(FTitleMsgs) do
    with FTitleMsgs[i] do
    begin
      case Time of
        0 .. TitleMsgMoveTime - 1,
        TitleMsgMoveTime + TitleMsgShowTime .. 2 * TitleMsgMoveTime + TitleMsgShowTime - 1:
          Pos := Pos - Delta;
        2 * TitleMsgMoveTime + TitleMsgShowTime: Msg := '';
      end;
      if Msg <> '' then
        Inc(Time);
    end;
end;

procedure TScene.AddObject(Sender: TObject; const Args: array of const);
begin
  Assert((Length(Args) = 1) and (Args[0].VType = vtObject));
  Objects.Add(Args[0].VObject as TGameObject);
end;

procedure TScene.RemoveObject(Sender: TObject; const Args: array of const);
begin
  Assert((Length(Args) = 1) and (Args[0].VType = vtObject));
  Objects.Remove(Args[0].VObject as TGameObject);
end;

procedure TScene.ShowTitleMessage(Sender: TObject; const Args: array of const);
begin
  Assert((Length(Args) >= 2) and (Args[0].VType = vtAnsiString) and (Args[1].VType = vtInteger));
  Assert((Length(Args) = 2) or (Args[2].VType = vtObject));
  with FTitleMsgs[Max(0, Min(Args[1].VInteger, High(FTitleMsgs)))], Render2D.VSBounds do
  begin
    Msg := string(Args[0].VAnsiString);
    if Length(Args) > 2 then
      Msg := '[' + (Args[2].VObject as TPlayer).Character.Profile.RuName + '] ' + Msg;
    Time := 0;
    Pos := Right;
    Delta := (Right - Left + Render2D.TextWidth(FTitleFont, Msg)) / (2 * TitleMsgMoveTime);
  end;
end;

procedure TScene.SetQuarterHlColor(Sender: TObject; const Args: array of const);
begin
  Assert((Length(Args) = 1) and (Args[0].VType = vtInteger));
  FQuarterHlColor := Args[0].VInteger;
end;

function TScene.ObjectAt(X, Y: Integer): TGameObject;
var
  StencilValue: Byte;
begin
  glReadPixels(X, Core.ResolutionY - Y - 1, 1, 1, GL_STENCIL_INDEX, GL_BYTE, @StencilValue);
  Result := Objects[StencilValue - StencilObjShift];
end;

{$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
function TScene.ClearSceneHandler(Sender: TObject; Args: array of const):
  Boolean;
begin
  FObjects.Clear;
  Result := true;
end;
{$IFEND}

end.
