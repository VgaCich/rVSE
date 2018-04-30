unit Scene;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
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
    FObjects: array of TGameObject;
    FTitleFont, FInfoFont: Cardinal;
    FTitleMsgStatus: TTitleMsgStatus;
    function GetObjCount: Integer;
    function GetObject(Index: Integer): TGameObject;
    {$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
    function ClearSceneHandler(Sender: TObject; Args: array of const): Boolean;
    {$IFEND}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Update;
    function AddObject(Obj: TGameObject): Integer;
    procedure RemoveObject(Obj: TGameObject); overload;
    procedure RemoveObject(Index: Integer); overload;
    procedure ShowTitleMessage(const Msg: string);
    function ObjectAt(X, Y: Integer): TGameObject;
    property ObjectsCount: Integer read GetObjCount;
    property Objects[Index: Integer]: TGameObject read GetObject; default;
  end;

const
  MapBounds: record
    Min: TVector2D;
    Max: TVector2D;
  end = (Min: (X: -60.0; Y: -30.0); Max: (X: 60.0; Y: 45.0));

implementation

uses VSERender2D, VSETexMan, VSEMemPak, VSEFormManager, StateMenu
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
  FMap := TPriModel.Create('Models\Map.vpm');
  FTitleFont := Render2D.CreateFont(UIFont, 20, false);
  FInfoFont := Render2D.CreateFont(UIFont, 12, true);
  {$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
  Console.OnCommand['clearscene'] := ClearSceneHandler;
  {$IFEND}
end;

destructor TScene.Destroy;
begin
  Finalize(FObjects);
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
  Obj: TGameObject;
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
  for i := 0 to High(FObjects) do
  begin
    glStencilFunc(GL_ALWAYS, StencilObjShift + i, $FF);
    FObjects[i].Draw;
  end;
  with Core.MouseCursor do
    if not FormManager.MouseBusy(X, Y) then
      Obj := ObjectAt(X, Y)
    else
      Obj := nil;
  if Assigned(Obj) and (Obj is TQuarter) then 
    (Obj as TQuarter).DrawHighlight(clLime);
  glPopAttrib;
  Render2D.Enter;
  if Assigned(Obj) and (Obj is TCharacter) then
    with Render2D.MapCursor(Core.MouseCursor), Obj as TCharacter do
    begin
      H := Render2D.TextHeight(FInfoFont);
      gleColor($80000000);
      with Render2D.VSBounds do
        Render2D.DrawRect(X, Y - H, Render2D.TextWidth(FInfoFont, Profile.RuName) + 4, H);
      gleColor(clWhite);
      Render2D.TextOut(FInfoFont, X + 2, Y - H, Profile.RuName);
    end;
  with FTitleMsgStatus do
    if Msg <> '' then
    begin
      gleColor($80000000);
      with Render2D.VSBounds do
        Render2D.DrawRect(Left, TitleMsgPos, Right - Left, Render2D.TextHeight(FTitleFont));
      gleColor(clWhite);
      Render2D.TextOut(FTitleFont, Pos, TitleMsgPos, Msg);
    end;
  Render2D.Leave;
end;

procedure TScene.Update;
begin
  with FTitleMsgStatus do
    if Msg <> '' then
    begin
      case Time of
        0 .. TitleMsgMoveTime - 1,
        TitleMsgMoveTime + TitleMsgShowTime .. 2 * TitleMsgMoveTime + TitleMsgShowTime - 1:
          Pos := Pos - Delta;
        2 * TitleMsgMoveTime + TitleMsgShowTime: Msg := '';
      end;
      Inc(Time);
    end;
end;

function TScene.AddObject(Obj: TGameObject): Integer;
begin
  SetLength(FObjects, Length(FObjects) + 1);
  FObjects[High(FObjects)] := Obj;
  Result := High(FObjects);
end;

procedure TScene.RemoveObject(Obj: TGameObject);
var
  i: Integer;
begin
  for i := 0 to High(FObjects) do
    if FObjects[i] = Obj then
      RemoveObject(i);
end;

procedure TScene.RemoveObject(Index: Integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index > High(FObjects)) then Exit;
  for i := Index to High(FObjects) - 1 do
    FObjects[i] := FObjects[i + 1];
  SetLength(FObjects, Length(FObjects) - 1);
end;

procedure TScene.ShowTitleMessage(const Msg: string);
begin
  FTitleMsgStatus.Msg := Msg;
  with FTitleMsgStatus, Render2D.VSBounds do
  begin
    Time := 0;
    Pos := Right;
    Delta := (Right - Left + Render2D.TextWidth(FTitleFont, Msg)) / (2 * TitleMsgMoveTime);
  end;
end;

function TScene.ObjectAt(X, Y: Integer): TGameObject;
var
  StencilValue: Byte;
begin
  glReadPixels(X, Core.ResolutionY - Y - 1, 1, 1, GL_STENCIL_INDEX, GL_BYTE, @StencilValue);
  Result := Objects[StencilValue - StencilObjShift];
end;

function TScene.GetObjCount: Integer;
begin
  Result := Length(FObjects);
end;

function TScene.GetObject(Index: Integer): TGameObject;
begin
  if (Index >= 0) and (Index < Length(FObjects)) then
    Result := FObjects[Index]
  else
    Result := nil;
end;

{$IF Defined(VSE_DEBUG) and Defined(VSE_CONSOLE)}
function TScene.ClearSceneHandler(Sender: TObject; Args: array of const):
  Boolean;
begin
  Finalize(FObjects);
  Result := true;
end;
{$IFEND}

end.
