unit PlayerActions;

interface

uses
  AvL, avlUtils, avlEventBus, avlMath, avlVectors, VSECore, Game, GameObjects;

type
  TPlaceCharacterAction = class(TPlayerAction)
  private
    FChar: TCharacter;
    function CheckQuarter(Quarter: TQuarter): Boolean;
    procedure CharSelected(Sender: TObject; Char: TCharacter);
    procedure MouseEvent(Sender: TObject; const Args: array of const);
    procedure HighlightQuarter(Sender: TObject; const Args: array of const);
  public
    constructor Create(Player: TPlayer);
    destructor Destroy; override;
  end;
  TQuarterSelectCriteria = (qscFree, qscOneStep, qscTwoSteps);
  TSelectQuarterAction = class(TPlayerAction)
  private
    FCriteria: TQuarterSelectCriteria;
    function CheckQuarter(Quarter: TQuarter): Boolean;
    procedure MouseEvent(Sender: TObject; const Args: array of const);
    procedure HighlightQuarter(Sender: TObject; const Args: array of const);
  public
    constructor Create(Player: TPlayer; Criteria: TQuarterSelectCriteria);
    destructor Destroy; override;
  end;

implementation

uses GameData, GameForms, Scene
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TPlaceCharacterAction }

constructor TPlaceCharacterAction.Create(Player: TPlayer);
begin
  inherited;
  FForm := TCharSelectForm.Create(Player.Objects);
  (FForm as TCharSelectForm).OnSelect := CharSelected;
  EventBus.SendEvent(SceneShowTitleMessage, Self, ['Размести персонажа', 1, Player]);
  {$IFDEF VSE_DEBUG}
  Player.Game.ActivePlayer := Player;
  {$ENDIF} 
end;

destructor TPlaceCharacterAction.Destroy;
begin
  EventBus.RemoveListeners([HighlightQuarter, MouseEvent]);
  FAN(FForm);
  inherited;
end;

function TPlaceCharacterAction.CheckQuarter(Quarter: TQuarter): Boolean;
begin
  Result := (Quarter.Index > 0) and not Assigned(Quarter.Objects.ObjOfType[TCharacter, 0]);
end;

procedure TPlaceCharacterAction.CharSelected(Sender: TObject; Char: TCharacter);
begin
  FForm := nil;
  FChar := Char;
  EventBus.AddListener(SceneOnQuarterHighlight, HighlightQuarter);
  EventBus.AddListener(GameOnMouseEvent, MouseEvent);
end;

procedure TPlaceCharacterAction.MouseEvent(Sender: TObject; const Args: array of const);
var
  Obj: TGameObject;
begin
  Assert((Length(Args) = 2) and (Args[0].VType = vtInteger) and (Args[1].VType = vtPointer));
  if FPlayer.Game.ActivePlayer <> FPlayer then Exit;
  if TMouseEvent(Args[0].VInteger) = meDown then
  begin
    Obj := FPlayer.Game.Scene.ObjectAtMouse;
    if not ((Obj is TQuarter) and CheckQuarter(Obj as TQuarter)) then Exit;
    with TVector3D(Args[1].VPointer^) do
      FChar.Pos := Vector3D(X, 0, Z);
    FChar.Quarter := Obj as TQuarter;
    FPlayer.Objects.Remove(FChar);
    EventBus.SendEvent(SceneAddObject, Self, [FChar]);
    Complete;
  end;
end;

procedure TPlaceCharacterAction.HighlightQuarter(Sender: TObject; const Args: array of const);
const
  HlColor: array[Boolean] of TColor = (clRed, clLime);
begin
  Assert((Length(Args) = 1) and (Args[0].VType = vtObject));
  if FPlayer.Game.ActivePlayer <> FPlayer then Exit;
  EventBus.SendEvent(SceneSetQuarterHlColor, Self, [HlColor[CheckQuarter(Args[0].VObject as TQuarter)]])
end;

{ TSelectQuarterAction }

constructor TSelectQuarterAction.Create(Player: TPlayer; Criteria: TQuarterSelectCriteria);
begin
  inherited Create(Player);
  EventBus.SendEvent(SceneShowTitleMessage, Self, ['Выбери квартал', 1, Player]);
  EventBus.AddListener(SceneOnQuarterHighlight, HighlightQuarter);
  EventBus.AddListener(GameOnMouseEvent, MouseEvent);
  {$IFDEF VSE_DEBUG}
  Player.Game.ActivePlayer := Player;
  {$ENDIF} 
end;

destructor TSelectQuarterAction.Destroy;
begin
  EventBus.RemoveListeners([HighlightQuarter, MouseEvent]);
  inherited;
end;

function TSelectQuarterAction.CheckQuarter(Quarter: TQuarter): Boolean;
begin
  case FCriteria of
    qscFree: Result := (not FPlayer.Character.Profile.IsDoctor or (Quarter.Index > 0)) and not Assigned(Quarter.Objects.ObjOfType[TCharacter, 0]);
    else raise Exception.Create('Criteria not implemented');
  end;
end;

procedure TSelectQuarterAction.MouseEvent(Sender: TObject; const Args: array of const);
var
  Obj: TGameObject;
begin
  Assert((Length(Args) = 2) and (Args[0].VType = vtInteger) and (Args[1].VType = vtPointer));
  if FPlayer.Game.ActivePlayer <> FPlayer then Exit;
  if TMouseEvent(Args[0].VInteger) = meDown then
  begin
    Obj := FPlayer.Game.Scene.ObjectAtMouse;
    if not ((Obj is TQuarter) and CheckQuarter(Obj as TQuarter)) then Exit;
    with TVector3D(Args[1].VPointer^) do
      FPlayer.Character.Pos := Vector3D(X, 0, Z);
    FPlayer.Character.Quarter := Obj as TQuarter;
    Complete; 
  end;
end;

procedure TSelectQuarterAction.HighlightQuarter(Sender: TObject; const Args: array of const);
const
  HlColor: array[Boolean] of TColor = (clRed, clLime);
begin
  Assert((Length(Args) = 1) and (Args[0].VType = vtObject));
  if FPlayer.Game.ActivePlayer <> FPlayer then Exit;
  EventBus.SendEvent(SceneSetQuarterHlColor, Self, [HlColor[CheckQuarter(Args[0].VObject as TQuarter)]])
end;

end.
