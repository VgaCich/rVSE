unit GameStages;

interface

uses
  AvL, avlUtils, avlEventBus, avlMath, avlVectors, VSECore, Game;

type
  TStageStart = class(TGameStage)
  private
    FCurrentPlayer: Integer;
    FSceneSetQuarterHlColor: Integer;
    procedure ActionCompleted(Sender: TObject; const Args: array of const);
  public
    constructor Create(Game: TGame);
    destructor Destroy; override;
  end;
  TStageDoctorPrepare = class(TGameStage)
  private
    FPlayer: TPlayer;
    FCtr: Integer;
  public
    constructor Create(Game: TGame; Player: TPlayer);
    function Update: TGameStage; override;
  end;

implementation

uses VSEMemPak, GameData, Scene, GameObjects, PlayerActions, Missions
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TStageStart }

constructor TStageStart.Create(Game: TGame);
var
  i: Integer;
  r: TResourceType;
  Char: TCharacter;
begin
  inherited;
  FCurrentPlayer := 0;
  FSceneSetQuarterHlColor := EventBus.GetEventId(SceneSetQuarterHlColor);
  EventBus.AddListener(EventBus.RegisterEvent(PlayerOnActionCompleted), ActionCompleted);
  EventBus.SendEvent(SceneShowTitleMessage, Self, ['Подготовка', 0]);
  for i := 0 to High(PlayerNames) do
    with FGame.Player[PlayerNames[i]] do
      if Character.Profile.IsDoctor then
      begin
        Arguments := 3;
        for r := Low(TResourceType) to High(TResourceType) do
          Resources[r] := 0;
        Resources[(Objects[Objects.Add(FGame.Character[Name])] as TCharacter).Profile.Resource] := 1;
        //TODO: Give Recipes deck and one Recipe
      end
      else begin
        //TODO: Give plague decks of Doomed and Strains, select Targets
      end;
  for i := 0 to High(Characters) do
  begin
    Char := FGame.Character[Characters[i].Name];
    Char.Quarantined := true;
    if not Char.Profile.IsDoctor and (Char.Profile.Master <> '') then
      FGame.Player[Char.Profile.Master].Objects.Add(Char);
  end;
  ActionCompleted(Self, []);
end;

destructor TStageStart.Destroy;
begin
  EventBus.RemoveListener(ActionCompleted);
  inherited;
end;

procedure TStageStart.ActionCompleted(Sender: TObject; const Args: array of const);
begin
  if Sender = FGame.Player[SPlague] then
  begin
    FNextStage := TStageDoctorPrepare.Create(FGame, FGame.Player[SBachelor]);
    Exit;
  end;
  Inc(FCurrentPlayer);
  if FCurrentPlayer > High(PlayerNames) then
    FCurrentPlayer := 1;
  if Assigned(FGame.Player[PlayerNames[FCurrentPlayer]].Objects.ObjOfType[TCharacter, 0]) then
    TPlaceCharacterAction.Create(FGame.Player[PlayerNames[FCurrentPlayer]])
  else
    TSelectQuarterAction.Create(FGame.Player[SPlague], qscFree);
end;

{ TStageDoctorPrepare }

constructor TStageDoctorPrepare.Create(Game: TGame; Player: TPlayer);
begin
  inherited Create(Game);
  EventBus.SendEvent(SceneShowTitleMessage, Self, [Player.Character.Profile.RuName + ': подготовка', 0])
end;

function TStageDoctorPrepare.Update: TGameStage;
begin
  Result := inherited Update;
  Inc(FCtr);
  if (FCtr mod 200 = 0) and (FGame.Missions.Count > 0) then
    (FGame.Missions.Take as TMission).Activate(FGame.ActivePlayer);
end;

end.
