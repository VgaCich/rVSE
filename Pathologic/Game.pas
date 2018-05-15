unit Game;

interface

uses
  AvL, avlUtils, avlEventBus, avlMath, avlVectors, VSECore, VSEGUI, Scene,
  GameData, GameObjects;

type
  TPlayer = class;
  TGameStage = class;
  TGame = class
  private
    FScene: TScene;
    FStage: TGameStage;
    FActivePlayer: TPlayer;
    FCharacters: array of TCharacter;
    FPlayers: array of TPlayer;
    FMissions: TDeck;
    function GetCharacter(const Name: string): TCharacter;
    function GetPlayer(const Name: string): TPlayer;
    procedure SetActivePlayer(const Value: TPlayer);
  public
    constructor Create(Scene: TScene);
    destructor Destroy; override;
    procedure Update;
    property Character[const Name: string]: TCharacter read GetCharacter;
    property Player[const Name: string]: TPlayer read GetPlayer;
    property Missions: TDeck read FMissions;
    property Scene: TScene read FScene;
    property Stage: TGameStage read FStage;
    property ActivePlayer: TPlayer read FActivePlayer write SetActivePlayer;
  end;
  TPlayerAction = class;
  TPlayerActionsArray = array of TPlayerAction;
  TPlayer = class
  private
  protected
    FName: string;
    FCharacter: TCharacter;
    FVictoryChip: TChip;
    FGame: TGame;
    FArguments: Integer;
    FAvailActions: TPlayerActionsArray;
    FObjects: TGameObjectsArray;
    FDeck: TDeck;
    function VictoryPos(Arguments: Integer): TVector3D;
    procedure SetArguments(Value: Integer);
  public
    Resources: array[TResourceType] of Integer;
    constructor Create(Game: TGame; const Name: string);
    destructor Destroy; override;
    procedure Update; virtual;
    procedure AddAction(Action: TPlayerAction);
    procedure RemoveAction(Action: TPlayerAction);
    procedure ClearActions;
    property Game: TGame read FGame;
    property Name: string read FName;
    property Character: TCharacter read FCharacter;
    property Arguments: Integer read FArguments write SetArguments;
    property AvailActions: TPlayerActionsArray read FAvailActions;
    property Objects: TGameObjectsArray read FObjects;
    property Deck: TDeck read FDeck;
  end;
  TPlayerAction = class
  protected
    FPlayer: TPlayer;
    FForm: TGUIForm;
    procedure Complete;
  public
    constructor Create(Player: TPlayer);
    destructor Destroy; override;
    property Form: TGUIForm read FForm;
  end;
  TGameStage = class
  protected
    FGame: TGame;
    FNextStage: TGameStage;
  public
    constructor Create(Game: TGame);
    function Update: TGameStage; virtual;
  end;

const
  GameOnActivePlayerChanged = 'Game.OnActivePlayerChanged'; //<Out> PrevPlayer, NewPlayer
  GameOnMouseEvent = 'Game.OnMouseEvent'; //<Out> Event, Mouse3D 
  PlayerOnActionsChanged = 'Player.OnActionsChanged'; //<Out>
  PlayerOnActionCompleted = 'Player.OnActionCompleted'; //<Out> Action

implementation

uses {VSERender2D, VSETexMan,} VSEMemPak, GameStages, Missions
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TGame }

constructor TGame.Create(Scene: TScene);
var
  i: Integer;
begin
  inherited Create;
  EventBus.RegisterEvent(GameOnActivePlayerChanged);
  FScene := Scene;
  for i := Low(TQuarterIndex) to High(TQuarterIndex) do
    FScene.Objects.Add(TQuarter.Create(i));
  SetLength(FCharacters, Length(Characters));
  for i := 0 to High(FCharacters) do
    with Characters[i] do
      FCharacters[i] := TCharacter.Create(Name, Profile^);
  SetLength(FPlayers, Length(PlayerNames));
  for i := 0 to High(PlayerNames) do
    FPlayers[i] := TPlayer.Create(Self, PlayerNames[i]);
  FMissions := TDeck.Create;
  CreateMissions(FMissions);
  FStage := TStageStart.Create(Self);
end;

destructor TGame.Destroy;
var
  i: Integer;
begin
  FScene.Objects.Clear;
  FStage.Free;
  FAN(FMissions);
  for i := 0 to High(FPlayers) do
    FAN(FPlayers[i]);
  Finalize(FPlayers);
  Finalize(FCharacters);
  inherited;
end;

procedure TGame.Update;
var
  i: Integer;
begin
  FStage := FStage.Update;
  for i := 0 to High(FPlayers) do
    FPlayers[i].Update;
end;

function TGame.GetCharacter(const Name: string): TCharacter;
var
  i: Integer;
begin
  for i := 0 to High(FCharacters) do
    if FCharacters[i].Name = Name then
    begin
      Result := FCharacters[i];
      Exit;
    end;
  Result := nil;
end;

function TGame.GetPlayer(const Name: string): TPlayer;
var
  i: Integer;
begin
  for i := 0 to High(FPlayers) do
    if FPlayers[i].Name = Name then
    begin
      Result := FPlayers[i];
      Exit;
    end;
  Result := nil;
end;

procedure TGame.SetActivePlayer(const Value: TPlayer);
var
  PrevPlayer: TPlayer;
begin
  PrevPlayer := FActivePlayer;
  FActivePlayer := Value;
  EventBus.SendEvent(GameOnActivePlayerChanged, Self, [PrevPlayer, Value]);
end;

{ TPlayer }

constructor TPlayer.Create(Game: TGame; const Name: string);
begin
  inherited Create;
  FName := Name;
  FGame := Game;
  FCharacter := FGame.GetCharacter(FName);
  FObjects := TGameObjectsArray.Create;
  FDeck := TDeck.Create;
  if Name <> SPlague then
  begin
    FVictoryChip := TChip.Create(FName, 0.75);
    FVictoryChip.Pos := VictoryPos(3);
    EventBus.SendEvent(SceneAddObject, Self, [FVictoryChip]);
  end;
end;

destructor TPlayer.Destroy;
begin
  ClearActions;
  FAN(FDeck);
  FAN(FObjects);
  inherited;
end;

procedure TPlayer.Update;
begin

end;

procedure TPlayer.AddAction(Action: TPlayerAction);
begin
  SetLength(FAvailActions, Length(FAvailActions) + 1);
  FAvailActions[High(FAvailActions)] := Action;
  EventBus.SendEvent(PlayerOnActionsChanged, Self, []);
end;

procedure TPlayer.RemoveAction(Action: TPlayerAction);
var
  i, j: Integer;
begin
  for i := 0 to High(FAvailActions) do
    if FAvailActions[i] = Action then
    begin
      for j := i to High(FAvailActions) - 1 do
        FAvailActions[j] := FAvailActions[j + 1];
      SetLength(FAvailActions, Length(FAvailActions) - 1);
      EventBus.SendEvent(PlayerOnActionsChanged, Self, []);
      Break;
    end;
end;

procedure TPlayer.ClearActions;
begin
  while Length(FAvailActions) > 0 do
    FAvailActions[High(FAvailActions)].Free;
  EventBus.SendEvent(PlayerOnActionsChanged, Self, []);
end;

function TPlayer.VictoryPos(Arguments: Integer): TVector3D;
var
  i: Integer;
begin
  for i := 0 to High(VictoryTracks) do
    with VictoryTracks[i] do
      if Name = FName then
        Result := Track[Max(0, Min(Arguments - 1, High(Track)))];
end;

procedure TPlayer.SetArguments(Value: Integer);
begin
  FArguments := Max(1, Value);
  FVictoryChip.AddAnimationStep(aaMoveTo, VictoryPos(FArguments), 100);
end;

{ TPlayerAction }

procedure TPlayerAction.Complete;
begin
  EventBus.SendEvent(PlayerOnActionCompleted, FPlayer, [Self]);
  Free;
end;

constructor TPlayerAction.Create(Player: TPlayer);
begin
  inherited Create;
  FPlayer := Player;
  FPlayer.AddAction(Self);
end;

destructor TPlayerAction.Destroy;
begin
  FPlayer.RemoveAction(Self);
  inherited;
end;

{ TGameStage }

constructor TGameStage.Create(Game: TGame);
begin
  inherited Create;
  FGame := Game;
end;

function TGameStage.Update: TGameStage;
begin
  if Assigned(FNextStage) then
  begin
    Result := FNextStage;
    Free;
  end
  else
    Result := Self;
end;

end.
