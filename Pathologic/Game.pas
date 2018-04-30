unit Game;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, VSECore, Scene, GameObjects;

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
    function GetCharacter(const Name: string): TCharacter;
    function GetPlayer(const Name: string): TPlayer;
    procedure SetActivePlayer(const Value: TPlayer);
  public
    constructor Create(Scene: TScene);
    destructor Destroy; override;
    procedure Update;
    property Character[const Name: string]: TCharacter read GetCharacter;
    property Player[const Name: string]: TPlayer read GetPlayer;
    property Scene: TScene read FScene;
    property Stage: TGameStage read FStage;
    property ActivePlayer: TPlayer read FActivePlayer write SetActivePlayer;
  end;
  TPlayer = class
  protected
    FName: string;
    FCharacter: TCharacter;
    FGame: TGame;
  public
    constructor Create(Game: TGame);
    procedure Update; virtual;
    property Name: string read FName;
  end;
  TGameStage = class
  protected
    FGame: TGame;
  public
    constructor Create(Game: TGame);
    function Update: TGameStage; virtual;
  end;

implementation

uses {VSERender2D, VSETexMan,} VSEMemPak, GameData, GameStages, Players
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TGame }

constructor TGame.Create(Scene: TScene);
var
  i: Integer;
begin
  inherited Create;
  FScene := Scene;
  for i := Low(TQuarterIndex) to High(TQuarterIndex) do
    FScene.AddObject(TQuarter.Create(i));
  FStage := TStageStart.Create(Self);
  SetLength(FCharacters, Length(Characters));
  for i := 0 to High(FCharacters) do
  begin
    with Characters[i] do
      FCharacters[i] := TCharacter.Create(Name, Profile^);
    with FCharacters[i] do
    begin
      Pos := Vector3D(-60.0 + 120.0 * Random, 0, -30.0 + 60.0 * Random);
      Quarantined := Random(2) = 1;
      Visible := true;
    end;
    FScene.AddObject(FCharacters[i]);
  end;
  SetLength(FPlayers, 4);
  FPlayers[0] := TPlayerPlague.Create(Self);
  FPlayers[1] := TPlayerBachelor.Create(Self);
  FPlayers[2] := TPlayerHaruspex.Create(Self);
  FPlayers[3] := TPlayerChangeling.Create(Self);
end;

destructor TGame.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FPlayers) do
    FAN(FPlayers[i]);
  Finalize(FPlayers);
  for i := 0 to High(FCharacters) do
  begin
    FScene.RemoveObject(FCharacters[i]);
    FAN(FCharacters[i]);
  end;
  Finalize(FCharacters);
  FStage.Free;
  inherited;
end;

procedure TGame.Update;
var
  i: Integer;
begin
  FStage := FStage.Update;
  for i := 0 to High(FPlayers) do
    FPlayers[i].Update;
  for i := 0 to High(FCharacters) do
    FCharacters[i].Update;
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
begin
  FActivePlayer := Value;
end;

{ TPlayer }

constructor TPlayer.Create(Game: TGame);
begin
  inherited Create;
  FGame := Game;
  FCharacter := FGame.GetCharacter(FName);
end;

procedure TPlayer.Update;
begin

end;

{ TGameStage }

constructor TGameStage.Create(Game: TGame);
begin
  inherited Create;
  FGame := Game;
end;

function TGameStage.Update: TGameStage;
begin
  Result := Self;
end;

end.
