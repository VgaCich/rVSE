unit GameStages;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, VSECore, Game;

type
  TStageStart = class(TGameStage)
  public
    constructor Create(Game: TGame);
    function Update: TGameStage; override; 
  end;

implementation

uses VSEMemPak, GameData, GameObjects
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TStageStart }

constructor TStageStart.Create(Game: TGame);
begin
  inherited;
  FGame.Scene.ShowTitleMessage('Старт игры!');
  FGame.ActivePlayer := FGame.Player[SBachelor];
end;

function TStageStart.Update: TGameStage;
var
  NewPos: TVector3D;
begin
  case Random(100) of
    0: with FGame.Character[Characters[Random(Length(Characters))].Name] do
      Quarantined := not Quarantined;
    1: with FGame.Character[Characters[Random(Length(Characters))].Name] do
      begin
        NewPos := Vector3D(-60.0 + 120.0 * Random, Height, -30.0 + 60.0 * Random);
        AddAnimationStep(aaMove, Vector4D(0, Height, 0, 0), 500);
        AddAnimationStep(aaMoveTo, Vector4D(NewPos.X, NewPos.Y, NewPos.Z, 0),
          Round(VectorSize(VectorSub(NewPos, Pos)) * 25));
        AddAnimationStep(aaMove, Vector4D(0, -Height, 0, 0), 500);
      end;
  end;
  if Random(1000) = 0 then
    FGame.Scene.ShowTitleMessage('~~ Random Hello! ~~');
  Result := inherited Update;
end;

end.
