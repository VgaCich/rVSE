unit GamePlayer;

interface

uses
  AvL, avlUtils, avlVectors, GameUnit;

type
  TPlayer=class(TObject)
  private
    FUnits: array of TUnit;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): TUnit;
  public
    constructor Create(UnitsCount: Integer);
    destructor Destroy; override;
    procedure Draw;
    procedure Update;
    property UnitsCount: Integer read GetUnitsCount;
    property Units[Index: Integer]: TUnit read GetUnit;
  end;

implementation

constructor TPlayer.Create(UnitsCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FUnits, UnitsCount);
  for i:=0 to High(FUnits) do
  begin
    FUnits[i]:=TUnit.Create;
    FUnits[i].Pos:=VectorSetValue(280, 0, 268+i*8);
  end;
end;

destructor TPlayer.Destroy;
var
  i: Integer;
begin
  for i:=0 to High(FUnits) do
    FUnits[i].Free;
  Finalize(FUnits);
  inherited Destroy;
end;

procedure TPlayer.Draw;
var
  i: Integer;
begin
  for i:=0 to High(FUnits) do
    FUnits[i].Draw;
end;

procedure TPlayer.Update;
var
  i: Integer;
begin
  for i:=0 to High(FUnits) do
    FUnits[i].Update;
end;

function TPlayer.GetUnitsCount: Integer;
begin
  Result:=Length(FUnits);
end;

function TPlayer.GetUnit(Index: Integer): TUnit;
begin
  if (Index>=Low(FUnits)) and (Index<=High(FUnits))
    then Result:=FUnits[Index]
    else Result:=nil;
end;

end.
