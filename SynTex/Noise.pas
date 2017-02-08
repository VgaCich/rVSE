unit Noise;

interface

uses
  AvL, avlVectors, avlMath;

type
  TPerlinNoise=class
  private
    FNormIndex: array[0..255, 0..255] of Byte;
    FFreq: Single;
    function Normal(X, Y: Integer): TVector3D;
  public
    constructor Create(Freq: Single; WrapAt: Integer = 256);
    function Noise(X, Y: Integer): Single; {$IFDEF INLINE} inline; {$ENDIF}
  end;

implementation

const
  Step=2*pi/256;

var
  Normals: array[0..255] of TVector3D;

constructor TPerlinNoise.Create(Freq: Single; WrapAt: Integer = 256);

  function Wrap(Val, Max: Integer): Integer;
  begin
    Val:=Val mod Max;
    if Val<0
      then Result:=Max+Val
      else Result:=Val;
  end;

var
  i, j: Integer;
begin
  FFreq:=Freq;
  for i:=0 to 255 do
    for j:=0 to 255 do
      if (i<WrapAt) and (j<WrapAt)
        then FNormIndex[i, j]:=Random(256)
        else FNormIndex[i, j]:=FNormIndex[Wrap(i, WrapAt), Wrap(j, WrapAt)];
end;

function TPerlinNoise.Noise(X, Y: Integer): Single;
var
  Pos: TVector3D;
  X0, X1, Y0, Y1: Integer;
  N0, N1, N2, N3, D0, D1, D2, D3: TVector3D;
  H0, H1, H2, H3, SX, SY, AvgX0, AvgX1: Single;
begin
  Pos:=VectorSetValue(X*FFreq, Y*FFreq, 0);
  X0:=Floor(Pos.X);
  X1:=X0+1;
  Y0:=Floor(Pos.Y);
  Y1:=Y0+1;
  N0:=Normal(X0, Y0);
  N1:=Normal(X0, Y1);
  N2:=Normal(X1, Y0);
  N3:=Normal(X1, Y1);
  D0:=VectorSetValue(Pos.X-X0, Pos.Y-Y0, 0);
  D1:=VectorSetValue(Pos.X-X0, Pos.Y-Y1, 0);
  D2:=VectorSetValue(Pos.X-X1, Pos.Y-Y0, 0);
  D3:=VectorSetValue(Pos.X-X1, Pos.Y-Y1, 0);
  H0:=VectorDotProduct(D0, N0);
  H1:=VectorDotProduct(D1, N1);
  H2:=VectorDotProduct(D2, N2);
  H3:=VectorDotProduct(D3, N3);
  SX:=6*IntPower(D0.X, 5)-15*IntPower(D0.X, 4)+10*IntPower(D0.X, 3);
  SY:=6*IntPower(D0.Y, 5)-15*IntPower(D0.Y, 4)+10*IntPower(D0.Y, 3);
  AvgX0:=H0+(SX*(H2-H0));
  AvgX1:=H1+(SX*(H3-H1));
  Result:=AvgX0+(SY*(AvgX1-AvgX0));
end;

function TPerlinNoise.Normal(X, Y: Integer): TVector3D;
begin
  Result:=Normals[FNormIndex[X mod 256, Y mod 256]];
end;

var
  i: Integer;

initialization

  for i:=0 to 255 do
    Normals[i]:=VectorSetValue(cos(Step*i), sin(Step*i), 0);

end.
