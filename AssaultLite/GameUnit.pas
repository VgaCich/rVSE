unit GameUnit;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, OpenGL, VSEPrimitiveModel, VSETerrain;

type
  TUnit=class(TObject)
  private
    FPos: TVector3D;
    FDir, FBaseRoll, FBasePitch: Single;
    FModel: TPriModel;
    FTerrain: TTerrain;
    FUpdateCount: Cardinal;
    procedure SetPos(Pos: TVector3D);
    procedure SetDir(Dir: Single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Update;
    property Pos: TVector3D read FPos write SetPos;
    property Dir: Single read FDir write SetDir;
  end;

implementation

uses
  VSECore, StateGame;

constructor TUnit.Create;
begin
  inherited Create;
  FModel:=TPriModel.Create('Tank.vpm');
  FTerrain:=TStateGame(Core.GetState(Core.FindState('Game'))).Terrain;
  Pos:=VectorSetValue(68);
end;

destructor TUnit.Destroy;
begin
  FAN(FModel);
  inherited Destroy;
end;

procedure TUnit.Draw;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  with FPos do glTranslate(X, Y, Z);
  glRotate(FDir, 0, 1, 0);
  glRotate(FBaseRoll, -1, 0, 0);
  glRotate(FBasePitch, 0, 0, 1);
  FModel.Draw;
  glPopMatrix;
end;

procedure TUnit.Update;
const
  Speed=0.1;
begin
  FPos.X:=FPos.X+Speed*cos(DegToRad(Dir));
  FPos.Z:=FPos.Z-Speed*sin(DegToRad(Dir));
  Pos:=FPos;
  Dir:=Dir+0.15;
  Inc(FUpdateCount);
  if FUpdateCount>10 then
  begin
    FUpdateCount:=0;
    Dir:=Dir+(2*Random-1);
  end;
end;

procedure TUnit.SetPos(Pos: TVector3D);
const
  dV1: TVector3D = (X: 3; Y: 0; Z: 2.25);
  dV2: TVector3D = (X: 3; Y: 0; Z: -2.25);
var
  V1, V2: TVector3D;
  H1, H2, H3, H4, H14, H23, H: Single;
begin
  FPos:=Pos;
  with Pos do
  begin
    V1:=dV1;
    VectorRotateY(-FDir, V1);
    V2:=dV2;
    VectorRotateY(-FDir, V2);
    H1:=FTerrain.Altitude(X-V1.X, Z-V1.Z);
    H2:=FTerrain.Altitude(X+V2.X, Z+V2.Z);
    H3:=FTerrain.Altitude(X-V2.X, Z-V2.Z);
    H4:=FTerrain.Altitude(X+V1.X, Z+V1.Z);
  end;
  H14:=(H1+H4)/2;
  H23:=(H2+H3)/2;
  H:=(H14+H23)/2;
  FPos.Y:=3+H;
  H14:=H-H14;
  H23:=H-H23;
  FBaseRoll:=RadToDeg(arctan((H3+H23-H1+H14)/4.5));
  FBasePitch:=RadToDeg(arctan((H2+H23-H1+H14)/6));
end;

procedure TUnit.SetDir(Dir: Single);
begin
  if (Dir>360) or (Dir<0)
    then FDir:=Dir-360*Floor(Dir/360)
    else FDir:=Dir;
end;

end.