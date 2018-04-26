unit VSECamera;

interface

uses
  OpenGL, avlVectors, avlMath;

type
  TCamera = class
  private
    FEye, FCenter: TVector3D;
    FAngle: TVector2D;
    procedure UpdateCenter;
    procedure SetAngle(const Value: TVector2D);
    procedure SetEye(const Value: TVector3D);
    procedure SetHeight(const Value: Single);
  public
    procedure Apply;
    procedure Move(Offset: TVector2D; LocalSpace: Boolean = true); overload;
    procedure Move(Offset, BoundsMin, BoundsMax: TVector2D; LocalSpace: Boolean = true); overload;
    property Angle: TVector2D read FAngle write SetAngle;
    property Center: TVector3D read FCenter;
    property Eye: TVector3D read FEye write SetEye;
    property Height: Single read FEye.Y write SetHeight;
  end;

const
  DegToRad = Pi / 180;

{var
  Camera: TCamera;}

implementation

procedure TCamera.Apply;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glMatrixMode(GL_PROJECTION);
  gluLookAt(FEye.X, FEye.Y, FEye.Z, FCenter.X, FCenter.Y, FCenter.Z, 0, 1, 0);
  glPopAttrib;
end;

procedure TCamera.Move(Offset: TVector2D; LocalSpace: Boolean);
var
  C, S, T: Single;
begin
  if LocalSpace then
  begin
    C := Cos(FAngle.X * DegToRad);
    S := Sin(FAngle.X * DegToRad);
    with Offset do
    begin
      T := C * Y + S * X;
      X := C * X - S * Y;
      Y := T;
    end;
  end;
  FEye.X := FEye.X + Offset.X;
  FEye.Z := FEye.Z + Offset.Y;
  UpdateCenter;
end;

procedure TCamera.Move(Offset, BoundsMin, BoundsMax: TVector2D; LocalSpace: Boolean);
begin
  Move(Offset, LocalSpace);
  FEye.X := Max(BoundsMin.X, Min(FEye.X, BoundsMax.X));
  FEye.Z := Max(BoundsMin.Y, Min(FEye.Z, BoundsMax.Y));
  UpdateCenter;
end;

procedure TCamera.UpdateCenter;
var
  Dist: Single;
begin
  Dist := cos(FAngle.Y * DegToRad);
  FCenter.X := FEye.X + sin(-FAngle.X * DegToRad) * Dist;
  FCenter.Y := FEye.Y + sin(FAngle.Y * DegToRad);
  FCenter.Z := FEye.Z + cos(-FAngle.X * DegToRad) * Dist;
end;

procedure TCamera.SetAngle(const Value: TVector2D);
begin
  FAngle := Value;
  UpdateCenter;
end;

procedure TCamera.SetEye(const Value: TVector3D);
begin
  FEye := Value;
  UpdateCenter;
end;

procedure TCamera.SetHeight(const Value: Single);
begin
  FEye.Y := Value;
  UpdateCenter;
end;

end.

