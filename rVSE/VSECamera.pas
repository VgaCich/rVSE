unit VSECamera;

interface

uses
  OpenGL, avlVectors;

type
  TCamera = class
  public
    Eye, Center: TVector3D;
    {Zoom, Height,}
    XAngle, YAngle: Single;
    procedure CalcVertex;
    procedure SetPos;
  end;

const
  DegToRad=Pi/180;

var
  Camera: TCamera;

implementation

const
  Rad=Pi/180;

procedure TCamera.CalcVertex;
var
  Dist: Single;
begin
  Dist:=cos(YAngle*DegToRad);
  Center.X:=Eye.X+sin(-xAngle*DegToRad)*Dist;
  Center.Y:=Eye.Y+sin(YAngle*DegToRad);
  Center.Z:=Eye.Z+cos(-XAngle*DegToRad)*Dist;
end;

procedure TCamera.SetPos;
begin
  gluLookAt(Eye.X, Eye.Y, Eye.Z, Center.X, Center.Y, Center.Z, 0, 1, 0);
end;

end.
