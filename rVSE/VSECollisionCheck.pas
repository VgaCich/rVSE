unit VSECollisionCheck;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors;

type
  TCollisionInfo=record
    Collided: Boolean;
  end;
  TCollisionObject=class
  protected
    function DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; virtual; abstract; //Implements collision checking, return true if check with passed type of object implemented, false otherwise
  public
    Transform: TMatrix4D;
    function CheckCollision(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; //Check for collision with Obj, check result returned in CollInfo, returns false if collision with passed type of object not implemented 
  end;
  TCollisionRay=class(TCollisionObject)
  private
    FDir: TVector3D;
    procedure SetDir(const Value: TVector3D);
  protected
    function DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; override;
  public
    Start: TVector3D;
    property Dir: TVector3D read FDir write SetDir;
  end;
  TCollisionSphere=class(TCollisionObject)
  protected
    function DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; override;
  public
    Center: TVector3D;
    Radius: Single;
  end;
  TCollisionAABB=class(TCollisionObject)
  public
    MinCorner, MaxCorner: TVector3D;
  end;
  TCollisionBox=class(TCollisionObject)
  public

  end;
  TCollisionTerrain=class(TCollisionObject)
  public

  end;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;

implementation

{TCollisionObject}

function TCollisionObject.CheckCollision(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean;
begin
  ZeroMemory(@CollInfo, SizeOf(CollInfo));
  Result:=DoCollisionCheck(Obj, CollInfo);
  if not Result then Result:=Obj.DoCollisionCheck(Self, CollInfo);
end;

{TCollisionRay}

function TCollisionRay.DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean;

  procedure CollideSphere(Obj: TCollisionSphere);
  var
    C, D: TVector3D;
    Dist: Single;
  begin
    C:=VectorSub(VectorMultiply(Obj.Center, Transform), VectorMultiply(Start, Transform));
    {TODO: transformation of Dir vector}
    D:=Dir;//D:=VectorMultiply(Dir, ExtractRotate(Transform));
    if VectorDotProduct(C, D)>=0
      then Dist:=VectorSize(VectorCrossProduct(D, C))
      else Dist:=VectorSize(C);
    CollInfo.Collided:=Dist<Obj.Radius;
  end;

begin
  Result:=true;
  if Obj is TCollisionSphere then CollideSphere(Obj as TCollisionSphere)
    else Result:=false;
end;

procedure TCollisionRay.SetDir(const Value: TVector3D);
begin
  FDir:=Value;
  VectorNormalize(FDir);
end;

{TCollisionSphere}

function TCollisionSphere.DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean;

  procedure CollideSphere(Obj: TCollisionSphere);
  var
    C1, C2: TVector3D;
  begin
    C1:=VectorMultiply(Center, Transform);
    C2:=VectorMultiply(Obj.Center, Obj.Transform);
    CollInfo.Collided:=VectorSize(VectorSub(C1, C2))<Radius+Obj.Radius;
  end;

begin
  Result:=true;
  if Obj is TCollisionSphere then CollideSphere(Obj as TCollisionSphere)
    else Result:=false;
end;

{Misc}

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result:=(Point.X>=Rect.Left) and (Point.X<=Rect.Right) and
          (Point.Y>=Rect.Top) and (Point.Y<=Rect.Bottom);
end;

end.
