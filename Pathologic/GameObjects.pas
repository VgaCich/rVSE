unit GameObjects;

interface

uses
  AvL, avlUtils, avlMath, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, VSEPrimitiveModel;

type
  TResourceType = (rtCoin, rtKey, rtSecret);
  TAnimationAction = (aaMove, aaMoveTo, aaRotate);
  TAnimationStep = record
    Action: TAnimationAction;
    Param: TVector4D;
    Time: Cardinal;
  end;
  TGameObject = class
  protected
    FModel: TPriModel;
    FPos: TVector3D;
    FHeight: Single;
    FVisible: Boolean;
    FFollow: TGameObject;
    FAnimationStatus: TAnimationStep;
    FAnimationQueue: array of TAnimationStep;
    procedure SetPos(const Value: TVector3D); virtual;
    procedure SetVisible(Value: Boolean); virtual;
  public
    constructor Create(const Model: string);
    destructor Destroy; override;
    procedure Draw; virtual;
    procedure Update; virtual;
    procedure AddAnimationStep(Action: TAnimationAction; const Param: TVector4D; Time: Cardinal);
    property Follow: TGameObject read FFollow write FFollow;
    property Height: Single read FHeight;
    property Pos: TVector3D read FPos write SetPos;
    property Visible: Boolean read FVisible write SetVisible;
  end;
  TCharProfile = record
    RuName: string;
    IsDoctor: Boolean;
    Master: string;
    Resource: TResourceType;
  end;
  TCharacter = class(TGameObject)
  private
  protected
    FName: string;
    FProfile: TCharProfile;
    FQuarantined: Boolean;
    procedure SetQuarantined(const Value: Boolean); virtual;
  public
    constructor Create(const Name: string; const Profile: TCharProfile);
    property Name: string read FName;
    property Quarantined: Boolean read FQuarantined write SetQuarantined;
    property Profile: TCharProfile read FProfile;
  end;

implementation

uses VSETexMan, VSEMemPak
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{ TGameObject }

constructor TGameObject.Create(const Model: string);
begin
  inherited Create;
  FModel := TPriModel.Create(Model);
  FPos := Vector3D(0);
end;

destructor TGameObject.Destroy;
begin
  FAN(FModel);
  inherited;
end;

procedure TGameObject.Draw;
begin
  if not Visible then Exit;
  glPushMatrix;
  if Assigned(FFollow) and not ((Length(FAnimationQueue) > 0) or (FAnimationStatus.Time > 0)) then
    FPos := VectorAdd(FFollow.Pos, Vector3D(0, (FFollow.Height + FHeight) / 2, 0));
  glTranslate(FPos.X, FPos.Y + FHeight / 2, FPos.Z);
  FModel.Draw;
  glPopMatrix;
end;

procedure TGameObject.Update;
begin
  if (FAnimationStatus.Time = 0) and (Length(FAnimationQueue) > 0) then
  begin
    FAnimationStatus := FAnimationQueue[0];
    with FAnimationStatus do
    begin
      Time := Time div Core.UpdateInterval;
      case Action of
        aaMove: VectorScale(Param, 1 / Time);
        aaMoveTo: begin
          with VectorSub(Vector3D(Param), FPos) do
            Param := Vector4D(X, Y, Z, 0);
          VectorScale(Param, 1 / Time);
        end;
        aaRotate: Param.W := Param.W / Time;
      end;
    end;
    Move(FAnimationQueue[1], FAnimationQueue[0], (Length(FAnimationQueue) - 1) * SizeOf(TAnimationStep));
    SetLength(FAnimationQueue, Length(FAnimationQueue) - 1);
  end;
  with FAnimationStatus do
    if Time > 0 then
    begin
      case Action of
        aaMove, aaMoveTo: FPos := VectorAdd(FPos, Vector3D(Param));
        aaRotate: FModel.Transform.Rotate(Param.W, Param.X, Param.Y, Param.Z);
      end;
      Dec(FAnimationStatus.Time);
    end;
end;

procedure TGameObject.AddAnimationStep(Action: TAnimationAction; const Param: TVector4D; Time: Cardinal);
begin
  SetLength(FAnimationQueue, Length(FAnimationQueue) + 1);
  FAnimationQueue[High(FAnimationQueue)].Action := Action;
  FAnimationQueue[High(FAnimationQueue)].Param := Param;
  FAnimationQueue[High(FAnimationQueue)].Time := Time;
end;

procedure TGameObject.SetPos(const Value: TVector3D);
begin
  FPos := Value;
end;

procedure TGameObject.SetVisible(Value: Boolean);
begin
  FVisible := Value;
end;

{ TCharacter }

const
  CharObj = $52414843;
  CharCardMtl = 3;
  CharHeight = 6.6;
  CharModel = 'Models\Character.vpm';
  CharTex = 'Chars\%s.jpg';
  DoctorScale = 1.2;

constructor TCharacter.Create(const Name: string; const Profile: TCharProfile);
begin
  inherited Create(CharModel);
  FName := Name;
  FProfile := Profile;
  FQuarantined := true;
  FModel.Materials[CharCardMtl].Texture := TexMan.GetTex(Format(CharTex, [Name]));
  FHeight := CharHeight;
  if Profile.IsDoctor then
  begin
    FModel.Objects[CharObj].Transform.Scale(DoctorScale, DoctorScale, DoctorScale);
    FHeight := FHeight * DoctorScale;
  end;
end;

procedure TCharacter.SetQuarantined(const Value: Boolean);
begin
  if Value <> FQuarantined then
  begin
    AddAnimationStep(aaMove, Vector4D(0, FHeight, 0, 0), 500);
    AddAnimationStep(aaRotate, Vector4D(1, 0, 0, Pi), 500);
    AddAnimationStep(aaMove, Vector4D(0, -FHeight, 0, 0), 500);
  end;
    //FModel.Objects[CharObj].Transform.Rotate(Pi, 1, 0, 0);
  FQuarantined := Value;
end;

end.
