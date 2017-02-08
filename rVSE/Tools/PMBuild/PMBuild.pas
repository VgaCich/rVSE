unit PMBuild;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Vectors, Math, OpenGL;

{$I ..\..\VSEPrimitiveModel.inc}

type
  TVertex=packed record
    Vertex: TVector3D;
    Normal: TVector3D;
    TexCoord: TVector2D;
  end;
  TVertexArray=packed array of TVertex;
  TFace=packed record
    Vert1, Vert2, Vert3: Word;
  end;
  TIndexArray=packed array of TFace;
  TLineInfo=record
    Start, Stride: Integer;
  end;
  TVertInfo=record
    Count, LinesCount: Integer;
    Lines: array[0..3] of TLineInfo;
  end;
  TPMBOnLoadTex=function(const TexName: string): Cardinal of object;
  TPMBOnBindTex=procedure(ID: Cardinal) of object;
  TPMBModel=class;
  TPMBObject=class;
  TPMBMaterial=class
  private
    FModel: TPMBModel;
    FDiffuse, FAmbient, FSpecular, FEmission: TColor;
    FID, FShininess: Byte;
    FTexture: Cardinal;
    FTextureName: string;
    FUVCount: Integer;
    procedure SetTexture(const TexName: string);
  protected
    function  WriteChunk(Data: TStream): Boolean;
    procedure ReadChunk(Data: TStream; ChunkSize: Integer);
  public
    constructor Create(Model: TPMBModel);
    destructor Destroy; override;
    procedure DrawUV;
    procedure Apply;
    procedure ApplyUV;
    procedure EndUV;
    property Model: TPMBModel read FModel;
    property ID: Byte read FID;
    property Diffuse: TColor read FDiffuse write FDiffuse;
    property Ambient: TColor read FAmbient write FAmbient;
    property Specular: TColor read FSpecular write FSpecular;
    property Emission: TColor read FEmission write FEmission;
    property Shininess: Byte read FShininess write FShininess;
    property Texture: string read FTextureName write SetTexture;
  end;
  TPMBTransform=class
  private
    FPMTransform: TPMTransform;
    function  GetTranslateX: Single;
    procedure SetTranslateX(Value: Single);
    function  GetTranslateY: Single;
    procedure SetTranslateY(Value: Single);
    function  GetTranslateZ: Single;
    procedure SetTranslateZ(Value: Single);
    function  GetYaw: Single;
    procedure SetYaw(Value: Single);
    function  GetPitch: Single;
    procedure SetPitch(Value: Single);
    function  GetRoll: Single;
    procedure SetRoll(Value: Single);
    function  GetScaleX: Single;
    procedure SetScaleX(Value: Single);
    function  GetScaleY: Single;
    procedure SetScaleY(Value: Single);
    function  GetScaleZ: Single;
    procedure SetScaleZ(Value: Single);
  protected
    procedure Read(Data: TStream);
    procedure Write(Data: TStream);
  public
    constructor Create;
    procedure SetTranslate(Translate: TVector3D; Normalize: Boolean);
    procedure ScaleTranslate(Scale: Single);
    procedure SetScale(Scale: TVector3D; Normalize: Boolean);
    procedure ScaleScale(Scale: Single);
    procedure Apply; overload;
    procedure Apply(var VA: TVertexArray); overload;
    property TranslateX: Single read GetTranslateX write SetTranslateX;
    property TranslateY: Single read GetTranslateY write SetTranslateY;
    property TranslateZ: Single read GetTranslateZ write SetTranslateZ;
    property Yaw: Single read GetYaw write SetYaw;
    property Pitch: Single read GetPitch write SetPitch;
    property Roll: Single read GetRoll write SetRoll;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property ScaleZ: Single read GetScaleZ write SetScaleZ;
  end;
  TPMBMesh=class
  private
    FObj: TPMBObject;
    FTransform: TPMBTransform;
    FVisible, FDrawNormals, FHasNormals, FHasUV: Boolean;
    FHighlightVert: Integer;
    FDrawVerts: TVertexArray;
    FVerts: packed array of TPMVertex;
    FFaces: TIndexArray;
    FSelected: Boolean;
    procedure CreateDrawVerts;
    function  GetVertsCount: Integer;
    function  GetVertex(Index: Byte): TPMVertex;
    procedure SetSelected(Value: Boolean);
    procedure SetVertex(Index: Byte; Vertex: TPMVertex);
  protected
    function  WriteChunk(Data: TStream): Boolean;
    procedure ReadChunk(Data: TStream; ChunkID: Byte; ChunkSize: Integer);
  public
    constructor Create(Obj: TPMBObject);
    destructor Destroy; override;
    procedure Draw;
    procedure DrawUV;
    function  Import(MeshData: TStream): Boolean;
    procedure Relink(Obj: TPMBObject);
    property Obj: TPMBObject read FObj;
    property Transform: TPMBTransform read FTransform;
    property Visible: Boolean read FVisible write FVisible;
    property DrawNormals: Boolean read FDrawNormals write FDrawNormals;
    property HasNormals: Boolean read FHasNormals write FHasNormals;
    property HasUV: Boolean read FHasUV write FHasUV;
    property VertsCount: Integer read GetVertsCount;
    property Verts[Index: Byte]: TPMVertex read GetVertex write SetVertex;
    property HighlightVert: Integer read FHighlightVert write FHighlightVert;
    property Selected: Boolean read FSelected write SetSelected;
  end;
  TPMBPrimitive=class
  private
    FSelected: Boolean;
    procedure SetSelected(Value: Boolean);
  protected
    FObj: TPMBObject;
    FType, FFlags: Byte;
    FTransform: TPMBTransform;
    FVerts: TVertexArray;
    FFaces: TIndexArray;
    FVisible, FDrawNormals, FTexGenUV, FInvertNormals: Boolean;
    procedure Quad(At: Integer; V1, V2, V3, V4: Word);
    function  CreateCircle(const Center: TVector3D; Radius: Single; Sector: Byte; Count: Integer; Smooth, DoubleLine: Boolean): TVertInfo;
    procedure CreateTCLine(FromX, ToX, FromY, ToY: Single; const VertInfo: TVertInfo);
    procedure CreateTCCircle(CenterX, CenterY, Radius: Single; const VertInfo: TVertInfo);
    procedure CreateStrip(Line1Start, Line1Stride, Line2Start, Line2Stride, Count: Integer; Smooth: Boolean);
    function  CreateVerts: Boolean; virtual; abstract;
    function  WriteChunk(Data: TStream): Boolean;
    class function ReadChunk(Obj: TPMBObject; Data: TStream; ChunkSize: Integer): TPMBPrimitive;
    procedure UpdateFlags; virtual; abstract;
    function  DoWriteChunk(Data: TStream): Boolean; virtual; abstract;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); virtual; abstract;
  public
    constructor Create(Obj: TPMBObject);
    destructor Destroy; override;
    procedure Draw;
    procedure DrawUV;
    procedure Relink(Obj: TPMBObject);
    property Obj: TPMBObject read FObj;
    property PriType: Byte read FType;
    property Transform: TPMBTransform read FTransform;
    property Visible: Boolean read FVisible write FVisible;
    property DrawNormals: Boolean read FDrawNormals write FDrawNormals;
    property TexGenUV: Boolean read FTexGenUV write FTexGenUV;
    property InvertNormals: Boolean read FInvertNormals write FInvertNormals;
    property Selected: Boolean read FSelected write SetSelected;
  end;
  TPMBPrimitiveCube=class(TPMBPrimitive)
  private
    FTexUV: packed array[0..2] of TPMUVRect;
    FTexMergeSides: array[0..2] of Boolean;
    function  GetUV(Index: Byte): TPMUVRect;
    procedure SetUV(Index: Byte; Value: TPMUVRect);
    function  GetSplitSides(Index: Byte): Boolean;
    procedure SetSplitSides(Index: Byte; Value: Boolean);
  protected
    function  CreateVerts: Boolean; override;
    procedure UpdateFlags; override;
    function  DoWriteChunk(Data: TStream): Boolean; override;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); override;
  public
    constructor Create(Obj: TPMBObject);
    property TexUV[Index: Byte]: TPMUVRect read GetUV write SetUV;
    property TexSplitSides[Index: Byte]: Boolean read GetSplitSides write SetSplitSides;
  end;
  TPMBPrimitiveSphere = class(TPMBPrimitive)
  private
    FSmooth: Boolean;
    FSlices, FStacks, FSlicesSector, FStacksSector: Byte;
    FTexUV: TPMUVRect;
  protected
    function  CreateVerts: Boolean; override;
    procedure UpdateFlags; override;
    function  DoWriteChunk(Data: TStream): Boolean; override;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); override;
  public
    constructor Create(Obj: TPMBObject);
    property Smooth: Boolean read FSmooth write FSmooth;
    property Slices: Byte read FSlices write FSlices;
    property Stacks: Byte read FStacks write FStacks;
    property SlicesSector: Byte read FSlicesSector write FSlicesSector;
    property StacksSector: Byte read FStacksSector write FStacksSector;
    property TexUV: TPMUVRect read FTexUV write FTexUV;
  end;
  TPMBPrimitiveCone=class(TPMBPrimitive)
  private
    FSmooth: Boolean;
    FRadiusT, FRadiusB, FSlices, FSlicesSector: Byte;
    FUVSide: TPMUVRect;
    FUVBaseT, FUVBaseB: TPMUVCircle;
  protected
    function  CreateVerts: Boolean; override;
    procedure UpdateFlags; override;
    function  DoWriteChunk(Data: TStream): Boolean; override;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); override;
  public
    constructor Create(Obj: TPMBObject);
    property Smooth: Boolean read FSmooth write FSmooth;
    property RadiusT: Byte read FRadiusT write FRadiusT;
    property RadiusB: Byte read FRadiusB write FRadiusB;
    property Slices: Byte read FSlices write FSlices;
    property SlicesSector: Byte read FSlicesSector write FSlicesSector;
    property UVSide: TPMUVRect read FUVSide write FUVSide;
    property UVBaseT: TPMUVCircle read FUVBaseT write FUVBaseT;
    property UVBaseB: TPMUVCircle read FUVBaseB write FUVBaseB;
  end;
  TPMBPrimitiveTorus=class(TPMBPrimitive)
  private

  protected
    function  CreateVerts: Boolean; override;
    procedure UpdateFlags; override;
    function  DoWriteChunk(Data: TStream): Boolean; override;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); override;
  public
    constructor Create(Obj: TPMBObject);
    destructor Destroy; override;

  end;
  TPMBPrimitiveTube=class(TPMBPrimitive)
  private

  protected
    function  CreateVerts: Boolean; override;
    procedure UpdateFlags; override;
    function  DoWriteChunk(Data: TStream): Boolean; override;
    procedure DoReadChunk(Data: TStream; ChunkSize: Integer); override;
  public
    constructor Create(Obj: TPMBObject);
    destructor Destroy; override;

  end;
  TPMBObject=class
  private
    FModel: TPMBModel;
    FParent: TPMBObject;
    FID: Cardinal;
    FVisible: Boolean;
    FObjects, FPrimitives: TList;
    FMaterial: TPMBMaterial;
    FTransform: TPMBTransform;
    FMesh: TPMBMesh;
    FSelected: Boolean;
    function  GetID: string;
    procedure SetID(const ID: string);
    procedure SetIID(IID: Cardinal);
    function  GetObjectsCount: Integer;
    function  GetObject(Index: Integer): TPMBObject;
    procedure SetMesh(Mesh: TPMBMesh);
    function  GetPrimitivesCount: Integer;
    function  GetPrimitive(Index: Integer): TPMBPrimitive;
    procedure SetSelected(Value: Boolean);
  protected
    function  AddObject(Obj: TPMBObject): Integer;
    procedure DeleteObject(Obj: TPMBObject);
    function  AddPrimitive(Primitive: TPMBPrimitive): Integer;
    procedure DeletePrimitive(Primitive: TPMBPrimitive);
    function  WriteChunk(Data: TStream): Boolean;
    procedure ReadChunk(Data: TStream; ChunkSize: Integer); overload; //read object chunk
    procedure ReadChunk(Data: TStream); overload; //read subchunks
  public
    constructor Create(Model: TPMBModel; Parent: TPMBObject);
    destructor Destroy; override;
    procedure DeselectAll;
    procedure Draw;
    procedure DrawUV;
    procedure Relink(Obj: TPMBObject); overload;
    procedure Relink(Model: TPMBModel); overload;
    procedure SetVisibility(Visibility: Boolean);
    property Model: TPMBModel read FModel;
    property Parent: TPMBObject read FParent;
    property Visible: Boolean read FVisible write FVisible;
    property ID: string read GetID write SetID;
    property IID: Cardinal read FID write SetIID;
    property ObjectsCount: Integer read GetObjectsCount;
    property Objects[Index: Integer]: TPMBObject read GetObject;
    property Material: TPMBMaterial read FMaterial write FMaterial;
    property Transform: TPMBTransform read FTransform;
    property Mesh: TPMBMesh read FMesh write SetMesh;
    property PrimitivesCount: Integer read GetPrimitivesCount;
    property Primitives[Index: Integer]: TPMBPrimitive read GetPrimitive;
    property Selected: Boolean read FSelected write SetSelected;
  end;
  TPMBModel=class
  private
    FOnLoadTex: TPMBOnLoadTex;
    FOnBindTex: TPMBOnBindTex;
    FObjects, FMaterials: TList;
    function  GetObjectsCount: Integer;
    function  GetObject(Index: Integer): TPMBObject;
    function  GetMaterialsCount: Integer;
    function  GetMaterial(Index: Integer): TPMBMaterial;
  protected
    function  AddObject(Obj: TPMBObject): Integer;
    procedure DeleteObject(Obj: TPMBObject);
    function  ObjIDExists(ID: Cardinal): Boolean;
    function  AddMaterial(Mat: TPMBMaterial): Integer;
    procedure DeleteMaterial(Mat: TPMBMaterial);
    function  GetMaterialID: Byte;
    function  LoadTexture(const TexName: string): Cardinal;
    procedure BindTexture(ID: Cardinal);
    procedure ReadChunk(Data: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeselectAll;
    procedure Draw;
    procedure SetVisibility(Visibility: Boolean);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function  FindMaterial(ID: Byte): TPMBMaterial;
    property ObjectsCount: Integer read GetObjectsCount;
    property Objects[Index: Integer]: TPMBObject read GetObject;
    property MaterialsCount: Integer read GetMaterialsCount;
    property Materials[Index: Integer]: TPMBMaterial read GetMaterial;
    property OnLoadTex: TPMBOnLoadTex read FOnLoadTex write FOnLoadTex;
    property OnBindTex: TPMBOnBindTex read FOnBindTex write FOnBindTex;
  end;

function PriTypeToString(PriType: Byte): string;
procedure ComputeNormalsTriangles(var VA: array of TVertex; const IA: array of TFace);

implementation

const
  SCannotRelinkObjectToAnotherModel = 'Cannot relink object to another model';
  SCannotSaveModelResultingFileSize = 'Cannot save model: resulting file size is greater than 65535 bytes';
  SCannotLoadModelInvalidFirstChunk = 'Cannot load model: invalid first chunk';
  STooLongTextureName = 'Too long texture name';
  SCannotFindValidMaterialID = 'Cannot find valid Material ID';
  SCannotLoadModelInvalidChunkAt = 'Cannot load model: invalid chunk %d at %d';
  SCannotLoadModelChunkSizeMismatch = 'Cannot load model: chunk size mismatch';
  NormalizeTo=1;

type
  PIDC=^TIDC;
  TIDC=packed array[1..4] of Char;

function ColorTo4f(Color: TColor): TVector4f;
var
  Clr: packed array[0..3] of Byte absolute Color;
begin
  with Result do
  begin
    Red:=Clr[0]/255;
    Green:=Clr[1]/255;
    Blue:=Clr[2]/255;
    Alpha:=Clr[3]/255;
  end;
end;

function SelI(Expr: Boolean; ValTrue, ValFalse: Integer): Integer;
begin
  if Expr then Result:=ValTrue else Result:=ValFalse;
end;

procedure ComputeNormalsTriangles(var VA: array of TVertex; const IA: array of TFace);
var
  i: Integer;
  Normal: TVector3D;
begin
  for i:=0 to High(IA) do
  with IA[i] do
    begin
      if VectorIsEqual(VA[Vert1].Vertex, VA[Vert2].Vertex) or
         VectorIsEqual(VA[Vert1].Vertex, VA[Vert3].Vertex) or
         VectorIsEqual(VA[Vert2].Vertex, VA[Vert3].Vertex) then Continue;
      Assert((Vert1<Length(VA)) and (Vert2<Length(VA)) and (Vert3<Length(VA)), Format('ComputeNormals: too large index %d: %d, %d, %d', [i, Vert1, Vert2, Vert3]));
      Normal:=TriangleNormal(VA[Vert1].Vertex, VA[Vert2].Vertex, VA[Vert3].Vertex);
      VA[Vert1].Normal:=VectorAdd(VA[Vert1].Normal, VectorMultiply(Normal,
        TriangleAngle(VA[Vert1].Vertex, VA[Vert2].Vertex, VA[Vert3].Vertex)));
      VA[Vert2].Normal:=VectorAdd(VA[Vert2].Normal, VectorMultiply(Normal,
        TriangleAngle(VA[Vert2].Vertex, VA[Vert3].Vertex, VA[Vert1].Vertex)));
      VA[Vert3].Normal:=VectorAdd(VA[Vert3].Normal, VectorMultiply(Normal,
        TriangleAngle(VA[Vert3].Vertex, VA[Vert1].Vertex, VA[Vert2].Vertex)));
    end;
  for i:=0 to High(VA) do VectorNormalize(VA[i].Normal);
end;

{TPMBMaterial}

constructor TPMBMaterial.Create(Model: TPMBModel);
begin
  inherited Create;
  FModel:=Model;
  FID:=FModel.GetMaterialID;
  if FID=0 then raise Exception.Create(SCannotFindValidMaterialID);
  FModel.AddMaterial(Self);
  FDiffuse:=TColor($FFFFFFFF);
  FSpecular:=TColor($FFFFFFFF);
  FAmbient:=TColor($FF404040);
  FEmission:=TColor($FF000000);
  FShininess:=64;
end;

destructor TPMBMaterial.Destroy;
begin
  FModel.DeleteMaterial(Self);
  inherited Destroy;
end;

procedure TPMBMaterial.DrawUV;

  procedure DrawObj(Obj: TPMBObject);
  var
    i: Integer;
  begin
    if Obj.Material=Self then Obj.DrawUV;
    for i:=0 to Obj.ObjectsCount-1 do
      DrawObj(Obj.Objects[i]);
  end;

var
  i: Integer;
begin
  ApplyUV;
  for i:=0 to FModel.ObjectsCount-1 do
    DrawObj(FModel.Objects[i]);
  EndUV;
end;

procedure TPMBMaterial.Apply;
var
  Color: TVector4f;
begin
  FModel.BindTexture(FTexture);
  Color:=ColorTo4f(FDiffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @Color);
  Color:=ColorTo4f(FSpecular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Color);
  Color:=ColorTo4f(FAmbient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @Color);
  Color:=ColorTo4f(FEmission);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @Color);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, FShininess/2);
end;

procedure TPMBMaterial.ApplyUV;
const
  ClrWhite: TVector4f = (Red: 1; Green: 1; Blue: 1; Alpha: 1);
  ClrBlack: TVector4f = (Red: 0; Green: 0; Blue: 0; Alpha: 1);
begin
  Inc(FUVCount);
  if FUVCount<>1 then Exit;
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @ClrWhite);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @ClrBlack);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @ClrWhite);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @ClrBlack);
  FModel.BindTexture(FTexture);
  glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex(0, 0);
    glTexCoord(1, 0);
    glVertex(1, 0);
    glTexCoord(1, 1);
    glVertex(1, 1);
    glTexCoord(0, 1);
    glVertex(0, 1);
  glEnd;
  FModel.BindTexture(0);
end;

procedure TPMBMaterial.EndUV;
begin
  if FUVCount>0 then Dec(FUVCount);
end;

procedure TPMBMaterial.SetTexture(const TexName: string);
begin
  if Length(TexName)>255 then raise Exception.Create(STooLongTextureName);
  FTextureName:=TexName;
  FTexture:=FModel.LoadTexture(TexName);
end;

function TPMBMaterial.WriteChunk(Data: TStream): Boolean;
var
  ChunkType, TexNameLen: Byte;
  ChunkSize: Word;
  ChunkStart: Integer;
begin
  Result:=false;
  ChunkStart:=Data.Position;
  ChunkType:=ChunkMaterial;
  Data.Write(ChunkType, SizeOf(ChunkType));
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Write(FID, SizeOf(FID));
  TexNameLen:=Length(FTextureName);
  Data.Write(TexNameLen, SizeOf(TexNameLen));
  if TexNameLen>0 then Data.Write(FTextureName[1], TexNameLen);
  Data.Write(FDiffuse, SizeOf(FDiffuse));
  Data.Write(FSpecular, SizeOf(FSpecular));
  Data.Write(FAmbient, SizeOf(FAmbient));
  Data.Write(FEmission, SizeOf(FEmission));
  Data.Write(FShininess, SizeOf(FShininess));
  ChunkSize:=Data.Position-ChunkStart;
  Data.Seek(ChunkStart+SizeOf(ChunkType), soFromBeginning);
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Seek(0, soFromEnd);
  Result:=true;
end;

procedure TPMBMaterial.ReadChunk(Data: TStream; ChunkSize: Integer);
var
  TexNameLen: Byte;
begin
  ChunkSize:=Data.Position+ChunkSize; //ChunkEnd
  Data.Read(FID, SizeOf(FID));
  Data.Read(TexNameLen, SizeOf(TexNameLen));
  if TexNameLen<>0 then
  begin
    SetLength(FTextureName, TexNameLen);
    Data.Read(FTextureName[1], TexNameLen);
    FTexture:=FModel.LoadTexture(FTextureName);
  end;
  Data.Read(FDiffuse, SizeOf(FDiffuse));
  Data.Read(FSpecular, SizeOf(FSpecular));
  Data.Read(FAmbient, SizeOf(FAmbient));
  Data.Read(FEmission, SizeOf(FEmission));
  Data.Read(FShininess, SizeOf(FShininess));
  if Data.Position<>ChunkSize then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;
      
{TPMBTransform}

constructor TPMBTransform.Create;
begin
  FPMTransform.ScaleX:=512;
  FPMTransform.ScaleY:=512;
  FPMTransform.ScaleZ:=512;
end;

procedure TPMBTransform.SetTranslate(Translate: TVector3D; Normalize: Boolean);
var
  NormScale: Single;
begin
  if Normalize then
  begin
    NormScale:=NormalizeTo/Max(Abs(Translate.X), Max(Abs(Translate.Y), Abs(Translate.Z)));
    Translate.X:=Translate.X*NormScale;
    Translate.Y:=Translate.Y*NormScale;
    Translate.Z:=Translate.Z*NormScale;
  end;
  with FPMTransform do
  begin
    TranslateX:=Round(Translate.X*512);
    TranslateY:=Round(Translate.Y*512);
    TranslateZ:=Round(Translate.Z*512);
  end;
end;

procedure TPMBTransform.ScaleTranslate(Scale: Single);
begin
  with FPMTransform do
  begin
    TranslateX:=Round(Scale*TranslateX);
    TranslateY:=Round(Scale*TranslateY);
    TranslateZ:=Round(Scale*TranslateZ);
  end;
end;

procedure TPMBTransform.SetScale(Scale: TVector3D; Normalize: Boolean);
var
  NormScale: Single;
begin
  if Normalize then
  begin
    NormScale:=NormalizeTo/Max(Abs(Scale.X), Max(Abs(Scale.Y), Abs(Scale.Z)));
    Scale.X:=Scale.X*NormScale;
    Scale.Y:=Scale.Y*NormScale;
    Scale.Z:=Scale.Z*NormScale;
  end;
  with FPMTransform do
  begin
    ScaleX:=Round(Scale.X*512);
    ScaleY:=Round(Scale.Y*512);
    ScaleZ:=Round(Scale.Z*512);
  end;
end;

procedure TPMBTransform.ScaleScale(Scale: Single);
begin
  with FPMTransform do
  begin
    ScaleX:=Round(Scale*ScaleX);
    ScaleY:=Round(Scale*ScaleY);
    ScaleZ:=Round(Scale*ScaleZ);
  end;
end;

procedure TPMBTransform.Apply;
begin
  glScale(ScaleX, ScaleY, ScaleZ);
  glRotate(RadToDeg(Yaw), 0, 1, 0);
  glRotate(RadToDeg(Pitch), 1, 0, 0);
  glRotate(RadToDeg(Roll), 0, 0, -1);
  glTranslate(TranslateX, TranslateY, TranslateZ);
end;

procedure TPMBTransform.Apply(var VA: TVertexArray);
var
  i: Integer;
  Scale, NScale, Translate: TVector3D;
  Yaw, Pitch, Roll: Single;
begin
  with Scale, FPMTransform do
  begin
    X:=ScaleX/512;
    Y:=ScaleY/512;
    Z:=ScaleZ/512;
  end;
  with NScale, FPMTransform do
  begin
    X:=512/ScaleX;
    Y:=512/ScaleY;
    Z:=512/ScaleZ;
  end;
  with Translate, FPMTransform do
  begin
    X:=TranslateX/512;
    Y:=TranslateY/512;
    Z:=TranslateZ/512;
  end;
  Yaw:=RadToDeg(FPMTransform.Yaw*BDegToRad);
  Pitch:=RadToDeg(FPMTransform.Pitch*BDegToRad);
  Roll:=RadToDeg(FPMTransform.Roll*BDegToRad);
  for i:=0 to High(VA) do
    with VA[i] do
    begin
      Vertex:=VectorMultiply(Vertex, Scale);
      Normal:=VectorMultiply(Normal, NScale);
      VectorRotateY(-Yaw, Vertex);
      VectorRotateY(-Yaw, Normal);
      VectorRotateX(Pitch, Vertex);
      VectorRotateX(Pitch, Normal);
      VectorRotateZ(-Roll, Vertex);
      VectorRotateZ(-Roll, Normal);
      Vertex:=VectorAdd(Vertex, Translate);
      VectorNormalize(Normal);
    end;
end;

function TPMBTransform.GetTranslateX: Single;
begin
  Result:=FPMTransform.TranslateX/512;
end;

procedure TPMBTransform.SetTranslateX(Value: Single);
begin
  FPMTransform.TranslateX:=Round(Value*512);
end;

function TPMBTransform.GetTranslateY: Single;
begin
  Result:=FPMTransform.TranslateY/512;
end;

procedure TPMBTransform.SetTranslateY(Value: Single);
begin
  FPMTransform.TranslateY:=Round(Value*512);
end;

function TPMBTransform.GetTranslateZ: Single;
begin
  Result:=FPMTransform.TranslateZ/512;
end;

procedure TPMBTransform.SetTranslateZ(Value: Single);
begin
  FPMTransform.TranslateZ:=Round(Value*512);
end;

function TPMBTransform.GetYaw: Single;
begin
  Result:=BDegToRad*FPMTransform.Yaw;
end;

procedure TPMBTransform.SetYaw(Value: Single);
begin
  FPMTransform.Yaw:=Round(Value*RadToBDeg);
end;

function TPMBTransform.GetPitch: Single;
begin
  Result:=BDegToRad*FPMTransform.Pitch;
end;

procedure TPMBTransform.SetPitch(Value: Single);
begin
  FPMTransform.Pitch:=Round(Value*RadToBDeg);
end;

function TPMBTransform.GetRoll: Single;
begin
  Result:=BDegToRad*FPMTransform.Roll;
end;

procedure TPMBTransform.SetRoll(Value: Single);
begin
  FPMTransform.Roll:=Round(Value*RadToBDeg);
end;

function TPMBTransform.GetScaleX: Single;
begin
  Result:=FPMTransform.ScaleX/512;
end;

procedure TPMBTransform.SetScaleX(Value: Single);
begin
  FPMTransform.ScaleX:=Round(Value*512);
end;

function TPMBTransform.GetScaleY: Single;
begin
  Result:=FPMTransform.ScaleY/512;
end;

procedure TPMBTransform.SetScaleY(Value: Single);
begin
  FPMTransform.ScaleY:=Round(Value*512);
end;

function TPMBTransform.GetScaleZ: Single;
begin
  Result:=FPMTransform.ScaleZ/512;
end;

procedure TPMBTransform.SetScaleZ(Value: Single);
begin
  FPMTransform.ScaleZ:=Round(Value*512);
end;

procedure TPMBTransform.Read(Data: TStream);
begin
  Data.Read(FPMTransform, SizeOf(FPMTransform));
end;

procedure TPMBTransform.Write(Data: TStream);
begin
  Data.Write(FPMTransform, SizeOf(FPMTransform));
end;

{TPMBMesh}

constructor TPMBMesh.Create(Obj: TPMBObject);
begin
  inherited Create;
  FObj:=Obj;
  FObj.Mesh:=Self;
  FTransform:=TPMBTransform.Create;
  FVisible:=true;
  FHighlightVert:=-1;
  FHasNormals:=true;
  FHasUV:=true;
end;

destructor TPMBMesh.Destroy;
begin
  FTransform.Free;
  FObj.Mesh:=nil;
  Finalize(FFaces);
  Finalize(FVerts);
  Finalize(FDrawVerts);
  inherited Destroy;
end;

procedure TPMBMesh.Draw;
var
  i: Integer;
  NVert: TVector3D;
begin
  if not FVisible then Exit;
  CreateDrawVerts;
  if FSelected then
  begin
    glPushAttrib(GL_LIGHTING_BIT or GL_CURRENT_BIT);
    glColor(0.5, 0.5, 1.0);
    glEnable(GL_COLOR_MATERIAL);
  end;
  glBegin(GL_TRIANGLES);
    for i:=0 to High(FFaces) do
    begin
      glNormal3fv(@FDrawVerts[FFaces[i].Vert1].Normal);
      glTexCoord2fv(@FDrawVerts[FFaces[i].Vert1].TexCoord);
      glVertex3fv(@FDrawVerts[FFaces[i].Vert1].Vertex);
      glNormal3fv(@FDrawVerts[FFaces[i].Vert2].Normal);
      glTexCoord2fv(@FDrawVerts[FFaces[i].Vert2].TexCoord);
      glVertex3fv(@FDrawVerts[FFaces[i].Vert2].Vertex);
      glNormal3fv(@FDrawVerts[FFaces[i].Vert3].Normal);
      glTexCoord2fv(@FDrawVerts[FFaces[i].Vert3].TexCoord);
      glVertex3fv(@FDrawVerts[FFaces[i].Vert3].Vertex);
    end;
  glEnd;
  if FSelected then glPopAttrib;
  glPushAttrib(GL_ENABLE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT);
  glEnable(GL_COLOR_MATERIAL);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor3f(1, 1, 1);
  if FDrawNormals then
  begin
    glBegin(GL_LINES);
      for i:=0 to High(FFaces) do
      begin
        NVert:=VectorAdd(FDrawVerts[FFaces[i].Vert1].Vertex, FDrawVerts[FFaces[i].Vert1].Normal);
        glVertex3fv(@FDrawVerts[FFaces[i].Vert1].Vertex);
        glVertex3fv(@NVert);
        NVert:=VectorAdd(FDrawVerts[FFaces[i].Vert2].Vertex, FDrawVerts[FFaces[i].Vert2].Normal);
        glVertex3fv(@FDrawVerts[FFaces[i].Vert2].Vertex);
        glVertex3fv(@NVert);
        NVert:=VectorAdd(FDrawVerts[FFaces[i].Vert3].Vertex, FDrawVerts[FFaces[i].Vert3].Normal);
        glVertex3fv(@FDrawVerts[FFaces[i].Vert3].Vertex);
        glVertex3fv(@NVert);
      end;
    glEnd;
  end;
  if (FHighlightVert>=0) and (FHighlightVert<VertsCount) then
  begin
    glBegin(GL_LINES);
      with FDrawVerts[FHighlightVert].Vertex do
      begin
        glVertex3f(X-0.1, Y, Z);
        glVertex3f(X+0.1, Y, Z);
        glVertex3f(X, Y-0.1, Z);
        glVertex3f(X, Y+0.1, Z);
        glVertex3f(X, Y, Z-0.1);
        glVertex3f(X, Y, Z+0.1);
      end;
    glEnd;
  end;
  glPopAttrib;
end;

procedure TPMBMesh.DrawUV;
var
  i: Integer;
begin
  if not FHasUV then Exit;
  CreateDrawVerts;
  if Assigned(FObj.Material) then FObj.Material.ApplyUV;
  try
    for i:=0 to High(FFaces) do
    begin
      glBegin(GL_LINE_LOOP);
        glVertex2fv(@FDrawVerts[FFaces[i].Vert1].TexCoord);
        glVertex2fv(@FDrawVerts[FFaces[i].Vert2].TexCoord);
        glVertex2fv(@FDrawVerts[FFaces[i].Vert3].TexCoord);
      glEnd;
    end;
  finally
    if Assigned(FObj.Material) then FObj.Material.EndUV;
  end;
end;

function TPMBMesh.Import(MeshData: TStream): Boolean;
var
  VertsCount: Byte;
  Verts: packed array of TVertex;
  FacesCount: Word;
  i: Integer;
  MinVert, MaxVert, Scale, NScale, Translate: TVector3D;
  NPhi, NTheta: Single;
begin
  Result:=false;
  MeshData.Read(VertsCount, SizeOf(VertsCount));
  SetLength(Verts, VertsCount+1);
  MeshData.Read(Verts[0], (VertsCount+1)*SizeOf(TVertex));
  MeshData.Read(FacesCount, SizeOf(FacesCount));
  SetLength(FFaces, FacesCount);
  MeshData.Read(FFaces[0], FacesCount*SizeOf(TFace));
  for i:=0 to High(Verts) do
  begin
    MinVert.X:=Min(MinVert.X, Verts[i].Vertex.X);
    MinVert.Y:=Min(MinVert.Y, Verts[i].Vertex.Y);
    MinVert.Z:=Min(MinVert.Z, Verts[i].Vertex.Z);
    MaxVert.X:=Max(MaxVert.X, Verts[i].Vertex.X);
    MaxVert.Y:=Max(MaxVert.Y, Verts[i].Vertex.Y);
    MaxVert.Z:=Max(MaxVert.Z, Verts[i].Vertex.Z);
  end;
  Scale.X:=255/(MaxVert.X-MinVert.X);
  Scale.Y:=255/(MaxVert.Y-MinVert.Y);
  Scale.Z:=255/(MaxVert.Z-MinVert.Z);
  NScale.X:=1/Scale.X;
  NScale.Y:=1/Scale.Y;
  NScale.Z:=1/Scale.Z;
  Translate.X:=-(MaxVert.X+MinVert.X)/2;
  Translate.Y:=-(MaxVert.Y+MinVert.Y)/2;
  Translate.Z:=-(MaxVert.Z+MinVert.Z)/2;
  SetLength(FVerts, Length(Verts));
  for i:=0 to High(Verts) do
  with Verts[i], FVerts[i] do
    begin
      X:=Max(Min(Round(Scale.X*(Vertex.X+Translate.X)), 127), -128);
      Y:=Max(Min(Round(Scale.Y*(Vertex.Y+Translate.Y)), 127), -128);
      Z:=Max(Min(Round(Scale.Z*(Vertex.Z+Translate.Z)), 127), -128);
      VectorMultiply(Normal, NScale);
      VectorScale(Normal, -1);
      VectorNormalize(Normal);
      if Normal.X<>0 then
      begin
        NPhi:=arctan(Normal.Y/Normal.X);
        if Normal.X<0 then NPhi:=NPhi+pi;
      end
        else NPhi:=0;
      NTheta:=arccos(Normal.Z);
      Phi:=Round(RadToBDeg*NPhi);
      Theta:=Round(2*RadToBDeg*NTheta);
      if TexCoord.X<0
        then TexCoord.X:=TexCoord.X-Floor(TexCoord.X)
        else TexCoord.X:=Frac(TexCoord.X);
      if TexCoord.Y<0
        then TexCoord.Y:=TexCoord.Y-Floor(TexCoord.Y)
        else TexCoord.Y:=Frac(TexCoord.Y);
      U:=Round(TexCoord.X*255);
      V:=Round(TexCoord.Y*255);
    end;
  Scale.X:=1/Scale.X;
  Scale.Y:=1/Scale.Y;
  Scale.Z:=1/Scale.Z;
  FTransform.SetScale(Scale, true);
  Result:=true;
end;

function TPMBMesh.WriteChunk(Data: TStream): Boolean;
var
  ChunkType: Byte;
  ChunkSize: Word;
  ChunkStart, i: Integer;
  Header: TPMMesh;
  PMFaces: packed array of TPMFace;
begin
  Result:=false;
  ChunkStart:=Data.Position;
  ChunkType:=ChunkObjectMesh;
  Data.Write(ChunkType, SizeOf(ChunkType));
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Header.VertsCount:=Length(FVerts)-1;
  Header.FacesCount:=Length(FFaces);
  Header.Flags:=0;
  if FHasNormals then Header.Flags:=Header.Flags or MeshHasNormals;
  if FHasUV then Header.Flags:=Header.Flags or MeshHasUV;
  Data.Write(Header, SizeOf(Header));
  FTransform.Write(Data);
  for i:=0 to High(FVerts) do
  begin
    Data.Write(FVerts[i].X, 3*SizeOf(FVerts[0].X));
    if FHasNormals then Data.Write(FVerts[i].Phi, SizeOf(FVerts[0].Phi)+SizeOf(FVerts[0].Theta));
    if FHasUV then Data.Write(FVerts[i].U, 2*SizeOf(FVerts[0].U));
  end;
  SetLength(PMFaces, Length(FFaces));
  for i:=0 to High(FFaces) do
  begin
    PMFaces[i].Vert1:=FFaces[i].Vert1;
    PMFaces[i].Vert2:=FFaces[i].Vert2;
    PMFaces[i].Vert3:=FFaces[i].Vert3;
  end;
  Data.Write(PMFaces[0], Length(PMFaces)*SizeOf(TPMFace));
  ChunkSize:=Data.Position-ChunkStart;
  Data.Seek(ChunkStart+SizeOf(ChunkType), soFromBeginning);
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Seek(0, soFromEnd);
  Result:=true;
end;

procedure TPMBMesh.ReadChunk(Data: TStream; ChunkID: Byte; ChunkSize: Integer);
var
  Header: TPMMesh;
  PMFaces: packed array of TPMFace;
  i: Integer;
begin
  ChunkSize:=ChunkSize+Data.Position;
  Data.Read(Header, SizeOf(Header));
  SetLength(FVerts, Header.VertsCount+1);
  FHasNormals:=Header.Flags and MeshHasNormals <> 0;
  FHasUV:=Header.Flags and MeshHasUV <> 0;
  FTransform.Read(Data);
  for i:=0 to High(FVerts) do
  begin
    Data.Read(FVerts[i].X, 3*SizeOf(FVerts[0].X));
    if FHasNormals then Data.Read(FVerts[i].Phi, SizeOf(FVerts[0].Phi)+SizeOf(FVerts[0].Theta));
    if FHasUV then Data.Read(FVerts[i].U, 2*SizeOf(FVerts[0].U));
  end;
  SetLength(PMFaces, Header.FacesCount);
  Data.Read(PMFaces[0], Header.FacesCount*SizeOf(TPMFace));
  SetLength(FFaces, Header.FacesCount);
  for i:=0 to Header.FacesCount-1 do
  begin
    FFaces[i].Vert1:=PMFaces[i].Vert1;
    FFaces[i].Vert2:=PMFaces[i].Vert2;
    FFaces[i].Vert3:=PMFaces[i].Vert3;
  end;
  Finalize(PMFaces);
  if Data.Position<>ChunkSize then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

procedure TPMBMesh.CreateDrawVerts;
var
  i: Integer;
begin
  SetLength(FDrawVerts, Length(FVerts));
  for i:=0 to High(FVerts) do
    with FDrawVerts[i], FVerts[i] do
    begin
      Vertex.X:=X/128;
      Vertex.Y:=Y/128;
      Vertex.Z:=Z/128;
      if FHasNormals then
      begin
        Normal.X:=cos(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
        Normal.Y:=sin(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
        Normal.Z:=cos(Theta*BDegToRad/2);
      end
        else VectorClear(Normal);
      if FHasUV then
      begin
        TexCoord.X:=U/255;
        TexCoord.Y:=V/255;
      end
        else VectorClear(TexCoord);
    end;
  FTransform.Apply(FDrawVerts);
  if not FHasNormals
    then ComputeNormalsTriangles(FDrawVerts, FFaces);
end;

function TPMBMesh.GetVertsCount: Integer;
begin
  Result:=Length(FVerts);
end;

function TPMBMesh.GetVertex(Index: Byte): TPMVertex;
begin
  if Index<VertsCount then Result:=FVerts[Index];
end;

procedure TPMBMesh.Relink(Obj: TPMBObject);
begin
  if Obj=FObj then Exit;
  FObj.Mesh:=nil;
  FObj:=Obj;
  FObj.Mesh:=Self;
end;

procedure TPMBMesh.SetSelected(Value: Boolean);
begin
  if Value then FObj.Model.DeselectAll;
  FSelected:=Value;
end;

procedure TPMBMesh.SetVertex(Index: Byte; Vertex: TPMVertex);
begin
  if Index<VertsCount then FVerts[Index]:=Vertex;
end;

{TPMBPrimitive}

constructor TPMBPrimitive.Create(Obj: TPMBObject);
begin
  inherited Create;
  FObj:=Obj;
  FObj.AddPrimitive(Self);
  FTransform:=TPMBTransform.Create;
  FVisible:=true;
end;

destructor TPMBPrimitive.Destroy;
begin
  FObj.DeletePrimitive(Self);
  FTransform.Free;
  Finalize(FFaces);
  Finalize(FVerts);
  inherited Destroy;
end;

procedure TPMBPrimitive.Draw;
var
  i: Integer;
  NVert: TVector3D;
begin
  if not FVisible or not CreateVerts then Exit;
  if FInvertNormals then
    for i:=0 to High(FVerts) do
      VectorScale(FVerts[i].Normal, -1);
  if FSelected then
  begin
    glPushAttrib(GL_LIGHTING_BIT or GL_CURRENT_BIT);
    glColor(0.5, 0.5, 1.0);
    glEnable(GL_COLOR_MATERIAL);
  end;
  glBegin(GL_TRIANGLES);
    for i:=0 to High(FFaces) do
    begin
      glNormal3fv(@FVerts[FFaces[i].Vert1].Normal);
      glTexCoord2fv(@FVerts[FFaces[i].Vert1].TexCoord);
      glVertex3fv(@FVerts[FFaces[i].Vert1].Vertex);
      glNormal3fv(@FVerts[FFaces[i].Vert2].Normal);
      glTexCoord2fv(@FVerts[FFaces[i].Vert2].TexCoord);
      glVertex3fv(@FVerts[FFaces[i].Vert2].Vertex);
      glNormal3fv(@FVerts[FFaces[i].Vert3].Normal);
      glTexCoord2fv(@FVerts[FFaces[i].Vert3].TexCoord);
      glVertex3fv(@FVerts[FFaces[i].Vert3].Vertex);
    end;
  glEnd;
  if FSelected then glPopAttrib;
  if FDrawNormals then
  begin
    glPushAttrib(GL_ENABLE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT);
    glEnable(GL_COLOR_MATERIAL);
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
    glColor3f(1, 1, 1);
    glBegin(GL_LINES);
      for i:=0 to High(FFaces) do
      begin
        NVert:=VectorAdd(FVerts[FFaces[i].Vert1].Vertex, FVerts[FFaces[i].Vert1].Normal);
        glVertex3fv(@FVerts[FFaces[i].Vert1].Vertex);
        glVertex3fv(@NVert);
        NVert:=VectorAdd(FVerts[FFaces[i].Vert2].Vertex, FVerts[FFaces[i].Vert2].Normal);
        glVertex3fv(@FVerts[FFaces[i].Vert2].Vertex);
        glVertex3fv(@NVert);
        NVert:=VectorAdd(FVerts[FFaces[i].Vert3].Vertex, FVerts[FFaces[i].Vert3].Normal);
        glVertex3fv(@FVerts[FFaces[i].Vert3].Vertex);
        glVertex3fv(@NVert);
      end;
    glEnd;
    glPopAttrib;
  end;
end;

procedure TPMBPrimitive.DrawUV;
var
  i: Integer;
begin
  if not CreateVerts then Exit;
  if Assigned(FObj.Material) then FObj.Material.ApplyUV;
  try
    for i:=0 to High(FFaces) do
    begin
      glBegin(GL_LINE_LOOP);
        glVertex2fv(@FVerts[FFaces[i].Vert1].TexCoord);
        glVertex2fv(@FVerts[FFaces[i].Vert2].TexCoord);
        glVertex2fv(@FVerts[FFaces[i].Vert3].TexCoord);
      glEnd;
    end;
  finally
    if Assigned(FObj.Material) then FObj.Material.EndUV;
  end;
end;

procedure TPMBPrimitive.Quad(At: Integer; V1, V2, V3, V4: Word);
begin
  with FFaces[At] do
  begin
    Vert1:=V1;
    Vert2:=V2;
    Vert3:=V3;
  end;
  with FFaces[At+1] do
  begin
    Vert1:=V1;
    Vert2:=V3;
    Vert3:=V4;
  end;
end;

function TPMBPrimitive.CreateCircle(const Center: TVector3D; Radius: Single; Sector: Byte; Count: Integer; Smooth, DoubleLine: Boolean): TVertInfo;
var
  i, j: Integer;
  dPhi: Single;
  V: TVector3D;
  Stride: Integer;
begin
  Result.Count:=Count+1;
  Stride:=SelI(Smooth, 1, 2);
  Result.LinesCount:=SelI(DoubleLine, SelI(Smooth, 2, 4), SelI(Smooth, 1, 2));
  for i:=0 to Result.LinesCount-1 do
    with Result do
    begin
      Lines[i].Start:=Length(FVerts)+(i div 2)*2*Count+(i mod 2)*SelI(Smooth, Count, 1);
      Lines[i].Stride:=Stride;
    end;
  dPhi:=(Sector+1)*pi/(128*Count);
  SetLength(FVerts, Length(FVerts)+Result.Count*Result.LinesCount);
  for i:=0 to Count do
  begin
    with V do
    begin
      X:=Center.X+Radius*Cos(i*dPhi);
      Y:=Center.Y;
      Z:=Center.Z+Radius*Sin(i*dPhi);
    end;
    for j:=0 to Result.LinesCount-1 do
      FVerts[Result.Lines[j].Start+i*Result.Lines[j].Stride].Vertex:=V;
  end;
end;

procedure TPMBPrimitive.CreateTCLine(FromX, ToX, FromY, ToY: Single; const VertInfo: TVertInfo);
var
  dX, dY: Single;
  i: Integer;
begin
  {dX:=(ToX-FromX)/Count;
  dY:=(ToY-FromY)/Count;
  for i:=0 to Count do
  with VA[i].TexCoord do
  begin
    X:=FromX+i*dX;
    Y:=FromY+i*dY;
  end; }
end;

procedure TPMBPrimitive.CreateTCCircle(CenterX, CenterY, Radius: Single; const VertInfo: TVertInfo);
begin

end;

procedure TPMBPrimitive.CreateStrip(Line1Start, Line1Stride, Line2Start, Line2Stride, Count: Integer; Smooth: Boolean);
var
  i, Start, L1, L2: Integer;
begin
  Start:=Length(FFaces);
  SetLength(FFaces, Start+2*Count);
  for i:=0 to Count-1 do
  begin
    L1:=Line1Start+i*Line1Stride;
    L2:=Line2Start+i*Line2Stride;
    //Quad(Start+i*2, Line1Start+i*Line1Stride
  end;
end;

function TPMBPrimitive.WriteChunk(Data: TStream): Boolean;
var
  ChunkType: Byte;
  ChunkSize: Word;
  ChunkStart: Integer;
begin
  Result:=false;
  ChunkStart:=Data.Position;
  ChunkType:=ChunkObjectPrimitive;
  Data.Write(ChunkType, SizeOf(ChunkType));
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Write(FType, SizeOf(FType));
  FTransform.Write(Data);
  FFlags:=0;
  if FTexGenUV then FFlags:=FFlags or PMFTexInfo;
  if FInvertNormals then FFlags:=FFlags or PMFInvertNormals;
  UpdateFlags;
  Data.Write(FFlags, SizeOf(FFlags));
  DoWriteChunk(Data);
  ChunkSize:=Data.Position-ChunkStart;
  Data.Seek(ChunkStart+SizeOf(ChunkType), soFromBeginning);
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Seek(0, soFromEnd);
  Result:=true;
end;

class function TPMBPrimitive.ReadChunk(Obj: TPMBObject; Data: TStream; ChunkSize: Integer): TPMBPrimitive;
var
  ChunkEnd: Integer;
  PriType: Byte;
begin
  Result:=nil;
  ChunkEnd:=Data.Position+ChunkSize;
  Data.Read(PriType, SizeOf(PriType));
  case PriType of
    PrimitiveCube: Result:=TPMBPrimitiveCube.Create(Obj);
    PrimitiveSphere: Result:=TPMBPrimitiveSphere.Create(Obj);
    PrimitiveCone: Result:=TPMBPrimitiveCone.Create(Obj);
    PrimitiveTorus: Result:=TPMBPrimitiveTorus.Create(Obj);
    PrimitiveTube: Result:=TPMBPrimitiveTube.Create(Obj);
    else Exit;
  end;
  with Result do
  begin
    FTransform.Read(Data);
    Data.Read(FFlags, SizeOf(FFlags));
    FTexGenUV:=FFlags and PMFTexInfo <> 0;
    FInvertNormals:=FFlags and PMFInvertNormals <> 0;
    DoReadChunk(Data, ChunkEnd-Data.Position);
  end;
  if Data.Position<>ChunkEnd then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

procedure TPMBPrimitive.Relink(Obj: TPMBObject);
begin
  if Obj=FObj then Exit;
  FObj.DeletePrimitive(Self);
  FObj:=Obj;
  FObj.AddPrimitive(Self);
end;

procedure TPMBPrimitive.SetSelected(Value: Boolean);
begin
  if Value then FObj.Model.DeselectAll;
  FSelected:=Value;
end;

{TPMBPrimitiveCube}

constructor TPMBPrimitiveCube.Create(Obj: TPMBObject);
begin
  inherited Create(Obj);
  FType:=PrimitiveCube;
end;

function TPMBPrimitiveCube.GetUV(Index: Byte): TPMUVRect;
begin
  if Index<3 then Result:=FTexUV[Index];
end;

procedure TPMBPrimitiveCube.SetUV(Index: Byte; Value: TPMUVRect);
begin
  if Index<3 then FTexUV[Index]:=Value;
end;

function TPMBPrimitiveCube.GetSplitSides(Index: Byte): Boolean;
begin
  if Index<3 then Result:=FTexMergeSides[Index] else Result:=false;
end;

procedure TPMBPrimitiveCube.SetSplitSides(Index: Byte; Value: Boolean);
begin
  if Index<3 then FTexMergeSides[Index]:=Value;
end;

function TPMBPrimitiveCube.CreateVerts: Boolean;

  function Coord(Bit: Integer; Pattern: Byte): Single;
  begin
    Result:=((Pattern and (1 shl Bit)) shr Bit);
  end;

const
  Verts: array[0..23] of Byte = (
    $06, $0F, $1B, $12,
    $20, $29, $3D, $34,
    $46, $4A, $58, $54,
    $63, $6F, $7D, $71,
    $82, $8B, $99, $90,
    $A7, $AE, $BC, $B5);
var
  MergeInfo: Byte;
  i, FaceType: Integer;
begin
  Result:=false;
  SetLength(FVerts, 24);
  ZeroMemory(@FVerts[0], SizeOf(TVertex)*Length(FVerts));
  SetLength(FFaces, 12);
  MergeInfo:=0;
  for i:=0 to 2 do
    if FTexMergeSides[i] then MergeInfo:=MergeInfo or 1 shl i;
  for i:=0 to High(FVerts) do
    with FVerts[i] do
    begin
      with Vertex do
      begin
        X:=Coord(0, Verts[i])-0.5;
        Y:=Coord(1, Verts[i])-0.5;
        Z:=Coord(2, Verts[i])-0.5;
      end;
      if FTexGenUV then
        with TexCoord do
        begin
          FaceType:=(Verts[i] and $C0) shr 6;
          X:=FTexUV[FaceType].OrigU/255+Coord(3, Verts[i])*FTexUV[FaceType].SizeU/255;
          Y:=FTexUV[FaceType].OrigV/255+Coord(4, Verts[i])*FTexUV[FaceType].SizeV/255
            +Coord(5, Verts[i])*Coord(FaceType, MergeInfo)*FTexUV[FaceType].SizeV/255;
        end;
    end;
  for i:=0 to 5 do
    Quad(2*i, 4*i, 4*i+1, 4*i+2, 4*i+3);
  FTransform.Apply(FVerts);
  ComputeNormalsTriangles(FVerts, FFaces);
  Result:=true;
end;

procedure TPMBPrimitiveCube.UpdateFlags;
var
  i: Byte;
begin
  for i:=0 to 2 do
    if FTexMergeSides[i] then FFlags:=FFlags or 1 shl i;
end;

function TPMBPrimitiveCube.DoWriteChunk(Data: TStream): Boolean;
begin
  if FTexGenUV then Data.Write(FTexUV, SizeOf(FTexUV));
  Result:=true;
end;

procedure TPMBPrimitiveCube.DoReadChunk(Data: TStream; ChunkSize: Integer);
var
  i: Integer;
begin
  for i:=0 to 2 do
    FTexMergeSides[i]:=FFlags and (1 shl i) <> 0;
  if FTexGenUV then Data.Read(FTexUV, SizeOf(FTexUV));
end;

{TPMBPrimitiveSphere}

constructor TPMBPrimitiveSphere.Create(Obj: TPMBObject);
begin
  inherited Create(Obj);
  FType:=PrimitiveSphere;
  FSmooth:=true;
  FSlices:=16;
  FStacks:=8;
  FSlicesSector:=255;
  FStacksSector:=255;
end;

function TPMBPrimitiveSphere.CreateVerts: Boolean;
var
  Slice, Stack, StackLen, VertBase, Start, Start2: Integer;
  R, H, V, dPhi, dTheta, U0, V0, dU, dV: Single;
  Vert: TVertex;
begin
  Result:=false;
  dTheta:=(FStacksSector+1)*pi/(256*FStacks);
  //dPhi:=2*FSlicesSector*pi/(255*FSlices);
  U0:=FTexUV.OrigU/255;
  V0:=FTexUV.OrigV/255;
  dU:=FTexUV.SizeU/(255*FSlices);
  dV:=FTexUV.SizeV/(255*FStacks);
  StackLen:=SelI(FSmooth, FSlices+1, 4*FSlices);
  VertBase:=SelI(FSmooth, 0, -2*FSlices);
  VectorClear(Vert.Normal);
  SetLength(FVerts, SelI(FSmooth, (FSlices+1)*(FStacks+1), 4*FSlices*FStacks));
  SetLength(FFaces, 2*FStacks*FSlices);
  for Stack:=0 to FStacks do
  begin
    R:=Sin(Stack*dTheta);
    if (Stack=0) or ((Stack=FStacks) and (FStacksSector=255)) then R:=0;
    H:=Cos(Stack*dTheta);
    V:=V0+Stack*dV;
    Start:=Stack*StackLen+VertBase;
    Start2:=Start+StackLen;
    for Slice:=0 to FSlices do
    begin
      with Vert do
      begin
        with Vertex do
        begin
          X:=R*Cos(Slice*dPhi);
          Y:=H;
          Z:=R*Sin(Slice*dPhi);
        end;
        if FSmooth then
        begin
          Normal:=Vertex;
          VectorNormalize(Normal);
        end;
        if FTexGenUV then
          with TexCoord do
          begin
            X:=U0+Slice*dU;
            Y:=V;
          end;
      end;
      if FSmooth then FVerts[Start+Slice]:=Vert
      else begin
        if Stack>0 then
        begin
          if Slice>0 then FVerts[Start+2*Slice-1]:=Vert;
          if Slice<FSlices then FVerts[Start+2*Slice]:=Vert;
        end;
        if Stack<FStacks then
        begin
          if Slice>0 then FVerts[Start+2*FSlices+2*Slice-1]:=Vert;
          if Slice<FSlices then FVerts[Start+2*FSlices+2*Slice]:=Vert;
        end;
      end;
      if (Stack<FStacks) and (Slice<FSlices) then
        if FSmooth
          then Quad(2*(Stack*FSlices+Slice), Start+Slice, Start+Slice+1, Start2+Slice+1, Start2+Slice)
          else Quad(2*(Stack*FSlices+Slice), Start+2*FSlices+2*Slice, Start+2*FSlices+2*Slice+1, Start2+2*Slice+1, Start2+2*Slice);
    end;
  end;
  FTransform.Apply(FVerts);
  if not FSmooth then ComputeNormalsTriangles(FVerts, FFaces);
  Result:=true;
end;

procedure TPMBPrimitiveSphere.UpdateFlags;
begin
  if FSmooth then FFlags:=FFlags or PMFSmooth;
end;

function TPMBPrimitiveSphere.DoWriteChunk(Data: TStream): Boolean;
var
  Sphere: TPPSphere;
begin
  Result:=false;
  Sphere.Slices:=FSlices;
  Sphere.Stacks:=FStacks;
  Sphere.SlicesSector:=FSlicesSector;
  Sphere.StacksSector:=FStacksSector;
  Data.Write(Sphere, SizeOf(Sphere)-SizeOf(Sphere.UV));
  Sphere.UV:=FTexUV;
  if FTexGenUV then Data.Write(Sphere.UV, SizeOf(Sphere.UV));
  Result:=true;
end;

procedure TPMBPrimitiveSphere.DoReadChunk(Data: TStream; ChunkSize: Integer);
var
  Sphere: TPPSphere;
begin
  FSmooth:=FFlags and PMFSmooth <> 0;
  Data.Read(Sphere, SizeOf(Sphere)-SizeOf(Sphere.UV));
  if FTexGenUV then Data.Read(Sphere.UV, SizeOf(Sphere.UV));
  FSlices:=Sphere.Slices;
  FStacks:=Sphere.Stacks;
  FSlicesSector:=Sphere.SlicesSector;
  FStacksSector:=Sphere.StacksSector;
  if FTexGenUV then FTexUV:=Sphere.UV;
end;

{TPMBPrimitiveCone}

constructor TPMBPrimitiveCone.Create(Obj: TPMBObject);
begin
  inherited Create(Obj);
  FType:=PrimitiveCone;
  FSmooth:=true;
  FSlices:=16;
  FSlicesSector:=255;
  FRadiusT:=128;
  FRadiusB:=255;
end;

function TPMBPrimitiveCone.CreateVerts: Boolean;
const
  Sign: array[0..1] of Integer=(1, -1);
var
  Slice, iY, BaseLen: Integer;
  dPhi, R, U0, V0, dU, dV: Single;
  Vert: TVertex;
  Normal: TVector3D;
  Radius: array[0..1] of Single;
begin
  Result:=false;
  dPhi:=2*FSlicesSector*pi/(255*FSlices);
  U0:=FUVSide.OrigU/255;
  V0:=FUVSide.OrigV/255;
  dU:=FUVSide.SizeU/(255*FSlices);
  dV:=FUVSide.SizeV/255;
  Radius[0]:=FRadiusB/255;
  Radius[1]:=FRadiusT/255;
  VectorClear(Vert.Normal);
  SetLength(FVerts, SelI(FSmooth, FSlices*4+2, 6*FSlices));
  SetLength(FFaces, 4*FSlices-2);
  BaseLen:=SelI(FSmooth, FSlices+1, 2*FSlices);
  for iY:=0 to 1 do
    for Slice:=0 to FSlices do
    begin
      with Vert do
      begin
        with Vertex do
        begin
          X:=Radius[iY]*Cos(Slice*dPhi);
          Y:=iY-0.5;
          Z:=Radius[iY]*Sin(Slice*dPhi);
        end;
        if FTexGenUV then
          with TexCoord do
          begin
            X:=U0+Slice*dU;
            Y:=V0+iY*dV;
          end;
      end;
      if FSmooth or (Slice<FSlices)
        then FVerts[iY*BaseLen+SelI(FSmooth, 1, 2)*Slice]:=Vert;
      if (Slice>0) and not FSmooth
        then FVerts[iY*BaseLen+SelI(FSmooth, 1, 2)*Slice-1]:=Vert;
      if Slice<FSlices then FVerts[2*BaseLen+iY*FSlices+Slice]:=Vert;
      if (iY=0) and (Slice<FSlices) then
        if FSmooth
          then Quad(2*Slice, BaseLen+Slice, BaseLen+Slice+1, Slice+1, Slice)
          else Quad(2*Slice, BaseLen+2*Slice, BaseLen+2*Slice+1, 2*Slice+1, 2*Slice);
      if Slice<FSlices-1 then
      begin
        with FFaces[2*FSlices+iY*(FSlices-1)+Slice] do
        begin
          Vert1:=2*BaseLen+iY*FSlices;
          Vert2:=2*BaseLen+2*iY*FSlices+Sign[iY]*(Slice+1);
          Vert3:=2*BaseLen+2*iY*FSlices+Sign[iY]*(Slice+2);
        end;
      end;
    end;
  FTransform.Apply(FVerts);
  ComputeNormalsTriangles(FVerts, FFaces);
  if FSmooth and (FSlicesSector=255) then
    for iY:=0 to 1 do
    begin
      Normal:=VectorAdd(FVerts[iY*BaseLen].Normal, FVerts[iY*BaseLen+BaseLen-1].Normal);
      VectorNormalize(Normal);
      FVerts[iY*BaseLen].Normal:=Normal;
      FVerts[iY*BaseLen+BaseLen-1].Normal:=Normal;
    end;
  Result:=true;
end;

procedure TPMBPrimitiveCone.UpdateFlags;
begin
  if FSmooth then FFlags:=FFlags or PMFSmooth;
end;

function TPMBPrimitiveCone.DoWriteChunk(Data: TStream): Boolean;
var
  Cone: TPPCone;
begin
  Result:=false;
  Cone.RadiusT:=FRadiusT;
  Cone.RadiusB:=FRadiusB;
  Cone.Slices:=FSlices;
  Cone.SlicesSector:=FSlicesSector;
  if FTexGenUV then
  begin
    Cone.UVSide:=UVSide;
    Cone.UVBaseT:=UVBaseT;
    Cone.UVBaseB:=UVBaseB;
  end;
  Data.Write(Cone, SizeOf(Cone)-ConeUVSize);
  if FTexGenUV then Data.Write(Cone.UVSide, ConeUVSize);
  Result:=true;
end;

procedure TPMBPrimitiveCone.DoReadChunk(Data: TStream; ChunkSize: Integer);
var
  Cone: TPPCone;
begin
  FSmooth:=FFlags and PMFSmooth <> 0;
  Data.Read(Cone, SizeOf(Cone)-ConeUVSize);
  if FTexGenUV then Data.Read(Cone.UVSide, ConeUVSize);
  FRadiusT:=Cone.RadiusT;
  FRadiusB:=Cone.RadiusB;
  FSlices:=Cone.Slices;
  FSlicesSector:=Cone.SlicesSector;
  if FTexGenUV then
  begin
    UVSide:=Cone.UVSide;
    UVBaseT:=Cone.UVBaseT;
    UVBaseB:=Cone.UVBaseB;
  end;
end;

{TPMBPrimitiveTorus}

constructor TPMBPrimitiveTorus.Create(Obj: TPMBObject);
begin
  inherited Create(Obj);
  FType:=PrimitiveTorus;
  //to do
end;

destructor TPMBPrimitiveTorus.Destroy;
begin
  //to do
  inherited Destroy;
end;

function TPMBPrimitiveTorus.CreateVerts: Boolean;
begin
  Result:=false;
end;

procedure TPMBPrimitiveTorus.UpdateFlags;
begin
  //to do
end;

function TPMBPrimitiveTorus.DoWriteChunk(Data: TStream): Boolean;
begin
  //to do
end;

procedure TPMBPrimitiveTorus.DoReadChunk(Data: TStream; ChunkSize: Integer);
begin
  //to do
end;

{TPMBPrimitiveTube}

constructor TPMBPrimitiveTube.Create(Obj: TPMBObject);
begin
  inherited Create(Obj);
  FType:=PrimitiveTube;
  //to do
end;

destructor TPMBPrimitiveTube.Destroy;
begin
  //to do
  inherited Destroy;
end;

function TPMBPrimitiveTube.CreateVerts: Boolean;
begin
  Result:=false;
end;

procedure TPMBPrimitiveTube.UpdateFlags;
begin
  //to do
end;

function TPMBPrimitiveTube.DoWriteChunk(Data: TStream): Boolean;
begin
  //to do
end;

procedure TPMBPrimitiveTube.DoReadChunk(Data: TStream; ChunkSize: Integer);
begin
  //to do
end;

{TPMBObject}

constructor TPMBObject.Create(Model: TPMBModel; Parent: TPMBObject);
begin
  inherited Create;
  FModel:=Model;
  FParent:=Parent;
  if Assigned(FParent)
    then FParent.AddObject(Self)
    else FModel.AddObject(Self);
  FObjects:=TList.Create;
  FPrimitives:=TList.Create;
  FTransform:=TPMBTransform.Create;
  FVisible:=true;
end;

destructor TPMBObject.Destroy;
begin
  FTransform.Free;
  if Assigned(FParent)
    then FParent.DeleteObject(Self)
    else FModel.DeleteObject(Self);
  while ObjectsCount>0 do Objects[0].Free;
  FObjects.Free;
  while PrimitivesCount>0 do Primitives[0].Free;
  FPrimitives.Free;
  inherited Destroy;
end;

procedure TPMBObject.Draw;
var
  i: Integer;
begin
  glPushMatrix;
  FTransform.Apply;
  if FVisible then
  begin
    glEnable(GL_NORMALIZE);
    if Assigned(FMaterial) then FMaterial.Apply;
    if FSelected then
    begin
      glPushAttrib(GL_LIGHTING_BIT or GL_CURRENT_BIT);
      glColor(0.5, 0.5, 1.0);
      glEnable(GL_COLOR_MATERIAL);
    end;
    if Assigned(FMesh) then FMesh.Draw;
    for i:=0 to PrimitivesCount-1 do Primitives[i].Draw;
    if FSelected then glPopAttrib;
  end;
  for i:=0 to ObjectsCount-1 do Objects[i].Draw;
  glPopMatrix;
end;

procedure TPMBObject.DrawUV;
var
  i: Integer;
begin
  if Assigned(FMaterial) then FMaterial.ApplyUV;
  try
    if Assigned(FMesh) then FMesh.DrawUV;
    for i:=0 to PrimitivesCount-1 do Primitives[i].DrawUV;
  finally
    if Assigned(FMaterial) then FMaterial.EndUV;
  end;
end;

procedure TPMBObject.SetVisibility(Visibility: Boolean);
var
  i: Integer;
begin
  FVisible:=Visibility;
  if Assigned(FMesh) then FMesh.Visible:=Visibility;
  for i:=0 to PrimitivesCount-1 do Primitives[i].Visible:=Visibility;
  for i:=0 to ObjectsCount-1 do Objects[i].SetVisibility(Visibility);
end;

function TPMBObject.GetID: string;
var
  i: Integer;
  IDC: PIDC;
begin
  SetLength(Result, 4);
  IDC:=@FID;
  for i:=1 to 4 do
    if Byte(IDC^[i])<32
      then Result[i]:=' '
      else Result[i]:=IDC[i];
end;

procedure TPMBObject.SetID(const ID: string);
var
  i: Integer;
  IDC: TIDC;
begin
  for i:=1 to 4 do
    if i<=Length(ID)
      then IDC[i]:=ID[i]
      else IDC[i]:=#32;
  IID:=Cardinal(IDC);
end;

procedure TPMBObject.SetIID(IID: Cardinal);
begin
  FID:=IID;
end;

function TPMBObject.GetObjectsCount: Integer;
begin
  Result:=FObjects.Count;
end;

function TPMBObject.GetObject(Index: Integer): TPMBObject;
begin
  Result:=nil;
  if (Index<0) or (Index>FObjects.Count-1) then Exit;
  Result:=TPMBObject(FObjects[Index]);
end;

procedure TPMBObject.SetMesh(Mesh: TPMBMesh);
begin
  if Assigned(FMesh) and Assigned(Mesh) then FMesh.Free;
  FMesh:=Mesh;
end;

function TPMBObject.GetPrimitivesCount: Integer;
begin
  Result:=FPrimitives.Count;
end;

function TPMBObject.GetPrimitive(Index: Integer): TPMBPrimitive;
begin
  Result:=nil;
  if (Index<0) or (Index>FPrimitives.Count-1) then Exit;
  Result:=TPMBPrimitive(FPrimitives[Index]);
end;

function TPMBObject.AddObject(Obj: TPMBObject): Integer;
begin
  Result:=FObjects.Add(Obj);
end;

procedure TPMBObject.DeleteObject(Obj: TPMBObject);
begin
  FObjects.Remove(Obj)
end;

function TPMBObject.AddPrimitive(Primitive: TPMBPrimitive): Integer;
begin
  Result:=FPrimitives.Add(Primitive);
end;

procedure TPMBObject.DeletePrimitive(Primitive: TPMBPrimitive);
begin
  FPrimitives.Remove(Primitive);
end;

procedure TPMBObject.DeselectAll;
var
  i: Integer;
begin
  FSelected:=false;
  if Assigned(FMesh) then FMesh.FSelected:=false;
  for i:=0 to PrimitivesCount-1 do
    Primitives[i].FSelected:=false;
  for i:=0 to ObjectsCOunt-1 do
    Objects[i].DeselectAll;
end;

function TPMBObject.WriteChunk(Data: TStream): Boolean;
var
  ChunkType, MatID: Byte;
  ChunkSize: Word;
  ChunkStart: Integer;
  i: Integer;
begin
  Result:=false;
  ChunkStart:=Data.Position;
  ChunkType:=ChunkModelObject;
  Data.Write(ChunkType, SizeOf(ChunkType));
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Write(FID, SizeOf(FID));
  FTransform.Write(Data);
  if Assigned(FMaterial)
    then MatID:=FMaterial.ID
    else MatID:=0;
  Data.Write(MatID, SizeOf(MatID));
  if Assigned(FMesh) then
    if not FMesh.WriteChunk(Data) then Exit;
  for i:=0 to PrimitivesCount-1 do
    if not Primitives[i].WriteChunk(Data) then Exit;
  for i:=0 to ObjectsCount-1 do
    if not Objects[i].WriteChunk(Data) then Exit;
  ChunkSize:=Data.Position-ChunkStart;
  Data.Seek(ChunkStart+SizeOf(ChunkType), soFromBeginning);
  Data.Write(ChunkSize, SizeOf(ChunkSize));
  Data.Seek(0, soFromEnd);
  Result:=true;
end;

procedure TPMBObject.ReadChunk(Data: TStream; ChunkSize: Integer);
var
  MaterialID: Byte;
begin
  ChunkSize:=Data.Position+ChunkSize; //ChunkEnd
  Data.Read(FID, SizeOf(FID));
  FTransform.Read(Data);
  Data.Read(MaterialID, SizeOf(MaterialID));
  FMaterial:=TPMBMaterial(Integer(MaterialID)); //needs further resolving
  while Data.Position<ChunkSize do ReadChunk(Data);
  if Data.Position<>ChunkSize then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

procedure TPMBObject.ReadChunk(Data: TStream);
var
  ChunkID: Byte;
  ChunkSize: Word;
  ChunkEnd: Integer;
begin
  ChunkEnd:=Data.Position;
  Data.Read(ChunkID, SizeOf(ChunkID));
  Data.Read(ChunkSize, SizeOf(ChunkSize));
  Inc(ChunkEnd, ChunkSize);
  case ChunkID of
    ChunkModelObject: TPMBObject.Create(FModel, Self).ReadChunk(Data, ChunkEnd-Data.Position);
    ChunkObjectPrimitive: TPMBPrimitive.ReadChunk(Self, Data, ChunkEnd-Data.Position);
    ChunkObjectMesh:
      begin
        TPMBMesh.Create(Self);
        FMesh.ReadChunk(Data, ChunkID, ChunkEnd-Data.Position);
      end;
    else raise Exception.CreateFmt(SCannotLoadModelInvalidChunkAt, [ChunkID, ChunkEnd-ChunkSize]);
  end;
  while ChunkEnd>Data.Position do ReadChunk(Data);
  if ChunkEnd<>Data.Position then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

procedure TPMBObject.Relink(Obj: TPMBObject);
begin
  if (Obj=Self) or (Obj=FParent) then Exit;
  if Assigned(FParent)
    then FParent.DeleteObject(Self)
    else FModel.DeleteObject(Self);
  FParent:=Obj;
  FParent.AddObject(Self);
end;

procedure TPMBObject.Relink(Model: TPMBModel);
begin
  if not Assigned(FParent) then Exit;
  if Model<>FModel then raise Exception.Create(SCannotRelinkObjectToAnotherModel);
  FParent.DeleteObject(Self);
  FParent:=nil;
  FModel.AddObject(Self);
end;

procedure TPMBObject.SetSelected(Value: Boolean);
begin
  if Value then FModel.DeselectAll;
  FSelected:=Value;
end;

{TPMBModel}

constructor TPMBModel.Create;
begin
  inherited Create;
  FObjects:=TList.Create;
  FMaterials:=TList.Create;
end;

destructor TPMBModel.Destroy;
begin
  while ObjectsCount>0 do Objects[0].Free;
  while MaterialsCount>0 do Materials[0].Free;
  FObjects.Free;
  FMaterials.Free;
  inherited Destroy;
end;

procedure TPMBModel.Draw;
var
  i: Integer;
begin
  for i:=0 to ObjectsCount-1 do Objects[i].Draw;
end;

procedure TPMBModel.SetVisibility(Visibility: Boolean);
var
  i: Integer;
begin
  for i:=0 to ObjectsCount-1 do Objects[i].SetVisibility(Visibility);
end;

procedure TPMBModel.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  if not FileExists(FileName) then Exit;
  F:=TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TPMBModel.LoadFromStream(Stream: TStream);

  procedure ProcessObj(Obj: TPMBObject);
  var
    i: Integer;
  begin
    Obj.Material:=FindMaterial(Byte(Integer(Obj.Material)));
    for i:=0 to Obj.ObjectsCount-1 do ProcessObj(Obj.Objects[i]);
  end;

var
  ChunkType: Byte;
  ChunkSize: Word;
  i: Integer;
begin
  Stream.Read(ChunkType, SizeOf(ChunkType));
  if ChunkType<>ChunkModel then raise Exception.Create(SCannotLoadModelInvalidFirstChunk);
  while ObjectsCount>0 do Objects[0].Free;
  while MaterialsCount>0 do Materials[0].Free;
  Stream.Read(ChunkSize, SizeOf(ChunkSize));
  while Stream.Position<ChunkSize do ReadChunk(Stream);
  for i:=0 to ObjectsCount-1 do ProcessObj(Objects[i]);
  if Stream.Position<>ChunkSize then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

procedure TPMBModel.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  if FileExists(FileName) then DeleteFile(FileName);
  F:=TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TPMBModel.SaveToStream(Stream: TStream);
var
  ChunkType: Byte;
  ChunkSize: Word;
  i: Integer;
begin
  ChunkType:=ChunkModel;
  ChunkSize:=0;
  Stream.Write(ChunkType, SizeOf(ChunkType));
  Stream.Write(ChunkSize, SizeOf(ChunkSize));
  for i:=0 to ObjectsCount-1 do
    if not Objects[i].WriteChunk(Stream) then Exit;
  for i:=0 to MaterialsCount-1 do
    if not Materials[i].WriteChunk(Stream) then Exit;
  if Stream.Size>MaxWord then raise Exception.Create(SCannotSaveModelResultingFileSize);
  ChunkSize:=Stream.Size;
  Stream.Seek(SizeOf(ChunkType), soFromBeginning);
  Stream.Write(ChunkSize, SizeOf(ChunkSize));
end;

function TPMBModel.FindMaterial(ID: Byte): TPMBMaterial;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to MaterialsCount-1 do
    if Materials[i].ID=ID then
    begin
      Result:=Materials[i];
      Exit;
    end;
end;

function TPMBModel.GetObjectsCount: Integer;
begin
  Result:=FObjects.Count;
end;

function TPMBModel.GetObject(Index: Integer): TPMBObject;
begin
  Result:=nil;
  if (Index<0) or (Index>FObjects.Count-1) then Exit;
  Result:=TPMBObject(FObjects[Index]);
end;

function TPMBModel.GetMaterialsCount: Integer;
begin
  Result:=FMaterials.Count;
end;

function TPMBModel.GetMaterial(Index: Integer): TPMBMaterial;
begin
  Result:=nil;
  if (Index<0) or (Index>FMaterials.Count-1) then Exit;
  Result:=TPMBMaterial(FMaterials[Index]);
end;

function TPMBModel.AddObject(Obj: TPMBObject): Integer;
begin
  Result:=FObjects.Add(Obj);
end;

procedure TPMBModel.DeleteObject(Obj: TPMBObject);
begin
  FObjects.Remove(Obj);
end;

function TPMBModel.ObjIDExists(ID: Cardinal): Boolean;

  function CheckObj(Obj: TPMBObject): Boolean;
  var
    i: Integer;
  begin
    Result:=Obj.IID=ID;
    if not Result then
      for i:=0 to Obj.ObjectsCount-1 do
        if CheckObj(Obj.Objects[i]) then
        begin
          Result:=true;
          Exit;
        end;
  end;

var
  i: Integer;
begin
  Result:=false;
  for i:=0 to ObjectsCount-1 do
    if CheckObj(Objects[i]) then
    begin
      Result:=true;
      Exit;
    end;
end;

function TPMBModel.AddMaterial(Mat: TPMBMaterial): Integer;
begin
  Result:=FMaterials.Add(Mat);
end;

procedure TPMBModel.DeleteMaterial(Mat: TPMBMaterial);

  procedure ProcessObj(Obj: TPMBObject);
  var
    i: Integer;
  begin
    if Obj.Material=Mat then Obj.Material:=nil;
    for i:=0 to Obj.ObjectsCount-1 do ProcessObj(Obj.Objects[i]);
  end;

var
  i: Integer;
begin
  for i:=0 to ObjectsCount-1 do ProcessObj(Objects[i]);
  FMaterials.Remove(Mat);
end;

function TPMBModel.GetMaterialID: Byte;
var
  IDs: array[1..255] of Boolean;
  i: Integer;
begin
  Result:=0;
  for i:=0 to High(IDs) do IDs[i]:=false;
  for i:=0 to MaterialsCount-1 do
    IDs[Materials[i].ID]:=true;
  for i:=1 to High(IDs) do
    if not IDs[i] then
    begin
      Result:=i;
      Exit;
    end;
end;

function TPMBModel.LoadTexture(const TexName: string): Cardinal;
begin
  if Assigned(FOnLoadTex) and (TexName<>'')
    then Result:=FOnLoadTex(TexName)
    else Result:=0;
end;

procedure TPMBModel.BindTexture(ID: Cardinal);
begin
  if Assigned(FOnBindTex) then FOnBindTex(ID);
end;

procedure TPMBModel.DeselectAll;
var
  i: Integer;
begin
  for i:=0 to ObjectsCount-1 do
    Objects[i].DeselectAll;
end;

procedure TPMBModel.ReadChunk(Data: TStream);
var
  ChunkID: Byte;
  ChunkSize: Word;
  ChunkEnd: Integer;
begin
  ChunkEnd:=Data.Position;
  Data.Read(ChunkID, SizeOf(ChunkID));
  Data.Read(ChunkSize, SizeOf(ChunkSize));
  Inc(ChunkEnd, ChunkSize);
  case ChunkID of
    ChunkModelObject: TPMBObject.Create(Self, nil).ReadChunk(Data, ChunkEnd-Data.Position);
    ChunkMaterial: TPMBMaterial.Create(Self).ReadChunk(Data, ChunkEnd-Data.Position);
    else raise Exception.CreateFmt(SCannotLoadModelInvalidChunkAt, [ChunkID, ChunkEnd-ChunkSize]);
  end;
  while ChunkEnd>Data.Position do ReadChunk(Data);
  if ChunkEnd<>Data.Position then raise Exception.Create(SCannotLoadModelChunkSizeMismatch);
end;

function PriTypeToString(PriType: Byte): string;
begin
  case PriType of
    PrimitiveCube: Result:='Cube';
    PrimitiveSphere: Result:='Sphere';
    PrimitiveCone: Result:='Cone';
    PrimitiveTorus: Result:='Torus';
    PrimitiveTube: Result:='Tube';
    else Result:='Unknown';
  end;
end;

end.

