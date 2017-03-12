unit VSEPrimitiveModel;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSEArrayBuffer, VSEVertexArrayUtils, VSEMemPak;

{$I VSEPrimitiveModel.inc}

type
  TVertexArray=array of TVertex;
  TFacesArray=array of TFaceW;
  TTransform=class //Affine transform
  private
    FDefMatrix, FMatrix: TMatrix4D;
  public
    constructor Create; overload;
    constructor Create(const Transform: TPMTransform); overload; //internally used
    procedure Assign(const Transform: TTransform); //Assign values from TTransform
    procedure Apply; //Apply transform to OpenGL
    procedure Reset; //Reset to default values
    procedure SetAsDefault; //Set current values as default
    procedure Translate(X, Y, Z: Single);
    procedure Rotate(Angle, X, Y, Z: Single);
    procedure Scale(X, Y, Z: Single);
    procedure Mul(Transform: TTransform); overload; //Concatenate with other transform
    procedure Mul(const Matrix: TMatrix4D); overload; //Concatenate with transform matrix
  end;
  TPriModel=class;
  TPriModelObject=class //Model object
  private
    FID: Cardinal;
    FModel: TPriModel;
    FVisible, FVertLoaded: Boolean;
    FMaterialID: Byte;
    FIndexBuffer: TArrayBuffer;
    FTransform: TTransform;
    FObjects: array of TPriModelObject;
    function  LastVert(VertexBuffer: TStream): Integer;
    function  ReadChunk(Data, VertexBuffer, IndexBuffer: TStream): Boolean;
    function  GenCube(Params: PPPCube; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
    function  GenSphere(Params: PPPSphere; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
    function  GenCone(Params: PPPCone; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
    function  GenTorus(Params: PPPTorus; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
    function  GenTube(Params: PPPTube; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
    procedure Quad(var IA: TFacesArray; At: Integer; V1, V2, V3, V4: Word);
    procedure ApplyTransform(var VA: TVertexArray; Transform: TPMTransform);
    function  GetObject(ID: Cardinal): TPriModelObject;
  public
    constructor Create(Model: TPriModel; Data, VertexBuffer: TStream); //internally used
    destructor Destroy; override; //internally used
    procedure Draw; //Draw object
    property ID: Cardinal read FID; //Object ID
    property Visible: Boolean read FVisible write FVisible; //Object visibility
    property MaterialID: Byte read FMaterialID write FMaterialID; //Object material ID
    property Objects[ID: Cardinal]: TPriModelObject read GetObject; default; //Subobjects
    property Transform: TTransform read FTransform; //Object transform
  end;
  TPriModelMaterial=class //Model material
  private
    FDiffuse, FSpecular, FAmbient, FEmission: TColor;
    FShininess: Single;
    FTexture: Cardinal;
    FID: Byte;
  public
    constructor Create(Model: TPriModel; Data: TStream); //internally used
    //destructor Destroy; override;
    procedure Apply; //internally used
    property ID: Byte read FID; //Material ID
    property Diffuse: TColor read FDiffuse write FDiffuse; //Diffuse color
    property Specular: TColor read FSpecular write FSpecular; //Specular color
    property Ambient: TColor read FAmbient write FAmbient; //Ambient color
    property Emission: TColor read FEmission write FEmission; //Emission color
    property Shininess: Single read FShininess write FShininess; //Shininess power
    property Texture: Cardinal read FTexture write FTexture; //Texture ID
  end;
  TOnDrawObject=function(Sender: TObject; Obj: TPriModelObject): Boolean of object;
  TPriModel=class //Model
  private
    FVertexBuffer: TArrayBuffer;
    FObjects: array of TPriModelObject;
    FMaterials: array of TPriModelMaterial;
    FOnDrawObject: TOnDrawObject;
    {$IFDEF PM_DRAWNORMALS}FNormals: TArrayBuffer;{$ENDIF}
    function ReadChunk(Data, VertexBuffer: TStream): Boolean;
    function GetObject(ID: Cardinal): TPriModelObject;
    function GetMaterial(ID: Byte): TPriModelMaterial;
  public
    constructor Create(const FileName: string); //Create model from file
    destructor Destroy; override;
    procedure Draw; //Draw model
    property Objects[ID: Cardinal]: TPriModelObject read GetObject; default; //Model objects
    property Materials[ID: Byte]: TPriModelMaterial read GetMaterial; //Model materials
    property OnDrawObject: TOnDrawObject read FOnDrawObject write FOnDrawObject; //Draw object event
  end;

implementation

uses VSETexMan;

const
  SPriModelObjectCreateCannotLoadMo = 'PriModelObject.Create: Cannot load model object';
  SPriModelObjectReadChunkPrimitive = 'PriModelObject.ReadChunk: primitive %d generation failed at %d';
  SPriModelObjectReadChunkUnknownPr = 'PriModelObject.ReadChunk: unknown primitive chunk %d at %d';
  SPriModelReadChunkUnknownChunkAt = 'PriModel.ReadChunk: unknown chunk %d at %d';
  SPriModelCreateCannotLoadModel = 'PriModel.Create(%s): Cannot load model';
  SPriModelCreateCannotOpenFile = 'PriModel.Create(%s): Cannot open file';
  MatrixE: TMatrix4D=((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

function SelI(Expr: Boolean; ValTrue, ValFalse: Integer): Integer;
begin
  if Expr then Result:=ValTrue else Result:=ValFalse;
end;

{TTransform}

constructor TTransform.Create;
begin
  inherited Create;
  FDefMatrix:=MatrixE;
  Reset;
end;

constructor TTransform.Create(const Transform: TPMTransform);
begin
  inherited Create;
  FMatrix:=MatrixE;
  with Transform do
  begin
    Scale(ScaleX/512, ScaleY/512, ScaleZ/512);
    Rotate(BDegToRad*Yaw, 0, 1, 0);
    Rotate(BDegToRad*Pitch, 1, 0, 0);
    Rotate(BDegToRad*Roll, 0, 0, -1);
    Translate(TranslateX/512, TranslateY/512, TranslateZ/512);
  end;
  SetAsDefault;
end;

procedure TTransform.Assign(const Transform: TTransform);
begin
  FMatrix:=Transform.FMatrix;
end;

procedure TTransform.Apply;
begin
  glMultMatrixf(@FMatrix[0][0]);
end;

procedure TTransform.Reset;
begin
  FMatrix:=FDefMatrix;
end;

procedure TTransform.SetAsDefault;
begin
  FDefMatrix := FMatrix;
end;

procedure TTransform.Translate(X, Y, Z: Single);
var
  TMat: TMatrix4D;
begin
  TMat:=MatrixE;
  TMat[3, 0]:=X;
  TMat[3, 1]:=Y;
  TMat[3, 2]:=Z;
  Mul(TMat);
end;

procedure TTransform.Rotate(Angle, X, Y, Z: Single);
var
  S, C, NC: Single;
  V: TVector3D;
  RMat: TMatrix4D;
begin
  V:=VectorSetValue(X, Y, Z);
  VectorNormalize(V);
  S:=Sin(Angle);
  C:=Cos(Angle);
  NC:=1-C;
  RMat:=MatrixE;
  with V do
  begin
    RMat[0, 0]:=X*X*NC+C;
    RMat[1, 0]:=X*Y*NC-Z*S;
    RMat[2, 0]:=X*Z*NC+Y*S;
    RMat[0, 1]:=X*Y*NC+Z*S;
    RMat[1, 1]:=Y*Y*NC+C;
    RMat[2, 1]:=Y*Z*NC-X*S;
    RMat[0, 2]:=X*Z*NC-Y*S;
    RMat[1, 2]:=Y*Z*NC+X*S;
    RMat[2, 2]:=Z*Z*NC+C;
  end;
  Mul(RMat);
end;

procedure TTransform.Scale(X, Y, Z: Single);
var
  SMat: TMatrix4D;
begin
  SMat:=MatrixE;
  SMat[0, 0]:=X;
  SMat[1, 1]:=Y;
  SMat[2, 2]:=Z;
  Mul(SMat);
end;

procedure TTransform.Mul(Transform: TTransform);
begin
  Mul(Transform.FMatrix);
end;

procedure TTransform.Mul(const Matrix: TMatrix4D);
var
  Tmp: TMatrix4D;
  i, j: Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      Tmp[i, j]:=FMatrix[i, 0]*Matrix[0, j]+FMatrix[i, 1]*Matrix[1, j]+
                 FMatrix[i, 2]*Matrix[2, j]+FMatrix[i, 3]*Matrix[3, j];
  FMatrix:=Tmp;
end;

{TPriModelObject}

constructor TPriModelObject.Create(Model: TPriModel; Data, VertexBuffer: TStream);
var
  Transform: TPMTransform;
  ChunkSize: Word;
  ChunkEnd: Integer;
  IndexBuffer: TMemoryStream;
begin
  inherited Create;
  FModel:=Model;
  ChunkEnd:=Data.Seek(-SizeOf(ChunkSize), soFromCurrent);
  Data.Read(ChunkSize, SizeOf(ChunkSize));
  Inc(ChunkEnd, ChunkSize-1);
  Data.Read(FID, SizeOf(FID));
  Data.Read(Transform, SizeOf(Transform));
  FTransform:=TTransform.Create(Transform);
  Data.Read(FMaterialID, SizeOf(FMaterialID));
  FVisible:=true;
  IndexBuffer:=TMemoryStream.Create;
  try
    while ChunkEnd>Data.Position do
      if not ReadChunk(Data, VertexBuffer, IndexBuffer)
        then raise Exception.Create(SPriModelObjectCreateCannotLoadMo);
    FIndexBuffer:=TArrayBuffer.Create;
    FIndexBuffer.SetData(IndexBuffer.Memory, IndexBuffer.Size);
  finally
    FAN(IndexBuffer);
  end;
end;

destructor TPriModelObject.Destroy;
var
  i: Integer;
begin
  FAN(FIndexBuffer);
  FAN(FTransform);
  for i:=0 to High(FObjects) do FObjects[i].Free;
  Finalize(FObjects);
  inherited Destroy;
end;

procedure TPriModelObject.Draw;
var
  i: Integer;
begin
  if not FVisible then Exit;
  if Assigned(FModel.FOnDrawObject) and not FModel.FOnDrawObject(FModel, Self) then Exit;
  if FMaterialID <> 0 then FModel.Materials[FMaterialID].Apply;
  glPushMatrix;
  FTransform.Apply;
  if FVertLoaded and (FIndexBuffer.Size>0) then
  begin
    FIndexBuffer.Bind(GL_ELEMENT_ARRAY_BUFFER_ARB);
    glDrawElements(GL_TRIANGLES, FIndexBuffer.Size div SizeOf(Word), GL_UNSIGNED_SHORT, FIndexBuffer.Data);
    FIndexBuffer.Unbind;
  end;
  for i:=0 to High(FObjects) do FObjects[i].Draw;
  glPopMatrix;
end;

function TPriModelObject.LastVert(VertexBuffer: TStream): Integer;
begin
  Result:=VertexBuffer.Size div SizeOf(TVertex);
end;

function TPriModelObject.ReadChunk(Data, VertexBuffer, IndexBuffer: TStream): Boolean;
var
  ChunkID, TempByte, FlagsByte: Byte;
  ChunkSize, TempWord: Word;
  FacesBuf: packed array of Byte;
  VertsBuf: packed array of TPMVertex;
  Transform: TPMTransform;
  PrimiParamsBuf: Pointer;
  VA: TVertexArray;
  IA: TFacesArray;
  ChunkEnd, Index, StartVert, i: Integer;
  FlagHasNormals, FlagHasUV: Boolean;
begin
  Result:=false;
  PrimiParamsBuf:=nil;
  ChunkEnd:=Data.Position;
  Data.Read(ChunkID, SizeOf(ChunkID));
  Data.Read(ChunkSize, SizeOf(ChunkSize));
  Inc(ChunkEnd, ChunkSize);
  case ChunkID of
    ChunkModelObject:
      begin
        Index:=Length(FObjects);
        SetLength(FObjects, Index+1);
        FObjects[Index]:=TPriModelObject.Create(FModel, Data, VertexBuffer);
      end;
    ChunkObjectPrimitive:
      try
        Data.Read(TempByte, SizeOf(TempByte)); //Primitive type
        Data.Read(Transform, SizeOf(Transform)); //Transform
        Data.Read(FlagsByte, SizeOf(FlagsByte)); //Flags
        GetMem(PrimiParamsBuf, PrimitiveParamSizes[TempByte]);
        TempWord:=PrimitiveParamSizes[TempByte]-Data.Read(PrimiParamsBuf^, PrimitiveParamSizes[TempByte]); //Primitive params
        case TempByte of
          PrimitiveCube: Index:=GenCube(PrimiParamsBuf, FlagsByte, VA, IA);
          PrimitiveSphere: Index:=GenSphere(PrimiParamsBuf, FlagsByte, VA, IA);
          PrimitiveCone: Index:=GenCone(PrimiParamsBuf, FlagsByte, VA, IA);
          PrimitiveTorus: Index:=GenTorus(PrimiParamsBuf, FlagsByte, VA, IA);
          PrimitiveTube: Index:=GenTube(PrimiParamsBuf, FlagsByte, VA, IA);
          else
            raise Exception.CreateFmt(SPriModelObjectReadChunkUnknownPr, [TempByte, ChunkEnd-ChunkSize]);
        end;
        if Index>0 then
          raise Exception.CreateFmt(SPriModelObjectReadChunkPrimitive, [TempByte, ChunkEnd-ChunkSize])
          else Data.Seek(Index+TempWord, soFromCurrent);
        ApplyTransform(VA, Transform);
        if FlagsByte and PMFInvertNormals <> 0 then
          for i:=0 to High(VA) do
            VectorScale(VA[i].Normal, -1);
        StartVert:=LastVert(VertexBuffer);
        for i:=0 to High(IA) do
        with IA[i] do
        begin
          Inc(Vert1, StartVert);
          Inc(Vert2, StartVert);
          Inc(Vert3, StartVert);
        end;
        VertexBuffer.Write(VA[0], Length(VA)*SizeOf(TVertex));
        IndexBuffer.Write(IA[0], Length(IA)*SizeOf(TFaceW));
        FVertLoaded:=true;
      finally
        Finalize(VA);
        Finalize(IA);
        FreeMem(PrimiParamsBuf);
      end;
    ChunkObjectMesh:
      try
        StartVert:=LastVert(VertexBuffer);
        Data.Read(TempByte, SizeOf(TempByte)); //Vertices count temp
        Data.Read(TempWord, SizeOf(TempWord)); //Faces count
        Data.Read(FlagsByte, SizeOf(FlagsByte)); //Flags
        FlagHasNormals:=FlagsByte and MeshHasNormals <> 0;
        FlagHasUV:=FlagsByte and MeshHasUV <> 0;
        Data.Read(Transform, SizeOf(Transform)); //Transform
        Index:=TempByte+1; //Real vertices count
        SetLength(VertsBuf, Index);
        SetLength(VA, Index);
        SetLength(FacesBuf, Integer(3*TempWord));
        SetLength(IA, TempWord);
        for i:=0 to High(VertsBuf) do
        begin
          Data.Read(VertsBuf[i].X, 3*SizeOf(VertsBuf[0].X));
          if FlagHasNormals then Data.Read(VertsBuf[i].Phi, SizeOf(VertsBuf[0].Phi)+SizeOf(VertsBuf[0].Theta));
          if FlagHasUV then Data.Read(VertsBuf[i].U, 2*SizeOf(VertsBuf[0].U));
        end;
        Data.Read(FacesBuf[0], Length(FacesBuf));
        for i:=0 to Index-1 do
          with VA[i], VertsBuf[i] do
          begin
            Vertex.X:=X/128;
            Vertex.Y:=Y/128;
            Vertex.Z:=Z/128;
            if FlagHasNormals then
              with Normal do
              begin
                X:=cos(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
                Y:=sin(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
                Z:=cos(Theta*BDegToRad/2);
              end
              else VectorClear(Normal);
            if FlagHasUV then
              with TexCoord do
              begin
                X:=U/255;
                Y:=V/255;
              end
              else VectorClear(TexCoord);
          end;
        ApplyTransform(VA, Transform);
        for i:=0 to High(FacesBuf) div 3 do
        begin
          IA[i].Vert1:=StartVert+FacesBuf[3*i];
          IA[i].Vert2:=StartVert+FacesBuf[3*i+1];
          IA[i].Vert3:=StartVert+FacesBuf[3*i+2];
        end;
        if not FlagHasNormals
          then ComputeNormalsTriangles(VA, IA);
        VertexBuffer.Write(VA[0], Length(VA)*SizeOf(TVertex));
        IndexBuffer.Write(IA[0], Length(IA)*SizeOf(TFaceW));
        FVertLoaded:=true;
      finally
        Finalize(VertsBuf);
        Finalize(FacesBuf);
        Finalize(VA);
        Finalize(IA);
      end;
    else
      raise Exception.CreateFmt(SPriModelReadChunkUnknownChunkAt, [ChunkID, ChunkEnd-ChunkSize]);
  end;
  while ChunkEnd>Data.Position do
    if not ReadChunk(Data, VertexBuffer, IndexBuffer) then Exit;
  Result:=ChunkEnd=Data.Position;
end;

function TPriModelObject.GenCube(Params: PPPCube; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;

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
  i, FaceType: Integer;
begin
  Result:=1;
  SetLength(VA, 24);
  SetLength(IA, 12);
  ZeroMemory(@VA[0], SizeOf(VA));
  for i:=0 to 23 do
    with VA[i] do
    begin
      with Vertex do
      begin
        X:=Coord(0, Verts[i])-0.5;
        Y:=Coord(1, Verts[i])-0.5;
        Z:=Coord(2, Verts[i])-0.5;
      end;
      if Flags and PMFTexInfo<>0 then
        with TexCoord do
        begin
          FaceType:=(Verts[i] and $C0) shr 6;
          X:=Params.UV[FaceType].OrigU/255+Coord(3, Verts[i])*Params.UV[FaceType].SizeU/255;
          Y:=Params.UV[FaceType].OrigV/255+Coord(4, Verts[i])*Params.UV[FaceType].SizeV/255
            +Coord(5, Verts[i])*Coord(FaceType, Flags)*Params.UV[FaceType].SizeV/255;
        end;
    end;
  for i:=0 to 5 do
    Quad(IA, 2*i, 4*i, 4*i+1, 4*i+2, 4*i+3);
  ComputeNormalsTriangles(VA, IA);
  if Flags and PMFTexInfo<>0
    then Result:=0
    else Result:=-SizeOf(Params.UV);
end;

function TPriModelObject.GenSphere(Params: PPPSphere; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
var
  Slice, Stack, StackLen, VertBase, Start, Start2: Integer;
  R, H, V, dPhi, dTheta, U0, V0, dU, dV: Single;
  Vert: TVertex;
  Smooth, TexGenUV: Boolean;
begin
  Result:=1;
  with Params^ do
  begin
    Smooth:=Flags and PMFSmooth <> 0;
    TexGenUV:=Flags and PMFTexInfo <> 0;
    dTheta:=StacksSector*pi/(255*Stacks);
    dPhi:=2*SlicesSector*pi/(255*Slices);
    U0:=UV.OrigU/255;
    V0:=UV.OrigV/255;
    dU:=UV.SizeU/(255*Slices);
    dV:=UV.SizeV/(255*Stacks);
    StackLen:=SelI(Smooth, Slices+1, 4*Slices);
    VertBase:=SelI(Smooth, 0, -2*Slices);
    VectorClear(Vert.Normal);
    SetLength(VA, SelI(Smooth, (Slices+1)*(Stacks+1), 4*Slices*Stacks));
    SetLength(IA, Integer(2*Stacks*Slices));
    for Stack:=0 to Stacks do
    begin
      R:=Sin(Stack*dTheta);
      if (Stack=0) or ((Stack=Stacks) and (StacksSector=255)) then R:=0;
      H:=Cos(Stack*dTheta);
      V:=V0+Stack*dV;
      Start:=Stack*StackLen+VertBase;
      Start2:=Start+StackLen;
      for Slice:=0 to Slices do
      begin
        with Vert do
        begin
          with Vertex do
          begin
            X:=R*Cos(Slice*dPhi);
            Y:=H;
            Z:=R*Sin(Slice*dPhi);
          end;
          if Smooth then
          begin
            Normal:=Vertex;
            VectorNormalize(Normal);
          end;
          if TexGenUV then
            with TexCoord do
            begin
              X:=U0+Slice*dU;
              Y:=V;
            end;
        end;
        if Smooth then VA[Start+Slice]:=Vert
        else begin
          if Stack>0 then
          begin
            if Slice>0 then VA[Start+2*Slice-1]:=Vert;
            if Slice<Slices then VA[Start+2*Slice]:=Vert;
          end;
          if Stack<Stacks then
          begin
            if Slice>0 then VA[Start+2*Slices+2*Slice-1]:=Vert;
            if Slice<Slices then VA[Start+2*Slices+2*Slice]:=Vert;
          end;
        end;
        if (Stack<Stacks) and (Slice<Slices) then
          if Smooth
            then Quad(IA, 2*(Stack*Slices+Slice), Start+Slice, Start+Slice+1, Start2+Slice+1, Start2+Slice)
            else Quad(IA, 2*(Stack*Slices+Slice), Start+2*Slices+2*Slice, Start+2*Slices+2*Slice+1, Start2+2*Slice+1, Start2+2*Slice);
      end;
    end;
  end;
  if not Smooth then ComputeNormalsTriangles(VA, IA);
  if TexGenUV
    then Result:=0
    else Result:=-SizeOf(Params^.UV);
end;

function TPriModelObject.GenCone(Params: PPPCone; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
begin
  //to do
end;

function TPriModelObject.GenTorus(Params: PPPTorus; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
begin
  //to do
end;

function TPriModelObject.GenTube(Params: PPPTube; Flags: Byte; var VA: TVertexArray; var IA: TFacesArray): Integer;
begin
  //to do
end;

procedure TPriModelObject.Quad(var IA: TFacesArray; At: Integer; V1, V2, V3, V4: Word);
begin
  with IA[At] do
  begin
    Vert1:=V1;
    Vert2:=V2;
    Vert3:=V3;
  end;
  with IA[At+1] do
  begin
    Vert1:=V1;
    Vert2:=V3;
    Vert3:=V4;
  end;
end;

procedure TPriModelObject.ApplyTransform(var VA: TVertexArray; Transform: TPMTransform);
var
  i: Integer;
  Scale, NScale, Translate: TVector3D;
  Yaw, Pitch, Roll: Single;
begin
  with Scale, Transform do
  begin
    X:=ScaleX/512;
    Y:=ScaleY/512;
    Z:=ScaleZ/512;
  end;
  with NScale, Transform do
  begin
    X:=512/ScaleX;
    Y:=512/ScaleY;
    Z:=512/ScaleZ;
  end;
  with Translate, Transform do
  begin
    X:=TranslateX/512;
    Y:=TranslateY/512;
    Z:=TranslateZ/512;
  end;
  Yaw:=RadToDeg(Transform.Yaw*BDegToRad);
  Pitch:=RadToDeg(Transform.Pitch*BDegToRad);
  Roll:=RadToDeg(Transform.Roll*BDegToRad);
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

function TPriModelObject.GetObject(ID: Cardinal): TPriModelObject;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FObjects) do
    if FObjects[i].ID=ID then
    begin
      Result:=FObjects[i];
      Exit;
    end;
end;

{TPriModelMaterial}

constructor TPriModelMaterial.Create(Model: TPriModel; Data: TStream);
var
  TexNameLen, Shininess: Byte;
  TexName: string;
begin
  inherited Create;
  Data.Read(FID, SizeOf(FID));
  Data.Read(TexNameLen, SizeOf(TexNameLen));
  if TexNameLen>0 then
  begin
    SetLength(TexName, TexNameLen);
    Data.Read(TexName[1], TexNameLen);
    FTexture:=TexMan.GetTex(TexName);
  end;
  Data.Read(FDiffuse, SizeOf(FDiffuse));
  Data.Read(FSpecular, SizeOf(FSpecular));
  Data.Read(FAmbient, SizeOf(FAmbient));
  Data.Read(FEmission, SizeOf(FEmission));
  Data.Read(Shininess, SizeOf(Shininess));
  FShininess:=Shininess/2;
end;

{destructor TPriModelMaterial.Destroy;
begin

  inherited Destroy;
end;}

procedure TPriModelMaterial.Apply;
var
  Color: TVector4f;
begin
  if FTexture<>0
    then TexMan.Bind(FTexture)
    else TexMan.Unbind;
  Color:=gleColorTo4f(FDiffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @Color);
  Color:=gleColorTo4f(FSpecular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Color);
  Color:=gleColorTo4f(FAmbient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @Color);
  Color:=gleColorTo4f(FEmission);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @Color);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, FShininess);
end;

{TPriModel}

constructor TPriModel.Create(const FileName: string);
var
  Data: TStream;
  VertexBuffer: TMemoryStream;
  ChunkID: Byte;
  ChunkSize: Word;
  {$IFDEF PM_DRAWNORMALS}
  i: Integer;
  VA: array of TVertex;
  NA: array of packed record V1, V2: TVector3D end;
  {$ENDIF}
begin
  inherited Create;
  Data:=GetFile(FileName);
  if not Assigned(Data)
    then raise Exception.CreateFmt(SPriModelCreateCannotOpenFile, [FileName]);
  FVertexBuffer:=TArrayBuffer.Create;
  VertexBuffer:=TMemoryStream.Create;
  try
    Data.Read(ChunkID, SizeOf(ChunkID));
    if ChunkID<>ChunkModel then raise Exception.CreateFmt(SPriModelCreateCannotLoadModel, [FileName]);
    Data.Read(ChunkSize, SizeOf(ChunkSize));
    while Data.Position<ChunkSize do
      if not ReadChunk(Data, VertexBuffer)
        then raise Exception.CreateFmt(SPriModelCreateCannotLoadModel, [FileName]);
    if Data.Position<>ChunkSize then raise Exception.CreateFmt(SPriModelCreateCannotLoadModel, [FileName]);
    {$IFDEF PM_DRAWNORMALS}
    SetLength(VA, VertexBuffer.Size div SizeOf(TVertex));
    VertexBuffer.Seek(0, soFromBeginning);
    VertexBuffer.Read(VA[0], VertexBuffer.Size);
    SetLength(NA, Length(VA));
    for i:=0 to High(VA) do
    begin
      NA[i].V1:=VA[i].Vertex;
      NA[i].V2:=VectorAdd(VA[i].Vertex, VA[i].Normal);
    end;
    FNormals:=TArrayBuffer.Create;
    FNormals.SetData(@NA[0], Length(NA)*SizeOf(NA[0]));
    {$ENDIF}
    FVertexBuffer.SetData(VertexBuffer.Memory, VertexBuffer.Size);
  finally
    FAN(VertexBuffer);
    FAN(Data);
    {$IFDEF PM_DRAWNORMALS}
    Finalize(VA);
    Finalize(NA);
    {$ENDIF}
  end;
end;

destructor TPriModel.Destroy;
var
  i: Integer;
begin
  FAN(FVertexBuffer);
  {$IFDEF PM_DRAWNORMALS}FAN(FNormals);{$ENDIF}
  for i:=0 to High(FObjects) do FObjects[i].Free;
  Finalize(FObjects);
  for i:=0 to High(FMaterials) do FMaterials[i].Free;
  Finalize(FMaterials);
  inherited Destroy;
end;

procedure TPriModel.Draw;
var
  i: Integer;
begin
  glPushAttrib(GL_ENABLE_BIT or GL_POLYGON_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT or GL_TEXTURE_BIT);
  glEnable(GL_NORMALIZE);
  FVertexBuffer.Bind(GL_ARRAY_BUFFER_ARB);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer.Data, SizeOf(TVector3D)));
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer.Data, 2*SizeOf(TVector3D)));
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FVertexBuffer.Data);
  {$IFDEF PM_DRAWNORMALS}
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  {$ENDIF}
  for i:=0 to High(FObjects) do FObjects[i].Draw;
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
  FVertexBuffer.Unbind;
  {$IFDEF PM_DRAWNORMALS}
  FNormals.Bind(GL_ARRAY_BUFFER_ARB);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 12, FNormals.Data);
  glDrawArrays(GL_LINES, 0, FNormals.Size div 12);
  glDisableClientState(GL_VERTEX_ARRAY);
  FNormals.Unbind;
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  {$ENDIF}
  glPopAttrib;
end;

function TPriModel.ReadChunk(Data, VertexBuffer: TStream): Boolean;
var
  ChunkID: Byte;
  ChunkSize: Word;
  ChunkEnd, Index: Integer;
begin
  Result:=false;
  ChunkEnd:=Data.Position;
  Data.Read(ChunkID, SizeOf(ChunkID));
  Data.Read(ChunkSize, SizeOf(ChunkSize));
  Inc(ChunkEnd, ChunkSize);
  case ChunkID of
    ChunkModelObject:
      begin
        Index:=Length(FObjects);
        SetLength(FObjects, Index+1);
        FObjects[Index]:=TPriModelObject.Create(Self, Data, VertexBuffer);
      end;
    ChunkMaterial:
      begin
        Index:=Length(FMaterials);
        SetLength(FMaterials, Index+1);
        FMaterials[Index]:=TPriModelMaterial.Create(Self, Data);
      end;
    else
      raise Exception.CreateFmt(SPriModelReadChunkUnknownChunkAt, [ChunkID, ChunkEnd-ChunkSize]);
  end;
  while ChunkEnd>Data.Position do
    if not ReadChunk(Data, VertexBuffer) then Exit;
  Result:=ChunkEnd=Data.Position;
end;

function TPriModel.GetObject(ID: Cardinal): TPriModelObject;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FObjects) do
    if FObjects[i].ID=ID then
    begin
      Result:=FObjects[i];
      Exit;
    end;
end;

function TPriModel.GetMaterial(ID: Byte): TPriModelMaterial;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FMaterials) do
    if FMaterials[i].ID=ID then
    begin
      Result:=FMaterials[i];
      Exit;
    end;
end;

end.
