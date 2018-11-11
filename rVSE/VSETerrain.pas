unit VSETerrain;

interface

uses
  AvL, avlUtils, avlMath, OpenGL, VSEOpenGLExt, oglExtensions, avlVectors,
  VSEArrayBuffer, VSETexMan, SynTex, VSEVertexArrayUtils;

type
  TTerrain=class(TObject)
  private
    function  GetHeightMap(X, Y: Word): Byte;
  protected
    FTerrainData: PByteArray;
    FVertexBuffer, FIndexBuffer: TArrayBuffer;
    //Draw normals:
    //FNormalsBuffer: TArrayBuffer;
    FTexture: Cardinal;
    FWidth, FHeight: Word;
    FHScale, FVScale: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const Tex: TSynTexRegister; TexSize: Integer; HScale, VScale: Single); //Load terrain from SynTex texture; Tex: SynTex texture; TexSize: texture size; HScale: horizontal scale; VScale: vertical scale
    procedure Draw; //Draw terrain
    function  Altitude(X, Y: Single): Single; //Terrain altitude at (X, Y)
    procedure Morph(X, Y, Width, Height: Word; Data: array of SmallInt); //Modify terrain; X, Y: coordinales of modified zone; Width, Height: size of modified zone; Data: height delta array
    property Width: Word read FWidth; //Terrain width
    property Height: Word read FHeight; //Terain height
    property HeightMap[X, Y: Word]: Byte read GetHeightMap; //heightmap data
    property Texture: Cardinal read FTexture write FTexture; //Terrain texture
  end;

implementation

uses
  VSECore;

constructor TTerrain.Create;
begin
  inherited Create;
  FVertexBuffer:=TArrayBuffer.Create;
  FIndexBuffer:=TArrayBuffer.Create;
end;

destructor TTerrain.Destroy;
begin
  if FTerrainData<>nil then FreeMem(FTerrainData, FWidth*FHeight);
  FAN(FVertexBuffer);
  FAN(FIndexBuffer);
  inherited Destroy;
end;

procedure TTerrain.Load(const Tex: TSynTexRegister; TexSize: Integer; HScale, VScale: Single);
var
  i, j: Cardinal;
  VertexBuffer: array of TVertex;
  IndexBuffer: array of Integer;
  //Draw normals:
  //NormBuf: array of TVector3D;
  IndexLen: Cardinal;
begin
  FHScale:=HScale;
  FVScale:=VScale;
  GetMem(FTerrainData,  TexSize*TexSize);
  for i:=0 to TexSize*TexSize-1 do
    FTerrainData[i]:=Tex[i].R;
  FWidth:=TexSize;
  FHeight:=TexSize;
  try
    SetLength(VertexBuffer, FWidth*FHeight);
    for i:=0 to FWidth*FHeight-1 do
    begin
      VertexBuffer[i].Vertex.X:=FHScale*(i mod FWidth);
      VertexBuffer[i].Vertex.Z:=FHScale*(i div FWidth);
      VertexBuffer[i].Vertex.Y:=FVScale*FTerrainData[i];
    end;
    IndexLen:=2*(FWidth*FHeight-FWidth+FHeight-1);
    SetLength(IndexBuffer, IndexLen);
    for j:=0 to FHeight-1 do
    begin
      for i:=0 to FWidth-1 do
      begin
        if j<FHeight-1 then
        begin
          IndexBuffer[2*(j*(FWidth+1)+i)]:=j*FWidth+i;
          IndexBuffer[2*(j*(FWidth+1)+i)+1]:=(j+1)*FWidth+i;
        end;
        VectorClear(VertexBuffer[j*FWidth+i].Normal);
        VertexBuffer[j*FWidth+i].TexCoord.X:=i/32;
        VertexBuffer[j*FWidth+i].TexCoord.Y:=j/32;
      end;
      if j<FHeight-1 then
      begin
        IndexBuffer[2*(j*(FWidth+1)+i)]:=(j+1)*FWidth+i-1;
        IndexBuffer[2*(j*(FWidth+1)+i)+1]:=(j+1)*FWidth;
      end;
    end;
    ComputeNormalsTrianglesStrip(VertexBuffer, IndexBuffer);
    //Draw normals:
    {SetLength(NormBuf, Length(VertexBuffer)*2);
    for i:=0 to High(VertexBuffer) do
    begin
      NormBuf[2*i]:=VertexBuffer[i].Vertex;
      NormBuf[2*i+1]:=VectorAdd(VertexBuffer[i].Vertex, VertexBuffer[i].Normal);
    end;
    FNormalsBuffer:=TArrayBuffer.Create;
    FNormalsBuffer.SetData(@NormBuf[0], SizeOf(TVector3D)*Length(NormBuf));
    Finalize(NormBuf);}
    FVertexBuffer.SetData(@VertexBuffer[0], SizeOf(TVertex)*FWidth*FHeight);
    FIndexBuffer.SetData(@IndexBuffer[0], IndexLen*SizeOf(Integer));
  finally
    Finalize(VertexBuffer);
    Finalize(IndexBuffer);
  end;
end;

procedure TTerrain.Draw;

  procedure SetColor(Target: GLenum; Color: TColor);
  var
    C: TVector4D;
  begin
    C:=gleColorTo4f(Color or $FF000000);
    glMaterialfv(GL_FRONT_AND_BACK, Target, @C);
  end;

begin
  glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT);
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  TexMan.Bind(FTexture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  SetColor(GL_DIFFUSE, clWhite);
  SetColor(GL_SPECULAR, clGray);
  SetColor(GL_AMBIENT, $404040);
  SetColor(GL_EMISSION, clBlack);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 10.0);
  FVertexBuffer.Bind(GL_ARRAY_BUFFER_ARB);
  FIndexBuffer.Bind(GL_ELEMENT_ARRAY_BUFFER_ARB);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer.Data, SizeOf(TVector3D)));
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer.Data, 2*SizeOf(TVector3D)));
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FVertexBuffer.Data);
  glDrawElements(GL_TRIANGLE_STRIP, FIndexBuffer.Size div SizeOf(Integer), GL_UNSIGNED_INT, FIndexBuffer.Data);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
  FVertexBuffer.Unbind;
  FIndexBuffer.Unbind;
  glPopAttrib;
end;

function TTerrain.Altitude(X, Y: Single): Single;
var
  X0, X1, Y0, Y1: Integer;
  H0, H1, H2, H3, AH1, AH2, WX, WY: Single;
begin
  if X<0 then X:=0;
  if Y<0 then Y:=0;
  X:=X/FHScale;
  Y:=Y/FHScale;
  X0:=Floor(X);
  Y0:=Floor(Y);
  X1:=X0+1;
  Y1:=Y0+1;
  WX:=X-X0;
  WY:=Y-Y0;
  H0:=HeightMap[X0, Y0];
  H1:=HeightMap[X0, Y1];
  H2:=HeightMap[X1, Y0];
  H3:=HeightMap[X1, Y1];
  AH1:=(1-WY)*H0+WY*H1;
  AH2:=(1-WY)*H2+WY*H3;
  Result:=((1-WX)*AH1+WX*AH2)*FVScale;
end;

procedure TTerrain.Morph(X, Y, Width, Height: Word; Data: array of SmallInt);
begin

end;

function TTerrain.GetHeightMap(X, Y: Word): Byte;
begin
  if X>=FWidth then X:=FWidth-1;
  if Y>=FHeight then Y:=FHeight-1;
  Result:=FTerrainData[Y*FWidth+X];
end;

end.
