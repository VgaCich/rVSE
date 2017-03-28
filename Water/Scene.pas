unit scene;

interface

uses
  Windows, AvL, avlUtils, avlVectors, OpenGL, oglExtensions, VSEOpenGLExt,
  VSECore, VSEMemPak, VSETexMan, VSEImageCodec;

type
  TFace = array [0..2] of WORD;

  TSceneObj = object
    Texture  : Cardinal;
    V_Count  : Integer;
    F_Count  : Integer;
    T_Count  : Integer;
    Vertex   : array of TVector3D;
    Normal   : array of TVector3D;
    Face     : array of TFace;
    TexCoord : array of TVector2D;
    TexFace  : array of TFace;
    procedure Init;
    procedure Render;
  end;

  TScene = object
    O_Count : Integer;
    Objects : array of TSceneObj;
    Sky     : array [0..4] of Cardinal;
    function Load(const SceneName: string): Boolean;
    procedure RenderSkyBox(Pos: TVector3D);
    procedure Render;
  end;

function LoadTex(FileName: string; Clamp: Boolean=false; MipMap: Boolean=true): Cardinal;

implementation

function LoadTex(FileName: string; Clamp: Boolean=false; MipMap: Boolean=true): Cardinal;
var
  Name: string;
  Data: TStream;
  Image: TImage;
begin
  Name:=ChangeFileExt(ExtractFileName(FileName), '');
  Result:=TexMan.GetTex(Name, true);
  if Result<>0 then Exit;
  Data:=GetFile(FileName);
  try
    Image:=TImage.Create;
    try
      Image.Load(Data);
      Result:=TexMan.AddTexture(Name, Image, Clamp, MipMap);
    finally
      FAN(Image);
    end;
  finally
    FAN(Data);
  end;
end;

procedure LightColor(ID: Integer; R, G, B: Single);
var
  c : array [0..3] of Single;
begin
  c[0] := R;
  c[1] := G;
  c[2] := B;
  c[3] := 1;
  glLightfv(ID, GL_DIFFUSE,  @c);
  glLightfv(ID, GL_SPECULAR, @c);
  c[0] := c[0] / 5;
  c[1] := c[1] / 5;
  c[2] := c[2] / 5;
  glLightfv(ID, GL_AMBIENT, @c);
end;

procedure LightPos(ID: Integer; X, Y, Z: Single);
var
  p : array [0..3] of Single;
begin
  p[0] := X;
  p[1] := Y;
  p[2] := Z;
  p[3] := 1;
  glLightfv(ID, GL_POSITION, @p);
end;

procedure TSceneObj.Init;
var
  i      : Integer;
  v1, v2 : TVector3D;
begin
  SetLength(Normal, F_Count);
  for i := 0 to F_Count - 1 do
  begin
    v1 := VectorSub(Vertex[Face[i][2]], Vertex[Face[i][1]]);
    v2 := VectorSub(Vertex[Face[i][0]], Vertex[Face[i][1]]);
    Normal[i] := VectorCrossProduct(v1, v2);
    VectorNormalize(Normal[i]);
  end;
  for i:=0 to T_Count-1 do TexCoord[i].Y:=1-TexCoord[i].Y;
end;

procedure TSceneObj.Render;

  procedure SetColor(Target: GLenum; Color: TColor);
  var
    C: TVector4f;
  begin
    C:=gleColorTo4f(Color or $FF000000);
    glMaterialfv(GL_FRONT_AND_BACK, Target, @C);
  end;

var
  i : Integer;
begin
  TexMan.Bind(Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  SetColor(GL_DIFFUSE, clWhite);
  SetColor(GL_SPECULAR, clGray);
  SetColor(GL_AMBIENT, clWhite);
  SetColor(GL_EMISSION, clBlack);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 10.0);
  glBegin(GL_TRIANGLES);
  for i := 0 to F_Count - 1 do
  begin
    glNormal3fv(@Normal[i]);
    glTexCoord2fv(@TexCoord[TexFace[i][0]]); glVertex3fv(@Vertex[Face[i][0]]);
    glTexCoord2fv(@TexCoord[TexFace[i][1]]); glVertex3fv(@Vertex[Face[i][1]]);
    glTexCoord2fv(@TexCoord[TexFace[i][2]]); glVertex3fv(@Vertex[Face[i][2]]);
  end;
  glEnd;
end;

function TScene.Load(const SceneName: string): Boolean;
var
  i : Integer;
  //F    : File;
  F: TStream;

  function ReadStr: string;
  var
    i : Integer;
  begin
    F.Read(i, SizeOf(i));
    SetLength(Result, i);
    F.Read(Result[1], i);
  end;

begin
  F:=GetFile(SceneName+'.exs');
  try
    F.Read(O_Count, SizeOf(O_Count));
    SetLength(Objects, O_Count);
    for i := 0 to O_Count - 1 do
      with Objects[i] do
      begin
        ReadStr;
        Texture := LoadTex(ReadStr, False, True); // Repeat texture with mipmap levels
        F.Read(V_Count, SizeOf(V_Count));
        F.Read(F_Count, SizeOf(F_Count));
        F.Read(T_Count, SizeOf(T_Count));
        SetLength(Vertex, V_Count);
        SetLength(Face, F_Count);
        SetLength(TexCoord, T_Count);
        SetLength(TexFace, F_Count);
        F.Read(Vertex[0], V_Count * SizeOf(TVector3D));
        F.Read(Face[0], F_Count * SizeOf(TFace));
        F.Read(TexCoord[0], T_Count * SizeOf(TVector2D));
        F.Read(TexFace[0], F_Count * SizeOf(TFace));
        Init;
      end;
    FAN(F);
    Result := True;
  except
    FAN(F);
    Result := False;
  end;

  for i := 0 to 4 do
    Sky[i] := LoadTex('sky_' + IntToStr(i + 1) + '.jpg', True);
end;

procedure TScene.RenderSkyBox(Pos: TVector3D);
begin
  glDisable(GL_DEPTH_TEST);
  glColor4f(1, 1, 1, 1);
  glPushMatrix;
  with Pos do
    glTranslatef(X, Y, Z);
  TexMan.Bind(Sky[2]); // front
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, -1);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, -1);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, -1);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, -1);
  glEnd;
  TexMan.Bind(Sky[3]); // left
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(-1, -1,  1);
    glTexCoord2f(0, 1); glVertex3f(-1,  1,  1);
    glTexCoord2f(1, 1); glVertex3f(-1,  1, -1);
    glTexCoord2f(1, 0); glVertex3f(-1, -1, -1);
  glEnd;
  TexMan.Bind(Sky[0]); // back
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f( 1, -1,  1);
    glTexCoord2f(0, 1); glVertex3f( 1,  1,  1);
    glTexCoord2f(1, 1); glVertex3f(-1,  1,  1);
    glTexCoord2f(1, 0); glVertex3f(-1, -1,  1);
  glEnd;
  TexMan.Bind(Sky[1]); // right
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f( 1, -1, -1);
    glTexCoord2f(0, 1); glVertex3f( 1,  1, -1);
    glTexCoord2f(1, 1); glVertex3f( 1,  1,  1);
    glTexCoord2f(1, 0); glVertex3f( 1, -1,  1);
  glEnd;
  TexMan.Bind(Sky[4]); // top
  glBegin(GL_QUADS);
    glTexCoord2f(1, 0); glVertex3f(-1,  1,  1);
    glTexCoord2f(0, 0); glVertex3f( 1,  1,  1);
    glTexCoord2f(0, 1); glVertex3f( 1,  1, -1);
    glTexCoord2f(1, 1); glVertex3f(-1,  1, -1);
  glEnd;
  glPopMatrix;
  glEnable(GL_DEPTH_TEST);
end;

procedure TScene.Render;
var
  i : Integer;
begin
  glDisable(GL_ALPHA_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  LightColor(GL_LIGHT0, 1, 1, 1);
  LightPos(GL_LIGHT0, 1000, 1500, 577);
  glColor3f(1, 1, 1);
  for i := 0 to O_Count - 1 do
    Objects[i].Render;
  TexMan.Unbind;
  glDisable(GL_LIGHTING);
end;

end.
