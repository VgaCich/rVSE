program VSEWater;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions, VSEOpenGLExt,
  VSECore, VSETexMan, VSERender2D, VSEShaders, VSEMemPak, VSELog, Scene;

type
  TStateMain=class(TGameState)
  private
    FScene: TScene;
    FPos, FAngle: TVector3D;
    FWaterShader: TShader;
    FUWaterTex0, FUWaterTex1, FUWaterDot3Tex, FUWaterPhase: TShaderUniform;
    FWaterRT: Cardinal;
    FWaterTex: array[0..1] of Cardinal;
    FWaterPhase: Single;
    FFont: Cardinal;
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
  end;

{$R MemPak.res}

const
  TEX_SIZE=512;

constructor TStateMain.Create;
var
  Data: TStream;
  Temp: array[0..TEX_SIZE*TEX_SIZE*3] of Byte;
begin
  inherited Create;
  FPos:=VectorSetValue(0, 5, 0);
  FAngle:=VectorSetValue(0);
  FWaterPhase:=0;
  FScene.Load('scene');
  FWaterTex[0]:=TexMan.AddTexture('water0', @Temp, TEX_SIZE, TEX_SIZE, GL_RGB8, GL_RGB, true, false);
  FWaterTex[1]:=TexMan.AddTexture('water1', @Temp, TEX_SIZE, TEX_SIZE, GL_RGB8, GL_RGB, true, false);
  LoadTex('water_dot3.jpg', false, false);
  FWaterRT:=TexMan.InitRTT(TEX_SIZE, TEX_SIZE);
  FWaterShader:=TShader.Create;
  Data:=GetFile('Water.shd');
  if Assigned(Data) then
  try
    FWaterShader.Load(Data);
    FWaterShader.Link;
    if not FWaterShader.Valid
      then Log(llError, 'StateMain.Create: WaterShader is not valid');
    LogMultiline(llInfo, 'WaterShader log:'#13+FWaterShader.InfoLog);
  finally
    FAN(Data);
  end;
  FUWaterTex0:=FWaterShader.GetUniform('tex0');
  FUWaterTex1:=FWaterShader.GetUniform('tex1');
  FUWaterDot3Tex:=FWaterShader.GetUniform('dot3');
  FUWaterPhase:=FWaterShader.GetUniform('phase');
  FFont:=Render2D.CreateFont('Courier New', 10, true);
end;

destructor TStateMain.Destroy;
begin
  FWaterShader.Free;
  inherited Destroy;
end;

procedure TStateMain.Draw;
const
  Plane: array [0..1, 0..3] of Double=((0, -1, 0, 0), (0, 1, 0, 0));
  WaterVert: array [0..3] of TVector3D=(
    (X: -25; Z: -25), (X: 25; Z: -25), (X: 25; Z: 25), (X: -25; Z: 25));
var
  i: Integer;
  FPS: string;
begin
  glePerspectiveMatrix2(75.0, Core.ResolutionX, Core.ResolutionY, 0.4, 128);
  glEnable(GL_DEPTH_TEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glPushMatrix;
  glRotatef(FAngle.Z, 0, 0, 1);
  glRotatef(FAngle.X, 1, 0, 0);
  glRotatef(FAngle.Y, 0, 1, 0);
  glTranslatef(-FPos.X, -FPos.Y, -FPos.Z);
  glPushMatrix;
  for i:=0 to 1 do
  begin
    TexMan.RTTBegin(FWaterRT, FWaterTex[i]);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glScalef(1, -1, 1);
    FScene.RenderSkyBox(VectorSetValue(FPos.X, FPos.Y*(i*2-1), FPos.Z));
    glEnable(GL_CLIP_PLANE0);
    glClipPlane(GL_CLIP_PLANE0, @Plane[i xor 1]);
    FScene.Render;
    glDisable(GL_CLIP_PLANE0);
    TexMan.RTTEnd(FWaterRT);
  end;
  glPopMatrix;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FScene.RenderSkyBox(FPos);
  FScene.Render;
  TexMan.Bind(TexMan.GetTex('water_dot3'), 0);
  TexMan.Bind(FWaterTex[1], 2);
  TexMan.Bind(FWaterTex[0], 1);
  FWaterShader.Enabled:=true;
  FUWaterTex0.Value(1);
  FUWaterTex1.Value(2);
  FUWaterDot3Tex.Value(0);
  FUWaterPhase.Value(FWaterPhase);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
    glVertex3fv(@WaterVert[0]);
    glVertex3fv(@WaterVert[1]);
    glVertex3fv(@WaterVert[2]);
    glVertex3fv(@WaterVert[3]);
  glEnd;
  FWaterShader.Enabled:=false;
  TexMan.Unbind(2);
  TexMan.Unbind(1);
  TexMan.Unbind(0);
  glPopMatrix;
  Render2D.Enter;
  FPS:='FPS: '+IntToStr(Core.FPS);
  Render2D.TextOut(FFont, Render2D.VSWidth-Render2D.TextWidth(FFont, FPS), 0, FPS);
  Render2D.Leave;
end;

procedure TStateMain.Update;
var
  Speed: TVector3D;
  S, C: Single;
begin
  Speed:=VectorSetValue(0);
  if Core.KeyPressed[ord('W')] then Speed:=VectorAdd(Speed, VectorSetValue(0, 0, -1));
  if Core.KeyPressed[ord('S')] then Speed:=VectorAdd(Speed, VectorSetValue(0, 0, 1));
  if Core.KeyPressed[ord('A')] then Speed:=VectorAdd(Speed, VectorSetValue(-1, 0, 0));
  if Core.KeyPressed[ord('D')] then Speed:=VectorAdd(Speed, VectorSetValue(1, 0, 0));
  VectorNormalize(Speed);
  VectorScale(Speed, 0.1);
  S:=sin(DegToRad(FAngle.Y));
  C:=cos(DegToRad(FAngle.Y));
  with Speed do
    FPos:=VectorAdd(FPos, VectorSetValue(X*C-Z*S, 0, X*S+Z*C));
  FWaterPhase:=FWaterPhase+0.0007;
end;

function TStateMain.Activate: Cardinal;
begin
  Result:=10;
  Core.MouseCapture:=true;
end;

procedure TStateMain.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
const
  MSens=0.5;
begin
  if Event=meMove then
  begin
    FAngle.Y:=FAngle.Y+X*MSens;
    FAngle.X:=FAngle.X+Y*MSens;
    if FAngle.X<-90 then FAngle.X:=-90;
    if FAngle.X>90 then FAngle.X:=90;
  end;
end;

function TStateMain.GetName: string;
begin
  Result:='Main';
end;

procedure InitStates;
begin
  Core.SwitchState(Core.AddState(TStateMain.Create));
end;

begin
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:='rVSE Water';
  InitSettings.Version:='1.0';
  InitSettings.InitStates:=InitStates;
  VSEStart;
end.
