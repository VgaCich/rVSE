program VSEWater;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions, VSEOpenGLExt,
  VSECore, VSEMemPak, VSEConsole, VSEConsoleInterface, VSETexMan, VSERender2D,
  VSEShaders, VSELog, Scene;

type
  TStateMain=class(TGameState)
  private
    FScene: TScene;
    FPos, FAngle: TVector3D;
    FShader, FWaterShader: TShader;
    FWaterRT: Cardinal;
    FWaterTex: array[0..1] of Cardinal;
    FFont: Cardinal;
    FDebugInfo: Boolean;
    function LoadShaderHandler(Sender: TObject; Args: array of const): Boolean;
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    function MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean; override;
  end;

{$R MemPak.res}

const
  TEX_SIZE=512;

constructor TStateMain.Create;
var
  Temp: array[0..TEX_SIZE*TEX_SIZE*3] of Byte;
begin
  inherited Create;
  FPos:=Vector3D(-12, 5, -20);
  FAngle:=Vector3D(0, 160, 0);
  FScene.Load('scene');
  FWaterTex[0]:=TexMan.AddTexture('water0', @Temp, TEX_SIZE, TEX_SIZE, GL_RGB8, GL_RGB, true, false);
  FWaterTex[1]:=TexMan.AddTexture('water1', @Temp, TEX_SIZE, TEX_SIZE, GL_RGB8, GL_RGB, true, false);
  LoadTex('water_dot3.jpg', false, false);
  FWaterRT:=TexMan.InitRTT(TEX_SIZE, TEX_SIZE);
  FFont:=Render2D.CreateFont('Courier New', 10, true);
  Console['loadshader target=emain:water ?shader=s']:=LoadShaderHandler;
  Console['debuginfo ?val=eoff:on']:=Console.GetConVarHandler(FDebugInfo, cvBool);
end;

destructor TStateMain.Destroy;
begin
  FShader.Free;
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
  glMatrixMode(GL_PROJECTION);
  glRotatef(FAngle.Z, 0, 0, 1);
  glRotatef(FAngle.X, 1, 0, 0);
  glRotatef(FAngle.Y, 0, 1, 0);
  glTranslatef(-FPos.X, -FPos.Y, -FPos.Z);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FScene.RenderSkyBox(FPos);
  if Assigned(FShader) and FShader.Valid then
  begin
    FShader.Enabled:=true;
    FShader['eye'].Value(FPos.X, FPos.Y, FPos.Z);
    FShader['tex'].Value(0);
  end;
  FScene.Render;
  if Assigned(FShader) and FShader.Valid then
    FShader.Enabled:=false;
  if Assigned(FWaterShader) and FWaterShader.Valid then
  begin
    glPushMatrix;
    for i:=0 to 1 do
    begin
      TexMan.RTTBegin(FWaterRT, FWaterTex[i]);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glMatrixMode(GL_PROJECTION);
      glScalef(1, -1, 1);
      glMatrixMode(GL_MODELVIEW);
      FScene.RenderSkyBox(Vector3D(FPos.X, FPos.Y*(i*2-1), FPos.Z));
      glEnable(GL_CLIP_PLANE0);
      glClipPlane(GL_CLIP_PLANE0, @Plane[i xor 1]);
      if Assigned(FShader) and FShader.Valid then
      begin
        FShader.Enabled:=true;
        FShader['eye'].Value(FPos.X, FPos.Y, FPos.Z);
        FShader['tex'].Value(0);
      end;
      FScene.Render;
      if Assigned(FShader) and FShader.Valid then
        FShader.Enabled:=false;
      glDisable(GL_CLIP_PLANE0);
      TexMan.RTTEnd(FWaterRT);
    end;
    glPopMatrix;
    TexMan.Bind(TexMan.GetTex('water_dot3'), 0);
    TexMan.Bind(FWaterTex[1], 2);
    TexMan.Bind(FWaterTex[0], 1);
    FWaterShader.Enabled:=true;
    FWaterShader['dot3'].Value(0);
    FWaterShader['tex0'].Value(1);
    FWaterShader['tex1'].Value(2);
    FWaterShader['phase'].Value(0.00005*(Core.Time mod 20000));
    FWaterShader['eye'].Value(FPos.X, FPos.Y, FPos.Z);
    glColor3f(1, 1, 1);
    glBegin(GL_QUADS);
      glNormal3f(0, 1, 0);
      glVertex3fv(@WaterVert[0]);
      glVertex3fv(@WaterVert[1]);
      glVertex3fv(@WaterVert[2]);
      glVertex3fv(@WaterVert[3]);
    glEnd;
    FWaterShader.Enabled:=false;
    TexMan.Unbind(2);
    TexMan.Unbind(1);
    TexMan.Unbind(0);
  end;
  Render2D.Enter;
  FPS:='FPS: '+IntToStr(Core.FPS);
  with Render2D do
    TextOut(FFont, Floor(VSBounds.Right)-TextWidth(FFont, FPS), Ceil(VSBounds.Top), FPS);
  if FDebugInfo then
    with Render2D do
    begin
      TextOut(FFont, Ceil(VSBounds.Left), Ceil(VSBounds.Top), Format('Pos: %s %s %s', [FloatToStr2(FPos.X, 4, 2), FloatToStr2(FPos.Y, 4, 2), FloatToStr2(FPos.Z, 4, 2)]));
      TextOut(FFont, Ceil(VSBounds.Left), Ceil(VSBounds.Top)+20, Format('Angle: %s %s %s', [FloatToStr2(FAngle.X, 4, 2), FloatToStr2(FAngle.Y, 4, 2), FloatToStr2(FAngle.Z, 4, 2)]));
    end;
  Render2D.Leave;
end;

procedure TStateMain.Update;
var
  Speed: TVector3D;
  S, C: Single;
begin
  Speed:=Vector3D(0);
  if Core.KeyPressed[ord('W')] then Speed:=VectorAdd(Speed, Vector3D(0, 0, -1));
  if Core.KeyPressed[ord('S')] then Speed:=VectorAdd(Speed, Vector3D(0, 0, 1));
  if Core.KeyPressed[ord('A')] then Speed:=VectorAdd(Speed, Vector3D(-1, 0, 0));
  if Core.KeyPressed[ord('D')] then Speed:=VectorAdd(Speed, Vector3D(1, 0, 0));
  VectorNormalize(Speed);
  VectorScale(Speed, 0.1);
  S:=sin(DegToRad(FAngle.Y));
  C:=cos(DegToRad(FAngle.Y));
  with Speed do
    FPos:=VectorAdd(FPos, Vector3D(X*C-Z*S, 0, X*S+Z*C));
end;

function TStateMain.Activate: Cardinal;
begin
  Result:=10;
  Core.MouseCapture:=true;
end;

function TStateMain.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean;
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

function TStateMain.LoadShaderHandler(Sender: TObject; Args: array of const): Boolean;
var
  Data: TStream;
  Shader: TShader;

  procedure SetShader(var Target);
  begin
    FAN(Target);
    Pointer(Target):=Pointer(Shader);
  end;

begin
  Shader:=nil;
Result:=false;
  if Length(args) > 2 then
  begin
    Data:=Core.GetFile(string(Args[2].VAnsiString));
    if Assigned(Data) then
    try
      Log(llInfo, 'Loading shader '+string(Args[2].VAnsiString));
      Shader:=TShader.Create;
      Shader.Load(Data);
      Shader.Link;
      if not Shader.Valid
        then Log(llError, 'Shader is not valid');
      LogMultiline(llInfo, 'Shader log:'#13+Shader.InfoLog);
      Result:=Shader.Valid;
    finally
      FAN(Data);
    end;
  end
  else
    Result:=true;
  if Result then
    case Args[1].VInteger of
      0: SetShader(FShader);
      1: SetShader(FWaterShader);
    end
  else
    FAN(Shader);
end;

procedure InitStates;
begin
  Core.SwitchState(Core.AddState(TStateMain.Create));
end;

begin
  Randomize;
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:='rVSE Water';
  InitSettings.Version:='1.0';
  InitSettings.InitStates:=InitStates;
  VSEStart;
end.
