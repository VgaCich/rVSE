unit VSEOpenGLExt;

interface

uses Windows, OpenGL, oglExtensions, AvL, avlVectors{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TResolution=record //Resolution info
    Width, Height: Cardinal;
    RefreshRate: Cardinal;
    RefreshRates: array of Cardinal;
  end;
  TResolutions=array of TResolution;

function  gleGoFullscreen(Width, Height, Refresh, Depth: Integer): Boolean; //used internally
procedure gleGoBack; //used internally
function  gleSetPix(DC: HDC; Depth: Cardinal): HGLRC; //used internally
procedure gleSetGL; //Set default OpenGL state
procedure gleResizeWnd(Width, Height: Integer); //used internally
procedure glePushMatrix; //Push Projection & ModelView matrices
procedure glePopMatrix; //Pop Projection & ModelView matrices
procedure glePerspectiveMatrix(FOV: Single; Width, Height: Integer); //Set perspective projection; FOV: Field of Vision; Width, Height: Viewport size
procedure glePerspectiveMatrix2(FOV: Single; Width, Height: Integer; ZNear, ZFar: Single); //Set perspective projection; ZNear, ZFar: Z cutting planes
procedure gleOrthoMatrix(Width, Height: Integer); //Set orthogonal projection; Width, Height: projection dimensions
procedure gleOrthoMatrix2(Left, Top, Right, Bottom: Double); //Set orthogonal projection; Left, Top, Right, Bottom: projection dimensions
function  gleError(GLError: Cardinal): string; //Convert OpenGL error code to text
procedure gleColor(Color: TColor); //Set current OpenGL color
function  gleColorTo4f(Color: TColor): TVector4D; //Convert GDI color to OpenGL color
function  gleGetResolutions: TResolutions; //List available screen resolutions, RefreshRate not used
function  gleGetCurrentResolution: TResolution; //Returns current resolution, RefreshRates not used
function  gleScreenTo3D(X, Y: Integer; GetDepth: Boolean=false): TVector3D; //Translates screen coordinates to 3D coordinates; GetDepth: fetch screen depth from framebuffer
function  gle3DToScreen(X, Y, Z: Double): TPoint; //Translates 3D coordinates to screen coordinates

implementation

const
  ENUM_CURRENT_SETTINGS=LongWord(-1);
  ENUM_REGISTRY_SETTINGS=LongWord(-2);

function gleGoFullscreen(Width, Height, Refresh, Depth: Integer): Boolean;
var
  DM: DevMode;
begin
  {$IFDEF VSE_LOG}LogF(llInfo, 'Entering fullscreen. Resolution %dx%d@%d', [Width, Height, Refresh]);{$ENDIF}
  ZeroMemory(@DM, SizeOf(DM));
  DM.dmSize:=SizeOf(DM);
  DM.dmBitsPerPel:=Depth;
  DM.dmPelsWidth:=Width;
  DM.dmPelsHeight:=Height;
  DM.dmDisplayFrequency:=Refresh;
  DM.dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  if Refresh>0 then DM.dmFields:=DM.dmFields or DM_DISPLAYFREQUENCY;
  Result:=ChangeDisplaySettings(DM, CDS_TEST)=DISP_CHANGE_SUCCESSFUL;
  if Result then ChangeDisplaySettings(DM, CDS_FULLSCREEN);
end;

procedure gleGoBack;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Leaving fullscreen');{$ENDIF}
  ChangeDisplaySettings(DevMode(nil^), CDS_FULLSCREEN);
end;

function gleSetPix(DC: HDC; Depth: Cardinal): HGLRC;
var
  PFD: TPIXELFORMATDESCRIPTOR;
  PixelFormat: Cardinal;
begin
  Result:=0;
  {$IFDEF VSE_LOG}Log(llInfo, 'Setting pixel format');{$ENDIF}
  ZeroMemory(@PFD, SizeOf(TPIXELFORMATDESCRIPTOR));
  with PFD do
  begin
    nSize:=SizeOf(TPIXELFORMATDESCRIPTOR);
    nVersion:=1;
    dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_SWAP_EXCHANGE;
    iPixelType:=PFD_TYPE_RGBA;
    cColorBits:=Depth;
    //if Depth=32 then cAlphaBits:=8;
    cDepthBits:=24;
    cStencilBits:=8;
    iLayerType:=PFD_MAIN_PLANE;
  end;
  PixelFormat:=ChoosePixelFormat(DC, @PFD);
  if PixelFormat=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to find a suitable pixel format');{$ENDIF}
    Exit;
  end;
  if not SetPixelFormat(DC, PixelFormat, @PFD) then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to set the pixel format');{$ENDIF}
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Creating rendering context');{$ENDIF}
  Result:=wglCreateContext(DC);
  if Result=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to create an OpenGL rendering context');{$ENDIF}
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Activating rendering context');{$ENDIF}
  if not wglMakeCurrent(DC, Result) then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to activate OpenGL rendering context');{$ENDIF}
    wglDeleteContext(Result);
    Result:=0;
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Reading extensions');{$ENDIF}
  ReadExtensions;
end;

procedure gleSetGL;
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
end;

procedure gleResizeWnd(Width, Height: Integer);
begin
  if Height=0 then Height:=1;
  glViewport(0, 0, Width, Height);
end;

procedure glePushMatrix;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glPopAttrib;
end;

procedure glePopMatrix;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
  glPopAttrib;
end;

procedure glePerspectiveMatrix(FOV: Single; Width, Height: Integer);
begin
  glePerspectiveMatrix2(FOV, Width, Height, 0.1, 10000);
end;

procedure glePerspectiveMatrix2(FOV: Single; Width, Height: Integer; ZNear, ZFar: Single);
begin
  if Height<1 then Height:=1;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(FOV, Width/Height, ZNear, ZFar);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure gleOrthoMatrix(Width, Height: Integer);
begin
  gleOrthoMatrix2(0, 0, Width, Height);
end;

procedure gleOrthoMatrix2(Left, Top, Right, Bottom: Double);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(Left, Right, Bottom, Top, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

function gleError(GLError: Cardinal): string;
begin
  case GLError of
    GL_NO_ERROR: Result:='No errors';
    GL_INVALID_ENUM: Result:='Invalid enumeration';
    GL_INVALID_VALUE: Result:='Invalid value';
    GL_INVALID_OPERATION: Result:='Invalid operation';
    GL_STACK_OVERFLOW: Result:='Stack overflow';
    GL_STACK_UNDERFLOW: Result:='Stack underflow';
    GL_OUT_OF_MEMORY: Result:='Out of memory';
    else Result:='Unknown error';
  end;
end;

procedure gleColor(Color: TColor);
var
  Clr: packed array[0..3] of Byte absolute Color;
begin
  if Clr[3] = 0 then Clr[3] := $FF;
  glColor4ub(Clr[0], Clr[1], Clr[2], Clr[3]);
end;

function gleColorTo4f(Color: TColor): TVector4D;
var
  Clr: packed array[0..3] of Byte absolute Color;
begin
  with Result do
  begin
    X:=Clr[0]/255;
    Y:=Clr[1]/255;
    Z:=Clr[2]/255;
    W:=Clr[3]/255;
  end;
end;

function gleGetResolutions: TResolutions;

  procedure AddResolution(Width, Height, Refresh: Cardinal);
  var
    i, j: Integer;
  begin
    for i:=0 to High(Result) do
      if (Result[i].Width=Width) and (Result[i].Height=Height)
        then Break;
    if Length(Result)=0 then i:=0;
    if i=Length(Result) then SetLength(Result, i+1);
    Result[i].Width:=Width;
    Result[i].Height:=Height;
    for j:=0 to High(Result[i].RefreshRates) do
      if Result[i].RefreshRates[j]=Refresh then Exit;
    SetLength(Result[i].RefreshRates, Length(Result[i].RefreshRates)+1);
    Result[i].RefreshRates[High(Result[i].RefreshRates)]:=Refresh;
  end;

  function Compare(const A, B: TResolution): Boolean;
  begin
    Result:=A.Width>B.Width;
    if A.Width=B.Width then Result:=A.Height>B.Height;
  end;

var
  i, j, SC: Integer;
  TmpR: TResolution;
  TmpF: Cardinal;
  DM: TDevMode;
begin
  ZeroMemory(@DM, SizeOf(DM));
  i:=0;
  while EnumDisplaySettings(nil, i, DM) do
  begin
    if (DM.dmPelsWidth >= 640) and (DM.dmPelsHeight >= 480) and (DM.dmDisplayFrequency <> 1)
      then AddResolution(DM.dmPelsWidth, DM.dmPelsHeight, DM.dmDisplayFrequency);
    Inc(i);
  end;
  repeat
    SC:=0;
    for i:=0 to High(Result)-1 do
      if Compare(Result[i], Result[i+1]) then
      begin
        TmpR:=Result[i];
        Result[i]:=Result[i+1];
        Result[i+1]:=TmpR;
        Inc(SC);
      end;
  until SC=0;
  for i:=0 to High(Result) do
    repeat
      SC:=0;
      for j:=0 to High(Result[i].RefreshRates)-1 do
        if Result[i].RefreshRates[j]>Result[i].RefreshRates[j+1] then
        begin
          TmpF:=Result[i].RefreshRates[j];
          Result[i].RefreshRates[j]:=Result[i].RefreshRates[j+1];
          Result[i].RefreshRates[j+1]:=TmpF;
          Inc(SC);
        end;
    until SC=0;
end;

function gleGetCurrentResolution: TResolution;
var
  DM: TDevMode;
begin
  ZeroMemory(@DM, SizeOf(DM));
  ZeroMemory(@Result, SizeOf(Result));
  if not EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, DM) then Exit;
  Result.Width:=DM.dmPelsWidth;
  Result.Height:=DM.dmPelsHeight;
  Result.RefreshRate:=DM.dmDisplayFrequency;
end;

function gleScreenTo3D(X, Y: Integer; GetDepth: Boolean=false): TVector3D;
var
  Viewport: array[0..3] of Integer;
  ModelViewMatrix, ProjectionMatrix: array[0..15] of Double;
  Z: Single;
  OX, OY, OZ: Double;
begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @ModelViewMatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjectionMatrix);
  glGetIntegerv(GL_VIEWPORT, @Viewport);
  Y:=Viewport[3]-Y-1;
  if GetDepth
    then glReadPixels(X, Y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @Z)
    else Z:=0;
  gluUnProject(X, Y, Z, @ModelViewMatrix, @ProjectionMatrix, @Viewport, OX, OY, OZ);
  Result:=Vector3D(OX, OY, OZ);
end;

function gle3DToScreen(X, Y, Z: Double): TPoint;
var
  Viewport: array[0..3] of Integer;
  ModelViewMatrix, ProjectionMatrix: array[0..15] of Double;
begin
  glGetDoublev(GL_MODELVIEW_MATRIX, @ModelViewMatrix);
	glGetDoublev(GL_PROJECTION_MATRIX, @ProjectionMatrix);
	glGetIntegerv(GL_VIEWPORT, @Viewport);
  gluProject(X, Y, Z, @ModelViewMatrix, @ProjectionMatrix, @Viewport, X, Y, Z);
  Result:=Point(Round(X), Viewport[3]-Round(Y)-1);
end;

end.
