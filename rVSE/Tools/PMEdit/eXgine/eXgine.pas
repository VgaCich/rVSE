unit eXgine;
////////////////////////////////
//  eXgine v0.72 header file  //
//----------------------------//
// http://xproger.mirgames.ru //
////////////////////////////////
interface
{$I cfg.pas}

{$IFDEF EX_STATIC}
uses
  // к ним должны быть прописаны пути в dpr в случае, если он не в директории с ними
  sys_main,
  {$IFNDEF NO_LOG}log,{$ENDIF}
  {$IFNDEF NO_TEX}tex,
    {$IFNDEF NO_BJG}g_bjg,{$ENDIF}
    {$IFNDEF NO_BJG}g_tga,{$ENDIF}
  {$ENDIF}
  {$IFNDEF NO_INP}inp,{$ENDIF}
  {$IFNDEF NO_VBO}vbo,{$ENDIF}
  {$IFNDEF NO_VFP}vfp,{$ENDIF}
  {$IFNDEF NO_SND}snd,
     {$IFNDEF NO_OGG}s_ogg,{$ENDIF}
  {$ENDIF}
  {$IFNDEF NO_VEC}vec,{$ENDIF}
  com, eng, wnd, ogl;
{$ENDIF}

// Engine
const
  PROC_UPDATE  = 0;
  PROC_RENDER  = 1;
  PROC_MESSAGE = 2;
  PROC_ACTIVE  = 3;

// Input
const
// мышь
  M_BTN_1  = 257;
  M_BTN_2  = 258;
  M_BTN_3  = 259;
  M_BTN_4  = 260;
  M_BTN_5  = 261;
  M_BTN_6  = 262;
  M_BTN_7  = 263;
// джойстик
  J_BTN_1  = 264;
  J_BTN_2  = 265;
  J_BTN_3  = 266;
  J_BTN_4  = 267;
  J_BTN_5  = 268;
  J_BTN_6  = 269;
  J_BTN_7  = 270;
  J_BTN_8  = 271;
  J_BTN_9  = 272;
  J_BTN_10 = 273;
  J_BTN_11 = 274;
  J_BTN_12 = 275;
  J_BTN_13 = 276;
  J_BTN_14 = 277;
  J_BTN_15 = 278;
  J_BTN_16 = 279;
  J_BTN_17 = 280;
  J_BTN_18 = 281;
  J_BTN_19 = 282;
  J_BTN_20 = 283;
  J_BTN_21 = 284;
  J_BTN_22 = 285;
  J_BTN_23 = 286;
  J_BTN_24 = 287;
  J_BTN_25 = 288;
  J_BTN_26 = 289;
  J_BTN_27 = 290;
  J_BTN_28 = 291;
  J_BTN_29 = 292;
  J_BTN_30 = 293;
  J_BTN_31 = 294;
  J_BTN_32 = 295;

// OpenGL
const
// TBlendType
  BT_NONE = 0;
  BT_SUB  = 1;
  BT_ADD  = 2;
  BT_MULT = 3;

// Texture
const
// Texture Mode
  TM_COLOR = 1;
  TM_DEPTH = 2;
// Filter Type
  FT_NONE       = 0;
  FT_BILINEAR   = 1;
  FT_TRILINEAR  = 2;
  FT_ANISOTROPY = 3;

// VBO
const
// TVBOdata
  VBO_INDEX     = 0;
  VBO_VERTEX    = 1;
  VBO_NORMAL    = 2;
  VBO_COLOR     = 3;
  VBO_TEXCOORD  = 4;
  VBO_TEXCOORD1 = 4;
  VBO_TEXCOORD2 = 5;

// Log
const
  MSG_NONE    = $00000000;
  MSG_ERROR   = $00000010;
  MSG_INFO    = $00000040;
  MSG_WARNING = $00000030;

// Math
const
  deg2rad = pi / 180;
  rad2deg = 180 / pi;

type
  TByteArray = array [0..1024] of Byte;
  PByteArray = ^TByteArray;

// Engine
{
  TProcRender  = procedure;
  TProcUpdate  = procedure;
  TProcMessage = procedure (Msg: Cardinal; wP, lP: Integer);
  TProcActive  = procedure (Active: Boolean);
}

{$IFDEF EX_STATIC}
type
// IOpenGL
  TFont        = com.TFont;
// IVBO
  TVBOid       = com.TVBOid;
// ITexture
  TTexture     = com.TTexture;
  TTexMode     = com.TTexMode;
  TBlendType   = com.TBlendType;
// IShader
  TShader      = com.TShader;
  TShAttrib    = com.TShAttrib;
  TShUniform   = com.TShUniform;
// ISound
  TSound       = com.TSound;
  TChannel     = com.TChannel;
// IInput
  TJoyAxis     = com.TJoyAxis;
// IVec
  TVector      = com.TVector;
  TVector2D    = com.TVector2D;
// other
  TRGB         = com.TRGB;
  TRGBA        = com.TRGBA;
{$ELSE}
type
// IOpenGL
  TFont      = Cardinal;

// ITexture
  TTexture   = Cardinal;
  TTexMode   = Integer;  // TM
  TBlendType = Integer;  // BT

// IShader
  TShader    = Cardinal;
  TShAttrib  = Integer;
  TShUniform = Integer;

// IVBO
  TVBOid     = Integer;

// ISound
  TSound     = Integer;
  TChannel   = Integer;

// IInput
  TJoyAxis = record
    X, Y, Z, R, U, V : Single;
  end;

// IVec
  TVector = record
    X, Y, Z : Single;
  end;

  TVector2D = record
    X, Y : Single;
  end;

// other
  TRGB = record
    R, G, B : Byte;
  end;

  TRGBA = record
    R, G, B, A : Byte;
  end;

  ILog = interface
    function  Create(FileName: PChar): Boolean; 
    procedure Print(Text: PChar); 
    function  Msg(Caption, Text: PChar; ID: Cardinal = 0): Integer; 
    procedure TimeStamp(Active: Boolean = True);
    procedure Flush(Active: Boolean = True);
    procedure Free; 
  end;

  IWindow = interface
    function  Create(Caption: PChar; OnTop: Boolean = True): Boolean; overload;
    function  Create(Handle: Cardinal): Boolean; overload;
    function  Handle: Cardinal;
    procedure Caption(Text: PChar);
    function  Width: Integer;
    function  Height: Integer;
    function  Mode(FullScreen: Boolean; W, H, BPP, Freq: Integer): Boolean;
    procedure Show(Minimized: Boolean);
    function  Active: Boolean;
  end;

  IOpenGL = interface
    function  FPS: Integer;
    procedure VSync(Active: Boolean); overload;
    function  VSync: Boolean; overload;
    procedure Clear(Color: Boolean = True; Depth: Boolean = False; Stencil: Boolean = False);
    procedure Swap;
    procedure AntiAliasing(Samples: Integer); overload;
    function  AntiAliasing: Integer; overload;
    procedure Set2D(x, y, w, h: Single);
    procedure Set3D(FOV, zNear, zFar: Single);
    procedure LightPos(ID: Integer; X, Y, Z: Single);
    procedure LightColor(ID: Integer; R, G, B: Single);
  {$IFNDEF NO_TEX}
    function  FontCreate(Name: PChar; Size: Integer): TFont;
    procedure FontFree(Font: TFont);
    procedure TextOut(Font: TFont; X, Y: Single; Text: PChar);
    function  TextLen(Font: TFont; Text: PChar): Integer;
  {$ENDIF}
    procedure Blend(BType: TBlendType);
    function  ScreenShot(FileName: PChar): Boolean;
  end;

{$IFNDEF NO_INP}
  IInput = interface
    procedure Reset;
    function  Down(Key: Integer): Boolean;
    function  LastKey: Integer;
    function  MDelta: TVector;
    function  WDelta: Integer;
  {$IFNDEF NO_JOY}
    function  JCount: Integer;
    function  JAxis(ID: Integer = 0): TJoyAxis;
    function  JPOV(ID: Integer = 0): Single;
  {$ENDIF}
    procedure MCapture(Active: Boolean = True);
  end;
{$ENDIF}

{$IFNDEF NO_VBO}
  IVBuffer = interface
    procedure Clear;
    procedure Add(DataType: Cardinal; Count: Cardinal; Data: Pointer);
    function  Compile: TVBOid;
    procedure Free(ID: TVBOid);
    procedure Offset(ID: TVBOid; DataType: Cardinal; Offset: Cardinal);
    procedure Render(ID: TVBOid; mode: Cardinal; Count: Integer = 0);
  end;
{$ENDIF}

{$IFNDEF NO_TEX}
  ITexture = interface
    function  Create(Name: PChar; c, f, W, H: Integer; Data: Pointer; Clamp: Boolean = False; MipMap: Boolean = True; Group: Integer = 0): TTexture;
    function  Load(FileName: PChar; Clamp: Boolean = False; MipMap: Boolean = True; Group: Integer = 0): TTexture; overload;
    function  Load(Name: PChar; Mem: Pointer; Size: Integer; Clamp: Boolean = False; MipMap: Boolean = True; Group: Integer = 0): TTexture; overload;
    function  Load(FileName: PChar; var W, H, BPP: Integer; var Data: Pointer): Boolean; overload;
    function  Load(Name: PChar; Mem: Pointer; Size: Integer; var W, H, BPP: Integer; var Data: Pointer): Boolean; overload;
    procedure Free(var Data: Pointer); overload;
    procedure Free(ID: TTexture); overload;
    procedure Enable(ID: TTexture; Channel: Integer = 0);
    procedure Disable(Channel: Integer = 0);
    procedure Update_Begin(Group: Integer);
    procedure Update_End(Group: Integer);
    procedure Filter(FilterType: Integer; Group: Integer = 0);
    procedure Render_Copy(ID: TTexture; X, Y, W, H, Format: Integer; Level: Integer = 0);
  {$IFNDEF NO_FBO}
    function  Render_Init(TexSize: Integer): Boolean;
    procedure Render_Begin(ID: TTexture; Mode: TTexMode = TM_COLOR);
    procedure Render_End;
  {$ENDIF}    
  end;
{$ENDIF}

{$IFNDEF NO_VFP}
  IShader = interface
    procedure Clear;
    function  Add(FileName: PChar; Name: PChar = nil): Boolean; overload;
    function  Add(Mem: Pointer; Size: Integer; Name: PChar = nil): Boolean; overload;
    function  Compile: TShader;
    procedure Free(Shader: TShader);
    function  GetAttrib(Shader: TShader; Name: PChar): TShAttrib;
    function  GetUniform(Shader: TShader; Name: PChar): TShUniform;
    procedure Attrib(a: TShAttrib; x: Single); overload;
    procedure Attrib(a: TShAttrib; x, y: Single); overload;
    procedure Attrib(a: TShAttrib; x, y, z: Single); overload;
    procedure Uniform(u: TShUniform; x: Single); overload;
    procedure Uniform(u: TShUniform; x, y: Single); overload;
    procedure Uniform(u: TShUniform; x, y, z: Single); overload;
    procedure Uniform(u: TShUniform; i: Integer); overload;
    procedure Enable(Shader: TShader);
    procedure Disable;
  end;
{$ENDIF}

{$IFNDEF NO_SND}
  ISound = interface
    function  Load(FileName: PChar; Group: Integer = 0): TSound; overload;
    function  Load(Name: PChar; Mem: Pointer; Size: Integer; Group: Integer = 0): TSound; overload;
    function  Free(ID: TSound): Boolean;
    function  Play(ID: TSound; X, Y, Z: Single; Loop: Boolean = False): TChannel;
    procedure Stop(ID: TChannel);
    procedure Update_Begin(Group: Integer);
    procedure Update_End(Group: Integer);
    procedure Volume(Value: Integer);
    procedure Freq(Value: Integer);
    procedure Channel_Pos(ID: TChannel; X, Y, Z: Single);
    procedure Pos(X, Y, Z: Single);
    procedure Dir(dX, dY, dZ, uX, uY, uZ: Single);
    procedure Factor_Pan(Value: Single = 0.1);
    procedure Factor_Rolloff(Value: Single = 0.005);
   {$IF NOT (DEFINED(NO_MCI) AND DEFINED(NO_OGG))}
    procedure PlayFile(FileName: PChar; Loop: Boolean); {$IFNDEF NO_OGG}overload;
    procedure PlayFile(Mem: Pointer; Size: Integer; Loop: Boolean); overload;
    {$ENDIF}
    procedure StopFile;
   {$IFEND}
  end;
{$ENDIF}

{$IFNDEF NO_VEC}
  IVector = interface
    function Create(X, Y, Z: Single): TVector; overload;
    function Create(X, Y: Single): TVector2D; overload;
    function Add(v1, v2: TVector): TVector;
    function Sub(v1, v2: TVector): TVector;
    function Mult(v: TVector; x: Single): TVector;
    function Length(v: TVector): Single;
    function LengthQ(v: TVector): Single;
    function Normalize(v: TVector): TVector;
    function Dot(v1, v2: TVector): Single;
    function Cross(v1, v2: TVector): TVector;
    function Angle(v1, v2: TVector): Single;
  end;
{$ENDIF}

  IEngine = interface
    function  log: ILog; 
    function  wnd: IWindow; 
    function  ogl: IOpenGL;
    {$IFNDEF NO_INP}function inp: IInput;{$ENDIF}
    {$IFNDEF NO_VBO}function vbo: IVBuffer;{$ENDIF}
    {$IFNDEF NO_TEX}function tex: ITexture;{$ENDIF}
    {$IFNDEF NO_VFP}function vfp: IShader;{$ENDIF}
    {$IFNDEF NO_SND}function snd: ISound;{$ENDIF}
    {$IFNDEF NO_VEC}function vec: IVector;{$ENDIF}
    function  Version: PChar; 
    procedure SetProc(ID: Integer; Proc: Pointer);
    procedure ActiveUpdate(OnlyActive: Boolean);
    function  GetTime: Integer;
    procedure ResetTimer;
    procedure MainLoop(UPS: Integer);
    procedure Update;
    procedure Render;
    procedure Quit; 
  end;
{$ENDIF}

// Самое необходимое из новых (> 1.3) версий OpenGL
const
  GL_CLAMP_TO_EDGE = $812F;
  GL_RGB8          = $8051;
  GL_RGBA8         = $8058;
  GL_BGR           = $80E0;
  GL_BGRA          = $80E1;
  GL_TEXTURE0_ARB  = $84C0;

var
  log : ILog;
  eX  : IEngine;
  wnd : IWindow;
  ogl : IOpenGL;
  {$IFNDEF NO_VBO}vbo : IVBuffer;{$ENDIF}
  {$IFNDEF NO_TEX}tex : ITexture;{$ENDIF}
  {$IFNDEF NO_VFP}vfp : IShader;{$ENDIF}
  {$IFNDEF NO_SND}snd : ISound;{$ENDIF}
  {$IFNDEF NO_INP}inp : IInput;{$ENDIF}
  {$IFNDEF NO_VEC}vec : IVector;{$ENDIF}

  procedure LogOut(const Text: string);
  function RGB(R, G, B: Byte): TRGB;
  function RGBA(R, G, B, A: Byte): TRGBA;

implementation

{$IFNDEF EX_STATIC}
  procedure exInit(out Engine: IEngine; LogFile: PChar = nil); external 'eXgine.dll';
{$ELSE}
procedure exInit(out Engine: IEngine; LogFile: PChar = nil);
begin
  if oeng = nil then
  begin
    olog := TLog.CreateEx;
    if LogFile <> nil then
      olog.Create(LogFile);
    {$IFNDEF NO_VEC}ovec := TVec.CreateEx;{$ENDIF}
    oeng := TEng.CreateEx;
    {$IFNDEF NO_SND}osnd := TSnd.CreateEx;{$ENDIF}
    {$IFNDEF NO_INP}oinp := TInp.CreateEx;{$ENDIF}
    oogl := TOGL.CreateEx;
    {$IFNDEF NO_VBO}ovbo := TVBO.CreateEx;{$ENDIF}
    {$IFNDEF NO_TEX}otex := TTex.CreateEx;{$ENDIF}
    ownd := TWnd.CreateEx;
    {$IFNDEF NO_VFP}ovfp := TVFP.CreateEx;{$ENDIF}
  end;
  Engine := oeng;
end;
{$ENDIF}

procedure LogOut(const Text: string);
begin
  log.Print(PChar(Text));
end;

function RGB(R, G, B: Byte): TRGB;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

function RGBA(R, G, B, A: Byte): TRGBA;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

initialization
  exInit(eX, nil);
  log := eX.log;
  wnd := eX.wnd;
  ogl := eX.ogl;
  {$IFNDEF NO_INP}inp := eX.inp;{$ENDIF}
  {$IFNDEF NO_VBO}vbo := eX.vbo;{$ENDIF}
  {$IFNDEF NO_TEX}tex := eX.tex;{$ENDIF}
  {$IFNDEF NO_VFP}vfp := eX.vfp;{$ENDIF}
  {$IFNDEF NO_VEC}vec := eX.vec;{$ENDIF}
  {$IFNDEF NO_SND}snd := eX.snd;{$ENDIF}

finalization
  {$IFNDEF NO_SND}snd := nil;{$ENDIF}
  {$IFNDEF NO_VEC}vec := nil;{$ENDIF}
  {$IFNDEF NO_VFP}vfp := nil;{$ENDIF}
  {$IFNDEF NO_TEX}tex := nil;{$ENDIF}
  {$IFNDEF NO_VBO}vbo := nil;{$ENDIF}
  {$IFNDEF NO_INP}inp := nil;{$ENDIF}
  ogl := nil;
  wnd := nil;
  eX  := nil;
  log := nil;
end.
