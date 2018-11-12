unit StateStart;

interface

uses
  Windows, Messages, AvL, avlUtils, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, SynTex, SynTexFilters;

type
  TStateStart=class;
  TLoadThread=class(TThread)
  protected
    procedure Execute; override;
    procedure Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
    function  Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean;
  public
    Parent: TStateStart;
  end;
  TStateStart=class(TGameState)
  private
    FFont: Cardinal;
    FLoadThread: TLoadThread;
  protected
    function  GetName: string; override;
    procedure DrawSegs(X, Y: Single; Segs: Integer);
    procedure SyncLoadCache;
    procedure SyncStore;
    procedure SyncLoad;
    procedure OnLoaded(Sender: TObject);
    {$IFDEF VSE_CONSOLE}
    function CacheHandler(Sender: TObject; Args: array of const): Boolean;
    function ClearCacheHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
    function LoadCache: Boolean;
    procedure SetCache(Enabled: Boolean);
    procedure ClearCache;
  end;

var
  UseCache: Boolean;
  CacheDir: string;

const
  SIDStart='Start';

implementation

uses VSEMemPak, VSETexMan, VSEImageCodec, VSERender2D
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF},
  StateMenu;

var
  LoadResult: Boolean=false;
  TexCache: string;
  SReg: PSynTexRegister;
  STexSize: Integer;
  SName: string;
  SSender: TObject;
  LDResult: Boolean;

const
  SLoad='Loading...';
  SRVSE='reduced VS Engine';
  STitle='Assault Lite';

{TLoadThread} //I'm not sure that is correct... May be a source of AV's

procedure TLoadThread.Execute;
var
  ST: TSynTex;
  STF: TSynTexFilters;
  STCode: TStream;
begin
  STCode:=GetFile('Textures.stc');
  ST:=nil;
  STF:=nil;
  if STCode=nil then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Textures synthesizing code not found');{$ENDIF}
    Exit;
  end;
  try
    ST:=TSynTex.Create(512);
    STF:=TSynTexFilters.Create(ST);
    ST.Code:=STCode;
    ST.OnStore:=Store;
    ST.OnLoad:=Load;
    Synchronize(Parent.SyncLoadCache);
    if not LoadResult then LoadResult:=ST.Synthesize;
  finally
    FAN(STF);
    FAN(ST);
    FAN(STCode);
  end;
end;

procedure TLoadThread.Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
begin
  SReg:=@Reg;
  STexSize:=TexSize;
  SName:=Name;
  SSender:=Sender;
  Synchronize(Parent.SyncStore);
end;

function TLoadThread.Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean;
begin
  SReg:=@Reg;
  STexSize:=TexSize;
  SName:=Name;
  SSender:=Sender;
  Synchronize(Parent.SyncLoad);
  Result:=LDResult;
end;

{TStateStart}

constructor TStateStart.Create;
begin
  inherited Create;
  FFont:=Render2D.CreateFont(UIFont, 20, false);
  TexCache:=CacheDir+'Tex\';
  if UseCache then ForceDirectories(TexCache);
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['cache ?val=eoff:on']:=CacheHandler;
  Console.OnCommand['cleartexcache']:=ClearCacheHandler;
  {$ENDIF}
end;

destructor TStateStart.Destroy;
begin
  FAN(FLoadThread);
  inherited Destroy;
end;

procedure TStateStart.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  Render2D.Enter;
  gleColor(clLime);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, SLoad)/2, 500, SLoad);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, STitle)/2, 250, STitle);
  glPushMatrix;
  glTranslate(200, 230, 0);
  glScalef(0.5, 0.5, 1);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, SRVSE)/2, 430, SRVSE);
  DrawSegs(300, 200, $426);
  DrawSegs(400, 200, $2C0);
  DrawSegs(500, 200, $079);
  glScalef(0.7, 0.7, 1);
  DrawSegs(300, 370, $473);
  glPopMatrix;
  Render2D.Leave;
end;

function TStateStart.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  FLoadThread:=TLoadThread.Create(true);
  FLoadThread.OnTerminate:=OnLoaded;
  FLoadThread.Resume;
  ShowCursor(false);
end;

procedure TStateStart.Deactivate;
begin
  inherited;
  FLoadThread.WaitFor;
  FAN(FLoadThread);
  ShowCursor(true);
end;

function TStateStart.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if (Notify=snMinimize) or (Notify=snConsoleActive) then Result:=true;
end;

function TStateStart.LoadCache: Boolean;
var
  Image: TImage;
  SR: TSearchRec;
  TexFile: TFileStream;
begin
  Result:=false;
  Image:=TImage.Create;
  try
    if UseCache and (DirSize(CacheDir)>0) then
    begin
      {$IFDEF VSE_LOG}Log(llInfo, 'Found texture cache; loading');{$ENDIF}
      if FindFirst(TexCache+'*', 0, SR)=0 then
        repeat
          TexFile:=TFileStream.Create(TexCache+SR.Name, fmOpenRead);
          try
            if not Image.LoadRaw(TexFile) then
            begin
              {$IFDEF VSE_LOG}Log(llError, 'Can''t load texture '+SR.Name+' from cache');{$ENDIF}
            end;
            TexMan.AddTexture(SR.Name, Image, false, true);
          finally
            TexFile.Free;
          end;
        until FindNext(SR)<>0;
      FindClose(SR);
      Result:=true;
    end;
  finally
    Image.Free;
  end;
end;

procedure TStateStart.SetCache(Enabled: Boolean);
begin
  UseCache:=Enabled;
  if Enabled
    then CreateDir(CacheDir)
    else DeleteDir(CacheDir);
end;

procedure TStateStart.ClearCache;
begin
  DeleteDir(TexCache);
end;

function TStateStart.GetName: string;
begin
  Result:=SIDStart;
end;

procedure TStateStart.DrawSegs(X, Y: Single; Segs: Integer);
const
  SegsCoord: array [0..10, 0..1] of TPoint=(
    ((X: 0; Y: 0),    (X: 100; Y: 0)),
    ((X: 100; Y: 0),  (X: 100; Y: 100)),
    ((X: 100; Y: 100), (X: 100; Y: 200)),
    ((X: 100; Y: 200),  (X: 0; Y: 200)),
    ((X: 0; Y: 200),   (X: 0; Y: 100)),
    ((X: 0; Y: 100),    (X: 0; Y: 0)),
    ((X: 0; Y: 100),  (X: 100; Y: 100)),
    ((X: 100; Y: 0),   (X: 0; Y: 100)),
    ((X: 0; Y: 0),   (X: 100; Y: 100)),
    ((X: 100; Y: 100),  (X: 0; Y: 200)),
    ((X: 0; Y: 100),  (X: 100; Y: 200)));
var
  i: Integer;
begin
  glPushMatrix;
  glLineWidth(7);
  glPointSize(6);
  glTranslatef(X, Y, 0);
  for i:=0 to 10 do
    if Segs and (1 shl i)<>0 then
      Render2D.DrawLine(SegsCoord[i, 0], SegsCoord[i, 1]);
  glBegin(GL_POINTS);
    for i:=0 to 10 do
      if Segs and (1 shl i)<>0 then
      begin
        glVertex2iv(@SegsCoord[i, 0]);
        glVertex2iv(@SegsCoord[i, 1]);
      end;
  glEnd;
  glPopMatrix;
end;

procedure TStateStart.SyncLoadCache;
begin
  LoadResult:=LoadCache;
end;

procedure TStateStart.SyncStore;
var
  TexFile: TFileStream;
  Image: TImage;
begin
  Image:=TImage.Create(STexSize, STexSize, pfRGBA32bit, STexSize*SizeOf(TRGBA));
  try
    Image.Pixels:=@SReg^[0];
    if UseCache then
    begin
      {$IFDEF VSE_LOG}Log(llInfo, 'Caching texture '+SName);{$ENDIF}
      TexFile:=TFileStream.Create(TexCache+SName, fmCreate or fmOpenWrite);
      try
        Image.SaveRaw(TexFile);
      finally
        TexFile.Free;
      end;
    end;
    TexMan.AddTexture(SName, Image, false, true);
  finally
    Image.Free;
  end;
end;

procedure TStateStart.SyncLoad;
var
  ID: Cardinal;
  W, H: Integer;
begin
  LDResult:=false;
  ID:=TexMan.GetTex(SName);
  if ID=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Can''t load texture '+SName+' for SynTex: texture not exists');{$ENDIF}
    Exit;
  end;
  TexMan.Bind(ID);
  try
    glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, PGLInt(@W));
    glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, PGLInt(@H));
    glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@ID));
    if (W<>STexSize) or (H<>STexSize) or (ID<>GL_RGBA8) then
    begin
      {$IFDEF VSE_LOG}Log(llError, 'TexMan: can''t load texture '+SName+' for SynTex: incorrect texture format');{$ENDIF}
      Exit;
    end;
    glPixelStore(GL_PACK_ALIGNMENT, 1);
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, @SReg^[0]);
    LDResult:=true;
  finally
    TexMan.Unbind;
  end;
end;

procedure TStateStart.OnLoaded(Sender: TObject);
begin
  if LoadResult
    then Core.SwitchState(SIDMenu)
    else begin
      {$IFDEF VSE_LOG}Log(llError, 'Loading textures failed');{$ENDIF}
      Core.StopEngine(StopUserError);
    end;
end;

{$IFDEF VSE_CONSOLE}
const
  BoolState: array[Boolean] of string = ('off', 'on');

function TStateStart.CacheHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1 then
    SetCache(Boolean(Args[1].VInteger))
  else
    Console.WriteLn('Cache: '+BoolState[UseCache]);
  Result:=UseCache;
end;

function TStateStart.ClearCacheHandler(Sender: TObject; Args: array of const): Boolean;
begin
  ClearCache;
  Result:=true;
end;
{$ENDIF}

end.
