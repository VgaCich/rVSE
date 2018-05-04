unit VSETexMan;

interface

uses
  Windows, AvL, avlUtils, avlMath, OpenGL, oglExtensions, VSEOpenGLExt,
  VSEImageCodec, VSECore{$IFDEF VSE_LOG}, VSELog{$ENDIF}{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

type
  TOnLostTex=function(Sender: TObject; const Name: string): Cardinal;
  TRTTMethod=(rttCopy, rttFBO); //Render-to-Texture method - CopyTexture (slow), FrameBuffer Object
  TTextureFilter=(tfNone, tfBilinear, tfTrilinear, tfAnisotropic, tfCustomAnisotropy); //Use tfCustomAnisotropy+AnisoLevel to set custom anisotropy level
  TTexture=record //internally used
    ID: Cardinal;
    Name: string;
  end;
  TRTTInfo=record //internally used
    Method: TRTTMethod;
    FBO, RBODepth, Color, Depth: Cardinal;
    RTWidth, RTHeight: Integer;
    Exist: Boolean;
  end;
  TTexMan=class(TModule)
  private
    FCount, FMaxChannel: Integer;
    FTextures: array of TTexture;
    FRTTs: array of TRTTInfo;
    FOnLostTex: TOnLostTex;
    FRTTMethod: TRTTMethod;
    procedure SetRTTMethod(Value: TRTTMethod);
    {$IFDEF VSE_CONSOLE}function TexFilterHandler(Sender: TObject; Args: array of const): Boolean;{$ENDIF}
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    class function Name: string; override; //internally used
    function  AddTexture(const Name: string; Stream: TStream; Clamp, MipMap: Boolean; FreeStream: Boolean = false): Cardinal; overload;  //Add texture from stream
    function  AddTexture(const Name: string; const Image: TImage; Clamp, MipMap: Boolean): Cardinal; overload; //Add texture from TImageData
    function  AddTexture(Name: string; Data: Pointer; Width, Height: Integer; Comps, Format: GLenum; Clamp, MipMap: Boolean): Cardinal; overload; //Add texture from memory
    function  GetTex(const Name: string; IgnoreLostTex: Boolean = false): Cardinal; //Get texture ID by texture name
    procedure SetFilter(ID: Cardinal; Filter: TTextureFilter); overload; //Set texture filter
    procedure SetFilter(const Prefix: string; Filter: TTextureFilter); overload; //Set filter for textures with name starting with Prefix
    procedure Bind(ID: Cardinal; Channel: Integer = 0); //Set current texture in specified texture channel
    procedure Unbind(Channel: Integer = 0); //Remove texture from specified texture channel
    {Render-To-Texture (RTT)}
    function  InitRTT(Width, Height: Integer): Cardinal; //Init Render-To-Texture target with specified dimencions; returns RTT target ID
    procedure FreeRTT(RTT: Cardinal); //Remove RTT target
    function  RTTBegin(RTT: Cardinal; TexColor: Cardinal; TexDepth: Cardinal = 0): Boolean; //Start render to RTT target; TexColor, TexDepth - target textures; returns true if successfully started
    procedure RTTEnd(RTT: Cardinal); //End render to RTT target
    {properties}
    property OnLostTex: TOnLostTex read FOnLostTex write FOnLostTex; //Invoked if texture not found; return TexID or 0
    property RTTMethod: TRTTMethod read FRTTMethod write SetRTTMethod; //Method, used for RTT; default: autodetect
  end;

var
  TexMan: TTexMan; //Global variable for accessing to Texture Manager

implementation

const
  TexCapDelta=16;

constructor TTexMan.Create;
begin
  inherited Create;
  TexMan:=Self;
  FCount:=0;
  SetLength(FTextures, TexCapDelta);
  FMaxChannel:=Max(glMaxTextureUnits, glMaxTextureImageUnits)-1;
  if GL_EXT_framebuffer_object
    then FRTTMethod:=rttFBO
    else FRTTMethod:=rttCopy;
  {$IFDEF VSE_CONSOLE}Console.OnCommand['texfilter flt=enone:bilin:trilin:aniso ?prefix=s']:=TexFilterHandler;{$ENDIF}
end;

destructor TTexMan.Destroy;
var
  i: Integer;
begin
  TexMan:=nil;
  {$IFDEF VSE_LOG}LogF(llInfo, 'TexMan: Freeing %d textures', [FCount]);{$ENDIF}
  for i:=0 to High(FRTTs) do FreeRTT(i);
  Finalize(FRTTs);
  for i:=0 to FCount-1 do
    glDeleteTextures(1, @FTextures[i].ID);
  Finalize(FTextures);
  inherited Destroy;
end;

class function TTexMan.Name: string;
begin
  Result:='TexMan';
end;

function TTexMan.AddTexture(const Name: string; Stream: TStream; Clamp, MipMap: Boolean; FreeStream: Boolean = false): Cardinal;
var
  Image: TImage;
begin
  Image:=TImage.Create;
  try
    Image.Load(Stream);
    Result:=AddTexture(Name, Image, Clamp, MipMap);
  finally
    Image.Free;
    if FreeStream then
      Stream.Free;
  end;
end;

function TTexMan.AddTexture(const Name: string; const Image: TImage; Clamp, MipMap: Boolean): Cardinal;
const
  Comp: array[TPixelFormat] of GLenum = (GL_LUMINANCE8, GL_RGB8, GL_RGBA8, GL_RGBA8);
  Format: array[TPixelFormat] of GLenum = (GL_LUMINANCE, GL_BGR, GL_RGBA, GL_BGRA);
begin
  Image.Pack;
  Result:=AddTexture(Name, Image.Pixels, Image.Width, Image.Height, Comp[Image.PixelFormat], Format[Image.PixelFormat], Clamp, MipMap);
end;

function TTexMan.AddTexture(Name: string; Data: Pointer; Width, Height: Integer; Comps, Format: GLenum; Clamp, MipMap: Boolean): Cardinal;
begin
  //{$IFDEF VSE_LOG}Log(llInfo, 'TexMan: adding texture '+Name);{$ENDIF}
  Name:=Name;
  Result:=GetTex(Name, true);
  if Result=0 then
  begin
    if FCount=High(FTextures) then SetLength(FTextures, Length(FTextures)+TexCapDelta);
    FTextures[FCount].Name:=Name;
    glGenTextures(1, @Result);
    FTextures[FCount].ID:=Result;
    Inc(FCount);
  end;
  glBindTexture(GL_TEXTURE_2D, Result);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  if Clamp then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end;
  glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  if MipMap then
  begin
    glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, Comps, Width, Height, Format, GL_UNSIGNED_BYTE, Data)
  end
  else begin
    glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, Comps, Width, Height, 0, Format, GL_UNSIGNED_BYTE, Data);
  end;
end;

function TTexMan.GetTex(const Name: string; IgnoreLostTex: Boolean = false): Cardinal;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to FCount-1 do
    if SameText(FTextures[i].Name, Name) then
    begin
      Result:=FTextures[i].ID;
      Exit;
    end;
  if not IgnoreLostTex then
  begin
    if Assigned(FOnLostTex) then
      Result:=FOnLostTex(Self, Name);
    {$IFDEF VSE_LOG}if Result=0 then Log(llError, 'TexMan: can''t find texture '+Name);{$ENDIF}
  end;
end;

procedure TTexMan.SetFilter(ID: Cardinal; Filter: TTextureFilter);
const
  MagFilters: array[Boolean] of Cardinal = (GL_NEAREST, GL_LINEAR);
  MinFilters: array[Boolean, tfNone..tfAnisotropic] of Cardinal = (
    (GL_NEAREST, GL_LINEAR, GL_LINEAR, GL_LINEAR),
    (GL_NEAREST, GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR)
  );
var
  MaxLevel: Cardinal;
begin
  Bind(ID);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, @MaxLevel);
  glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, MagFilters[Filter>tfNone]);
  glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilters[MaxLevel>0, TTextureFilter(Min(Integer(Filter), Integer(tfAnisotropic)))]);
  case Filter of
    tfNone..tfTrilinear: glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1);
    tfAnisotropic: glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, glMaxAnisotropy);
    else glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, Integer(Filter) - Integer(tfCustomAnisotropy));
  end;
  Unbind;
end;

procedure TTexMan.SetFilter(const Prefix: string; Filter: TTextureFilter);
var
  i: Integer;
begin
  for i:=0 to FCount-1 do
    with FTextures[i] do
      if ((Length(Prefix)=0) and (Copy(Name, 1, 2)<>'__')) or ((Length(Prefix)>0) and SameText(Copy(Name, 1, Length(Prefix)), Prefix)) then
        SetFilter(ID, Filter);
end;

procedure TTexMan.Bind(ID: Cardinal; Channel: Integer);
begin
  if not (Channel in [0..FMaxChannel]) then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'TexMan: can''t bind texture %d to channel %d', [ID, Channel]);{$ENDIF}
    Exit;
  end;
  if GL_ARB_multitexture then
    glActiveTextureARB(GL_TEXTURE0_ARB+Channel)
  else if Channel <> 0 then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'TexMan: can''t bind texture %d to channel %d: multitexturing not supported', [ID, Channel]);{$ENDIF}
    Exit;
  end;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, ID);
end;

procedure TTexMan.Unbind(Channel: Integer);
begin
  if not (Channel in [0..FMaxChannel]) then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'TexMan: can''t unbind texture from channel %d', [Channel]);{$ENDIF}
    Exit;
  end;
  if GL_ARB_multitexture then
    glActiveTextureARB(GL_TEXTURE0_ARB+Channel);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
end;

function TTexMan.InitRTT(Width, Height: Integer): Cardinal;
var
  i: Cardinal;
begin
  Result:=$FFFFFFFF;
  if Length(FRTTs)>0 then
    for i:=0 to High(FRTTs) do
      if not FRTTs[i].Exist then
      begin
        Result:=i;
        Break;
      end;
  if Result=$FFFFFFFF then
  begin
    Result:=Length(FRTTs);
    SetLength(FRTTs, Result+1);
  end;
  ZeroMemory(@FRTTs[Result], SizeOf(TRTTInfo));
  with FRTTs[Result] do
  begin
    Method:=FRTTMethod;
    RTWidth:=Width;
    RTHeight:=Height;
    Exist:=true;
    if Method=rttFBO then
    begin
      glGenFramebuffersEXT(1, @FBO);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FBO);
      glGenRenderbuffersEXT(1, @RBODepth);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, RBODepth);
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24_ARB, Width, Height);
	    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RBODepth);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    end;
  end;
end;

procedure TTexMan.FreeRTT(RTT: Cardinal);
begin
  if (RTT>High(FRTTs)) or not FRTTs[RTT].Exist then Exit;
  with FRTTs[RTT] do
  begin
    if Method=rttFBO then
    begin
      if FBO<>0 then glDeleteRenderbuffersEXT(1, @FBO);
      if RBODepth<>0 then glDeleteRenderbuffersEXT(1, @RBODepth);
    end;
    Exist:=false;
  end;
end;

function TTexMan.RTTBegin(RTT: Cardinal; TexColor, TexDepth: Cardinal): Boolean;
begin
  Result:=false;
  if (RTT>High(FRTTs)) or (TexColor=0) then Exit;
  with FRTTs[RTT] do
  begin
    glViewport(0, 0, RTWidth, RTHeight);
    Color:=TexColor;
    Depth:=TexDepth;
    Bind(TexColor);
    glTexParameter(GL_TEXTURE, GL_GENERATE_MIPMAP, Integer(GL_TRUE));
    Unbind;
    if Method=rttFBO then
    begin
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FBO);
      glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, TexColor, 0);
      if TexDepth<>0 then
      begin
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, 0);
        glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, TexDepth, 0);
      end
      else begin
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RBODepth);
      end;
      if glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT)<>GL_FRAMEBUFFER_COMPLETE_EXT then
      begin
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
        Exit;
      end;
    end;
    Result:=true;
  end;
end;

procedure TTexMan.RTTEnd(RTT: Cardinal);
var
  FMT: Cardinal;
begin
  if RTT>High(FRTTs) then Exit;
  with FRTTs[RTT] do
  begin
    case Method of
      rttCopy:
        begin
          Bind(Color);
          glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@Fmt));
          glCopyTexImage2D(GL_TEXTURE_2D, 0, Fmt, 0, 0, RTWidth, RTHeight, 0);
          if Depth<>0 then
          begin
            Bind(Depth);
            glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@Fmt));
            glCopyTexImage2D(GL_TEXTURE_2D, 0, Fmt, 0, 0, RTWidth, RTHeight, 0);
          end;
          Unbind;
        end;
      rttFBO:
        begin
          glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0);
          if Depth<>0 then glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, 0, 0);
          glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
        end;
    end;
  end;
  glViewport(0, 0, Core.ResolutionX, Core.ResolutionY);
end;

procedure TTexMan.SetRTTMethod(Value: TRTTMethod);
begin
  if not GL_EXT_framebuffer_object and (Value = rttFBO) then
    Value := rttCopy;
  FRTTMethod := Value;
end;

{$IFDEF VSE_CONSOLE}
function TTexMan.TexFilterHandler(Sender: TObject; Args: array of const): Boolean;
var
  Prefix: string;
begin
  Result:=true;
  if Length(Args)=3 then
    Prefix:=string(Args[2].VAnsiString)
  else
    Prefix:='';
  SetFilter(Prefix, TTextureFilter(Args[1].VInteger));
end;
{$ENDIF}

initialization
  RegisterModule(TTexMan);

end.
