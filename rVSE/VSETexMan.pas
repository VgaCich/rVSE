unit VSETexMan;

interface

uses
  Windows, AvL, avlUtils, avlMath, OpenGL, oglExtensions, VSEOpenGLExt,
  VSEImageCodec, VSECore{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TRTTMethod=(rttCopy, rttFBO); //Render-to-Texture method - CopyTexture (slow), FrameBuffer Object
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
    FRTTMethod: TRTTMethod;
    procedure SetRTTMethod(Value: TRTTMethod);
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    class function Name: string; override; //internally used
    //function  AddTexture(const Name: string; Stream: TStream; Clamp, MipMap: Boolean): Cardinal; overload;  //Add texture from stream
    function  AddTexture(const Name: string; const Image: TImage; Clamp, MipMap: Boolean): Cardinal; overload; //Add texture from TImageData
    function  AddTexture(Name: string; Data: Pointer; Width, Height: Integer; Comps, Format: GLenum; Clamp, MipMap: Boolean): Cardinal; overload; //Add texture from memory
    function  GetTex(Name: string; DontLogLostTex: Boolean = false): Cardinal; //Get texture ID by texture name
    procedure Bind(ID: Cardinal; Channel: Integer = 0); //Set current texture in specified texture channel
    procedure Unbind(Channel: Integer = 0); //Remove texture from specified texture channel
    {Render-To-Texture (RTT)}
    function  InitRTT(Width, Height: Integer): Cardinal; //Init Render-To-Texture target with specified dimencions; returns RTT target ID
    procedure FreeRTT(RTT: Cardinal); //Remove RTT target
    function  RTTBegin(RTT: Cardinal; TexColor: Cardinal; TexDepth: Cardinal = 0): Boolean; //Start render to RTT target; TexColor, TexDepth - target textures; returns true if successfully started
    procedure RTTEnd(RTT: Cardinal); //End render to RTT target
    {properties}
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
end;

destructor TTexMan.Destroy;
var
  i: Integer;
begin
  Texman:=nil;
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

(*function TTexMan.AddTexture(const Name: string; Stream: TStream; Clamp, MipMap: Boolean): Cardinal;
var
  ImageData: TImageData;
begin
  ImageData.Pixels := nil;
  if LoadImageFromStream(Stream, ImageData)
    then Result:=AddTexture(Name, ImageData, Clamp, MipMap)
    else Result:=0;
  {$IFDEF VSE_LOG}if Result=0 then Log(llError, 'TexMan: can''t load texture '+Name+' from stream');{$ENDIF}
  FreeImageData(ImageData);
end;*)

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
  {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: adding texture '+Name);{$ENDIF}
  Name:=UpperCase(Name);
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
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  if MipMap then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, Comps, Width, Height, Format, GL_UNSIGNED_BYTE, Data)
  end
  else begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, Comps, Width, Height, 0, Format, GL_UNSIGNED_BYTE, Data);
  end;
end;

function TTexMan.GetTex(Name: string; DontLogLostTex: Boolean = false): Cardinal;
var
  i: Integer;
begin
  Result:=0;
  Name:=UpperCase(Name);
  for i:=0 to FCount-1 do
    if FTextures[i].Name=Name then
    begin
      Result:=FTextures[i].ID;
      Exit;
    end;
  {$IFDEF VSE_LOG}if not DontLogLostTex
    then Log(llError, 'TexMan: can''t find texture '+Name);{$ENDIF}
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

initialization
  RegisterModule(TTexMan);

end.
