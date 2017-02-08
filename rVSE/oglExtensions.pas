unit oglExtensions;

interface

uses
  Windows, OpenGL;

type
  GLHandleARB = Integer;

function CheckExtension(const Ext: string): Boolean;
procedure ReadExtensions;

// Процедурки и константы отсутствующие в стандартном OpenGL.pas
const
// Textures
  GL_MAX_TEXTURE_UNITS_ARB = $84E2;
  GL_MAX_TEXTURE_SIZE      = $0D33;
  GL_CLAMP_TO_EDGE = $812F;
  GL_LUMINANCE8    = $8040;
  GL_RGB8          = $8051;
  GL_RGBA8         = $8058;
  GL_BGR           = $80E0;
  GL_BGRA          = $80E1;
  GL_ALPHA8        = $803C;
  GL_TEXTURE0_ARB  = $84C0;
  GL_TEXTURE1_ARB  = $84C1;
  GL_TEXTURE_MAX_ANISOTROPY_EXT     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

// AA
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB	= $2042;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;

// FBO
  GL_FRAMEBUFFER_EXT         = $8D40;
  GL_RENDERBUFFER_EXT        = $8D41;
  GL_DEPTH_COMPONENT24_ARB   = $81A6;
  GL_COLOR_ATTACHMENT0_EXT   = $8CE0;
  GL_DEPTH_ATTACHMENT_EXT    = $8D00;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
// Shaders
  GL_VERTEX_SHADER_ARB          = $8B31;
  GL_FRAGMENT_SHADER_ARB        = $8B30;
  GL_OBJECT_COMPILE_STATUS_ARB  = $8B81;
  GL_OBJECT_LINK_STATUS_ARB     = $8B82;
  GL_OBJECT_INFO_LOG_LENGTH_ARB = $8B84;
  GL_PROGRAM_OBJECT_ARB         = $8B40;
  GL_OBJECT_VALIDATE_STATUS_ARB = $8B83;

  GL_MAX_VERTEX_ATTRIBS_ARB               = $8869;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB    = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB               = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB   = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = $8B4D;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB  = $8B49; 
  GL_MAX_TEXTURE_COORDS_ARB               = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB          = $8872;

// VBO
  GL_NORMAL_ARRAY        = $8075;
  GL_COLOR_ARRAY         = $8076;
  GL_VERTEX_ARRAY        = $8074;
  GL_TEXTURE_COORD_ARRAY = $8078;

  GL_BUFFER_SIZE_ARB                                 = $8764;
  GL_BUFFER_USAGE_ARB                                = $8765;
  GL_ARRAY_BUFFER_ARB                                = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB                        = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB                        = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB                = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB                 = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB                 = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB                  = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB                  = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB          = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB              = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB        = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB         = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB                 = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB          = $889F;
  GL_READ_ONLY_ARB                                   = $88B8;
  GL_WRITE_ONLY_ARB                                  = $88B9;
  GL_READ_WRITE_ARB                                  = $88BA;
  GL_BUFFER_ACCESS_ARB                               = $88BB;
  GL_BUFFER_MAPPED_ARB                               = $88BC;
  GL_BUFFER_MAP_POINTER_ARB                          = $88BD;
  GL_STREAM_DRAW_ARB                                 = $88E0;
  GL_STREAM_READ_ARB                                 = $88E1;
  GL_STREAM_COPY_ARB                                 = $88E2;
  GL_STATIC_DRAW_ARB                                 = $88E4;
  GL_STATIC_READ_ARB                                 = $88E5;
  GL_STATIC_COPY_ARB                                 = $88E6;
  GL_DYNAMIC_DRAW_ARB                                = $88E8;
  GL_DYNAMIC_READ_ARB                                = $88E9;
  GL_DYNAMIC_COPY_ARB                                = $88EA;

  procedure glGenTextures(n: GLsizei; textures: PGLuint); stdcall; external opengl32;
  procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
  procedure glDeleteTextures(N: GLsizei; Textures: PGLuint); stdcall; external opengl32;
  function  glIsTexture(texture: GLuint): GLboolean; stdcall; external opengl32;
  procedure glCopyTexImage2D(target: GLEnum; level: GLint; internalFormat: GLEnum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall; external opengl32;

var
  glExtensionsString: string;
// VSync
  WGL_EXT_swap_control  : Boolean;
  wglSwapIntervalEXT    : function(interval: GLint): Boolean; stdcall;
  wglGetSwapIntervalEXT : function: GLint; stdcall;

// MultiTexture
  GL_ARB_multitexture      : Boolean;
  glActiveTextureARB       : procedure(texture: Cardinal); stdcall;
  glClientActiveTextureARB : procedure(texture: Cardinal); stdcall;
  glMaxTextureUnits, glMaxTextureSize, glMaxAnisotropy: Integer;

// FrameBuffer
  GL_EXT_framebuffer_object    : Boolean;
  glGenRenderbuffersEXT        : procedure(n: GLsizei; renderbuffers: PGLuint); stdcall;
  glDeleteRenderbuffersEXT     : procedure(n: GLsizei; const renderbuffers: PGLuint); stdcall;
  glBindRenderbufferEXT        : procedure(target: GLenum; renderbuffer: GLuint); stdcall;
  glRenderbufferStorageEXT     : procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); stdcall;
  glGenFramebuffersEXT         : procedure(n: GLsizei; framebuffers: PGLuint); stdcall;
  glDeleteFramebuffersEXT      : procedure(n: GLsizei; const framebuffers: PGLuint); stdcall;
  glBindFramebufferEXT         : procedure(target: GLenum; framebuffer: GLuint); stdcall;
  glFramebufferTexture2DEXT    : procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); stdcall;
  glFramebufferRenderbufferEXT : procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); stdcall;
  glCheckFramebufferStatusEXT  : function(target: GLenum): GLenum; stdcall;

// Shaders
  GL_ARB_shading_language_100   : Boolean;
  glDeleteObjectARB         : procedure(Obj: GLHandleARB); stdcall;
  glCreateProgramObjectARB  : function: GLHandleARB; stdcall;
  glCreateShaderObjectARB   : function(shaderType: GLEnum): GLHandleARB; stdcall;
  glShaderSourceARB         : procedure(shaderObj: GLHandleARB; count: GLSizei; src: Pointer; len: Pointer); stdcall;
  glAttachObjectARB         : procedure(programObj, shaderObj:GLhandleARB); stdcall;
  glLinkProgramARB          : procedure(programObj: GLHandleARB); stdcall;
  glUseProgramObjectARB     : procedure(programObj:GLHandleARB); stdcall;
  glCompileShaderARB        : function(shaderObj: GLHandleARB): GLboolean; stdcall;
  glGetObjectParameterivARB : procedure(Obj: GLHandleARB; pname: GLEnum; params: PGLuint); stdcall;
  glGetAttribLocationARB    : function(programObj: GLhandleARB; const char: PChar): GLInt; stdcall;
  glGetUniformLocationARB   : function(programObj:GLhandleARB; const char: PChar): GLInt; stdcall;
  glVertexAttrib1fARB       : procedure(index: GLuint; x: GLfloat); stdcall;
  glVertexAttrib2fARB       : procedure(index: GLuint; x, y: GLfloat); stdcall;
  glVertexAttrib3fARB       : procedure(index: GLuint; x, y, z: GLfloat); stdcall;
  glUniform1fARB            : procedure(location: GLint; v0 : GLfloat); stdcall;
  glUniform2fARB            : procedure(location: GLint; v0, v1: GLfloat); stdcall;
  glUniform3fARB            : procedure(location: GLint; v0, v1, v2: GLfloat); stdcall;
  glUniform4fARB            : procedure(location: GLint; v0, v1, v2, v3: GLfloat); stdcall;
  glUniform1iARB            : procedure(location: GLint; v0: GLint); stdcall;
  glGetInfoLogARB           : procedure(shaderObj: GLHandleARB; maxLength: glsizei; var length: glint; infoLog: PChar); stdcall;
  glGetHandleARB            : function(pname: GlEnum): GLHandleARB; stdcall;
  glValidateProgramARB      : procedure(programObj: GLhandleARB); stdcall;
  glMaxTextureImageUnits: Integer;

// Vertex Buffer Object
  GL_ARB_vertex_buffer_object : Boolean;
  glBindBufferARB    : procedure(target: GLenum; buffer: GLenum); stdcall;
  glDeleteBuffersARB : procedure(n: GLsizei; const buffers: PGLuint); stdcall;
  glGenBuffersARB    : procedure(n: GLsizei; buffers: PGLuint); stdcall;
  glBufferDataARB    : procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); stdcall;
  glBufferSubDataARB : procedure(target: GLenum; offset: GLsizei; size: GLsizei; const data: Pointer); stdcall;
  glMapBufferARB     : function(target: GLenum; access: GLenum): Pointer; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUnmapBufferARB   : function(target: GLenum): GLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}


  procedure glNormalPointer(type_: GLenum; stride: Integer; const P: Pointer); stdcall; external opengl32;
  procedure glColorPointer(size: Integer; _type: GLenum; stride: Integer; const _pointer: Pointer); stdcall; external opengl32;
  procedure glVertexPointer(size: Integer; _type: GLenum; stride: Integer; const _pointer: Pointer); stdcall; external opengl32;
  procedure glTexCoordPointer(size: Integer; _type: GLenum; stride: Integer; const _pointer: Pointer); stdcall; external opengl32;

  procedure glInterleavedArrays  (format: GLenum; stride: GLsizei; const _pointer: Pointer); stdcall; external opengl32;
  procedure glEnableClientState  (_array: GLenum); stdcall; external opengl32;
  procedure glDisableClientState (_array: GLenum); stdcall; external opengl32;
  procedure glDrawElements       (mode: GLenum; count: GLsizei; _type: GLenum; const indices: Pointer); stdcall; external opengl32;
  procedure glDrawArrays         (mode: GLenum; first: GLint; count: GLsizei); stdcall; external opengl32;

implementation

function CheckExtension(const Ext: string): Boolean;
begin
  Result := Pos(Ext, glExtensionsString) <> 0;
end;

procedure ReadExtensions;
begin
  glExtensionsString := glGetString(GL_EXTENSIONS);

  // Получаем адреса дополнительных процедур OpenGL
  glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @glMaxTextureUnits);
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @glMaxTextureSize);
  glGetIntegerv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @glMaxAnisotropy);

  // Управление вертикальной синхронизацией
  WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
  if WGL_EXT_swap_control then
  begin
    wglSwapIntervalEXT    := wglGetProcAddress('wglSwapIntervalEXT');
    wglGetSwapIntervalEXT := wglGetProcAddress('wglGetSwapIntervalEXT');
  end;

  // Мультитекстурирование
  GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture');
  if GL_ARB_multitexture then
  begin
    glActiveTextureARB       := wglGetProcAddress('glActiveTextureARB');
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
  end;

  // рендер в текстуру
  GL_EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
  if GL_EXT_framebuffer_object then
  begin
    glGenRenderbuffersEXT        := wglGetProcAddress('glGenRenderbuffersEXT');
    glDeleteRenderbuffersEXT     := wglGetProcAddress('glDeleteRenderbuffersEXT');
    glBindRenderbufferEXT        := wglGetProcAddress('glBindRenderbufferEXT');
    glRenderbufferStorageEXT     := wglGetProcAddress('glRenderbufferStorageEXT');
    glGenFramebuffersEXT         := wglGetProcAddress('glGenFramebuffersEXT');
    glDeleteFramebuffersEXT      := wglGetProcAddress('glDeleteFramebuffersEXT');
    glBindFramebufferEXT         := wglGetProcAddress('glBindFramebufferEXT');
    glFramebufferTexture2DEXT    := wglGetProcAddress('glFramebufferTexture2DEXT');
    glFramebufferRenderbufferEXT := wglGetProcAddress('glFramebufferRenderbufferEXT');
    glCheckFramebufferStatusEXT  := wglGetProcAddress('glCheckFramebufferStatusEXT');
  end;

  // шейдеры
  GL_ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
  if GL_ARB_shading_language_100 then
  begin
    glDeleteObjectARB         := wglGetProcAddress('glDeleteObjectARB');
    glCreateProgramObjectARB  := wglGetProcAddress('glCreateProgramObjectARB');
    glCreateShaderObjectARB   := wglGetProcAddress('glCreateShaderObjectARB');
    glShaderSourceARB         := wglGetProcAddress('glShaderSourceARB');
    glAttachObjectARB         := wglGetProcAddress('glAttachObjectARB');
    glLinkProgramARB          := wglGetProcAddress('glLinkProgramARB');
    glUseProgramObjectARB     := wglGetProcAddress('glUseProgramObjectARB');
    glCompileShaderARB        := wglGetProcAddress('glCompileShaderARB');
    glGetObjectParameterivARB := wglGetProcAddress('glGetObjectParameterivARB');
    glGetAttribLocationARB    := wglGetProcAddress('glGetAttribLocationARB');
    glGetUniformLocationARB   := wglGetProcAddress('glGetUniformLocationARB');
  // attribs
    glVertexAttrib1fARB := wglGetProcAddress('glVertexAttrib1fARB');
    glVertexAttrib2fARB := wglGetProcAddress('glVertexAttrib2fARB');
    glVertexAttrib3fARB := wglGetProcAddress('glVertexAttrib3fARB');
  // uniforms
    glUniform1fARB := wglGetProcAddress('glUniform1fARB');
    glUniform2fARB := wglGetProcAddress('glUniform2fARB');
    glUniform3fARB := wglGetProcAddress('glUniform3fARB');
    glUniform1iARB := wglGetProcAddress('glUniform1iARB');

    glGetInfoLogARB := wglGetProcAddress('glGetInfoLogARB');
    glGetHandleARB := wglGetProcAddress('glGetHandleARB');
    glValidateProgramARB := wglGetProcAddress('glValidateProgramARB');

    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB, @glMaxTextureImageUnits);
  end;

  // VBO :)
  GL_ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
  if GL_ARB_vertex_buffer_object then
  begin
    glBindBufferARB    := wglGetProcAddress('glBindBufferARB');
    glDeleteBuffersARB := wglGetProcAddress('glDeleteBuffersARB');
    glGenBuffersARB    := wglGetProcAddress('glGenBuffersARB');
    glBufferDataARB    := wglGetProcAddress('glBufferDataARB');
    glBufferSubDataARB := wglGetProcAddress('glBufferSubDataARB');
    glMapBufferARB     := wglGetProcAddress('glMapBufferARB');
    glUnmapBufferARB   := wglGetProcAddress('glUnmapBufferARB');
  end;
end;

end.
