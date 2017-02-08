unit VSEArrayBuffer;

interface

uses
  Windows, AvL, avlUtils, OpenGL, oglExtensions, avlVectors;

type
  TVertex=packed record
    Vertex: TVector3D;
    Normal: TVector3D;
    TexCoord: TVector2D;
  end;
  TFaceI=packed record
    Vert1, Vert2, Vert3: Integer;
  end;
  TFaceW=packed record
    Vert1, Vert2, Vert3: Word;
  end;
  PVertexArray=^TVertexArray;
  TVertexArray=array[0..MaxInt div SizeOf(TVertex)-1] of TVertex;
  TArrayBuffer=class
  private
    FID: Cardinal;
    FTarget: GLenum;
    FData: Pointer;
    FSize: Integer;
  public
    constructor Create(UseVBO: Boolean = true); //Create array buffer; UseVBO: use VBO if supported
    destructor Destroy; override;
    procedure Map(Access: GLenum); //Map buffer to system memory
    function  Unmap: Boolean; //Unmap buffer, returns True if successful
    procedure Bind(Target: GLenum); //Bind buffer to OpenGL, unmaps buffer
    procedure Unbind; //Unbind buffer
    procedure SetData(Data: Pointer; Size: Integer; Usage: GLenum = GL_STATIC_DRAW_ARB); //Set buffer data; Data: pointer to new buffer data; Size: size of data
    function  SetSubData(SubData: Pointer; Offset, Size: Integer): Boolean; //Set part of buffer data; SubData: pointer to new data; Offset: offset of data in buffer; Size: size of data
    property Data: Pointer read FData; //Pointer to buffer data, valid only when buffer mapped
    property Size: Integer read FSize; //Size of buffer
  end;

implementation

{$IFDEF VSE_LOG}uses VSELog;{$ENDIF}

constructor TArrayBuffer.Create(UseVBO: Boolean);
begin
  inherited Create;
  if UseVBO and GL_ARB_vertex_buffer_object then glGenBuffersARB(1, @FID);
end;

destructor TArrayBuffer.Destroy;
begin
  Unmap;
  Unbind;
  if FID<>0 then glDeleteBuffersARB(1, @FID);
  if Assigned(FData) then FreeMem(FData, FSize);
  inherited Destroy;
end;

procedure TArrayBuffer.Map(Access: GLenum);
begin
  if (FID=0) or (FTarget=0) then Exit;
  FData:=glMapBufferARB(FTarget, Access);
end;

function TArrayBuffer.Unmap: Boolean;
begin
  Result:=FID=0;
  if (FID<>0) and Assigned(FData) then
  begin
    Result:=glUnmapBufferARB(FTarget);
    FData:=nil;
  end;
end;

procedure TArrayBuffer.Bind(Target: GLenum);
begin
  if FID=0 then Exit;
  Unmap;
  glBindBufferARB(Target, FID);
  FTarget:=Target;
end;

procedure TArrayBuffer.Unbind;
begin
  if (FTarget<>0) and (FID<>0) then glBindBufferARB(FTarget, 0);
end;

procedure TArrayBuffer.SetData(Data: Pointer; Size: Integer; Usage: GLenum);
begin
  Unmap;
  if FID=0 then
  begin
    FreeMem(FData, FSize);
    GetMem(FData, Size);
    FSize:=Size;
    if Assigned(Data) then
      Move(Data^, FData^, Size);
  end
  else begin
    if FTarget=0 then FTarget:=GL_ARRAY_BUFFER_ARB;
    Bind(FTarget);
    glBufferDataARB(FTarget, Size, Data, Usage);
    FData:=nil;
    FSize:=Size;
    Unbind;
  end;
end;

function TArrayBuffer.SetSubData(SubData: Pointer; Offset, Size: Integer): Boolean;
begin
  Result:=(Offset+Size)<FSize;
  if not Result then Exit;
  Unmap;
  if FID=0
    then Move(Data^, IncPtr(FData, Offset)^, Size)
  else begin
    if FTarget=0 then FTarget:=GL_ARRAY_BUFFER_ARB;
    Bind(FTarget);
    glBufferSubDataARB(FTarget, Offset, Size, Data);
    Unbind;
  end;
end;

end.
