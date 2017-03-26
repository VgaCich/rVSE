unit VSEShaders;

interface

uses
  Windows, AvL, avlUtils, avlClasses, OpenGL, oglExtensions;

type
  TShader=class;
  TShaderVariable=class(TDLCListItem)
  protected
    FHandle: Integer;
    FName: string;
  public
    constructor Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string); virtual;
    property Name: string read FName;
  end;
  CShaderVariable=class of TShaderVariable;
  TShaderAttrib=class(TShaderVariable)
  private
    FHandle: Integer;
  public
    constructor Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string); override;
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    procedure ArrayPtr(size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; const data: Pointer);
    procedure EnableArray;
    procedure DisableArray;
    property Handle: Integer read FHandle;
  end;
  TShaderUniform=class(TShaderVariable)
  private
    FHandle: Integer;
  public
    constructor Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string); override;
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    procedure Value(x, y, z, w: Single); overload;
    procedure Value(i: Integer); overload;
    property Handle: Integer read FHandle;
  end;
  TShader=class
  private
    FHandle: Integer;
    FVariables: TShaderVariable;
    function  GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function  GetValid: Boolean;
    function  GetInfoLog: string;
    function  ObjectInfoLog(Obj: Integer): string;
    function CheckVariable(Item: TDLCListItem; Data: Integer): Boolean;
    function  GetVariable(const Name: string; Cls: CShaderVariable): TShaderVariable;
    function  GetAttrib(const Name: string): TShaderAttrib;
    function  GetUniform(const Name: string): TShaderUniform;
  protected
    function Compile(const Prog: string; ObjType: Integer): Boolean;
    function Error(Handle: Integer; Param: DWORD): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Data: TStream): Boolean;
    function AddVP(const VP: string): Boolean;
    function AddFP(const FP: string): Boolean;
    function Link: Boolean;
    property Attrib[const Name: string]: TShaderAttrib read GetAttrib;
    property Uniform[const Name: string]: TShaderUniform read GetUniform; default;
    property Handle: Integer read FHandle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Valid: Boolean read GetValid;
    property InfoLog: string read GetInfoLog;
  end;

implementation

uses
  {$IFDEF VSE_LOG}VSELog, {$ENDIF}VSECore;

type
  PFindRec=^TFindRec;
  TFindRec=record
    Name: string;
    Cls: CShaderVariable;
  end;

{TShaderVariable}

constructor TShaderVariable.Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
begin
  inherited Create(PrevItem);
  FName:=Name;
end;

{TShaderAttrib}

constructor TShaderAttrib.Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
begin
  inherited;
  FHandle:=glGetAttribLocationARB(ShaderHandle, PChar(Name));
end;

procedure TShaderAttrib.Value(x: Single);
begin
  if FHandle >= 0 then
    glVertexAttrib1fARB(FHandle, x);
end;

procedure TShaderAttrib.Value(x, y: Single);
begin
  if FHandle >= 0 then
    glVertexAttrib2fARB(FHandle, x, y);
end;

procedure TShaderAttrib.Value(x, y, z: Single);
begin
  if FHandle >= 0 then
    glVertexAttrib3fARB(FHandle, x, y, z);
end;

procedure TShaderAttrib.ArrayPtr(size: GLint; type_: GLenum; normalized: GLboolean; stride: GLsizei; const data: Pointer);
begin
  if FHandle >= 0 then
    glVertexAttribPointerARB(FHandle, size, type_, normalized, stride, data);
end;

procedure TShaderAttrib.EnableArray;
begin
  if FHandle >= 0 then
    glEnableVertexAttribArrayARB(FHandle);
end;

procedure TShaderAttrib.DisableArray;
begin
  if FHandle >= 0 then
    glDisableVertexAttribArrayARB(FHandle);
end;

{TShaderUniform}

constructor TShaderUniform.Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
begin
  inherited;
  FHandle:=glGetUniformLocationARB(ShaderHandle, PChar(Name));
end;

procedure TShaderUniform.Value(x: Single);
begin
  if FHandle >= 0 then
    glUniform1fARB(FHandle, x);
end;

procedure TShaderUniform.Value(x, y: Single);
begin
  if FHandle >= 0 then
    glUniform2fARB(FHandle, x, y);
end;

procedure TShaderUniform.Value(x, y, z: Single);
begin
  if FHandle >= 0 then
    glUniform3fARB(FHandle, x, y, z);
end;

procedure TShaderUniform.Value(x, y, z, w: Single);
begin
  if FHandle >= 0 then
    glUniform4fARB(FHandle, x, y, z, w);
end;

procedure TShaderUniform.Value(i: Integer);
begin
  if FHandle >= 0 then
    glUniform1iARB(FHandle, i);
end;

{TShader}
{public}

constructor TShader.Create;
begin
  if not GL_ARB_shading_language_100 then raise Exception.Create('Shader.Create: GL_ARB_shading_language_100 not supported');
  inherited Create;
  FHandle:=glCreateProgramObjectARB;
  {$IFDEF VSE_LOG}if FHandle=0
    then Log(llError, 'Shader.Create: cannot create shader');{$ENDIF}
end;

destructor TShader.Destroy;
begin
  if Assigned(FVariables) then FVariables.ClearList;
  FAN(FVariables);
  glDeleteObjectARB(FHandle);
  inherited Destroy;
end;

function TShader.Load(Data: TStream): Boolean;
var
  S, Prog, LastHeader: string;
  List: TStringList;
  i: Integer;

  function Add: Boolean;
  begin
    if LastHeader<>'' then
    begin
      if LastHeader='vertex_shader' then Result:=AddVP(Prog)
      else if LastHeader='fragment_shader' then Result:=AddFP(Prog)
      else begin
        Result:=false;
        {$IFDEF VSE_LOG}Log(llError, 'Shader.Load: unknown object header "'+LastHeader+'"');{$ENDIF}
      end;
    end
      else Result:=true;
  end;

begin
  Result:=false;
  if not Assigned(Data) then Exit;
  List:=TStringList.Create;
  try
    List.LoadFromStream(Data);
    Prog:='';
    LastHeader:='';
    for i:=0 to List.Count-1 do
    begin
      S:=Trim(List[i]);
      if (Length(S)>0) and (S[1]='!') then
      begin
        if not Add then Exit;
        LastHeader:=LowerCase(Copy(S, 2, MaxInt));
        Prog:='';
      end
        else Prog:=Prog+List[i]+#13#10;
    end;
    if not Add then Exit;
    Result:=true;
  finally
    FAN(List);
  end;
end;

function TShader.AddVP(const VP: string): Boolean;
begin
  Result:=Compile(VP, GL_VERTEX_SHADER_ARB);
end;

function TShader.AddFP(const FP: string): Boolean;
begin
  Result:=Compile(FP, GL_FRAGMENT_SHADER_ARB);
end;

function TShader.Link: Boolean;
begin
  glLinkProgramARB(FHandle);
  Result:=not Error(FHandle, GL_OBJECT_LINK_STATUS_ARB);
  {$IFDEF VSE_LOG}if not Result
    then Log(llError, 'Shader.Link: cannot link shader');{$ENDIF}
end;

{protected}

function TShader.Compile(const Prog: string; ObjType: Integer): Boolean;
{$IFDEF VSE_LOG}const
  ShaderType: array[false..true] of string=('Vertex shader', 'Fragment shader');{$ENDIF}
var
  ShTemp: Integer;
  P: PChar;
begin
  Result:=false;
  if Prog='' then Exit;
  P:=PChar(Prog);
  ShTemp:=glCreateShaderObjectARB(ObjType);
  glShaderSourceARB(ShTemp, 1, @P, nil);
  glCompileShaderARB(ShTemp);
  if Error(ShTemp, GL_OBJECT_COMPILE_STATUS_ARB) then
  begin
    {$IFDEF VSE_LOG}LogMultiline(llError, ShaderType[ObjType=GL_FRAGMENT_SHADER_ARB]+' compilation failed'#13+ObjectInfoLog(ShTemp));{$ENDIF}
    Exit;
  end;
  glAttachObjectARB(FHandle, ShTemp);
  glDeleteObjectARB(ShTemp);
  Result:=True;
end;

function TShader.Error(Handle: Integer; Param: DWORD): Boolean;
var
  Status: Integer;
begin
  glGetObjectParameterivARB(Handle, Param, @Status);
  Result:=Status=0;
end;

{private}

function TShader.GetEnabled: Boolean;
begin
  Result:=glGetHandleARB(GL_PROGRAM_OBJECT_ARB)=FHandle;
end;

procedure TShader.SetEnabled(Value: Boolean);
begin
  if Value
    then glUseProgramObjectARB(FHandle)
    else glUseProgramObjectARB(0);
end;

function TShader.GetValid: Boolean;
begin
  glValidateProgramARB(FHandle);
  Result:=not Error(FHandle, GL_OBJECT_VALIDATE_STATUS_ARB);
end;

function TShader.GetInfoLog: string;
begin
  Result:=ObjectInfoLog(FHandle);
end;

function TShader.ObjectInfoLog(Obj: Integer): string;
var
  LogLen, Written: Integer;
begin
  Result:='';
  glGetObjectParameterivARB(Obj, GL_OBJECT_INFO_LOG_LENGTH_ARB, @LogLen);
  if (glGetError<>GL_NO_ERROR) or (LogLen<1) then Exit;
  SetLength(Result, LogLen);
  glGetInfoLogARB(Obj, LogLen, Written, PChar(Result));
  SetLength(Result, Written);
end;

function TShader.CheckVariable(Item: TDLCListItem; Data: Integer): Boolean;
begin
  with PFindRec(Data)^ do
    Result:=(Item is Cls) and (TShaderVariable(Item).Name=Name);
end;

function TShader.GetVariable(const Name: string; Cls: CShaderVariable): TShaderVariable;
var
  FindRec: TFindRec;
begin
  Result:=nil;
  FindRec.Name:=Name;
  FindRec.Cls:=Cls;
  if Assigned(FVariables) then
    Result:=TShaderVariable(FVariables.FindItem(CheckVariable, Integer(@FindRec)));
  if not Assigned(Result) then
    Result:=Cls.Create(FVariables, FHandle, Name);
  if not Assigned(FVariables) then
    FVariables:=Result;
end;

function TShader.GetAttrib(const Name: string): TShaderAttrib;
begin
  Result:=GetVariable(Name, TShaderAttrib) as TShaderAttrib;
end;

function TShader.GetUniform(const Name: string): TShaderUniform;
begin
  Result:=GetVariable(Name, TShaderUniform) as TShaderUniform;
end;

end.
