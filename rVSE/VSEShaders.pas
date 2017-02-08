unit VSEShaders;

interface

uses
  Windows, AvL, avlUtils, avlClasses, OpenGL, oglExtensions;

type
  TShader=class;
  TShaderAttrib=class(TDLCListItem)
  private
    FHandle: Integer;
    FShader: TShader;
    FName: string;
  public
    constructor Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    property Handle: Integer read FHandle;
    property Name: string read FName;
  end;
  TShaderUniform=class(TDLCListItem)
  private
    FHandle: Integer;
    FShader: TShader;
    FName: string;
  public
    constructor Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    procedure Value(x, y, z, w: Single); overload;
    procedure Value(i: Integer); overload;
    property Handle: Integer read FHandle;
    property Name: string read FName;
  end;
  TShader=class
  private
    FHandle: Integer;
    FVaries: TDLCListItem;
    function  GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function  GetValid: Boolean;
    function  GetInfoLog: string;
    function  CheckAttrib(Item: TDLCListItem; Data: Integer): Boolean;
    function  CheckUniform(Item: TDLCListItem; Data: Integer): Boolean;
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
    function GetAttrib(const Name: string): TShaderAttrib;
    function GetUniform(const Name: string): TShaderUniform;
    property Handle: Integer read FHandle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Valid: Boolean read GetValid;
    property InfoLog: string read GetInfoLog;
  end;

implementation

uses
  {$IFDEF VSE_LOG}VSELog, {$ENDIF}VSECore;

{TShaderAttrib}

constructor TShaderAttrib.Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
begin
  inherited Create(PrevItem);
  FName:=Name;
  FHandle:=glGetAttribLocationARB(ShaderHandle, PChar(Name));
end;

procedure TShaderAttrib.Value(x: Single);
begin
  glVertexAttrib1fARB(FHandle, x);
end;

procedure TShaderAttrib.Value(x, y: Single);
begin
  glVertexAttrib2fARB(FHandle, x, y);
end;

procedure TShaderAttrib.Value(x, y, z: Single);
begin
  glVertexAttrib3fARB(FHandle, x, y, z);
end;

{TShaderUniform}

constructor TShaderUniform.Create(PrevItem: TDLCListItem; ShaderHandle: Integer; const Name: string);
begin
  inherited Create(PrevItem);
  FName:=Name;
  FHandle:=glGetUniformLocationARB(ShaderHandle, PChar(Name));
end;

procedure TShaderUniform.Value(x: Single);
begin
  glUniform1fARB(FHandle, x);
end;

procedure TShaderUniform.Value(x, y: Single);
begin
  glUniform2fARB(FHandle, x, y);
end;

procedure TShaderUniform.Value(x, y, z: Single);
begin
  glUniform3fARB(FHandle, x, y, z);
end;

procedure TShaderUniform.Value(x, y, z, w: Single);
begin
  glUniform4fARB(FHandle, x, y, z, w);
end;

procedure TShaderUniform.Value(i: Integer);
begin
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
var
  i: Integer;
begin
  if Assigned(FVaries) then FVaries.ClearList;
  FAN(FVaries);
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

function TShader.GetAttrib(const Name: string): TShaderAttrib;
begin
  if Assigned(FVaries) then
  begin
    Result:=TShaderAttrib(FVaries.FindItem(CheckAttrib, Integer(Name)));
    if Assigned(Result)
      then Result.AddRef
      else Result:=TShaderAttrib.Create(FVaries, FHandle, Name);
  end
  else begin
    Result:=TShaderAttrib.Create(FVaries, FHandle, Name);
    FVaries:=Result;
  end;
end;

function TShader.GetUniform(const Name: string): TShaderUniform;
begin
  if Assigned(FVaries) then
  begin
    Result:=TShaderUniform(FVaries.FindItem(CheckUniform, Integer(Name)));
    if Assigned(Result)
      then Result.AddRef
      else Result:=TShaderUniform.Create(FVaries, FHandle, Name);
  end
  else begin
    Result:=TShaderUniform.Create(FVaries, FHandle, Name);
    FVaries:=Result;
  end;
end;

{protected}

function TShader.Compile(const Prog: string; ObjType: Integer): Boolean;
{$IFDEF VSE_LOG}const
  ShaderType: array[false..true] of string=('Vertex shader', 'Fragment shader');{$ENDIF}
var
  ShTemp: Integer;
  P: PChar;

  {$IFDEF VSE_LOG}procedure CompileLog;
  var
    LogLen: Integer;
    LogStr: string;
  begin
    glGetObjectParameterivARB(ShTemp, GL_OBJECT_INFO_LOG_LENGTH_ARB, @LogLen);
    if (glGetError<>GL_NO_ERROR) or (LogLen<1) then Exit;
    SetLength(LogStr, LogLen);
    glGetInfoLogARB(ShTemp, LogLen, LogLen, PChar(LogStr));
    SetLength(LogStr, LogLen);
    Log(llInfo, LogStr);
  end; {$ENDIF}

begin
  Result:=false;
  if Prog='' then Exit;
  P:=PChar(Prog);
  ShTemp:=glCreateShaderObjectARB(ObjType);
  glShaderSourceARB(ShTemp, 1, @P, nil);
  glCompileShaderARB(ShTemp);
  if Error(ShTemp, GL_OBJECT_COMPILE_STATUS_ARB) then
  begin
    {$IFDEF VSE_LOG}Log(llError, ShaderType[ObjType=GL_FRAGMENT_SHADER_ARB]+' compilation failed');
    CompileLog;{$ENDIF}
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
var
  LogLen, Written: Integer;
begin
  Result:='';
  glGetObjectParameterivARB(FHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @LogLen);
  if LogLen<1 then Exit;
  SetLength(Result, LogLen);
  glGetInfoLogARB(FHandle, LogLen, Written, PChar(Result));
  SetLength(Result, Written);
end;

function TShader.CheckAttrib(Item: TDLCListItem; Data: Integer): Boolean;
begin
  if Item is TShaderAttrib
    then Result:=TShaderAttrib(Item).Name=string(Data);
end;

function TShader.CheckUniform(Item: TDLCListItem; Data: Integer): Boolean;
begin
  if Item is TShaderUniform
    then Result:=TShaderUniform(Item).Name=string(Data);
end;

end.
