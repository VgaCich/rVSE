unit VSEConsole;

{$IFNDEF VSE_CONSOLE}{$ERROR Please don't include VSEConsole unit without VSE_CONSOLE defined}{$ENDIF}

interface

uses
  Windows, AvL, avlSyncObjs, avlMath, avlUtils, OpenGL, VSEOpenGLExt, VSECore
  {$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TOnConsoleCommand = function(Sender: TObject; Args: array of const): Boolean of object; // Console command handler
  TOnConsoleExecute = function(Sender: TObject; const CommandLine: string): Boolean of object; // Console commands hook
  TConsoleVarType = (cvInt, cvBool, cvEnum, cvFloat, cvString); //Variable type
  TConsoleCommandArgument = class // internally used
  private
    FName: string;
    FOptional: Boolean;
    FValue: TVarRec;
    function GetNextOption(var Options: string): string;
    function GetArgument(var CommandLine: string): string;
    function GetDescription: string;
    function GetType: string; virtual; abstract;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; virtual; abstract;
    property Value: TVarRec read FValue;
    property Name: string read FName;
    property Description: string read GetDescription;
  end;
  TConsoleCommand = class // internally used
  private
    FName: string;
    FArguments: array of TConsoleCommandArgument;
    FHandler: TOnConsoleCommand;
    FPrev, FNext: TConsoleCommand;
    function CreateArgumentParser(ArgumentDesc: string): TConsoleCommandArgument;
    function GetDescription: string;
  public
    constructor Create(Prev: TConsoleCommand; CmdDesc: string; Handler: TOnConsoleCommand);
    destructor Destroy; override;
    function Execute(CommandLine: string): Boolean;
    function FindCommand(Name: string): TConsoleCommand;
    property Handler: TOnConsoleCommand read FHandler;
    property Name: string read FName;
    property Description: string read GetDescription;
    property Prev: TConsoleCommand read FPrev;
    property Next: TConsoleCommand read FNext;
  end;
  TConsole = class
  private
    FLog: TStringList;
    {$IFDEF VSE_LOG}
    FLogLevel: TLogLevel;
    FLogBuffer: TStringList;
    FLogBufferLock: TCriticalSection;
    FLogBufferEvent: TEvent;
    {$ENDIF}
    FGoTo: string;
    FOnExecute: TOnConsoleExecute;
    FCommands: TConsoleCommand;
    {$IFDEF VSE_LOG}procedure UpdateLog;{$ENDIF}
    function GetCommand(const CmdDesc: string): TOnConsoleCommand;
    procedure SetCommand(const CmdDesc: string; Value: TOnConsoleCommand);
    function CmdListHandler(Sender: TObject; Args: array of const): Boolean;
    function EchoHandler(Sender: TObject; Args: array of const): Boolean;
    function ExecHandler(Sender: TObject; Args: array of const): Boolean;
    function ExistHandler(Sender: TObject; Args: array of const): Boolean;
    function GotoHandler(Sender: TObject; Args: array of const): Boolean;
    function IfHandler(Sender: TObject; Args: array of const): Boolean;
    function ConVarIntHandler(Sender: TObject; Args: array of const): Boolean;
    function ConVarByteHandler(Sender: TObject; Args: array of const): Boolean;
    function ConVarFloatHandler(Sender: TObject; Args: array of const): Boolean;
    function ConVarStrHandler(Sender: TObject; Args: array of const): Boolean;
    {$IFDEF VSE_LOG}function LogLevelHandler(Sender: TObject; Args: array of const): Boolean;{$ENDIF}
    class function GetCommandName(CommandLine: string): string;
  public
    constructor Create; // internally used
    destructor Destroy; override; // internally used
    procedure Update; // internally used
    procedure WriteLn(const Line: string = ''); // Write line to console
    function Execute(CommandLine: string): Boolean; // Execute command; returns true if successful
    function GetCommands(Prefix: string): TStringList; // Get list of commands, starts with Prefix
    function GetCommandDescription(const Command: string): string; // Get description of a command
    function GetConVarHandler(const Variable; VarType: TConsoleVarType): TOnConsoleCommand; //Get command handler for console variable
    property Log: TStringList read FLog; //Console log; do not modify
    property OnCommand[const CmdDesc: string]: TOnConsoleCommand read GetCommand write SetCommand; default; // Command handlers; assign nil to delete command; see FmtDocs for commands description language
    property OnExecute: TOnConsoleExecute read FOnExecute write FOnExecute; // Console commands hook
  end;

var
  Console: TConsole; // Console interface

const
  PostfixWarning = #10;
  PostfixError = #13;

implementation

const
  SConsoleVariableTypeMismatch = 'Console: console variable type mismatch';
  vtInvalid = 255;

type
  TCCAInteger = class(TConsoleCommandArgument)
  private
    FMin, FMax: Integer;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAFloat = class(TConsoleCommandArgument)
  private
    FMin, FMax: Single;
    FValueBuffer: Extended;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAString = class(TConsoleCommandArgument)
  private
    FFullLine: Boolean;
    FValueBuffer: string;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAEnum = class(TConsoleCommandArgument)
  private
    FValues: array of string;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;

{$IFDEF VSE_LOG}
procedure LogUpdateHandler(Level: TLogLevel; const S: string);
begin
  if Assigned(Console) and (Level >= Console.FLogLevel) then
  begin
    Console.FLogBufferLock.Acquire;
    case Level of
      llWarning: Console.FLogBuffer.Add('Warning: ' + S + PostfixWarning);
      llError: Console.FLogBuffer.Add('Error: ' + S + PostfixError);
      else Console.FLogBuffer.Add(S);
    end;
    Console.FLogBufferLock.Release;
    Console.FLogBufferEvent.SetEvent;
  end;
end;
{$ENDIF}

{ TConsole }

constructor TConsole.Create;
{$IFDEF VSE_LOG}
var
  Level: TLogLevel;
  Levels: string;
{$ENDIF}
begin
  inherited;
  FLog := TStringList.Create;
  FCommands := TConsoleCommand.Create(nil, 'exec file=s', ExecHandler);
  OnCommand['cmdlist ?prefix=s'] := CmdListHandler;
  OnCommand['echo msg=s*'] := EchoHandler;
  OnCommand['exist file=s'] := ExistHandler;
  OnCommand['goto lbl=s'] := GotoHandler;
  OnCommand['if chk=s cmd=s*'] := IfHandler;
  OnCommand['ifnot chk=s cmd=s*'] := IfHandler;
  {$IFDEF VSE_LOG}
  FLogLevel := VSELog.LogLevel;
  Levels := '';
  for Level := Low(TLogLevel) to High(TLogLevel) do
    Levels := Levels + ':' + LogLevelNames[Level];
  Delete(Levels, 1, 1);
  OnCommand['conloglevel ?level=e' + Levels] := LogLevelHandler;
  VSELog.Log(llInfo, 'Console: Create');
  FLogBuffer := TStringList.Create;
  FLogBufferLock := TCriticalSection.Create;
  FLogBufferEvent := TEvent.Create(nil, false, false, '');
  WriteLn('Console: Create');
  {$ENDIF}
end;

destructor TConsole.Destroy;
begin
  {$IFDEF VSE_LOG}
  VSELog.Log(llInfo, 'Console: Destroy');
  UpdateLog;
  {$IFDEF VSE_DEBUG}FLog.SaveToFile(ChangeFileExt(FullExeName, '.console.log'));{$ENDIF}
  FAN(FLogBufferLock);
  FAN(FLogBufferEvent);
  FAN(FLogBuffer);
  {$ENDIF}
  while Assigned(FCommands.Next) do FCommands.Next.Free;
  FAN(FCommands);
  FAN(FLog);
  inherited;
end;

procedure TConsole.Update;
begin
  {$IFDEF VSE_LOG}UpdateLog;{$ENDIF}
end;

procedure TConsole.WriteLn(const Line: string = '');
begin
  {$IFDEF VSE_LOG}UpdateLog;{$ENDIF}
  FLog.Add(Line);
end;

function TConsole.Execute(CommandLine: string): Boolean;
var
  Command: TConsoleCommand;
begin
  Result := false;
  CommandLine := Trim(CommandLine);
  if CommandLine = '' then Exit;
  if Assigned(FOnExecute) then
    try
      if not FOnExecute(Self, CommandLine) then Exit;
    except
      {$IFDEF VSE_LOG}LogException('in Console.OnExecute handler');{$ENDIF}
      {$IFNDEF VSE_DEBUG}Core.StopEngine(StopUserException);{$ENDIF}
    end;
  Command := FCommands.FindCommand(GetCommandName(CommandLine));
  if not Assigned(Command) then
  begin
    WriteLn('Command "' + GetCommandName(CommandLine) + '" not found' + PostfixError);
    Exit;
  end;
  Result := Command.Execute(CommandLine);
end;

function TConsole.GetCommands(Prefix: string): TStringList;
var
  CurCmd: TConsoleCommand;
begin
  Result := TStringList.Create;
  CurCmd := FCommands;
  Prefix := LowerCase(Prefix);
  while Assigned(CurCmd) do
  begin
    if Prefix = Copy(CurCmd.Name, 1, Length(Prefix))
      then Result.Add(CurCmd.Name);
    CurCmd := CurCmd.Next;
  end;
  Result.Sort;
end;

function TConsole.GetCommandDescription(const Command: string): string;
var
  Cmd: TConsoleCommand;
begin
  Cmd := FCommands.FindCommand(Command);
  if Assigned(Cmd) then
    Result := Cmd.Description
  else
    Result := '';
end;

{$IFDEF VSE_LOG}
procedure TConsole.UpdateLog;
var
  i: Integer;
begin
  if Assigned(FLogBufferEvent) and (FLogBufferEvent.WaitFor(0) = wrSignaled) then
  begin
    FLogBufferLock.Acquire;
    try
      for i := 0 to FLogBuffer.Count-1 do
        WriteLn(FLogBuffer[i]);
      FLogBuffer.Clear;
    finally
      FLogBufferLock.Release;
    end;
  end;
end;
{$ENDIF}

function TConsole.GetCommand(const CmdDesc: string): TOnConsoleCommand;
var
  Command: TConsoleCommand;
begin
  Command := FCommands.FindCommand(GetCommandName(CmdDesc));
  if Assigned(Command)
    then Result := Command.Handler
    else Result := nil;
end;

function TConsole.GetConVarHandler(const Variable; VarType: TConsoleVarType): TOnConsoleCommand;
begin
  case VarType of
    cvInt: Result := ConVarIntHandler;
    cvBool, cvEnum: Result := ConVarByteHandler;
    cvFloat: Result := ConVarFloatHandler;
    cvString: Result := ConVarStrHandler;
  end;
  TMethod(Result).Data := @Variable;
end;

procedure TConsole.SetCommand(const CmdDesc: string; Value: TOnConsoleCommand);
var
  Command: TConsoleCommand;
begin
  Command := FCommands.FindCommand(GetCommandName(CmdDesc));
  if Command <> FCommands then
  begin
    Command.Free;
    if Assigned(Value) then TConsoleCommand.Create(FCommands, CmdDesc, Value);
  end;
end;

function TConsole.CmdListHandler(Sender: TObject; Args: array of const): Boolean;
var
  Prefix: string;
  Commands: TStringList;
  i: Integer;
begin
  Result := false;
  if Length(Args) > 1
    then Prefix := string(Args[1].VAnsiString)
    else Prefix := '';
  Commands := GetCommands(Prefix);
  try
    for i := 0 to Commands.Count - 1 do
      WriteLn(GetCommandDescription(Commands[i]));
    Result := Commands.Count > 0;
  finally
    FAN(Commands);
  end;
end;

function TConsole.EchoHandler(Sender: TObject; Args: array of const): Boolean;
begin
  WriteLn(string(Args[1].VAnsiString));
  Result := true;
end;

function TConsole.ExecHandler(Sender: TObject; Args: array of const): Boolean;

  procedure Error(const Fmt: string; const Args: array of const);
  begin
    {$IFDEF VSE_LOG}
    LogF(llError, Fmt, Args);
    {$ELSE}
    WriteLn(Format(Fmt, Args) + PostfixError);
    {$ENDIF}
  end;

var
  CmdFileName: string;
  CmdFile: TStringList;
  Line: Integer;
begin
  Result := false;
  CmdFileName := string(Args[1].VAnsiString);
  CmdFile := Core.GetFileText(CmdFileName);
  if not Assigned(CmdFile) then
  begin
    Error('Console: can''t execute file "%s": file not found', [CmdFileName]);
    Exit;
  end;
  try
    Line := 0;
    FGoTo := '';
    while Line < CmdFile.Count do
    begin
      if (CmdFile[Line] <> '') and not (CmdFile[Line][1] in [';', ':']) then
        Execute(CmdFile[Line]);
      if FGoTo <> '' then
      begin
        Line := 0;
        while Line < CmdFile.Count do
          if (CmdFile[Line] <> '') and (CmdFile[Line][1] = ':') and
             SameText(Trim(Copy(CmdFile[Line], 2, MaxInt)), FGoTo) then
            Break
          else
            Inc(Line);
        if Line = CmdFile.Count then
        begin
          Error('Console: label "%s" not found', [FGoTo]);
          Exit;
        end;
        FGoTo := '';
      end
        else Inc(Line);
    end;
    Result := true;
  finally
    FAN(CmdFile);
  end;
end;

function TConsole.ExistHandler(Sender: TObject; Args: array of const): Boolean;
var
  F: TStream;
begin
  F := Core.GetFile(string(Args[1].VAnsiString));
  Result := Assigned(F);
  F.Free;
end;

function TConsole.GotoHandler(Sender: TObject; Args: array of const): Boolean;
begin
  FGoTo := string(Args[1].VAnsiString);
  Result := true;
end;

function TConsole.IfHandler(Sender: TObject; Args: array of const): Boolean;
begin
  Result := Execute(string(Args[1].VAnsiString));
  if string(Args[0].VAnsiString) = 'ifnot' then Result := not Result;
  if Result then Result :=  Execute(string(Args[2].VAnsiString));
end;

function TConsole.ConVarIntHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args) = 1 then
    Console.WriteLn(IntToStr(PInteger(Self)^))
  else if Args[1].VType = vtInteger then
    PInteger(Self)^ := Args[1].VInteger
  else
    raise Exception.Create(SConsoleVariableTypeMismatch);
  Result := PInteger(Self)^ <> 0;
end;

function TConsole.ConVarByteHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args) = 1 then
    Console.WriteLn(IntToStr(PByte(Self)^))
  else if Args[1].VType = vtInteger then
    PByte(Self)^ := Args[1].VInteger
  else
    raise Exception.Create(SConsoleVariableTypeMismatch);
  Result := PByte(Self)^ <> 0;
end;

function TConsole.ConVarFloatHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args) = 1 then
    Console.WriteLn(FloatToStr2(PSingle(Self)^, 1, 4))
  else if Args[1].VType = vtExtended then
    PSingle(Self)^ := Args[1].VExtended^
  else
    raise Exception.Create(SConsoleVariableTypeMismatch);
  Result := PSingle(Self)^ <> 0;
end;

function TConsole.ConVarStrHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args) = 1 then
    Console.WriteLn(string(Self))
  else if Args[1].VType = vtAnsiString then
    string(Self) := string(Args[1].VAnsiString)
  else
    raise Exception.Create(SConsoleVariableTypeMismatch);
  Result := string(Self) <> '';
end;

{$IFDEF VSE_LOG}
function TConsole.LogLevelHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args) = 1 then
    WriteLn('Log level: ' + LogLevelNames[FLogLevel])
  else
    FLogLevel := TLogLevel(Args[1].VInteger);
  Result := true;
end;
{$ENDIF}

class function TConsole.GetCommandName(CommandLine: string): string;
begin
  Result := LowerCase(Trim(Tok(' ', CommandLine)));
end;

{ TConsoleCommandArgument }

constructor TConsoleCommandArgument.Create(const Name: string; Options: string);
begin
  inherited Create;
  FName := Name;
  if FName[1] = '?' then
  begin
    FOptional := true;
    Delete(FName, 1, 1);
  end;
end;

function TConsoleCommandArgument.GetNextOption(var Options: string): string;
begin
  Result := Trim(Tok(':', Options));
end;

function TConsoleCommandArgument.GetArgument(var CommandLine: string): string;
const
  Delim: array[Boolean] of Char = (' ', '"'); 
var
  IsQuoted: Boolean;
  i: Integer;
begin
  Result := '';
  CommandLine := TrimLeft(CommandLine);
  if CommandLine = '' then Exit;
  IsQuoted := CommandLine[1] = '"';
  i := PosEx(Delim[IsQuoted], CommandLine, 2);
  if i = 0 then i := Length(CommandLine);
  Result := Trim(Copy(CommandLine, 1, i));
  Delete(CommandLine, 1, i);
  if IsQuoted then Result := Copy(Result, 2, Length(Result) - 2);
end;

function TConsoleCommandArgument.GetDescription: string;
begin
  Result := Name + ': ' + GetType;
  if FOptional
    then Result := '[' + Result + ']'
    else Result := '<' + Result + '>';
end;

{ TCCAInteger }

constructor TCCAInteger.Create(const Name: string; Options: string);
begin
  inherited;
  if Options <> ''
    then FMin := StrToInt(GetNextOption(Options))
    else FMin := -MaxInt - 1;
  if Options <> ''
    then FMax := StrToInt(GetNextOption(Options))
    else FMax := MaxInt;
end;

function TCCAInteger.GetType: string;
begin
  Result := 'int';
end;

function TCCAInteger.Parse(var CommandLine: string): Boolean;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  FValue.VType := vtInteger;
  Result := TryStrToInt(GetArgument(CommandLine), FValue.VInteger) and (FValue.VInteger >= FMin) and (FValue.VInteger <= FMax);
end;

{ TCCAFloat }

constructor TCCAFloat.Create(const Name: string; Options: string);
begin
  inherited;
  if Options <> ''
    then FMin := StrToFloat(GetNextOption(Options))
    else FMin := -MaxSingle;
  if Options <> ''
    then FMax := StrToFloat(GetNextOption(Options))
    else FMax := MaxSingle;
end;

function TCCAFloat.GetType: string;
begin
  Result := 'float'; 
end;

function TCCAFloat.Parse(var CommandLine: string): Boolean;
var
  Val: Single;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  Result := TryStrToFloat(GetArgument(CommandLine), Val) and  (Val >= FMin) and (Val <= FMax);
  FValueBuffer := Val;
  FValue.VType := vtExtended;
  FValue.VExtended := @FValueBuffer;
end;

{ TCCAString }

constructor TCCAString.Create(const Name: string; Options: string);
begin
  inherited;
  if GetNextOption(Options) = '*' then FFullLine := true;
end;

function TCCAString.GetType: string;
begin
  Result := 'str';
end;

function TCCAString.Parse(var CommandLine: string): Boolean;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  if FFullLine then
  begin
    FValueBuffer := TrimLeft(CommandLine);
    CommandLine := '';
  end
    else FValueBuffer := GetArgument(CommandLine);
  FValue.VType := vtAnsiString;
  FValue.VAnsiString := Pointer(FValueBuffer);
  Result := true;
end;

{ TCCAEnum }

constructor TCCAEnum.Create(const Name: string; Options: string);
begin
  inherited;
  while Options <> '' do
  begin
    SetLength(FValues, Length(FValues) + 1);
    FValues[High(FValues)] := LowerCase(GetNextOption(Options));
  end;
end;

function TCCAEnum.GetType: string;
var
  i: Integer;
begin
  Result := '';
  if Length(FValues) = 0 then Exit;
  if Length(FValues) <= 5 then
  begin
    for i := 0 to High(FValues) do
      Result := Result + '|' + FValues[i];
    Delete(Result, 1, 1);
  end
    else Result := FValues[0] + ' .. ' + FValues[High(FValues)];
end;

function TCCAEnum.Parse(var CommandLine: string): Boolean;
var
  Val: string;
  i: Integer;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  Val := LowerCase(GetArgument(CommandLine));
  for i := 0 to High(FValues) do
    if Val = FValues[i] then
    begin
      FValue.VType := vtInteger;
      FValue.VInteger := i;
      Result := true;
      Exit;
    end;
  Result := false;
end;

{ TConsoleCommand }

constructor TConsoleCommand.Create(Prev: TConsoleCommand; CmdDesc: string; Handler: TOnConsoleCommand);
var
  i: Integer;
begin
  inherited Create;
  if Assigned(Prev) then
  begin
    FNext := Prev.FNext;
    FPrev := Prev;
    Prev.FNext := Self;
    if Assigned(FNext) then FNext.FPrev := Self;
  end;
  FName := LowerCase(Trim(Tok(' ', CmdDesc)));
  FHandler := Handler;
  while CmdDesc <> '' do
  begin
    SetLength(FArguments, Length(FArguments) + 1);
    FArguments[High(FArguments)] := CreateArgumentParser(Trim(Tok(' ', CmdDesc)));
    if not Assigned(FArguments[High(FArguments)]) then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Console: can''t parse command "%s" arguments description', [FName]);{$ENDIF}
      raise Exception.Create('Unable to parse command arguments description');
    end;
  end;
  for i := 1 to High(FArguments) do
    if FArguments[i - 1].FOptional and not FArguments[i].FOptional then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Console: optional argument before mandatory in command "%s"', [FName]);{$ENDIF}
      raise Exception.Create('Unable to parse command arguments description');
    end;
end;

destructor TConsoleCommand.Destroy;
var
  i: Integer;
begin
  if Assigned(FPrev) then FPrev.FNext := FNext;
  if Assigned(FNext) then FNext.FPrev := FPrev;
  for i := 0 to High(FArguments) do
    FArguments[i].Free;
  Finalize(FArguments);
  inherited;
end;

function TConsoleCommand.CreateArgumentParser(ArgumentDesc: string): TConsoleCommandArgument;
var
  Name: string;
  Type_: Char;
begin
  Result := nil;
  Name := Tok('=', ArgumentDesc);
  if (Name = '') or (Length(ArgumentDesc) < 2) then Exit;
  Type_ := UpCase(ArgumentDesc[2]);
  Delete(ArgumentDesc, 1, 2);
  case Type_ of
    'I': Result := TCCAInteger.Create(Name, ArgumentDesc);
    'F': Result := TCCAFloat.Create(Name, ArgumentDesc);
    'E': Result := TCCAEnum.Create(Name, ArgumentDesc);
    'S': Result := TCCAString.Create(Name, ArgumentDesc);
  end;
end;

function TConsoleCommand.Execute(CommandLine: string): Boolean;
var
  i, ArgsCount: Integer;
  Arguments: array of TVarRec;
begin
  Result := false;
  Tok(' ', CommandLine);
  CommandLine := TrimLeft(CommandLine);
  ArgsCount := 0;
  for i := 0 to High(FArguments) do
  begin
    if not FArguments[i].Parse(CommandLine) then
    begin
      Console.WriteLn('Error: invalid argument #' + IntToStr(i + 1) + PostfixError);
      Exit;
    end;
    if FArguments[i].Value.VType <> vtInvalid then Inc(ArgsCount);
  end;
  if Assigned(FHandler) then
    try
      SetLength(Arguments, ArgsCount + 1);
      Arguments[0].VType := vtString;
      Arguments[0].VAnsiString := Pointer(FName);
      for i := 1 to ArgsCount do
        Arguments[i] := FArguments[i - 1].Value;
      Result := FHandler(Self, Arguments);
    except
      {$IFDEF VSE_LOG}LogException('in console command "' + Name + '" handler');{$ENDIF}
      {$IFNDEF VSE_DEBUG}Core.StopEngine(StopUserException);{$ENDIF}
    end;
end;

function TConsoleCommand.FindCommand(Name: string): TConsoleCommand;
begin
  Name := LowerCase(Name);
  Result := Self;
  while Assigned(Result) do
    if Result.Name = Name
      then Exit
      else Result := Result.Next;
end;

function TConsoleCommand.GetDescription: string;
var
  i: Integer;
begin
  Result := FName;
  for i := 0 to High(FArguments) do
    Result := Result + ' ' + FArguments[i].Description;
end;

{$IFDEF VSE_LOG}
initialization
  LogOnUpdate := LogUpdateHandler;
{$ENDIF}

end.