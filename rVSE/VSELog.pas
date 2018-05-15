unit VSELog;

{$IFNDEF VSE_LOG}{$ERROR Please don't include VSELog unit without VSE_LOG defined}{$ENDIF}

interface

uses
  Windows, AvL, avlSyncObjs, avlUtils;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llAlways); //Level of log message: debug (omitted if VSE_DEBUG is not defined), informational, warning, error, mandatory

const
  LogLevelNames: array[TLogLevel] of string = ('debug', 'info', 'warning', 'error', 'always');

procedure Log(Level: TLogLevel; const S: string); //Add message to log
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const); //Add message to log, Format version
procedure LogRaw(Level: TLogLevel; const S: string; const Prefix: string = ''); //Add message to log as is
procedure LogAssert(Condition: Boolean; const Msg: string); //Add message if Condition=false
procedure LogMultiline(Level: TLogLevel; S: string); //Add multiline message to log

var
  LogLevel: TLogLevel = {$IFDEF VSE_DEBUG}llDebug{$ELSE}llInfo{$ENDIF}; //Minimal level of message, that will be passed to log
  LogOnUpdate: procedure(Level: TLogLevel; const S: string) = nil; //used by VSEConsole

implementation

type
  TLogger = class(TThread)
  private
    FLogFile: TFileStream;
    FLogBufferLock: TCriticalSection;
    FLogEvent: TEvent;
    FBuffer: string;
    FLogStart, FLastEvent: Cardinal;
    FErrors, FWarnings: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const S: string);
    function GetPrefix(Level: TLogLevel): string;
  end;

var
  Logger: TLogger;
  {$IFNDEF DEBUGMEM}StartHeapStatus, EndHeapStatus: THeapStatus;{$ENDIF}

function GetTickCount: Cardinal;
var
  T, F: Int64;
begin
  if QueryPerformanceFrequency(F) and QueryPerformanceCounter(T) then
    Result := 1000 * (T div F) + (1000 * (T mod F)) div F
  else
    Result := Windows.GetTickCount;
end;

function LogFileName: string;
begin
  Result := ChangeFileExt(FullExeName, '.log');
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then
    Log(llError, 'Assertion failed: ' + Msg);
end;

procedure LogMultiline(Level: TLogLevel; S: string);
begin
  while (S <> '') and (S[Length(S)] in [#10, #13]) do
    Delete(S, Length(S), 1);
  Log(Level, Tok(#10#13, S));
  while S <> '' do
    LogRaw(Level, Tok(#10#13, S));
end;

procedure Log(Level: TLogLevel; const S: string);
begin
  if not Assigned(Logger) then Exit;
  LogRaw(Level, S, Logger.GetPrefix(Level));
end;

procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  Log(Level, Format(Fmt, Args));
end;

procedure LogRaw(Level: TLogLevel; const S: string; const Prefix: string = '');
begin
  if (Level < LogLevel) or not Assigned(Logger) then Exit;
  if Assigned(LogOnUpdate) then
    LogOnUpdate(Level, S);
  Logger.Add(Prefix + S);
end;

{ TLogger }

constructor TLogger.Create;
begin
  if not FileExists(LogFileName) then
    CloseHandle(FileCreate(LogFileName));
  FLogFile := TFileStream.Create(LogFileName, fmOpenWrite or fmShareDenyWrite);
  FLogFile.Position := 0;
  SetEndOfFile(FLogFile.Handle);
  FLogEvent := TEvent.Create(nil, false, false, '');
  FLogBufferLock := TCriticalSection.Create;
  FLogStart := GetTickCount;
  FLastEvent := FLogStart;
  Add('Log started at ' + DateTimeToStr(Now) + #13#10);
  inherited Create(false);
end;

destructor TLogger.Destroy;
begin
  Add(#13#10'Log closed at ' + DateTimeToStr(Now));
  Add('Errors: ' + IntToStr(FErrors) + ', Warnings: ' + IntToStr(FWarnings));
  Terminate;
  FLogEvent.SetEvent;
  WaitFor;
  FAN(FLogEvent);
  FAN(FLogBufferLock);
  FAN(FLogFile);
  inherited;
end;

procedure TLogger.Add(const S: string);
begin
  FLogBufferLock.Acquire;
  try
    FBuffer := FBuffer + S + #13#10;
  finally
    FLogBufferLock.Release;
  end;
  FLogEvent.SetEvent;
end;

function TLogger.GetPrefix(Level: TLogLevel): string;
var
  Time, Delta: Cardinal;
begin
  FLogBufferLock.Acquire;
  try
    Time := GetTickCount;
    Delta := Time - FLastEvent;
    FLastEvent := Time;
    Time := Time - FLogStart;
  finally
    FLogBufferLock.Release;
  end;
  Result := Format('[%02d:%02d:%02d.%03d (+%d ms)] ', [Time div 3600000, Time
    mod 3600000 div 60000, Time mod 60000 div 1000, Time mod 1000, Delta]);
  if Level = llError then
  begin
    InterlockedIncrement(FErrors);
    Result := Result + 'Error: ';
  end
  else if Level = llWarning then
  begin
    InterlockedIncrement(FWarnings);
    Result := Result + 'Warning: ';
  end;
end;

procedure TLogger.Execute;
var
  Buffer: string;

  procedure WriteBuffer;
  begin
    FLogBufferLock.Acquire;
    try
      Buffer := FBuffer;
      FBuffer := '';
    finally
      FLogBufferLock.Release;
    end;
    FLogFile.Write(Buffer[1], Length(Buffer));
    FlushFileBuffers(FLogFile.Handle);
  end;

begin
  while not Terminated do
    if FLogEvent.WaitFor(INFINITE) = wrSignaled then
      WriteBuffer
    else begin
      Buffer := 'LogEvent error: ' + SysErrorMessage(FLogEvent.LastError);
      FLogFile.Write(Buffer[1], Length(Buffer));
      FlushFileBuffers(FLogFile.Handle);
      Break;
    end;
  WriteBuffer;
end;

{$IFNDEF DEBUGMEM}
procedure CheckLeaks;
var
  F: TFileStream;

  procedure WriteLn(S: string);
  begin
    S := S + #13#10;
    F.Write(S[1], Length(S));
  end;

  procedure LogHeapStatus(Status: THeapStatus);
  begin
    with Status do
    begin
      WriteLn(Format('  Totals: Allocated: %u; Free: %u; Committed: %u; Uncommitted: %u; AddrSpace: %u',
        [TotalAllocated, TotalFree, TotalCommitted, TotalUncommitted, TotalAddrSpace]));
      WriteLn(Format('  Free: Small: %u, Big: %u; Unused: %u; Overhead: %u; ErrorCode: %u', [FreeSmall, FreeBig, Unused, Overhead, HeapErrorCode]));
    end;
  end;

begin
  if EndHeapStatus.TotalAllocated <> StartHeapStatus.TotalAllocated then
  begin
    F := TFileStream.Create(LogFileName, fmOpenWrite or fmShareDenyWrite);
    try
      F.Position := F.Size;
      WriteLn(#13#10'Memory leak detected: ' + SizeToStr(Int64(EndHeapStatus.TotalAllocated) - StartHeapStatus.TotalAllocated));
      WriteLn('Start heap status:');
      LogHeapStatus(StartHeapStatus);
      WriteLn('End heap status:');
      LogHeapStatus(EndHeapStatus);
    finally
      F.Free;
    end;
  end;
end;
{$ENDIF}

initialization
  IsMultiThread := true;
  {$IFNDEF DEBUGMEM}StartHeapStatus := GetHeapStatus;{$ENDIF}
  Logger := TLogger.Create;

finalization
  FAN(Logger);
  {$IFNDEF DEBUGMEM}
  EndHeapStatus := GetHeapStatus;
  CheckLeaks;
  {$ENDIF}

end.

