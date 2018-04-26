unit VSELog;

{$IFNDEF VSE_LOG}{$ERROR Please don't include VSELog unit without VSE_LOG defined}{$ENDIF}

interface

uses Windows, AvL, avlSyncObjs, avlUtils;

type
  TLogLevel=(llDebug, llInfo, llWarning, llError, llAlways); //Level of log message: debug (omitted if VSE_DEBUG is not defined), informational, warning, error, mandatory

const
  LogLevelNames: array[TLogLevel] of string =
    ('debug', 'info', 'warning', 'error', 'always');

procedure Log(Level: TLogLevel; const S: string); //Add message to log
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const); //Add message to log, Format version
procedure LogRaw(Level: TLogLevel; const S: string; const Prefix: string = '');
procedure LogAssert(Condition: Boolean; const Msg: string); //Add message if Condition=false
procedure LogMultiline(Level: TLogLevel; S: string); //Add multiline message to log

var
  LogLevel: TLogLevel = {$IFDEF VSE_DEBUG}llDebug{$ELSE}llInfo{$ENDIF}; //Minimal level of message, that will be passed to log
  LogOnUpdate: procedure(Level: TLogLevel; const S: string) = nil; //used by VSEConsole

implementation

type
  TLoggerThread=class(TThread)
  protected
    procedure Execute; override;
  end;

var
  LogBuffer: string = '';
  LogBufferLock: TCriticalSection;
  LogEvent: TEvent;
  Logger: TLoggerThread;
  LogInitialized: Boolean;
  LogStart, LastEvent: Cardinal;
  LogErrors: Integer = 0;
  LogWarnings: Integer = 0;

function GetTickCount: Cardinal;
var
  T, F: Int64;
begin
  if QueryPerformanceFrequency(F) and QueryPerformanceCounter(T) then
    Result := 1000 * (T div F) + (1000 * (T mod F)) div F
  else
    Result := Windows.GetTickCount;
end;

procedure TLoggerThread.Execute;
var
  LogFile: TFileStream;
  Buffer, LogFileName: string;

  procedure WriteBuffer;
  begin
    LogBufferLock.Acquire;
    try
      Buffer:=LogBuffer;
      LogBuffer:='';
    finally
      LogBufferLock.Release;
    end;
    LogFile.Write(Buffer[1], Length(Buffer));
    FlushFileBuffers(LogFile.Handle);
  end;

begin
  LogFileName:=ChangeFileExt(FullExeName, '.log');
  if not FileExists(LogFileName)
    then CloseHandle(FileCreate(LogFileName));
  LogFile:=TFileStream.Create(LogFileName, fmOpenWrite or fmShareDenyWrite);
  LogFile.Position:=0;
  SetEndOfFile(LogFile.Handle);
  try
    while not Terminated do
      if LogEvent.WaitFor(INFINITE)=wrSignaled
        then WriteBuffer
      else begin
        Buffer:='LogEvent error: '+SysErrorMessage(LogEvent.LastError);
        LogFile.Write(Buffer[1], Length(Buffer));
        FlushFileBuffers(LogFile.Handle);
        Break;
      end;
    WriteBuffer;
  finally
    FAN(LogFile);
  end;
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(llError, 'Assertion failed: '+Msg);
end;

procedure LogMultiline(Level: TLogLevel; S: string);
begin
  while (S<>'') and (S[Length(S)] in [#10, #13]) do
    Delete(S, Length(S), 1);
  Log(Level, Tok(#10#13, S));
  while S<>'' do
    LogRaw(Level, Tok(#10#13, S));
end;

procedure Log(Level: TLogLevel; const S: string);
var
  TimeStamp: string;
  Time, Delta: Cardinal;
begin
  LogBufferLock.Acquire;
  try
    Time:=GetTickCount;
    Delta:=Time-LastEvent;
    LastEvent:=Time;
    Time:=Time-LogStart;
  finally
    LogBufferLock.Release;
  end;
  TimeStamp:=Format('[%02d:%02d:%02d.%03d (+%d ms)] ', [Time div 3600000, Time mod 3600000 div 60000, Time mod 60000 div 1000, Time mod 1000, Delta]);
  case Level of
    llWarning: begin
      LogRaw(Level, S, TimeStamp+'Warning: ');
      InterlockedIncrement(LogWarnings);
    end;
    llError: begin
      LogRaw(Level, S, TimeStamp+'Error: ');
      InterlockedIncrement(LogErrors);
    end
    else LogRaw(Level, S, TimeStamp);
  end;
end;

procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  Log(Level, Format(Fmt, Args));
end;

procedure LogRaw(Level: TLogLevel; const S: string; const Prefix: string = '');
begin
  if (Level<LogLevel) or not LogInitialized then Exit;
  if Assigned(LogOnUpdate) then LogOnUpdate(Level, S);
  LogBufferLock.Acquire;
  try
    LogBuffer:=LogBuffer+Prefix+S+#13#10;
  finally
    LogBufferLock.Release;
  end;
  LogEvent.SetEvent;
end;

initialization

  IsMultiThread:=true;
  LogEvent:=TEvent.Create(nil, false, false, '');
  LogBufferLock:=TCriticalSection.Create;
  Logger:=TLoggerThread.Create(false);
  LogStart:=GetTickCount;
  LastEvent:=LogStart;
  LogInitialized:=true;
  LogRaw(llAlways, 'Log started at '+DateTimeToStr(Now));
  LogRaw(llAlways, '');

finalization

  LogRaw(llAlways, '');
  LogRaw(llAlways, 'Log closed at '+DateTimeToStr(Now));
  LogRaw(llAlways, 'Errors: '+IntToStr(LogErrors)+', Warnings: '+IntToStr(LogWarnings));
  LogInitialized:=false;
  Logger.Terminate;
  LogEvent.SetEvent;
  Logger.WaitFor;
  FAN(Logger);
  FAN(LogEvent);
  FAN(LogBufferLock);
  LogBuffer:='';

end.
