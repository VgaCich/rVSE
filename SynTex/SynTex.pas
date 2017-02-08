unit SynTex;

interface

uses
  Windows, AvL, avlUtils, avlMath;

const
  SynTexRegsCount=16;

type
  TCmdCode=(ccRet, ccJump, ccCall, ccLoad, ccStore, ccFilter, ccSetRandomSeed);
  TRGBA=packed record
    R, G, B, A: Byte;
  end;
  PSynTexRegister=^TSynTexRegister;
  TSynTexRegister=packed array of TRGBA;
  TSynTexCmdFixed=packed record
    CmdCode: Byte;
    CmdParamsLen: Byte;
  end;
  TSynTexCmd=packed record
    Fixed: TSynTexCmdFixed;
    RegsCodes: array of Byte;
    Regs: array of PSynTexRegister;
    Params: TStream;
  end;
  TSynTex=class;
  TSynTexFilter=function(Regs: array of PSynTexRegister; RegsCount: Integer; Params: Pointer; ParamsSize: Integer): Boolean of object;
  TSynTexFilterRec=record
    ID: Byte;
    Filter: TSynTexFilter;
  end;
  TOnStore=procedure(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string) of object;
  TOnLoad=function(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean of object;
  TSynTex=class
  private
    FTexSize: Integer;
    FCode: TStream;
    FRegisters: array[0..SynTexRegsCount-1] of TSynTexRegister;
    FCallStack: TList;
    FFilters: array of TSynTexFilterRec;
    FOnStore: TOnStore;
    FOnLoad: TOnLoad;
    function  LoadCmd(var Cmd: TSynTexCmd): Boolean;
    procedure FreeCmd(var Cmd: TSynTexCmd);
    function  FindFilter(ID: Byte): TSynTexFilter;
  protected
    function Step: Boolean;
  public
    constructor Create(TexSize: Integer);
    destructor Destroy; override;
    function AddFilter(ID: Byte; Filter: TSynTexFilter): Boolean;
    function Synthesize: Boolean;
    property Code: TStream read FCode write FCode;
    property OnStore: TOnStore read FOnStore write FOnStore;
    property OnLoad: TOnLoad read FOnLoad write FOnLoad;
    property TexSize: Integer read FTexSize;
  end;
  TSynTexDebugger=class
  private
    FSynTex: TSynTex;
    FBreakPoints: TList;
    function  GetPos: Integer;
    procedure SetPos(Value: Integer);
    function  GetRegs(Index: Integer): PSynTexRegister;
    function  GetCallStackCount: Integer;
    function  GetCallStack(Index: Integer): Integer;
    function  GetBreakPointsCount: Integer;
    function  GetBreakPoint(Index: Integer): Integer;
    procedure SetBreakPoint(Index, Value: Integer);
  public
    constructor Create(SynTex: TSynTex);
    destructor Destroy; override;
    procedure Execute;
    function  Step: Boolean;
    function  Run: Boolean;
    function  AddBreakPoint(BreakPoint: Integer): Integer;
    procedure RemoveBreakPoint(BreakPoint: Integer);
    property Synthesizer: TSynTex read FSynTex;
    property Pos: Integer read GetPos write SetPos;
    property Regs[Index: Integer]: PSynTexRegister read GetRegs; default;
    property CallStackCount: Integer read GetCallStackCount;
    property CallStack[Index: Integer]: Integer read GetCallStack;
    property BreakPointsCount: Integer read GetBreakPointsCount;
    property BreakPoints[Index: Integer]: Integer read GetBreakPoint write SetBreakPoint;
  end;

function CheckRemain(Stream: TStream; Needs: Integer{$IFDEF SYNTEX_USELOG}; const FailFunc: string{$ENDIF}): Boolean; {$IFDEF INLINE} inline; {$ENDIF}

const
  Commands: array[TCmdCode] of string=('RET', 'JMP', 'CALL', 'LDR', 'STR', 'FLT', 'SRS');

{$IFDEF SYNTEX_USELOG}
var
  LogCB: procedure(const S: string)=nil;

procedure Log(const S: string);
{$ENDIF}

implementation

{$IFDEF SYNTEX_USELOG}
procedure Log(const S: string);
begin
  if Assigned(LogCB) then LogCB(S);
end;

procedure LogException(const Comment: string);
begin
  Log('Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" '+Comment);
end;
{$ENDIF}

function CheckRemain(Stream: TStream; Needs: Integer{$IFDEF SYNTEX_USELOG}; const FailFunc: string{$ENDIF}): Boolean;
begin
  Result:=(Stream.Size-Stream.Position)>=Needs;
  {$IFDEF SYNTEX_USELOG}if not Result then Log(FailFunc+' failed: not enough data in codestream');{$ENDIF}
end;

{TSynTex}

constructor TSynTex.Create(TexSize: Integer);
var
  i: Integer;
begin
  inherited Create;
  FTexSize:=TexSize;
  for i:=0 to SynTexRegsCount-1 do
    SetLength(FRegisters[i], TexSize*TexSize);
  FCallStack:=TList.Create;
end;

destructor TSynTex.Destroy;
var
  i: Integer;
begin
  Finalize(FFilters);
  FAN(FCallStack);
  for i:=0 to SynTexRegsCount-1 do
    SetLength(FRegisters[i], 0);
  inherited Destroy;
end;

function TSynTex.AddFilter(ID: Byte; Filter: TSynTexFilter): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to High(FFilters) do
    if FFilters[i].ID=ID then
    begin
      {$IFDEF SYNTEX_USELOG}Log('Filter with ID='+IntToStr(ID)+' already exists');{$ENDIF}
      Exit;
    end;
  SetLength(FFilters, Length(FFilters)+1);
  FFilters[High(FFilters)].ID:=ID;
  FFilters[High(FFilters)].Filter:=Filter;
  Result:=true;
end;

function TSynTex.Synthesize: Boolean;
begin
  Result:=false;
  FCallStack.Clear;
  {$IFDEF SYNTEX_USELOG}
  try
  {$ENDIF}
    if not Assigned(FCode) then Exit;
    try
      while FCode.Position<FCode.Size do
        if not Step then Exit;
      Result:=true;
    except
      Result:=false;
      {$IFDEF SYNTEX_USELOG}LogException('in Synthesize');{$ENDIF}
    end;
  {$IFDEF SYNTEX_USELOG}
  finally
    if not Result then Log('Synthesize: execution stopped at '+IntToStr(FCode.Position));
  end;
  {$ENDIF}
end;

function TSynTex.Step: Boolean;
var
  Cmd: TSynTexCmd;
  i, CmdPos, DataInt: Integer;
  Filter: TSynTexFilter;
  P: Pointer;

  function ReadStr: string;
  var
    StrLen: Byte;
  begin
    Result:='';
    if not Assigned(Cmd.Params) then Exit;
    if Cmd.Params.Position>=Cmd.Params.Size then Exit;
    Cmd.Params.Read(StrLen, SizeOf(StrLen));
    SetLength(Result, StrLen);
    SetLength(Result, Cmd.Params.Read(Result[1], StrLen));
  end;

begin
  Result:=false;
  if not Assigned(FCode) then Exit;
  CmdPos:=FCode.Position;
  try
    if not LoadCmd(Cmd) then Exit;
    case TCmdCode(Cmd.Fixed.CmdCode) of
      ccRet: if FCallStack.Count>0 then
        begin
          FCode.Position:=Integer(FCallStack.Last);
          FCallStack.Delete(FCallStack.Count-1);
        end
        else begin
          Result:=true;
          Exit;
        end;
      ccJump, ccCall:
        begin
          if not Assigned(Cmd.Params) then Exit;
          if not CheckRemain(Cmd.Params, SizeOf(DataInt){$IFDEF SYNTEX_USELOG}, 'Step:'+Commands[TCmdCode(Cmd.Fixed.CmdCode)]{$ENDIF})
            then Exit;
          Cmd.Params.Read(DataInt, SizeOf(DataInt));
          if (DataInt<0) or (DataInt>FCode.Size) then Exit;
          if TCmdCode(Cmd.Fixed.CmdCode)=ccCall
            then FCallStack.Add(Pointer(FCode.Position));
          FCode.Position:=DataInt;
        end;
      ccLoad: if Assigned(FOnLoad) then
        for i:=0 to High(Cmd.RegsCodes) do
          if not FOnLoad(Self, FRegisters[Cmd.RegsCodes[i]], FTexSize, ReadStr) then Exit;
      ccStore: if Assigned(FOnStore) then
        for i:=0 to High(Cmd.RegsCodes) do
          FOnStore(Self, FRegisters[Cmd.RegsCodes[i]], FTexSize, ReadStr);
      ccFilter:
        begin
          if not Assigned(Cmd.Params) then Exit;
          if not CheckRemain(Cmd.Params, 1{$IFDEF SYNTEX_USELOG}, 'Step:Filter'{$ENDIF}) then Exit;
          DataInt:=0;
          Cmd.Params.Read(DataInt, 1);
          Filter:=FindFilter(DataInt);
          if not Assigned(Filter) then
          begin
            {$IFDEF SYNTEX_USELOG}Log('Step: filter ID='+IntToStr(DataInt)+' not found');{$ENDIF}
            Exit;
          end;
          DataInt:=Cmd.Params.Size-1;
          try
            if DataInt>0 then
            begin
              GetMem(P, DataInt);
              Cmd.Params.Read(P^, DataInt);
            end
              else P:=nil;
            if not Filter(Cmd.Regs, Length(Cmd.Regs), P, DataInt) then Exit;
          finally
            if Assigned(P) then FreeMem(P);
          end;
        end;
      ccSetRandomSeed:
        begin
          if not Assigned(Cmd.Params) then Exit;
          if not CheckRemain(Cmd.Params, SizeOf(DataInt){$IFDEF SYNTEX_USELOG}, 'Step:SetRandomSeed'{$ENDIF}) then Exit;
          Cmd.Params.Read(DataInt, SizeOf(DataInt));
          if DataInt=0
            then Randomize
            else RandSeed:=DataInt;
        end;
      else begin
        Result:=false;
        {$IFDEF SYNTEX_USELOG}Log('Step: unknown command code at '+IntToStr(CmdPos));{$ENDIF}
        Exit;
      end;
    end;
    Result:=true;
  finally
    FreeCmd(Cmd);
    if not Result then FCode.Position:=CmdPos;
  end;
end;

function TSynTex.LoadCmd(var Cmd: TSynTexCmd): Boolean;
const
  ERemainFail='LoadCmd';
var
  i, RegsCount: Integer;
  Regs: packed array of Byte;
begin
  Result:=false;
  if not Assigned(FCode) then Exit;
  if not CheckRemain(FCode, SizeOf(TSynTexCmdFixed){$IFDEF SYNTEX_USELOG}, ERemainFail{$ENDIF}) then Exit;
  FCode.Read(Cmd.Fixed, SizeOf(TSynTexCmdFixed));
  RegsCount:=Cmd.Fixed.CmdCode and $0F;
  Cmd.Fixed.CmdCode:=Cmd.Fixed.CmdCode shr 4;
  i:=Ceil(RegsCount/2); //regs list size
  if not CheckRemain(FCode, i{$IFDEF SYNTEX_USELOG}, ERemainFail{$ENDIF}) then Exit;
  SetLength(Regs, i);
  FCode.Read(Regs[0], i);
  SetLength(Cmd.RegsCodes, RegsCount);
  SetLength(Cmd.Regs, RegsCount);
  for i:=0 to RegsCount-1 do
  begin
    Cmd.RegsCodes[i]:=(Regs[i div 2] shr ((i mod 2)*4)) and $0F;
    Cmd.Regs[i]:=@FRegisters[Cmd.RegsCodes[i]];
  end;
  if Cmd.Fixed.CmdParamsLen>0 then
  begin
    if not CheckRemain(FCode, Cmd.Fixed.CmdParamsLen{$IFDEF SYNTEX_USELOG}, ERemainFail{$ENDIF}) then Exit;
    Cmd.Params:=TMemoryStream.Create;
    Cmd.Params.CopyFrom(FCode, Cmd.Fixed.CmdParamsLen);
    Cmd.Params.Position:=0;
  end
    else Cmd.Params:=nil;
  Result:=true;
end;

procedure TSynTex.FreeCmd(var Cmd: TSynTexCmd);
begin
  SetLength(Cmd.RegsCodes, 0);
  SetLength(Cmd.Regs, 0);
  FAN(Cmd.Params);
  ZeroMemory(@Cmd, SizeOf(Cmd));
end;

function TSynTex.FindFilter(ID: Byte): TSynTexFilter;
var
 i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FFilters) do
    if FFilters[i].ID=ID
      then Result:=FFilters[i].Filter; 
end;

{TSynTexDebugger}

constructor TSynTexDebugger.Create(SynTex: TSynTex);
begin
  inherited Create;
  FSynTex:=SynTex;
  FBreakPoints:=TList.Create;
end;

destructor TSynTexDebugger.Destroy;
begin
  FAN(FBreakPoints);
  inherited Destroy;
end;

procedure TSynTexDebugger.Execute;
begin
  FSynTex.FCallStack.Clear;
end;

function TSynTexDebugger.Step: Boolean;
begin
  if Assigned(FSynTex.FCode) and (FSynTex.FCode.Position<FSynTex.FCode.Size)
    then Result:=FSynTex.Step
    else Result:=false;
end;

function TSynTexDebugger.Run: Boolean;
begin
  Result:=false;
  if not Assigned(FSynTex.FCode) then Exit;
  try
    while FSynTex.FCode.Position<FSynTex.FCode.Size do
      if FBreakPoints.IndexOf(Pointer(FSynTex.FCode.Position))>=0
        then Break
        else
          if not FSynTex.Step then Exit;
    Result:=true;
  except
    Result:=false;
  end;
end;

function TSynTexDebugger.AddBreakPoint(BreakPoint: Integer): Integer;
begin
  Result:=-1;
  if not Assigned(FSynTex.FCode) then Exit;
  if (BreakPoint<0) or (BreakPoint>FSynTex.FCode.Size) then Exit;
  Result:=FBreakPoints.Add(Pointer(BreakPoint));
end;

procedure TSynTexDebugger.RemoveBreakPoint(BreakPoint: Integer);
begin
  FBreakPoints.Remove(Pointer(BreakPoint));
end;

function TSynTexDebugger.GetPos: Integer;
begin
  Result:=-1;
  if not Assigned(FSynTex.FCode) then Exit;
  Result:=FSynTex.FCode.Position;
end;

procedure TSynTexDebugger.SetPos(Value: Integer);
begin
  if not Assigned(FSynTex.FCode) then Exit;
  if (Value<0) or (Value>FSynTex.FCode.Size) then Exit;
  FSynTex.FCode.Position:=Value;
end;

function TSynTexDebugger.GetRegs(Index: Integer): PSynTexRegister;
begin
  Result:=nil;
  if (Index<0) or (Index>SynTexRegsCount-1) then Exit;
  Result:=@FSynTex.FRegisters[Index];
end;

function TSynTexDebugger.GetCallStackCount: Integer;
begin
  Result:=0;
  if Assigned(FSynTex) then Result:=FSynTex.FCallStack.Count;
end;

function TSynTexDebugger.GetCallStack(Index: Integer): Integer;
begin
  Result:=-1;
  if not Assigned(FSynTex) then Exit;
  if (Index<0) or (Index>=FSynTex.FCallStack.Count) then Exit;
  Result:=Integer(FSynTex.FCallStack[Index]);
end;

function TSynTexDebugger.GetBreakPointsCount: Integer;
begin
  Result:=FBreakPoints.Count;
end;

function TSynTexDebugger.GetBreakPoint(Index: Integer): Integer;
begin
  Result:=-1;
  if (Index<0) or (Index>=FBreakPoints.Count) then Exit;
  Result:=Integer(FBreakPoints[Index]);
end;

procedure TSynTexDebugger.SetBreakPoint(Index, Value: Integer);
begin
  if (Index<0) or (Index>=FBreakPoints.Count) then Exit;
  if not Assigned(FSynTex.FCode) then Exit;
  if (Value<0) or (Value>FSynTex.FCode.Size) then Exit;
  FBreakPoints[Index]:=Pointer(Value);
end;

end.
