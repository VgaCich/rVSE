unit SynTexAssembler;

interface

uses
  Windows, AvL, avlUtils, avlMath, SynTex;

type
  TOnError=procedure(Sender: TObject; Line: Integer; const Msg: string) of object;
  TSynTexDebugInfo=record
    Line, Addr: Integer;
  end;
  TSynTexTokenType=(stString, stFloat, stInteger, stCommand, stRegister, stIdentifier, stLabel);
  PSynTexToken=^TSynTexToken;
  TSynTexToken=record
    Next, Prev: PSynTexToken;
    TokenType: TSynTexTokenType;
    Value: string;
  end;
  TSynTexLink=record
    Name: string;
    Addr: Integer;
    Exist: Boolean;
  end;
  TSynTexFilterAssembler=function(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean of object;
  TSynTexFilterAssemblerRec=record
    Name: string;
    ID: Byte;
    FilterAssembler: TSynTexFilterAssembler;
  end;
  TSynTexAssembler=class
  private
    FSource: TStringList;
    FCode: TStream;
    FDebugInfo: array of TSynTexDebugInfo;
    FLabels: array of TSynTexLink;
    FLinks: array of TSynTexLink;
    FFilterAssemblers: array of TSynTexFilterAssemblerRec;
    FOnError: TOnError;
    FCurLine: Integer;
    function  NewToken(Prev: PSynTexToken): PSynTexToken;
    procedure FreeToken(Token: PSynTexToken);
    procedure FinTokens(Token: PSynTexToken);
    function  FindFilterAssembler(Name: string): Integer;
    function  GetDebugInfoCount: Integer;
    function  GetDebugInfo(Index: Integer): TSynTexDebugInfo;
  protected
    procedure PreProcess;
    function  ParseLine(Line: string): PSynTexToken;
    function  ParseToken(Token: PSynTexToken; var Line: string): Boolean;
    function  ParseTokenString(Token: PSynTexToken; var Line: string): Boolean;
    function  ParseTokenInteger(Token: PSynTexToken; var Line: string): Boolean;
    function  ParseTokenRegister(Token: PSynTexToken; var Line: string): Boolean;
    function  ParseTokenIdentifier(Token: PSynTexToken; var Line: string): Boolean;
    procedure FinTokenParse(var Line: string);
    function  IsCommand(S: string): Boolean;
    function  AssembleCommand(var Token: PSynTexToken): Boolean;
    function  AssembleLink(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
    function  AssembleStoreLoad(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
    function  AssembleFilter(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
    function  WriteCommand(var Cmd: TSynTexCmd): Boolean;
    procedure AddDebugInfo(Line, Addr: Integer);
    function  AddLabel(Name: string; Addr: Integer): Boolean;
    procedure AddLink(Name: string; Addr: Integer);
    function  GetRegister(const Name: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddFilterAssembler(Name: string; ID: Byte; FilterAssembler: TSynTexFilterAssembler): Boolean;
    function  Assemble: Boolean;
    function  NextToken(var Token: PSynTexToken; const ErrMsg: string): Boolean;
    function  TokenValueInteger(Token: PSynTexToken; out Value: Integer): Boolean;
    function  TokenValueFloat(Token: PSynTexToken; out Value: Single): Boolean;
    procedure Error(const Msg: string);
    property Source: TStringList read FSource;
    property Code: TStream read FCode;
    property DebugInfoCount: Integer read GetDebugInfoCount;
    property DebugInfo[Index: Integer]: TSynTexDebugInfo read GetDebugInfo;
    property OnError: TOnError read FOnError write FOnError;
  end;

const
  TokenName: array[TSynTexTokenType] of string=('string', 'float', 'integer', 'command', 'register', 'identifier', 'label');

implementation

constructor TSynTexAssembler.Create;
begin
  inherited Create;
  FSource:=TStringList.Create;
  FCode:=TMemoryStream.Create;
end;

destructor TSynTexAssembler.Destroy;
begin
  FAN(FCode);
  FAN(FSource);
  Finalize(FDebugInfo);
  Finalize(FLabels);
  Finalize(FLinks);
  Finalize(FFilterAssemblers);
  inherited Destroy;
end;

function TSynTexAssembler.AddFilterAssembler(Name: string; ID: Byte; FilterAssembler: TSynTexFilterAssembler): Boolean;
var
  i: Integer;
begin
  Result:=false;
  Name:=UpperCase(Name);
  for i:=0 to High(FFilterAssemblers) do
  begin
    if FFilterAssemblers[i].ID=ID then
    begin
      Error('Filter with ID='+IntToStr(ID)+' already exists');
      Exit;
    end;
    if FFilterAssemblers[i].Name=Name then
    begin
      Error('Filter with Name='+Name+' already exists');
      Exit;
    end;
  end;
  SetLength(FFilterAssemblers, Length(FFilterAssemblers)+1);
  FFilterAssemblers[High(FFilterAssemblers)].Name:=Name;
  FFilterAssemblers[High(FFilterAssemblers)].ID:=ID;
  FFilterAssemblers[High(FFilterAssemblers)].FilterAssembler:=FilterAssembler;
  Result:=true;
end;

function TSynTexAssembler.Assemble: Boolean;
var
  Line, i: Integer;
  Token: PSynTexToken;
begin
  Result:=false;
  Finalize(FDebugInfo);
  Finalize(FLabels);
  SetLength(FLabels, 4);
  Finalize(FLinks);
  SetLength(FLinks, 4);
  (FCode as TMemoryStream).Clear;
  PreProcess;
  for Line:=0 to FSource.Count-1 do
  begin
    FCurLine:=Line+1;
    if FSource[Line]='' then Continue;
    AddDebugInfo(Line+1, FCode.Position);
    Token:=ParseLine(FSource[Line]);
    if not Assigned(Token) then Exit;
    try
      if (Token.TokenType<>stCommand) and (Token.TokenType<>stLabel) then
      begin
        Error('Command or label expected, but '+TokenName[Token.TokenType]+' found');
        Exit;
      end;
      if Token.TokenType=stLabel then
      begin
        if not AddLabel(Token.Value, FCode.Position) then Exit;
        if Assigned(Token.Next) then
        begin
          Error('Extra token(s) after label');
          Exit;
        end;
      end;
      if Token.TokenType=stCommand then
        if not AssembleCommand(Token) then Exit;
    finally
      FinTokens(Token);
    end;
  end;
  FCode.Position:=0;
  Result:=true;
  for i:=0 to High(FLinks) do
    if FLinks[i].Exist then
    begin
      Error('Unresolved link: '+FLinks[i].Name);
      Result:=false;
    end;
end;

function TSynTexAssembler.NextToken(var Token: PSynTexToken; const ErrMsg: string): Boolean;
begin
  Result:=Assigned(Token.Next);
  if Result
    then Token:=Token.Next
    else Error(ErrMsg);
end;

function TSynTexAssembler.TokenValueInteger(Token: PSynTexToken; out Value: Integer): Boolean;
begin
  Result:=TryStrToInt(Token.Value, Value);
  if not Result then
  begin
    Error('Invalid integer value');
    Exit;
  end;
end;

function TSynTexAssembler.TokenValueFloat(Token: PSynTexToken; out Value: Single): Boolean;
begin
  Result:=TryStrToFloat(Token.Value, Value);
  if not Result then
  begin
    Error('Invalid float value');
    Exit;
  end;
end;

procedure TSynTexAssembler.Error(const Msg: string);
begin
  if Assigned(FOnError) then FOnError(Self, FCurLine, Msg);
end;

procedure TSynTexAssembler.PreProcess;
var
  i, P: Integer;
begin
  for i:=0 to FSource.Count-1 do
  begin
    P:=Pos('//', FSource[i]);
    if P>0
      then FSource[i]:=Copy(FSource[i], 1, P-1);
    FSource[i]:=Trim(FSource[i]);
  end;
end;

function TSynTexAssembler.ParseLine(Line: string): PSynTexToken;
begin
  Result:=nil;
  repeat
    Result:=NewToken(Result);
    if not ParseToken(Result, Line) then
    begin
      if Assigned(Result) then FinTokens(Result);
      Result:=nil;
      Exit;
    end;
  until Line='';
  while Assigned(Result.Prev) do Result:=Result.Prev;
end;

function TSynTexAssembler.ParseToken(Token: PSynTexToken; var Line: string): Boolean;
begin
  Result:=false;
  if Line='' then Exit;
  case Line[1] of
    '''': Result:=ParseTokenString(Token, Line);
    '0'..'9', '$', '-': Result:=ParseTokenInteger(Token, Line);
    '#': Result:=ParseTokenRegister(Token, Line);
    '_', 'a'..'z', 'A'..'Z': Result:=ParseTokenIdentifier(Token, Line);
    else begin
      Error('Unknown token '+Line[1]);
      Exit;
    end;
  end;
end;

function TSynTexAssembler.ParseTokenString(Token: PSynTexToken; var Line: string): Boolean;
begin
  Result:=false;
  Token.TokenType:=stString;
  Delete(Line, 1, 1);
  if Line='' then Exit;
  while Line<>'' do
  begin
    if Line[1]='''' then
    begin
      Delete(Line, 1, 1);
      if (Line='') or (Line[1]<>'''') then
      begin
        Result:=true;
        Break;
      end;
    end;
    Token.Value:=Token.Value+Line[1];
    Delete(Line, 1, 1);
  end;
  if not Result then Error('Unterminated string');
  FinTokenParse(Line);
end;

function TSynTexAssembler.ParseTokenInteger(Token: PSynTexToken; var Line: string): Boolean;
var
  IsHex: Boolean;
begin
  Result:=false;
  Token.TokenType:=stInteger;
  Token.Value:=Line[1];
  IsHex:=Token.Value='$';
  Delete(Line, 1, 1);
  if (Token.Value[1] in ['$', '-']) and (Line='') then Exit;
  while Line<>'' do
  begin
    if (Line[1] in ['0'..'9']) or
      (IsHex and (Line[1] in ['a'..'f', 'A'..'F'])) or
      (not IsHex and (Token.TokenType=stInteger) and (Line[1]='.')) then
    begin
      Token.Value:=Token.Value+Line[1];
      if Line[1]='.' then Token.TokenType:=stFloat;
      Delete(Line, 1, 1);
    end
      else Break;
  end;
  Result:=true;
  FinTokenParse(Line);
end;

function TSynTexAssembler.ParseTokenRegister(Token: PSynTexToken; var Line: string): Boolean;
begin
  Result:=false;
  Token.TokenType:=stRegister;
  Delete(Line, 1, 1);
  if Line='' then Exit;
  while (Line<>'') and (Line[1] in ['0'..'9']) do
  begin
    Token.Value:=Token.Value+Line[1];
    Delete(Line, 1, 1);
  end;
  Result:=true;
  FinTokenParse(Line);
end;

function TSynTexAssembler.ParseTokenIdentifier(Token: PSynTexToken; var Line: string): Boolean;
begin
  Token.TokenType:=stIdentifier;
  while (Line<>'') and (Line[1] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
  begin
    Token.Value:=Token.Value+Line[1];
    Delete(Line, 1, 1);
  end;
  if IsCommand(Token.Value) then Token.TokenType:=stCommand;
  if (Line<>'') and (Line[1]=':') and (Token.TokenType<>stCommand) then
  begin
    Token.TokenType:=stLabel;
    Delete(Line, 1, 1);
  end;
  Result:=true;
  FinTokenParse(Line);
end;

procedure TSynTexAssembler.FinTokenParse(var Line: string);
begin
  Line:=TrimLeft(Line);
  if (Line<>'') and (Line[1]=',') then
  begin
    Delete(Line, 1, 1);
    Line:=TrimLeft(Line);
  end;
end;

function TSynTexAssembler.IsCommand(S: string): Boolean;
var
  i: TCmdCode;
begin
  Result:=false;
  S:=UpperCase(S);
  for i:=Low(TCmdCode) to High(TCmdCode) do
    if Commands[i]=S then Result:=true;
end;

function TSynTexAssembler.AssembleCommand(var Token: PSynTexToken): Boolean;
var
  Cmd: TSynTexCmd;
  CmdCode: TCmdCode;
  Int: Integer;
begin
  Result:=false;
  Cmd.Fixed.CmdCode:=0;
  Cmd.RegsCodes:=nil;
  Cmd.Params:=TMemoryStream.Create;
  try
    Token.Value:=UpperCase(Token.Value);
    for CmdCode:=Low(TCmdCode) to High(TCmdCode) do
      if Commands[CmdCode]=Token.Value then
      begin
        Cmd.Fixed.CmdCode:=Byte(CmdCode) shl 4;
        Break;
      end;
    case TCmdCode(Cmd.Fixed.CmdCode shr 4) of
      ccRet:
        begin
          Cmd.Fixed.CmdParamsLen:=0;
          Result:=WriteCommand(Cmd);
          if not Result then Exit; 
          Result:=not Assigned(Token.Next);
          if not Result then Error('Extra token(s) after RET');
        end;
      ccJump: Result:=AssembleLink(Token, Cmd);
      ccCall: Result:=AssembleLink(Token, Cmd);
      ccLoad: Result:=AssembleStoreLoad(Token, Cmd);
      ccStore: Result:=AssembleStoreLoad(Token, Cmd);
      ccFilter: Result:=AssembleFilter(Token, Cmd);
      ccSetRandomSeed:
        begin
          if not Assigned(Token.Next) then
          begin
            Error('Integer expected');
            Exit;
          end;
          Token:=Token.Next;
          if Token.TokenType<>stInteger then
          begin
            Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
            Exit;
          end;
          if not TokenValueInteger(Token, Int) then Exit;
          Cmd.Params.Write(Int, SizeOf(Int));
          Result:=WriteCommand(Cmd);
        end;
      else begin
        Error('Internal error AC1');
        Exit;
      end;
    end;
  finally
    FAN(Cmd.Params);
  end
end;

function TSynTexAssembler.AssembleLink(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
var
  Int: Integer;
begin
  Result:=false;
  if not Assigned(Token.Next) then
  begin
    Error('Identifier expected');
    Exit;
  end;
  Token:=Token.Next;
  if Token.TokenType<>stIdentifier then
  begin
    Error('Identifier expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  Int:=-1;
  Cmd.Params.Write(Int, SizeOf(Int));
  Result:=WriteCommand(Cmd);
  if Result then AddLink(Token.Value, FCode.Position-SizeOf(Int));
end;

function TSynTexAssembler.AssembleStoreLoad(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
var
  NamesCount, Reg: Integer;
  Len: Byte;
begin
  Result:=false;
  NamesCount:=0;
  while Assigned(Token.Next) do
  begin
    Token:=Token.Next;
    case Token.TokenType of
      stString:
        begin
          if Length(Token.Value)>255 then
          begin
            Error('Name "'+Token.Value+'" is longer than 255');
            Exit;
          end;
          Len:=Length(Token.Value);
          Cmd.Params.Write(Len, SizeOf(Len));
          Cmd.Params.Write(Token.Value[1], Len);
          Inc(NamesCount);
        end;
      stRegister:
        begin
          Reg:=GetRegister(Token.Value);
          if Reg<0 then Exit;
          SetLength(Cmd.RegsCodes, Length(Cmd.RegsCodes)+1);
          Cmd.RegsCodes[High(Cmd.RegsCodes)]:=Reg;
        end;
      else begin
        Error('String or register expected, but '+TokenName[Token.TokenType]+' found');
        Exit;
      end;
    end;
  end;
  if NamesCount<>Length(Cmd.RegsCodes) then
  begin
    Error('Names count don''t equal to registers count');
    Exit;
  end;
  Result:=WriteCommand(Cmd);
end;

function TSynTexAssembler.AssembleFilter(var Token: PSynTexToken; var Cmd: TSynTexCmd): Boolean;
var
  FilterID, Reg, i: Integer;
  CmdTokens: TList;
  FilterAssembler: TSynTexFilterAssembler;
  FilterToken: PSynTexToken;
begin
  Result:=false;
  FilterID:=-1;
  FilterAssembler:=nil;
  FilterToken:=Token;
  CmdTokens:=TList.Create;
  try
    while Assigned(Token.Next) do
    begin
      Token:=Token.Next;
      case Token.TokenType of
        stRegister:
          begin
            Reg:=GetRegister(Token.Value);
            if Reg<0 then Exit;
            SetLength(Cmd.RegsCodes, Length(Cmd.RegsCodes)+1);
            Cmd.RegsCodes[High(Cmd.RegsCodes)]:=Reg;
            CmdTokens.Add(Token);
          end;
        stIdentifier:
          begin
            if FilterID<0 then
            begin
              FilterID:=FindFilterAssembler(Token.Value);
              if FilterID<0 then
              begin
                Error('Filter '+Token.Value+' not found');
                Exit;
              end;
              FilterAssembler:=FFilterAssemblers[FilterID].FilterAssembler;
              FilterID:=FFilterAssemblers[FilterID].ID;
              Cmd.Params.Write(Byte(FilterID), 1);
              CmdTokens.Add(Token);
            end;
          end;
        stString, stInteger, stFloat:;
        else begin
          Error('String, integer, float, register or identifier expected, but '+TokenName[Token.TokenType]+' found');
          Exit;
        end;
      end;
    end;
    for i:=0 to CmdTokens.Count-1 do
      FreeToken(CmdTokens[i]);
    Token:=FilterToken;  
    if not (Assigned(FilterAssembler) and FilterAssembler(FilterToken.Next, Cmd.Params, Length(Cmd.RegsCodes))) then Exit;
  finally
    FAN(CmdTokens);
  end;
  Result:=WriteCommand(Cmd);
end;

function TSynTexAssembler.WriteCommand(var Cmd: TSynTexCmd): Boolean;
var
  Regs: packed array of Byte;
  i: Integer;
begin
  Result:=false;
  if Length(Cmd.RegsCodes)>15 then
  begin
    Error('Maximum 15 registers can processing in command');
    Exit;
  end;
  if Cmd.Params.Size>255 then
  begin
    Error('Maximum 255 bytes of parameters can be in command');
    Exit;
  end;
  Cmd.Fixed.CmdCode:=Cmd.Fixed.CmdCode or (Length(Cmd.RegsCodes) and $0F);
  Cmd.Fixed.CmdParamsLen:=Cmd.Params.Size;
  SetLength(Regs, Ceil(Length(Cmd.RegsCodes)/2));
  for i:=0 to High(Cmd.RegsCodes) do
    Regs[i div 2]:=Regs[i div 2] or ((Cmd.RegsCodes[i] and $0F) shl ((i mod 2)*4));
  FCode.Write(Cmd.Fixed, SizeOf(Cmd.Fixed));
  if Length(Cmd.RegsCodes)>0 then FCode.Write(Regs[0], Length(Regs));
  Cmd.Params.Position:=0;
  if Cmd.Params.Size>0 then FCode.CopyFrom(Cmd.Params, Cmd.Params.Size);
  Result:=true;
end;

procedure TSynTexAssembler.AddDebugInfo(Line, Addr: Integer);
begin
  SetLength(FDebugInfo, Length(FDebugInfo)+1);
  FDebugInfo[High(FDebugInfo)].Line:=Line;
  FDebugInfo[High(FDebugInfo)].Addr:=Addr;
end;

function TSynTexAssembler.AddLabel(Name: string; Addr: Integer): Boolean;
var
  i, FreePos: Integer;
begin
  Result:=false;
  FreePos:=-1;
  Name:=UpperCase(Name);
  for i:=0 to High(FLabels) do
  begin
    if FLabels[i].Exist and (FLabels[i].Name=Name) then
    begin
      Error('Label '+Name+' redeclared');
      Exit;
    end;
    if not FLabels[i].Exist then FreePos:=i;
  end;
  if FreePos=-1 then
  begin
    FreePos:=Length(FLabels);
    SetLength(FLabels, FreePos*2);
  end;
  FLabels[FreePos].Name:=Name;
  FLabels[FreePos].Addr:=Addr;
  FLabels[FreePos].Exist:=true;
  for i:=0 to High(FLinks) do
    if FLinks[i].Exist and (FLinks[i].Name=Name) then
    begin
      FCode.Position:=FLinks[i].Addr;
      FCode.Write(Addr, SizeOf(Addr));
      FCode.Seek(0, soFromEnd);
      FLinks[i].Exist:=false;
    end;
  Result:=true;
end;

procedure TSynTexAssembler.AddLink(Name: string; Addr: Integer);
var
  i: Integer;
begin
  Name:=UpperCase(Name);
  for i:=0 to High(FLabels) do
    if FLabels[i].Exist and (FLabels[i].Name=Name) then
    begin
      FCode.Position:=Addr;
      FCode.Write(FLabels[i].Addr, SizeOf(FLabels[i].Addr));
      FCode.Seek(0, soFromEnd);
      Exit;
    end;
  for i:=0 to High(FLinks) do
    if not FLinks[i].Exist then
    begin
      FLinks[i].Name:=Name;
      FLinks[i].Addr:=Addr;
      FLinks[i].Exist:=true;
      Exit;
    end;
  i:=Length(FLinks);
  SetLength(FLinks, i*2);
  FLinks[i].Name:=Name;
  FLinks[i].Addr:=Addr;
  FLinks[i].Exist:=true;
end;

function TSynTexAssembler.GetRegister(const Name: string): Integer;
begin
  Result:=StrToInt(Name);
  if Result>SynTexRegsCount-1 then Result:=-1;
  if Result<0 then Error('Register index '+Name+' out of bounds');
end;

function TSynTexAssembler.NewToken(Prev: PSynTexToken): PSynTexToken;
begin
  New(Result);
  Result.Prev:=Prev;
  if Assigned(Prev) then Prev.Next:=Result;
  Result.Next:=nil;
  Result.Value:='';
end;

procedure TSynTexAssembler.FreeToken(Token: PSynTexToken);
begin
  if not Assigned(Token) then Exit;
  if Assigned(Token.Prev) then Token.Prev.Next:=Token.Next;
  if Assigned(Token.Next) then Token.Next.Prev:=Token.Prev;
  Dispose(Token);
end;

procedure TSynTexAssembler.FinTokens(Token: PSynTexToken);
begin
  while Assigned(Token.Next) do Token:=Token.Next;
  while Assigned(Token.Prev) do
  begin
    Token:=Token.Prev;
    FreeToken(Token.Next);
  end;
  FreeToken(Token);
end;

function TSynTexAssembler.FindFilterAssembler(Name: string): Integer;
var
 i: Integer;
begin
  Result:=-1;
  Name:=UpperCase(Name);
  for i:=0 to High(FFilterAssemblers) do
    if FFilterAssemblers[i].Name=Name
      then Result:=i;
end;

function TSynTexAssembler.GetDebugInfoCount: Integer;
begin
  Result:=Length(FDebugInfo);
end;

function TSynTexAssembler.GetDebugInfo(Index: Integer): TSynTexDebugInfo;
begin
  Result.Line:=-1;
  Result.Addr:=-1;
  if (Index<0) or (Index>High(FDebugInfo)) then Exit;
  Result:=FDebugInfo[Index];
end;

end.