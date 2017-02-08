unit SynTexFilterAssemblers;

interface

uses
  Windows, AvL, avlUtils, SynTex, SynTexAssembler;

type
  TSynTexFilterAssemblers=class
  protected
    FAssembler: TSynTexAssembler;
    function GetInteger(Token: PSynTexToken; MinValue, MaxValue: Integer; FilterName, ParamName: string; var Value: Integer): Boolean;
    function IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
  public
    constructor Create(SynTexAssembler: TSynTexAssembler);
    function AssembleFill(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleAdd(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssemblePixels(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleBlend(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleMakeAlpha(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssemblePerlin(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleBump(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleNormals(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleGlowRect(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleDistort(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleTransform(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleBlur(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleColorRange(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleAdjust(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
  end;

implementation

{$I SynTexFilters.inc}

constructor TSynTexFilterAssemblers.Create(SynTexAssembler: TSynTexAssembler);

  procedure AddFilterAssembler(ID: Byte; FilterAssembler: TSynTexFilterAssembler);
  begin
    FAssembler.AddFilterAssembler(FltNames[ID], ID, FilterAssembler);
  end;

begin
  inherited Create;
  FAssembler:=SynTexAssembler;
  AddFilterAssembler(FLT_FILL, AssembleFill);
  AddFilterAssembler(FLT_ADD, AssembleAdd);
  AddFilterAssembler(FLT_PIXELS, AssemblePixels);
  AddFilterAssembler(FLT_BLEND, AssembleBlend);
  AddFilterAssembler(FLT_MAKEALPHA, AssembleMakeAlpha);
  AddFilterAssembler(FLT_PERLIN, AssemblePerlin);
  AddFilterAssembler(FLT_BUMP, AssembleBump);
  AddFilterAssembler(FLT_NORMALS, AssembleNormals);
  AddFilterAssembler(FLT_GLOWRECT, AssembleGlowRect);
  AddFilterAssembler(FLT_DISTORT, AssembleDistort);
  AddFilterAssembler(FLT_TRANSFORM, AssembleTransform);
  AddFilterAssembler(FLT_BLUR, AssembleBlur);
  AddFilterAssembler(FLT_COLORRANGE, AssembleColorRange);
  AddFilterAssembler(FLT_ADJUST, AssembleAdjust);
end;

function TSynTexFilterAssemblers.AssembleFill(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Color: Integer;
begin
  Result:=false;
  if RegsCount<1 then
  begin
    FAssembler.Error('Filter FILL needs at least 1 register');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  if Token.TokenType<>stInteger then
  begin
    FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  if not FAssembler.TokenValueInteger(Token, Color) then Exit;
  Params.Write(Color, SizeOf(Color));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter FILL color');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleAdd(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Mode: Byte;
begin
  Result:=false;
  if RegsCount<2 then
  begin
    FAssembler.Error('Filter ADD needs at least 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Identifier expected');
    Exit;
  end;
  if Token.TokenType<>stIdentifier then
  begin
    FAssembler.Error('Identifier expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  Mode:=IdentifierToIndex(AddModes, Token.Value);
  if Mode=$FF then
  begin
    FAssembler.Error('Unknown filter ADD mode');
    Exit;
  end;
  Params.Write(Mode, SizeOf(Mode));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter ADD mode');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssemblePixels(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..2] of Integer; //Count, Color0, Color1
  i: Integer;
begin
  Result:=false;
  if RegsCount<1 then
  begin
    FAssembler.Error('Filter PIXELS needs at least 1 register');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 2 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, ParamsBuf[i]) then Exit;
    if i<2 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf, SizeOf(Integer)*3);
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter PIXELS parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleBlend(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
begin
  Result:=false;
  if RegsCount<>4 then
  begin
    FAssembler.Error('Filter BLEND needs 4 registers');
    Exit;
  end;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter BLEND');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleMakeAlpha(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter MAKEALPHA needs 2 registers');
    Exit;
  end;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter MAKEALPHA');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssemblePerlin(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..5] of Integer; //Freq, Octaves, Fade, Amp, Color0, Color1
  i: Integer;
begin
  Result:=false;
  if RegsCount<>1 then
  begin
    FAssembler.Error('Filter PERLIN needs 1 register');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 5 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, ParamsBuf[i]) then Exit;
    if i<5 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  if (ParamsBuf[0]<0) or (ParamsBuf[0]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter FREQ out of bounds [0..255]');
    Exit;
  end;
  if (ParamsBuf[1]<0) or (ParamsBuf[1]>7) then
  begin
    FAssembler.Error('Filter PERLIN parameter OCTAVES out of bounds [0..7]');
    Exit;
  end;
  if (ParamsBuf[2]<0) or (ParamsBuf[2]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter FADE out of bounds [0..255]');
    Exit;
  end;
  if (ParamsBuf[3]<0) or (ParamsBuf[3]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter AMP out of bounds [0..255]');
    Exit;
  end;
  Params.Write(ParamsBuf[0], 1);
  Params.Write(ParamsBuf[1], 1);
  Params.Write(ParamsBuf[2], 1);
  Params.Write(ParamsBuf[3], 1);
  Params.Write(ParamsBuf[4], SizeOf(Integer)*2);
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter PERLIN parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleBump(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamsNames: array[0..5] of string =
    ('THETA', 'PHI', 'AMPLIFY', 'DIFFAMOUNT', 'SPECAMOUNT', 'SPECPOWER');
var
  ParamsBuf: packed array[0..5] of Byte; //Theta, Phi, Amplify, Diffuse amount, Specular amount, Specular power
  i, Val: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter BUMP needs 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 5 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, Val) then Exit;
    if (Val<0) or (Val>255) then
    begin
      FAssembler.Error('Filter BUMP parameter '+ParamsNames[i]+' out of bounds [0..255]');
      Exit;
    end;
    ParamsBuf[i]:=Val;
    if i<5 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf[0], SizeOf(ParamsBuf));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter BUMP parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleNormals(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Amount: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter NORMALS needs 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  if Token.TokenType<>stInteger then
  begin
    FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  if not FAssembler.TokenValueInteger(Token, Amount) then Exit;
  if Amount>255 then
  begin
    FAssembler.Error('Filter NORMALS parameter AMOUNT out of bounds [0..255]');
    Exit;
  end;
  Params.Write(Amount, SizeOf(Byte));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter NORMALS color');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleGlowRect(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamsNames: array[0..8] of string =
    ('POSX', 'POSY', 'RADX', 'RADY', 'SIZEX', 'SIZEY', 'BLEND', 'POWER', 'COLOR');
var
  ParamsBuf: packed array[0..7] of Word;
  Color: Integer;
  i, Val: Integer;
begin
  Result:=false;
  if RegsCount<1 then
  begin
    FAssembler.Error('Filter GLOWRECT needs at least 1 register');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 8 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, Val) then Exit;
    if i<8 then
    begin
      if (Val<0) or (Val>65535) then
      begin
        FAssembler.Error('Filter GLOWRECT parameter '+ParamsNames[i]+' out of bounds [0..65535]');
        Exit;
      end;
      ParamsBuf[i]:=Val;
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
    end
      else Color:=Val;
  end;
  Params.Write(ParamsBuf[0], SizeOf(ParamsBuf));
  Params.Write(Color, SizeOf(Color));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter GLOWRECT parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleDistort(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamsNames: array[0..1] of string =
    ('AMOUNT', 'FLAGS');
var
  ParamsBuf: packed array[0..1] of Byte;
  i, Val: Integer;
begin
  Result:=false;
  if RegsCount<>3 then
  begin
    FAssembler.Error('Filter DISTORT needs 3 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 1 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, Val) then Exit;
    if (Val<0) or (Val>255) then
    begin
      FAssembler.Error('Filter DISTORT parameter '+ParamsNames[i]+' out of bounds [0..255]');
      Exit;
    end;
    ParamsBuf[i]:=Val;
    if i<1 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf[0], SizeOf(ParamsBuf));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter DISTORT parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleTransform(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamsNames: array[0..5] of string =
    ('ANGLE', 'ZOOMX', 'ZOOMY', 'SCROLLX', 'SCROLLY', 'FLAGS');
  Bytes=[0, 5];
var
  ParamsBuf: packed array[0..5] of Word;
  i, Val: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter TRANSFORM needs 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 5 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, Val) then Exit;
    if ((Val<0) or (Val>65535)) and not (i in Bytes) then
    begin
      FAssembler.Error('Filter TRANSFORM parameter '+ParamsNames[i]+' out of bounds [0..65545]');
      Exit;
    end;
    if ((Val<0) or (Val>255)) and (i in Bytes) then
    begin
      FAssembler.Error('Filter TRANSFORM parameter '+ParamsNames[i]+' out of bounds [0..255]');
      Exit;
    end;
    ParamsBuf[i]:=Val;
    if i<5 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf[0], SizeOf(Byte));
  Params.Write(ParamsBuf[1], SizeOf(Word)*4);
  Params.Write(ParamsBuf[5], SizeOf(Byte));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter TRANSFORM parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleBlur(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
type
  TParams=packed record
    SizeX, SizeY, Filter, Amp, Flags: Byte;
  end;
var
  P: TParams;
  I: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter BLUR needs 2 registers');
    Exit;
  end;
  if not GetInteger(Token, 0, 255, 'BLUR', 'SIZEX', I) then Exit;
  P.SizeX:=I;
  if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  if not GetInteger(Token, 0, 255, 'BLUR', 'SIZEY', I) then Exit;
  P.SizeY:=I;
  if not FAssembler.NextToken(Token, 'Identifier expected') then Exit;
  if Token.TokenType<>stIdentifier then
  begin
    FAssembler.Error('Identifier expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  P.Filter:=IdentifierToIndex(BlurFilters, Token.Value);
  if P.Filter=$FF then
  begin
    FAssembler.Error('Unknown filter BLUR filter');
    Exit;
  end;
  if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  if not GetInteger(Token, 0, 255, 'BLUR', 'AMP', I) then Exit;
  P.Amp:=I;
  if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  if not GetInteger(Token, 0, 255, 'BLUR', 'FLAGS', I) then Exit;
  P.Flags:=I;
  Params.Write(P, SizeOf(P));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter BLUR mode');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleColorRange(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..1] of Integer;
  i: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter COLORRANGE needs 2 registers');
    Exit;
  end;
  for i:=0 to 1 do
  begin
    if not GetInteger(Token, -MaxInt-1, MaxInt, 'COLORRANGE', 'COLOR'+IntToStr(i), ParamsBuf[i]) then Exit;
    if i<1 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf, SizeOf(ParamsBuf));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter COLORRANGE parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleAdjust(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamNames: array[0..2] of string = ('BRIGHTNESS', 'CONTRAST', 'GAMMA');
var
  ParamsBuf: array[0..2] of Byte;
  i, Int: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter ADJUST needs 2 registers');
    Exit;
  end;
  for i:=0 to 2 do
  begin
    if not GetInteger(Token, 0, 255, 'ADJUST', ParamNames[i], Int) then Exit;
    ParamsBuf[i]:=Int;
    if i<2 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf, SizeOf(ParamsBuf));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter ADJUST parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.GetInteger(Token: PSynTexToken; MinValue, MaxValue: Integer; FilterName, ParamName: string; var Value: Integer): Boolean;
begin
  Result:=false;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  if Token.TokenType<>stInteger then
  begin
    FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  if not FAssembler.TokenValueInteger(Token, Value) then Exit;
  if (Value<MinValue) or (Value>MaxValue) then
  begin
    FAssembler.Error(Format('Filter %s parameter %s out of bounds [%d..%d]', [FilterName, ParamName, MinValue, MaxValue]));
    Exit;
  end;
  Token:=Token.Next;
  Result:=true;
end;

function TSynTexFilterAssemblers.IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
begin
  Identifier:=UpperCase(Identifier);
  for Result:=Low(Identifiers) to High(Identifiers) do
    if Identifiers[Result]=Identifier then Exit;
  Result:=-1;
end;

end.
