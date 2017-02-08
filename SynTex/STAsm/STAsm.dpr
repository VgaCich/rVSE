program STAsm;

{$APPTYPE CONSOLE}

uses
  Windows, AvL, avlUtils, SynTexAssembler, SynTexFilterAssemblers;

type
  TSynTexAssemblerC=class(TSynTexAssembler)
  private
    procedure ShowError(Sender: TObject; Line: Integer; const Msg: string);
  public
    constructor Create;
  end;

constructor TSynTexAssemblerC.Create;
begin
  inherited Create;
  OnError:=ShowError;
end;

procedure TSynTexAssemblerC.ShowError(Sender: TObject; Line: Integer; const Msg: string);
begin
  WriteLn(Line, ': Error: '+Msg);
end;

var
  F: TFileStream;
  STA: TSynTexAssemblerC;
  STFA: TSynTexFilterAssemblers;

begin
  WriteLn('SynTex Assembler 1.0');
  WriteLn('(c)VgaSoft, 2004-2007');
  WriteLn;
  ExitCode:=1;
  if ParamCount<>2 then
  begin
    WriteLn('Usage:');
    WriteLn('  STAsm <source file> <code file>');
    Exit;
  end;
  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('Error: source file "', ParamStr(1), '" not exists');
    Exit;
  end;
  STA:=TSynTexAssemblerC.Create;
  STFA:=TSynTexFilterAssemblers.Create(STA);
  try
    STA.Source.LoadFromFile(ParamStr(1));
    if STA.Assemble then
    begin
      WriteLn('File "'+ParamStr(1)+'" successfully assembled');
      F:=TFileStream.Create(ParamStr(2), fmCreate);
      try
        F.CopyFrom(STA.Code, 0);
      finally
        FAN(F);
      end;
      ExitCode:=0;
    end
      else WriteLn('File "'+ParamStr(1)+'" not assembled due to errors');
  finally
    FAN(STFA);
    FAN(STA);
  end;
end.