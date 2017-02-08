program BestCompress;

{$APPTYPE CONSOLE}

uses
  Windows, AvL, avlUtils;

function Exec(Name: string): boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI) , 0);
  with SI do
  begin
    cb := SizeOf( SI);
  end;
  if not CreateProcess(nil, PChar(Name), nil, nil, false, Create_default_error_mode,
                nil, nil, SI, PI)
    then begin
      Result:=false;
      Exit;
    end;
  WaitForSingleObject(PI.hProcess, infinite);
  Result:=true;
end;

var
  BestSize, BestLC, BestFB, LC, FB: Integer;
  FileName, BFileName: string;

begin
  WriteLn('BestCompress - front-end to Upack');
  WriteLn('(c)VgaSoft, 2004-2007');
  WriteLn;
  if ParamCount<>1 then
  begin
    WriteLn('Usage:');
    WriteLn('BestCompress <file>');
    Exit;
  end;
  FileName:=ExpandFileName(ParamStr(1));
  if not FileExists(FileName) then
  begin
    WriteLn('File "'+FileName+'" not found');
    Exit;
  end;
  BFileName:=ChangeFileExt(FileName, '.org');
  BestSize:=FlSize(FileName);
  BestLC:=-1;
  BestFB:=-1;
  CopyFile(PChar(FileName), PChar(BFileName), false);
  for LC:=0 to 6 do
    for FB:=5 to 273 do
    begin
      CopyFile(PChar(BFileName), PChar(FileName), false);
      if not Exec(Format('"%sUpack.exe" "%s" -c%d -f%d -set -srt', [ExePath, FileName, LC, FB])) then
      begin
        WriteLn('Compressing error');
        Exit;
      end;
      if FlSize(FileName)<BestSize then
      begin
        BestSize:=FlSize(FileName);
        BestLC:=LC;
        BestFB:=FB;
      end;
      WriteLn('LC=', LC, '; FB=', FB);
      WriteLn(FloatToStr2((268*LC+(FB-5))/18.76, 1, 2), '% done');
    end;
  if BestLC=-1 then
  begin
    WriteLn('File is not compressible');
    CopyFile(PChar(BFileName), PChar(FileName), false);
    DeleteFile(BFileName);
  end
  else begin
    WriteLn('Best: LC=', BestLC, '; FB=', BestFB);
    CopyFile(PChar(BFileName), PChar(FileName), false);
    if not Exec(Format('"%sUpack.exe" "%s" -c%d -f%d -set -srt', [ExePath, FileName, BestLC, BestFB])) then
    begin
      WriteLn('Compressing error');
      Exit;
    end;
    DeleteFile(BFileName);
  end;
end.
