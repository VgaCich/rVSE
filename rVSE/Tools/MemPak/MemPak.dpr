program MemPak;

{$APPTYPE CONSOLE}

uses
  Windows, AvL, avlUtils;

{$I VSEMemPakTypes.inc}

procedure FillList(FilesList: TStringList; const Mask: string);
var
  SR: TSearchRec;
  Dir: string;
begin
  Dir:=ExtractFilePath(Mask);
  if FindFirst(Mask, 0, SR)=0 then
    repeat
      FilesList.Add(ExpandFileName(Dir+SR.Name));
    until FindNext(SR)<>0;
  FindClose(SR);
end;

procedure Add(Pak: TStream; FileName: string);
var
  IFile: TFileStream;
  Hdr: TFileHeader;
begin
  IFile:=TFileStream.Create(FileName, fmOpenRead);
  try
    FileName:=ExtractFileName(FileName);
    WriteLn('Adding: ', FileName, ': ', IFile.Size);
    Hdr.NameLen:=Length(FileName);
    Hdr.FileSize:=IFile.Size;
    Pak.Write(Hdr, SizeOf(Hdr));
    Pak.Write(FileName[1], Hdr.NameLen);
    Pak.CopyFrom(IFile, Hdr.FileSize);
  finally
    FAN(IFile);
  end;
end;

procedure SaveToPas(Pak: TMemoryStream; const FileName: string);
var
  i: Integer;
  Pas: TStringList;
  S: string;
begin
  Pas:=TStringList.Create;
  try
    Pas.Text:='unit MemPak;'#13#10#13#10+
              'interface'#13#10#13#10;
    if Pak.Size>0 then
    begin
      Pas.Add('const MemPakData: array[0..'+IntToStr(Pak.Size-1)+'] of Byte = ');
      S:='(';
      for i:=0 to Pak.Size-1 do
      begin
        S:=S+'$'+IntToHex(PByteArray(Pak.Memory)[i], 2)+', ';
        if i mod 16 = 15 then
        begin
          Pas.Add(S);
          S:=' ';
        end;
      end;
      if S<>' ' then Pas.Add(S);
      Pas[Pas.Count-1]:=Copy(Pas[Pas.Count-1], 1, Length(Pas[Pas.Count-1])-2)+');';
    end
    else
      Pas.Add('const MemPakData: array[0..0] of Byte = (0);');
    Pas.Text:=Pas.Text+#13#10+
              'implementation'#13#10#13#10+
              'end.';
    Pas.SaveToFile(FileName);
  finally
    FAN(Pas);
  end;
end;

var
  FilesList: TStringList;
  Pak: TMemoryStream;
  i: Integer;

begin
  WriteLn('MemPak 2.0');
  WriteLn('(c)VgaSoft, 2007-2016');
  WriteLn;
  if ParamCount<2 then
  begin
    WriteLn('Usage:');
    WriteLn('  MemPak <out file> <in file> [in file] ...');
    Exit;
  end;
  FilesList:=TStringList.Create;
  try
    for i:=2 to ParamCount do
      FillList(FilesList, ParamStr(i));
    Pak:=TMemoryStream.Create;
    try
      for i:=0 to FilesList.Count-1 do
        Add(Pak, FilesList[i]);
      if SameText(ExtractFileExt(ParamStr(1)), '.pas') then
        SaveToPas(Pak, ParamStr(1))
      else
        Pak.SaveToFile(ParamStr(1));
    finally
      FAN(Pak);
    end;
  finally
    FAN(FilesList);
  end;
end.
