unit VSEMemPak;

interface

uses
  Windows, AvL, avlUtils;

function GetFile(FileName: string): TStream; //Get file as stream
function GetFileText(const FileName: string): TStringList; //Get text file as TStringList

implementation

{$IFDEF VSE_USE_MEMPAK_PAS}
uses
  MemPak;

type
  TPakStream=class(TCustomMemoryStream)
  public
    constructor Create;
  end;

constructor TPakStream.Create;
begin
  inherited;
  SetPointer(@MemPakData, SizeOf(MemPakData));
end;
{$ENDIF}

{$I VSEMemPakTypes.inc}

function GetFile(FileName: string): TStream;
var
  Hdr: TFileHeader;
  Name: string;
  Pak: TStream;
begin
  Result:=nil;
  if FileExists(ExePath+'Data\'+FileName) then
    Result:=TFileStream.Create(ExePath+'Data\'+FileName, fmOpenRead or fmShareDenyWrite)
  else begin
    {$IFDEF VSE_USE_MEMPAK_PAS}
    Pak:=TPakStream.Create;
    {$ELSE}
    Pak:=TResourceStream.Create(hInstance, 'MemPak', RT_RCDATA);
    {$ENDIF}
    try
      while Pak.Position<Pak.Size do
      begin
        Pak.Read(Hdr, SizeOf(Hdr));
        SetLength(Name, Hdr.NameLen);
        Pak.Read(Name[1], Hdr.NameLen);
        if SameText(Name, FileName) then
        begin
          Result:=TMemoryStream.Create;
          if Hdr.FileSize > 0 then
            Result.CopyFrom(Pak, Hdr.FileSize);
          Result.Position:=0;
          Exit;
        end
          else Pak.Seek(Hdr.FileSize, soFromCurrent);
      end;
    finally
      FAN(Pak);
    end;
  end;
end;

function GetFileText(const FileName: string): TStringList;
var
  F: TStream;
begin
  Result:=nil;
  F:=GetFile(FileName);
  if Assigned(F) then
  try
    Result:=TStringList.Create;
    Result.LoadFromStream(F);
  finally
    FAN(F);
  end;
end;

end.
