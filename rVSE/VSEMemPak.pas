unit VSEMemPak;

interface

uses
  Windows, AvL, avlUtils, VSECore;

implementation

{$IFDEF VSE_USE_MEMPAK_PAS}
uses
  MemPak;
{$ENDIF}

type
  TMemPak = class(TModule)
  private
    FData: TCustomMemoryStream;
    FOldHandler: TOnGetFile;
    function GetFile(const FileName: string): TStream;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
  end;
  TPakStream = class(TCustomMemoryStream)
  public
    constructor Create(Data: Pointer; Size: Longint);
  end;

{$I VSEMemPakTypes.inc}

{ TMemPak }

constructor TMemPak.Create;
begin
  inherited;
  {$IFDEF VSE_USE_MEMPAK_PAS}
  FData := TPakStream.Create(@MemPakData, SizeOf(MemPakData));
  {$ELSE}
  FData := TResourceStream.Create(hInstance, 'MemPak', RT_RCDATA);
  {$ENDIF}
  FOldHandler := Core.OnGetFile;
  Core.OnGetFile := GetFile;
end;

destructor TMemPak.Destroy;
begin
  Core.OnGetFile := FOldHandler;
  FAN(FData);
  inherited;
end;

function TMemPak.GetFile(const FileName: string): TStream;
var
  Hdr: TFileHeader;
  Name: string;
begin
  Result := nil;
  FData.Position := 0;
  while FData.Position < FData.Size do
  begin
    FData.Read(Hdr, SizeOf(Hdr));
    SetLength(Name, Hdr.NameLen);
    FData.Read(Name[1], Hdr.NameLen);
    if SameText(Name, FileName) then
    begin
      Result := TPakStream.Create(Pointer(Longint(FData.Memory) + FData.Position), Hdr.FileSize);
      Exit;
    end
      else FData.Seek(Hdr.FileSize, soFromCurrent);
  end;
  if Assigned(FOldHandler) then
    Result := FOldHandler(FileName);
end;

class function TMemPak.Name: string;
begin
  Result := 'MemPak';
end;

{ TPakStream }

constructor TPakStream.Create(Data: Pointer; Size: Longint);
begin
  inherited Create;
  SetPointer(Data, Size);
end;

initialization
  RegisterModule(TMemPak);

end.
