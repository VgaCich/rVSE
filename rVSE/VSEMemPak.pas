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
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure OnEvent(var Event: TCoreEvent); override;
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
end;

destructor TMemPak.Destroy;
begin
  FAN(FData);
  inherited;
end;

procedure TMemPak.OnEvent(var Event: TCoreEvent);
var
  Hdr: TFileHeader;
  Name: string;
begin
  if Event is TGetFileEvent then
    with Event as TGetFileEvent do
    begin
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
    end
  else inherited;
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
