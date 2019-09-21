unit VSESysInfo;

interface

uses
  Windows, AvL, avlUtils, OpenGL, oglExtensions, VSELog, VSECore;

type
  TSystemInfo = class(TModule)
  private
    procedure LogSysInfo;
    procedure LogGLCaps;
    {$IFDEF VSE_DEBUG}procedure LogUnits;{$ENDIF}
  public
    class function Name: string; override;
    procedure OnEvent(var Event: TCoreEvent); override;
  end;

implementation

type
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: Int64;
    ullAvailPhys: Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual: Int64;
    ullAvailVirtual: Int64;
    ullAvailExtendedVirtual: Int64;
  end;
  TGlobalMemoryStatusEx = procedure(var lpBuffer:TMemoryStatusEx); stdcall;
  TMemoryInfo = record
    TotalPhys, AvailPhys: Int64;
  end;

function GetCPU: string;
var
  CPUName: array [0..95] of Char;

  procedure GetCPUName;
  asm
    mov eax, $80000002
    db $0F, $A2
    mov dword ptr[CPUName], eax
    mov dword ptr[CPUName+4], ebx
    mov dword ptr[CPUName+8], ecx
    mov dword ptr[CPUName+12], edx

    mov eax, $80000003
    db $0F, $A2
    mov dword ptr[CPUName+16], eax
    mov dword ptr[CPUName+20], ebx
    mov dword ptr[CPUName+24], ecx
    mov dword ptr[CPUName+28], edx

    mov eax, $80000004
    db $0F, $A2
    mov dword ptr[CPUName+32], eax
    mov dword ptr[CPUName+36], ebx
    mov dword ptr[CPUName+40], ecx
    mov dword ptr[CPUName+44], edx
  end;

begin
  try
    GetCPUName;
    Result := Trim(CPUName);
  except
    Result := 'Error while detecting CPU!'
  end;
end;

function GetMemoryInfo: TMemoryInfo;
var
  MemStatus: TMemoryStatus;
  MemStatusEx: TMemoryStatusEx;
  GlobalMemoryStatusEx: TGlobalMemoryStatusEx;
  Kernel32: hModule;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatus(MemStatus);
  with Result, MemStatus do
  begin
    TotalPhys := dwTotalPhys + 655360;
    AvailPhys := dwAvailPhys;
  end;
  Kernel32 := LoadLibrary('kernel32.dll');
  if Kernel32 <> 0 then
  try
    GlobalMemoryStatusEx := GetProcAddress(Kernel32, 'GlobalMemoryStatusEx');
    if Assigned(GlobalMemoryStatusEx) then
    begin
      MemStatusEx.dwLength := SizeOf(MemStatusEx);
      GlobalMemoryStatusEx(MemStatusEx);
      with Result, MemStatusEx do
      begin
        TotalPhys := ullTotalPhys + 655360;
        AvailPhys := ullAvailPhys;
      end;
    end;
  finally
    FreeLibrary(Kernel32);
  end;
end;

{ TSystemInfo }

class function TSystemInfo.Name: string;
begin
  Result := 'SystemInfo';
end;

procedure TSystemInfo.OnEvent(var Event: TCoreEvent);
begin
  if (Event is TSysNotify) and ((Event as TSysNotify).Notify = snLogSysInfo) then
  begin
    LogRaw(llInfo, '');
    {$IFDEF VSE_DEBUG}LogUnits;{$ENDIF}
    LogSysInfo;
    LogGLCaps;
  end;
  inherited;
end;

procedure TSystemInfo.LogSysInfo;
begin
  GetWinVer;
  LogRaw(llInfo, 'System:');
  LogRaw(llInfo, Format('%s (%d.%d.%d %s)', [Win32Type, Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
  LogRaw(llInfo, 'CPU: ' + GetCPU);
  with GetMemoryInfo do
    LogRaw(llInfo, Format('Memory: total %s, free %s', [SizeToStr(TotalPhys), SizeToStr(AvailPhys)]));
  LogRaw(llInfo, '');
end;

procedure TSystemInfo.LogGLCaps;

  procedure LogGLCap(const Name: string; Cap: GLenum);
  var
    Value: Integer;
  begin
    glGetIntegerv(Cap, @Value);
    LogRaw(llInfo, Name + IntToStr(Value));
  end;

begin
  LogRaw(llInfo, 'OpenGL capabilities:');
  LogRaw(llInfo, 'GL_VENDOR = ' + string(glGetString(GL_VENDOR)));
  LogRaw(llInfo, 'GL_RENDERER = ' + string(glGetString(GL_RENDERER)));
  LogRaw(llInfo, 'GL_VERSION = ' + string(glGetString(GL_VERSION)));
  LogRaw(llInfo, 'GL_EXTENSIONS = ' + glExtensionsString);
  LogRaw(llInfo, 'VSync control support: ' + BoolToStr(WGL_EXT_swap_control));
  LogRaw(llInfo, 'Multitexturing support: ' + BoolToStr(GL_ARB_multitexture));
  LogRaw(llInfo, 'FBO support: ' + BoolToStr(GL_EXT_framebuffer_object));
  LogRaw(llInfo, 'VBO support: ' + BoolToStr(GL_ARB_vertex_buffer_object));
  LogRaw(llInfo, 'GLSL support: ' + BoolToStr(GL_ARB_shading_language_100));
  LogRaw(llInfo, 'Maximum texture units: ' + IntToStr(glMaxTextureUnits));
  LogRaw(llInfo, 'Maximum texture size: ' + IntToStr(glMaxTextureSize));
  LogRaw(llInfo, 'Maximum anisotropy filter: ' + IntToStr(glMaxAnisotropy));
  LogRaw(llInfo, 'Maximum texture image units: ' + IntToStr(glMaxTextureImageUnits));
  LogGLCap('Maximum vertex attribs: ', GL_MAX_VERTEX_ATTRIBS_ARB);
  LogGLCap('Maximum vertex uniform components: ', GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB);
  LogGLCap('Maximum varying floats: ', GL_MAX_VARYING_FLOATS_ARB);
  LogGLCap('Maximum vertex texture image units: ', GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB);
  LogGLCap('Maximum combined texture image units: ', GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB);
  LogGLCap('Maximum texture coords: ', GL_MAX_TEXTURE_COORDS_ARB);
  LogGLCap('Maximum fragment uniform components: ', GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB);
  LogRaw(llInfo, '');
end;

{$IFDEF VSE_DEBUG}
procedure TSystemInfo.LogUnits;
var
  MapFile: string;
  Map: TStringList;
  BaseAddr: Cardinal;
  InitContext: PInitContext;
  i: Integer;

  function FindSymbol(const Name: string): Cardinal;
  var
    i: Integer;
    S: string;
  begin
    Result := 0;
    for i := 0 to Map.Count - 1 do
      if Pos(Name, Map[i]) > 0 then
      begin
        S := Trim(Map[i]);
        Delete(S, FirstDelimiter(' ', S), MaxInt);
        Delete(S, 1, FirstDelimiter(':', S));
        Result := StrToCar('$' + S);
        Break;
      end;
  end;

  function FindAddress(Addr: Cardinal): string;
  var
    i: Integer;
    S: string;
  begin
    Result := '< not found >';
    S := Format(':%08X', [Addr]);
    for i := 0 to Map.Count - 1 do
      if Pos(S, Map[i]) > 0 then
      begin
        Result := Trim(Map[i]);
        Delete(Result, 1, LastDelimiter(' ', Result));
        Break;
      end;
  end;

begin
  MapFile := ChangeFileExt(FullExeName, '.map');
  if not FileExists(MapFile) then Exit;
  Map := TStringList.Create;
  try
    Map.LoadFromFile(MapFile);
    BaseAddr := Cardinal(@GetCPU) - FindSymbol('GetCPU');
    InitContext := Pointer(Cardinal(@InitSettings) - FindSymbol('InitSettings') + FindSymbol('InitContext'));
    LogRaw(llDebug, 'Units info:');
    if (BaseAddr = Cardinal(@GetCPU)) or (InitContext = @InitSettings) then
    begin
      LogRaw(llDebug, 'Error: can''t find InitContext address');
      Exit;
    end;
    LogRaw(llDebug, Format('InitContext: 0x%08x', [InitContext]));
    for i := 0 to InitContext.InitTable.UnitCount - 1 do
      with InitContext.InitTable.UnitInfo[i] do
        if Cardinal(Init) > 0 then
          LogRaw(llDebug, Format('Unit #%d: 0x%08x %s', [i, Cardinal(Init), FindAddress(Cardinal(Init) - BaseAddr)]));
  finally
    FAN(Map);
    LogRaw(llDebug, '');
  end;
end;
{$ENDIF}

initialization
  {$IF Defined(VSE_LOG) and not Defined(VSE_NOSYSINFO)}RegisterModule(TSystemInfo);{$IFEND}

end.
