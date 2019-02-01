unit VSESound;

interface

uses
  Windows, AvL, avlUtils, MMSystem, DirectSound, VSECore;

//{$I dsound.inc}

type
  TSound=class(TModule)
  private
    FDirectSound: IDirectSound;
    FMusicBuffer: IDirectSoundBuffer;
    FMusicFile: TCustomMemoryStream;
    FMusicPCM: TWaveFormatEx;
    FMusicBufferDesc: TDSBufferDesc;
    FEnableBGM: Boolean;
    procedure SetEnableBGM(Value: Boolean);
    function GetVolume: Integer;
    procedure SetVolume(const Value: Integer);
    {$IFDEF VSE_CONSOLE}
    function BGMHandler(Sender: TObject; Args: array of const): Boolean;
    function BGMVolHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    {$IF Defined(VSE_LOG) and not Defined(VSE_NOSYSINFO)}procedure LogCaps;{$IFEND}
    procedure Update; override;//internally used
    function SysNotify(Notify: TSysNotify): Boolean; override; //internally used
    procedure PlayMusic(const FileName: string); //Play music from file
    procedure StopMusic; //Stop music
    class function Name: string; override;
    property EnableBGM: Boolean read FEnableBGM write SetEnableBGM; //Enable/disable music playing
    property BGMVolume: Integer read GetVolume write SetVolume; //Music volume dB*100 from -100dB (-10000) to 0
  end;

var
  Sound: TSound;  //Global variable for access to Sound Engine

implementation

uses
  {$IFDEF VSE_CONSOLE}VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{uFMOD}

const
  SNameEnableBGM = 'EnableBGM';
  XM_MEMORY=1;
  uFMOD_BUFFER_SIZE=262144;
  uFMOD_MixRate = 44100;
  SSectionSound='Sound';

{$L dsufmod.obj}
function uFMOD_DSPlaySong(lpXM: Pointer; param, fdwSong: Integer;
  lpDSBuffer: IDirectSoundBuffer): Integer; stdcall; external;

{TSound}

constructor TSound.Create;
begin
  inherited Create;
  Sound:=Self;
  {$IFDEF VSE_CONSOLE}
  Console['bgm ?val=eoff:on']:=BGMHandler;
  Console['bgmvol ?val=f']:=BGMVolHandler;
  {$ENDIF}
  if DirectSoundCreate(nil, FDirectSound, nil)<>S_OK then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot initialize DirectSound');{$ENDIF}
    Exit;
  end;
  if FDirectSound.SetCooperativeLevel(Core.Handle, DSSCL_PRIORITY)<>S_OK then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot set cooperative level');{$ENDIF}
    FDirectSound:=nil;
    Exit;
  end;
  with FMusicPCM do
  begin
    wFormatTag:=WAVE_FORMAT_PCM;
    nChannels:=2;
    nSamplesPerSec:=uFMOD_MixRate;
    nAvgBytesPerSec:=uFMOD_MixRate*4;
    nBlockAlign:=4;
    wBitsPerSample:=16;
    cbSize:=0;
  end;
  with FMusicBufferDesc do
  begin
    dwSize:=SizeOf(FMusicBufferDesc);
    dwFlags:=DSBCAPS_STATIC or DSBCAPS_CTRLVOLUME or DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2;
    dwBufferBytes:=uFMOD_BUFFER_SIZE;
    lpwfxFormat:=@FMusicPCM;
  end;
  if FDirectSound.CreateSoundBuffer(FMusicBufferDesc, FMusicBuffer, nil)<>S_OK then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot create secondary buffer');{$ENDIF}
    FMusicBuffer:=nil;
  end;
  if Settings.FirstRun
    then Settings.Bool[SSectionSound, SNameEnableBGM]:=true;
  EnableBGM:=Settings.Bool[SSectionSound, SNameEnableBGM];
end;

destructor TSound.Destroy;
begin
  Sound:=nil;
  Settings.Bool[SSectionSound, SNameEnableBGM]:=EnableBGM;
  StopMusic;
  FMusicBuffer:=nil;
  FDirectSound:=nil;
  inherited Destroy;
end;

class function TSound.Name: string;
begin
  Result:='Sound';
end;

{$IF Defined(VSE_LOG) and not Defined(VSE_NOSYSINFO)}
procedure TSound.LogCaps;
const
  Flags: array[0..10] of record Name: string; Value: DWORD; end = (
    (Name: 'CONTINUOUSRATE'; Value: $00000010),
    (Name: 'EMULDRIVER'; Value: $00000020),
    (Name: 'CERTIFIED'; Value: $00000040),
    (Name: 'PRIMARYMONO'; Value: $00000001),
    (Name: 'PRIMARYSTEREO'; Value: $00000002),
    (Name: 'PRIMARY8BIT'; Value: $00000004),
    (Name: 'PRIMARY16BIT'; Value: $00000008),
    (Name: 'SECONDARYMONO'; Value: $00000100),
    (Name: 'SECONDARYSTEREO'; Value: $00000200),
    (Name: 'SECONDARY8BIT'; Value: $00000400),
    (Name: 'SECONDARY16BIT'; Value: $00000800));
var
  Caps: TDSCaps;
  i: Integer;
  S: string;
begin
  Caps.dwSize:=SizeOf(Caps);
  if FDirectSound.GetCaps(Caps)<>S_OK then
  begin
    Log(llError, 'Sound: Cannot retrieve DirectSound capabilities');
    Exit;
  end;
  with Caps do
  begin
    LogRaw(llInfo, 'DirectSound capabilities:');
    LogRaw(llInfo, Format('Hardware secondary buffers sample rate: min=%d, max=%d', [dwMinSecondarySampleRate, dwMaxSecondarySampleRate]));
    LogRaw(llInfo, 'Primary buffers: '+IntToStr(dwPrimaryBuffers));
    LogRaw(llInfo, Format('Hardware secondary buffers: total=%d, static=%d, streaming=%d', [dwMaxHwMixingAllBuffers, dwMaxHwMixingStaticBuffers,	dwMaxHwMixingStreamingBuffers]));
    LogRaw(llInfo, Format('Free hardware secondary buffers: total=%d, static=%d, streaming=%d', [dwFreeHwMixingAllBuffers, dwFreeHwMixingStaticBuffers,	dwFreeHwMixingStreamingBuffers]));
    LogRaw(llInfo, Format('Hardware secondary 3D buffers: total=%d, static=%d, streaming=%d', [dwMaxHw3DAllBuffers, dwMaxHw3DStaticBuffers,	dwMaxHw3DStreamingBuffers]));
    LogRaw(llInfo, Format('Free hardware secondary 3D buffers: total=%d, static=%d, streaming=%d', [dwFreeHw3DAllBuffers, dwFreeHw3DStaticBuffers,	dwFreeHw3DStreamingBuffers]));
    LogRaw(llInfo, Format('Hardware memory: max=%d, free=%d, contig=%d', [dwTotalHwMemBytes,	dwFreeHwMemBytes, dwMaxContigFreeHwMemBytes]));
    LogRaw(llInfo, 'Hardware buffers transfer rate: '+IntToStr(dwUnlockTransferRateHwBuffers));
    LogRaw(llInfo, 'CPU overhead: '+IntToStr(dwPlayCpuOverheadSwBuffers));
    S:='Flags: ';
    for i:=0 to High(Flags) do
      if dwFlags and Flags[i].Value <> 0
        then S:=S+Flags[i].Name+' ';
    LogRaw(llInfo, S);
    LogRaw(llInfo, '');
  end;
end;
{$IFEND}

procedure TSound.Update;
begin

end;

function TSound.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  {$IF Defined(VSE_LOG) and not Defined(VSE_NOSYSINFO)}
  if Notify=snLogSysInfo then
    LogCaps;
  {$IFEND}
  //TODO: pause on snPause
end;

procedure TSound.PlayMusic(const FileName: string);
var
  Data: TStream;
begin
  if not Assigned(FMusicBuffer) then Exit;
  if Assigned(FMusicFile) then StopMusic;
  Data:=Core.GetFile(FileName);
  if not Assigned(Data) then Exit;
  if Data is TCustomMemoryStream then
    FMusicFile:=Data as TCustomMemoryStream
  else begin
    FMusicFile:=TMemoryStream.Create;
    FMusicFile.CopyFrom(Data, 0);
    Data.Free;
  end;
  if EnableBGM
    then uFMOD_DSPlaySong(FMusicFile.Memory, FMusicFile.Size, XM_MEMORY, FMusicBuffer);
end;

procedure TSound.StopMusic;
begin
  uFMOD_DSPlaySong(nil, 0, 0, nil);
  FAN(FMusicFile);
end;

procedure TSound.SetEnableBGM(Value: Boolean);
begin
  if Value=FEnableBGM
    then Exit;
  FEnableBGM:=Value;
  if not FEnableBGM then
    uFMOD_DSPlaySong(nil, 0, 0, nil)
  else if Assigned(FMusicBuffer) and Assigned(FMusicFile) then
    uFMOD_DSPlaySong(FMusicFile.Memory, FMusicFile.Size, XM_MEMORY, FMusicBuffer);
end;

function TSound.GetVolume: Integer;
begin
  if Assigned(FMusicBuffer) then
    FMusicBuffer.GetVolume(Result);
end;

procedure TSound.SetVolume(const Value: Integer);
begin
  if Assigned(FMusicBuffer) then
    FMusicBuffer.SetVolume(Value);
end;

{$IFDEF VSE_CONSOLE}
const
  BoolState: array[Boolean] of string = ('off', 'on');

function TSound.BGMHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1 then
    EnableBGM:=Boolean(Args[1].VInteger)
  else
    Console.WriteLn('BGM: '+BoolState[EnableBGM]);
  Result:=true;
end;

function TSound.BGMVolHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>1 then
    BGMVolume:=Round(100*Args[1].VExtended^)
  else
    Console.WriteLn('BGM volume: '+FloatToStr2(BGMVolume/100, 1, 2)+'dB');
  Result:=true;
end;
{$ENDIF}

initialization
  RegisterModule(TSound);

end.
