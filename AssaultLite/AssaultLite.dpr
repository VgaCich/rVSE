program AssaultLite;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, VSECore, StateStart, StateMenu, StateGame, StateLoad;

{$R *.res}
{$R MemPak.res}

procedure InitStates;
begin
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateLoad.Create);
  Core.AddState(TStateMenu.Create);
  Core.SwitchState(Core.AddState(TStateStart.Create));
end;

const
  SUseCache='Enable cache?'#13#10+
            'It will speed up game loading but needs some HDD space';
  SUseFullscreen='Launch game in fullscreen mode?';

begin
  IsMultiThread := True;
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:='Assault Lite';
  InitSettings.Version:='0.1';
  CacheDir:=ExePath+'Cache\';
  if Settings.FirstRun then
  begin
    StateStart.UseCache:=MessageDlg(SUseCache, InitSettings.Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    InitSettings.Fullscreen:=MessageDlg(SUseFullscreen, InitSettings.Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    if InitSettings.Fullscreen then
    begin
      InitSettings.ResolutionX:=Screen.Width;
      InitSettings.ResolutionY:=Screen.Height;
    end
    else begin
      InitSettings.ResolutionX:=800;
      InitSettings.ResolutionY:=600;
    end;
  end
    else StateStart.UseCache:=DirectoryExists(CacheDir);
  while VSEStart = StopNeedRestart do
    Settings.ReloadInitSettings;
end.
