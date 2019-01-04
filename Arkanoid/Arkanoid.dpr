program Arkanoid;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, VSECore, VSEMemPak, StateStart, StateMenu, StateGame, StateGameEnd;

{$R MemPak.res}

const
  SUseCache='Enable cache?'#13#10+
            'It will speed up game loading but needs some HDD space';
  SUseFullscreen='Launch game in fullscreen mode?';

procedure InitStates;
begin
  Core.AddState(TStateGameEnd.Create);
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateMenu.Create);
  Core.SwitchState(Core.AddState(TStateStart.Create));
end;

begin
  Randomize;
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:=GameTitle;
  InitSettings.Version:=GameVer;
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
  VSEStart;
end.
