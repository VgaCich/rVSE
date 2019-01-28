program Pathologic;

uses
  {$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows, AvL, avlUtils, avlEventBus,
  VSECore, VSETexMan, VSEMemPak, {$IFDEF VSE_CONSOLE}VSEConsoleInterface,{$ENDIF}
  StateStart, StateMenu, StateGame, GameData;

{$R *.res}
{$R MemPak.res}

function LoadTexture(Self, Sender: TObject; const Name: string): Cardinal;
begin
  try
    Result := TexMan.AddTexture(Name, Core.GetFile(GetTexFileName(Name)), true, true, true);
    TexMan.SetFilter(Result, tfAnisotropic);
  except
    Result := 0;
  end;
end;

procedure InitStates;
begin
  Randomize;
  EventBus.ClearEvents;
  TexMan.OnLostTex := TOnLostTex(MakeMethod(@LoadTexture));
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateMenu.Create);
  Core.SwitchState(Core.AddState(TStateStart.Create));
end;

begin
  InitSettings.InitStates := InitStates;
  InitSettings.Caption := 'Pathologic';
  InitSettings.Version := '1.0';
  if Settings.FirstRun then
  begin
    InitSettings.Fullscreen := true;
    InitSettings.ResolutionX := Screen.Width;
    InitSettings.ResolutionY := Screen.Height;
    Settings.Int[SSectionSettings, SGraphicsQuality] := Integer(gqMed);
  end;
  while VSEStart = StopNeedRestart do
    Settings.ReloadInitSettings;
  EventBus.ClearEvents;
end.
