program VSEFBOTest;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, OpenGL, oglExtensions, VSELog, VSEOpenGLExt, VSECore, VSESysInfo,
  VSETexMan, VSERender2D;

type
  TStateMain = class(TGameState)
  private
    FRTTScreen: TVirtualScreen;
    FTex, FRTT, FFont: Cardinal;
  protected
    function GetName: string; override;
  public
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure OnEvent(var Event: TCoreEvent); override;
  end;

const
  TexSize = 512;

procedure TStateMain.Draw;
var
  RTTRes: Boolean;
begin
  RTTRes := TexMan.RTTBegin(FRTT, FTex);
  if RTTRes then
  begin
    glClearColor(0, 0.1, 0, 1);
    glColor(0.5, 0.5, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    Render2D.Enter(FRTTScreen);
    with Render2D, Render2D.Screen do
      DrawText(FFont, (Width - TextWidth(FFont, 'Hello!')) div 2, (Height - TextHeight(FFont)) div 2, 'Hello!');
    Render2D.Leave;
    TexMan.RTTEnd(FRTT);
  end;
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  gleColor(clWhite);
  Render2D.Enter;
  if RTTRes then
  begin
    TexMan.Bind(FTex);
    with Render2D, Render2D.Screen do
      DrawRect((Width - TexSize) div 2, (Height - TexSize) div 2, TexSize, TexSize, 0, 1, 1, -1);
    TexMan.Unbind;
  end
    else with Render2D, Render2D.Screen do
      DrawText(FFont, (Width - TextWidth(FFont, 'RTT Error')) div 2, (Height - TextHeight(FFont)) div 2, 'RTT Error');
  Render2D.Leave;
end;

procedure TStateMain.Update;
begin
  Core.Caption := InitSettings.Caption + ' | FBO: ' + BoolToStr(TexMan.RTTMethod = rttFBO) + ' | ' + IntToStr(Core.FPS) + ' FPS';
end;

function TStateMain.Activate: Cardinal;
var
  TexData: Pointer;
begin
  Result := 200;
  gleOrthoMatrix(Core.ResolutionX, Core.ResolutionY);
  TexMan.RTTMethod := rttFBO;
  GetMem(TexData, TexSize*TexSize*4);
  FTex := TexMan.AddTexture('rt', TexData, TexSize, TexSize, GL_RGBA8, GL_RGBA, true, false);
  FreeMem(TexData);
  FRTT := TexMan.InitRTT(TexSize, TexSize);
  FFont := Render2D.CreateFont('Tahoma', 16, false);
  FRTTScreen := TVirtualScreen.Create(TexSize, TexSize, TexSize div 8, TexSize div 8);
  FRTTScreen.RescaleFont(FFont);
end;

procedure TStateMain.Deactivate;
begin
  FRTTScreen.Free;
  TexMan.FreeRTT(FRTT);
end;

procedure TStateMain.OnEvent(var Event: TCoreEvent);
var
  Mode: TRTTMethod;
begin
  if (Event is TKeyEvent) and ((Event as TKeyEvent).EvType = keUp) and ((Event as TKeyEvent).Key = VK_SPACE) then
  begin
    Mode := TexMan.RTTMethod;
    if Mode = High(TRTTMethod) then
      Mode := Low(TRTTMethod)
    else
      Inc(Mode);
    TexMan.RTTMethod := Mode;
    TexMan.FreeRTT(FRTT);
    FRTT := TexMan.InitRTT(TexSize, TexSize);
  end
    else inherited;
end;

function TStateMain.GetName: string;
begin
  Result := 'Main';
end;

procedure InitStates;
begin
  Core.SwitchState(Core.AddState(TStateMain.Create));
end;

begin
  InitSettings.InitStates := InitStates;
  InitSettings.Caption := 'rVSE FBO Test';
  InitSettings.Version := '1.0';
  InitSettings.ResolutionX := 800;
  InitSettings.ResolutionY := 600;
  InitSettings.VSync := false;
  InitSettings.InitStates := InitStates;
  VSEStart;
end.
