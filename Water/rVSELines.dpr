program VSEHello;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, OpenGL, oglExtensions, VSEOpenGLExt, VSECore, VSEArrayBuffer;

type
  TStateMain = class(TGameState)
  private
    FLinesVBO: TArrayBuffer;
    FMode: Integer;
  protected
    function GetName: string; override;
  public
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure OnEvent(var Event: TCoreEvent); override;
  end;
  TVertex = packed record
    X, Y: Single;
    C: Cardinal;
  end;

const
  Modes: array[0..2] of record
    Name: string;
    Mode: GLenum;
  end = (
    (Name: 'Lines'; Mode: GL_LINES),
    (Name: 'Triangles'; Mode: GL_TRIANGLES),
    (Name: 'Quads'; Mode: GL_QUADS));

destructor TStateMain.Destroy;
begin
  FreeAndNil(FLinesVBO);
  inherited;
end;

procedure TStateMain.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLineWidth(3);
  glBegin(GL_LINE_STRIP);
    glColor(1.0, 1.0, 1.0);
    glVertex(0, 0);
    glVertex(Core.ResolutionX, 0);
    glVertex(Core.ResolutionX, Core.ResolutionY);
    glVertex(0, Core.ResolutionY);
    glVertex(0, 0);
  glEnd;
  glLineWidth(1);
  FLinesVBO.Bind(GL_ARRAY_BUFFER_ARB);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glBlendFunc(GL_ONE, GL_ZERO);
  glVertexPointer(2, GL_FLOAT, SizeOf(TVertex), FLinesVBO.Data);
  glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(TVertex), IncPtr(FLinesVBO.Data, 2 * SizeOf(Single)));
  glDrawArrays(Modes[FMode].Mode, 0, FLinesVBO.Size div SizeOf(TVertex));
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
  FLinesVBO.Unbind;
end;

procedure TStateMain.Update;
begin
  Core.Caption := InitSettings.Caption + ': ' + Modes[FMode].Name + ', ' + IntToStr(Core.FPS) + ' FPS';
end;

function TStateMain.Activate: Cardinal;
var
  VertBuf: array of TVertex;
  i, j, k: Cardinal;
begin
  Result := 100;
  FMode := 0;
  gleOrthoMatrix(Core.ResolutionX, Core.ResolutionY);
  SetLength(VertBuf, 2 * Core.ResolutionX * Core.ResolutionY);
  for j := 0 to Core.ResolutionY - 1 do
    for i := 0 to Core.ResolutionX - 1 do
    begin
      k := 2 * (j * Core.ResolutionX + i);
      VertBuf[k].X := i;
      VertBuf[k].Y := j;
      VertBuf[k].C := Random($FF) + (Random($FF) shl 16) + (Random($FF) shl 8) + $FF000000;
      VertBuf[k + 1].X := i + 10;
      VertBuf[k + 1].Y := j + 10;
      VertBuf[k + 1].C := VertBuf[k].C;
    end;
  FLinesVBO := TArrayBuffer.Create;
  FLinesVBO.SetData(@VertBuf[0], SizeOf(TVertex) * Length(VertBuf));
  Finalize(VertBuf);
end;

procedure TStateMain.OnEvent(var Event: TCoreEvent);
begin
  if (Event is TKeyEvent) and ((Event as TKeyEvent).EvType = keUp) and ((Event as TKeyEvent).Key = VK_SPACE) then
  begin
    Inc(FMode);
    if FMode > High(Modes) then
      FMode := 0;
  end
  else
    inherited;
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
  InitSettings.Caption := 'rVSE Lines';
  InitSettings.Version := '1.0';
  InitSettings.ResolutionX := 1900;
  InitSettings.ResolutionY := 1000;
  InitSettings.VSync := false;
  InitSettings.InitStates := InitStates;
  VSEStart;
end.
