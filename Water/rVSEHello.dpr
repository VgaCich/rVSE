program VSEHello;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, OpenGL, oglExtensions, VSEOpenGLExt, VSECore;

type
  TStateMain=class(TGameState)
  protected
    function GetName: string; override;
  public
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
  end;

procedure TStateMain.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glBegin(GL_TRIANGLES);
    glColor(1.0, 0.0, 0.0);
    glVertex(0, 250);
    glColor(0.0, 1.0, 0.0);
    glVertex(216, -125);
    glColor(0.0, 0.0, 1.0);
    glVertex(-216, -125);
  glEnd;
end;

procedure TStateMain.Update;
begin
  glRotate(0.5, 0.0, 0.0, 1.0);
end;

function TStateMain.Activate: Cardinal;
begin
  Result:=20;
  gleOrthoMatrix(800, 600);
  glTranslate(400, 300, 0);
end;

function TStateMain.GetName: string;
begin
  Result:='Main';
end;

procedure InitStates;
begin
  Core.SwitchState(Core.AddState(TStateMain.Create));
end;

begin
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:='rVSE Hello';
  InitSettings.Version:='1.0';
  InitSettings.InitStates:=InitStates;
  VSEStart;
end.
