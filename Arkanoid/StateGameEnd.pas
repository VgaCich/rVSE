unit StateGameEnd;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, oglExtensions, VSEOpenGLExt, VSECore;

type
  TStateGameEnd = class(TGameState)
  private
    FFont, FLargeFont: Cardinal;
    FHighScore: Boolean;
    FScores: array[0..4] of Integer;
    procedure DrawLine(Font, Y: Integer; const Line: string);
  protected
    function  GetName: string; override;
  public
    constructor Create;
    procedure Draw; override;
    function Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
  end;

implementation

uses
  VSERender2D, VSEGUI, StateMenu, StateGame
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  SScores = 'Scores';

{TStateGameEnd}

constructor TStateGameEnd.Create;
begin
  inherited Create;
  FFont:=Render2D.CreateFont(UIFont, 20, false);
  FLargeFont:=Render2D.CreateFont(UIFont, 40, false);
end;

procedure TStateGameEnd.Draw;
var
  i: Integer;
begin
  Core.GetState(Core.FindState('Game')).Draw;
  Render2D.Enter;
  gleColor(clRed);
  DrawLine(FLargeFont, 150, 'Game Over');
  if FHighScore then
    DrawLine(FFont, 250, 'You have got high score!');
  gleColor(clText);
  DrawLine(FFont, 300, 'High scores:');
  for i := 0 to High(FScores) do
    DrawLine(FFont, 350 + 30 * i, IntToStr(FScores[i]));
  Render2D.Leave;
end;

function TStateGameEnd.Activate: Cardinal;
var
  Score, i, j: Integer;
begin
  Result := inherited Activate;
  FHighScore := false;
  Score := TStateGame(Core.GetState(Core.FindState('Game'))).Score;
  for i := 0 to High(FScores) do
    FScores[i] := Settings.Int[SScores, IntToStr(i)];
  for i := 0 to High(FScores) do
    if Score > FScores[i] then
    begin
      FHighScore := true;
      for j := High(FScores) downto i + 1 do
        FScores[j] := FScores[j - 1];
      FScores[i] := Score;
      Break;
    end;
  if FHighScore then
    for i := 0 to High(FScores) do
      Settings.Int[SScores, IntToStr(i)] := FScores[i];
end;

procedure TStateGameEnd.Deactivate;
begin
  TStateGame(Core.GetState(Core.FindState('Game'))).ClearBricks;
end;

procedure TStateGameEnd.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if (Event = meDown) or (Event = meUp) then Core.SwitchState('Menu');
end;

procedure TStateGameEnd.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Event = keDown) or (Event = keUp) then Core.SwitchState('Menu');
end;

function TStateGameEnd.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if Notify=snMinimize then Core.SwitchState('Menu');
end;

function TStateGameEnd.GetName: string;
begin
  Result:='GameEnd';
end;

procedure TStateGameEnd.DrawLine(Font, Y: Integer; const Line: string);
begin
  Render2D.TextOut(Font, 400 - Render2D.TextWidth(Font, Line) / 2, Y, Line);
end;

end.
