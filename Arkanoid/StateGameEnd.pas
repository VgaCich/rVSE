unit StateGameEnd;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, oglExtensions, VSEOpenGLExt, VSECore;

const
  SIDGameEnd = 'GameEnd';
  SScores = 'Scores';
  ScoresCount = 5;

type
  TStateGameEnd = class(TGameState)
  private
    FFont, FLargeFont: Cardinal;
    FHighScore: Integer;
    FScores: array[0..ScoresCount - 1] of Integer;
    procedure DrawLine(Font, Y: Integer; const Line: string);
  protected
    function  GetName: string; override;
  public
    constructor Create;
    procedure Draw; override;
    function Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure OnEvent(var Event: TCoreEvent); override;
  end;

implementation

uses
  VSERender2D, VSEGUI, StateMenu, StateGame
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

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
  Core.GetState(Core.FindState(SIDGame)).Draw;
  Render2D.Enter;
  gleColor(clRed);
  DrawLine(FLargeFont, 150, 'Game Over');
  if FHighScore >= 0 then
    DrawLine(FFont, 250, 'You have got high score!');
  gleColor($80000000);
  Render2D.DrawRect(310, 300, 180, 210);
  gleColor(clText);
  DrawLine(FFont, 300, 'Scores:');
  for i := 0 to High(FScores) do
  begin
    if i = FHighScore then
      gleColor(clRed)
    else
      gleColor(clText);
    DrawLine(FFont, 350 + 30 * i, IntToStr(FScores[i]));
  end;
  Render2D.Leave;
end;

function TStateGameEnd.Activate: Cardinal;
var
  Score, i, j: Integer;
begin
  Result := inherited Activate;
  FHighScore := -1;
  Score := TStateGame(Core.GetState(Core.FindState(SIDGame))).Score;
  for i := 0 to High(FScores) do
    FScores[i] := Settings.Int[SScores, IntToStr(i)];
  for i := 0 to High(FScores) do
    if Score > FScores[i] then
    begin
      FHighScore := i;
      for j := High(FScores) downto i + 1 do
        FScores[j] := FScores[j - 1];
      FScores[i] := Score;
      Break;
    end;
  if FHighScore >= 0 then
    for i := 0 to High(FScores) do
      Settings.Int[SScores, IntToStr(i)] := FScores[i];
end;

procedure TStateGameEnd.Deactivate;
begin
  TStateGame(Core.GetState(Core.FindState(SIDGame))).ClearBricks;
end;

procedure TStateGameEnd.OnEvent(var Event: TCoreEvent);
begin
  if ((Event is TMouseEvent) and ((Event as TMouseEvent).EvType in [meDown, meUp])) or (Event is TKeyEvent) or
     ((Event is TSysNotify) and ((Event as TSysNotify).Notify = snMinimized)) then
    Core.SwitchState(SIDMenu);
  inherited;
end;

function TStateGameEnd.GetName: string;
begin
  Result:=SIDGameEnd;
end;

procedure TStateGameEnd.DrawLine(Font, Y: Integer; const Line: string);
begin
  Render2D.TextOut(Font, 400 - Render2D.TextWidth(Font, Line) / 2, Y, Line);
end;

end.
