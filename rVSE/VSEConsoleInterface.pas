unit VSEConsoleInterface;

{$IFNDEF VSE_CONSOLE}{$ERROR Please don't include VSEConsoleInterface unit without VSE_CONSOLE defined}{$ENDIF}

interface

uses
  Windows, AvL, avlMath, avlUtils, OpenGL, VSEOpenGLExt, VSECore, VSEConsole;

type
  TConsoleColors = (clBackground, clNormalLine, clWarningLine, clErrorLine,
    clBorderLine, clCmdLineBorder, clBgLines, clScroll, clFPS);
  TConsoleColorSet = array[TConsoleColors] of TColor;
  TConsoleInterface = class(TModule)
  private
    FActive, FBlocking, FFontBold: Boolean;
    FScreenHeight: Single;
    FLogCache, FCmdHistory: TStringList;
    FFont, FCachedLines, FLogPosition, FCursor, FCmdHistoryIndex, FLineLength: Integer;
    FColors: TConsoleColorSet;
    FCommandLine, FFontName: string;
    procedure SetActive(Value: Boolean);
    procedure AddToCache(const Line: string);
    function LogEndPosition: Integer;
    procedure UpdateFont(ScreenHeight: Single; Force: Boolean = false);
    function ConColorHandler(Sender: TObject; Args: array of const): Boolean;
    function ConFontHandler(Sender: TObject; Args: array of const): Boolean;
    function HelpHandler(Sender: TObject; Args: array of const): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
    procedure Draw; override;
    procedure Update; override;
    procedure OnEvent(var Event: TCoreEvent); override;
    procedure SetColors(const Colors: TConsoleColorSet);
    procedure SetFont(const Name: string; Bold: Boolean = true);
    property Active: Boolean read FActive write SetActive; //Console interface is opened
    property Blocking: Boolean read FBlocking write FBlocking; //Block state updates & events when active
  end;

var
  ConsoleInterface: TConsoleInterface; //ConsoleInterface interface

implementation

uses
  VSERender2D;

const
  DefFont = 'Courier New';
  DefColors: TConsoleColorSet = (
    $CC4D4D4D, $FF00FF00, $FF00FFFF, $FF0066FF,
    $FF00FF00, $CC80CC80, $80333333, $80808080, $FFFFFF00);
  ColorNames: array[TConsoleColors] of string = (
    'bg', 'nln', 'wln', 'eln', 'bd', 'clbd', 'bgln', 'scrl', 'fps');
  DisplayLines = 19;
  VK_TILDE = 192;

function ConColors: string;
var
  i: TConsoleColors;
begin;
  Result := ColorNames[Low(TConsoleColors)];
  for i := Succ(Low(TConsoleColors)) to High(TConsoleColors) do
    Result := Result + ':' + ColorNames[i];
end;

function LastChar(const S: string): Char;
begin
  Result := #0;
  if S <> '' then Result := S[Length(S)];
end;

{ TConsoleInterface }

constructor TConsoleInterface.Create;
begin
  inherited;
  FBlocking := true;
  FLogCache := TStringList.Create;
  FCmdHistory := TStringList.Create;
  SetColors(DefColors);
  SetFont(DefFont, true);
  Console['help'] := HelpHandler;
  Console['confont name=s ?weight=en:b'] := ConFontHandler;
  Console['concolor ?name=e' + ConColors + ' ?clr=i'] := ConColorHandler;
  ConsoleInterface := Self;
end;

destructor TConsoleInterface.Destroy;
begin
  ConsoleInterface := nil;
  FAN(FCmdHistory);
  FAN(FLogCache);
  inherited;
end;

class function TConsoleInterface.Name: string;
begin
  Result:='ConsoleInterface';
end;

procedure TConsoleInterface.Draw;

  function RemovePostfix(const S: string): string;
  begin
    if LastChar(S) in [PostfixWarning, PostfixError]
      then Result := Copy(S, 1, Length(S) - 1)
      else Result := S;
  end;

var
  i, CommandLineStart: Integer;
  ScreenWidth, ScreenHeight, CharWidth, CaretPos, CaretHeight, Temp: Single;
  Line: string;
begin
  if not FActive then Exit;
  Render2D.Enter;
  with Render2D.VSBounds do
  begin
    ScreenWidth := Right - Left;
    ScreenHeight := Bottom - Top;
    glPushMatrix;
    glTranslate(Left, Top, 0);
  end;
  UpdateFont(ScreenHeight);
  CharWidth := Render2D.CharWidth(FFont, '_');
  if FLineLength <> Floor(ScreenWidth / CharWidth) then
  begin
    FLineLength := Floor(ScreenWidth / CharWidth);
    FLogCache.Clear;
    FCachedLines := 0;
    for i := 0 to Console.Log.Count - 1 do
      AddToCache(Console.Log[i]);
    FLogPosition := LogEndPosition;
  end;
  glLineWidth(ScreenHeight / 600);
  Temp := 0.503 * ScreenHeight;
  gleColor(FColors[clBackground]);
  Render2D.DrawRect(0, 0, ScreenWidth, Temp);
  gleColor(FColors[clBorderLine]);
  Render2D.DrawLine(0, Temp, ScreenWidth, Temp);
  gleColor(FColors[clBgLines]);
  Temp := 0.005 * ScreenHeight;
  for i := 0 to 100 do
    Render2D.DrawLine(0, i * Temp, ScreenWidth, i * Temp);
  gleColor(FColors[clCmdLineBorder]);
  Temp := 0.475 * ScreenHeight;
  Render2D.DrawLine(0, Temp, ScreenWidth, Temp);
  gleColor(FColors[clScroll]);
  CaretHeight := Max(Temp * DisplayLines / FLogCache.Count, 0.01 * ScreenHeight);
  CaretPos := Min(Temp * FLogPosition / FLogCache.Count, Temp - CaretHeight);
  Temp := ScreenWidth - 0.025 * ScreenHeight;
  Render2D.DrawLine(Temp, 0, Temp, 0.475 * ScreenHeight);
  Render2D.DrawRect(Temp, CaretPos, 0.025 * ScreenHeight, CaretHeight);
  Temp := 0.025 * ScreenHeight;
  for i := 0 to Min(DisplayLines - 1, FLogCache.Count - FLogPosition - 1) do
  begin
    if LastChar(FLogCache[FLogPosition + i]) = PostfixError then
      gleColor(FColors[clErrorLine])
    else if LastChar(FLogCache[FLogPosition + i]) = PostfixWarning then
      gleColor(FColors[clWarningLine])
    else
      gleColor(FColors[clNormalLine]);
    Render2D.TextOut(FFont, 0, Temp * i, RemovePostfix(FLogCache[FLogPosition + i]));
  end;
  gleColor(FColors[clNormalLine]);
  Temp := 0.475 * ScreenHeight;
  CommandLineStart := Max(FCursor - FLineLength + 1, 0);
  Line := '>' + Copy(FCommandLine, CommandLineStart + 1, FLineLength - 1);
  Render2D.TextOut(FFont, 0, Temp, Line);
  if (Core.Time div 500) mod 2 = 0 then
    Render2D.TextOut(FFont, Render2D.TextWidth(FFont, Copy(Line, 1, FCursor - CommandLineStart)), Temp, '_');
  gleColor(FColors[clBackground]);
  Temp := ScreenWidth - 0.025 * ScreenHeight - 8 * CharWidth;
  Render2D.DrawRect(Temp, 0, 8 * CharWidth - 1, 0.025 * ScreenHeight);
  gleColor(FColors[clFPS]);
  Render2D.TextOut(FFont, Temp, 0, 'FPS: ' + IntToStr(Core.FPS));
  glPopMatrix;
  Render2D.Leave;
end;

procedure TConsoleInterface.Update;
var
  AtEnd: Boolean;
begin
  AtEnd := FLogPosition = LogEndPosition;
  while (FLineLength > 0) and (Console.Log.Count > FCachedLines) do
    AddToCache(Console.Log[FCachedLines]);
  if AtEnd then
    FLogPosition := LogEndPosition;
  if FActive and FBlocking then
    Core.InhibitUpdate := true;
end;

procedure TConsoleInterface.OnEvent(var Event: TCoreEvent);
var
  List: TStringList;
  i: Integer;
  S: string;

  procedure SetCommandLine(const Cmd: string);
  begin
    FCommandLine := Cmd;
    FCursor := Length(Cmd) + 1;
  end;

begin
  if FActive and (Event is TMouseEvent) and FBlocking then
    FreeAndNil(Event)
  else if FActive and (Event is TCharEvent) then
  begin
    with Event as TCharEvent do
      if Chr in [#31..#95, #97..#126, #128..#255] then
      begin
        Insert(Chr, FCommandLine, FCursor);
        Inc(FCursor);
      end;
    FreeAndNil(Event);
  end
  else if Event is TKeyEvent then
    with Event as TKeyEvent do
    begin
      if not FActive then
      begin
        if (EvType = keUp) and (Key = VK_TILDE) then Active := true;
      end
      else if (EvType = keDown) then
      begin
        case Key of
          VK_PRIOR: FLogPosition := Max(FLogPosition - DisplayLines + 1, 0);
          VK_NEXT: FLogPosition := Min(FLogPosition + DisplayLines - 1, LogEndPosition);
          VK_LEFT: FCursor := Max(FCursor - 1, 1);
          VK_RIGHT: FCursor := Min(FCursor + 1, Length(FCommandLine) + 1);
          VK_UP: if Core.KeyPressed[VK_CONTROL]
            then FLogPosition := Max(FLogPosition - 1, 0)
            else begin
              FCmdHistoryIndex := Max(FCmdHistoryIndex - 1, 0);
              SetCommandLine(FCmdHistory[FCmdHistoryIndex]);
            end;
          VK_DOWN: if Core.KeyPressed[VK_CONTROL]
            then FLogPosition := Min(FLogPosition + 1, LogEndPosition)
            else begin
              FCmdHistoryIndex := Min(FCmdHistoryIndex + 1, FCmdHistory.Count - 1);
              SetCommandLine(FCmdHistory[FCmdHistoryIndex]);
            end;
          VK_DELETE: Delete(FCommandLine, FCursor, 1);
          VK_BACK: if FCursor > 1 then
            begin
              Delete(FCommandLine, FCursor - 1, 1);
              FCursor := Max(FCursor - 1, 1);
            end;
        end;
      end
      else begin
        case Key of
          VK_TAB: if FCommandLine <> '' then
            begin
              List := Console.GetCommands(Trim(FCommandLine));
              try
                if List.Count = 1
                  then SetCommandLine(List[0] + ' ')
                else if List.Count > 1 then
                  for i := 0 to List.Count - 1 do
                    Console.WriteLn('    ' + List[i]);
              finally
                FAN(List);
              end;
            end;
          VK_HOME: if Core.KeyPressed[VK_CONTROL]
            then FLogPosition := 0
            else FCursor := 1;
          VK_END: if Core.KeyPressed[VK_CONTROL]
            then FLogPosition := LogEndPosition
            else FCursor := Length(FCommandLine) + 1;
          VK_INSERT:
            begin
              S := GetClipboardText;
              Insert(S, FCommandLine, FCursor);
              Inc(FCursor, Length(S)); 
            end;
          VK_ESCAPE: if FLogPosition <> LogEndPosition
            then FLogPosition := LogEndPosition
            else SetCommandLine('');
          VK_RETURN:
            begin
              Console.WriteLn('>' + FCommandLine);
              if FCommandLine <> '' then
              begin
                FCmdHistory.Add(FCommandLine);
                if FCmdHistory.Count > 32 then FCmdHistory.Delete(0);
                FCmdHistoryIndex := FCmdHistory.Count;
                Console.Execute(FCommandLine);
                SetCommandLine('');
              end;
            end;
          VK_TILDE: if not Core.KeyPressed[VK_SHIFT] then Active := false;
        end;
      end;
      if FActive then
        FreeAndNil(Event);
    end
  else inherited;
end;

procedure TConsoleInterface.SetActive(Value: Boolean);
begin
  FActive := Value;
  if FActive then
  begin
    if FCursor = 0 then
    begin
      UpdateFont(Render2D.VSBounds.Bottom - Render2D.VSBounds.Top);
      Console.WriteLn('Print "help" for help' + PostfixWarning);
    end;
    FLogPosition := LogEndPosition;
    FCommandLine:='';
    FCursor:=1;
    FCmdHistoryIndex := FCmdHistory.Count;
  end;
end;

procedure TConsoleInterface.SetColors(const Colors: TConsoleColorSet);
var
  i: TConsoleColors;
begin
  for i := Low(TConsoleColors) to High(TConsoleColors) do
    FColors[i] := DefColors[i];
end;

procedure TConsoleInterface.SetFont(const Name: string; Bold: Boolean);
begin
  FFontName := Name;
  FFontBold := Bold;
  UpdateFont(FScreenHeight, true);
end;

procedure TConsoleInterface.AddToCache(const Line: string);
var
  Pos: Integer;
  Postfix: Char;

  function AddPostfix(const S: string): string;
  begin
    if (Postfix = ' ') or (LastChar(S) = Postfix)
      then Result := S
      else Result := S + Postfix;
  end;

begin
  if FLineLength = 0 then Exit;
  if Line <> '' then
  begin
    Pos := 1;
    Postfix := LastChar(Line);
    if not (Postfix in [PostfixWarning, PostfixError]) then Postfix := ' ';
    while Pos <= Length(Line) do
    begin
      FLogCache.Add(AddPostfix(Copy(Line, Pos, FLineLength)));
      Inc(Pos, FLineLength);
    end;
  end
    else  FLogCache.Add('');
  Inc(FCachedLines);
end;

function TConsoleInterface.LogEndPosition: Integer;
begin
  Result := Max(FLogCache.Count - DisplayLines, 0);
end;

procedure TConsoleInterface.UpdateFont(ScreenHeight: Single; Force: Boolean);
begin
  if (ScreenHeight > 0) and (Force or (FScreenHeight <> ScreenHeight)) then
  begin
    FFont := Render2D.CreateFont(FFontName, Round(10 * ScreenHeight / 600), FFontBold);
    FScreenHeight := ScreenHeight;
  end;
end;

function TConsoleInterface.ConColorHandler(Sender: TObject; Args: array of const): Boolean;
begin
  Result := true;
  if Length(Args) = 1 then
    Console.WriteLn('Colors: ' + ConColors)
  else if Length(Args) = 2 then
    Console.WriteLn('$' + IntToHex(FColors[TConsoleColors(Args[1].VInteger)], 8))
  else
    FColors[TConsoleColors(Args[1].VInteger)] := Args[2].VInteger;
end;

function TConsoleInterface.ConFontHandler(Sender: TObject;  Args: array of const): Boolean;
begin
  Result := true;
  FFontName := string(Args[1].VAnsiString);
  if Length(Args) = 3 then
    FFontBold := Boolean(Args[2].VInteger);
  UpdateFont(FScreenHeight, true);
end;

function TConsoleInterface.HelpHandler(Sender: TObject; Args: array of const): Boolean;
begin
  with Console do
  begin
    WriteLn;
    WriteLn('Use command "cmdlist [prefix]" to get commands list');
    WriteLn('Use PageUp, PageDown, Ctrl+Up, Ctrl+Down, Ctrl+Home, Ctrl+End, Escape to navigate console log');
    WriteLn('Use Up/Down to navigate commands history');
    WriteLn('Press Tab to autocomplete command');
    WriteLn('Press Insert to paste from clipboard');
    WriteLn('Press Escape to clear command line');
  end;
  Result := true;
end;

initialization
  RegisterModule(TConsoleInterface);

end.