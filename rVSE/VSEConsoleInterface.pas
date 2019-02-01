unit VSEConsoleInterface;

{$IFNDEF VSE_CONSOLE}{$ERROR Please don't include VSEConsoleInterface unit without VSE_CONSOLE defined}{$ENDIF}

interface

uses
  Windows, AvL, avlMath, avlUtils, OpenGL, VSEOpenGLExt, VSECore, VSEConsole;

type
  TConsoleInterface = class(TModule)
  private
    FActive, FBlocking: Boolean;
    FScreenHeight: Single;
    FLogCache, FCmdHistory: TStringList;
    FFont, FCachedLines, FLogPosition, FCursor, FCmdHistoryIndex, FLineLength: Integer;
    FCommandLine: string;
    procedure SetActive(Value: Boolean);
    procedure AddToCache(const Line: string);
    function LogEndPosition: Integer;
    procedure UpdateFont(ScreenHeight: Single);
    function HelpHandler(Sender: TObject; Args: array of const): Boolean;
    function CmdListHandler(Sender: TObject; Args: array of const): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
    procedure Draw; override;
    procedure Update; override;
    function MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean; override;
    function KeyEvent(Key: Integer; Event: TKeyEvent): Boolean; override;
    function CharEvent(C: Char): Boolean; override;
    property Active: Boolean read FActive write SetActive; //Console interface is opened
    property Blocking: Boolean read FBlocking write FBlocking; //Block state updates & events when active
  end;

var
  ConsoleInterface: TConsoleInterface; //ConsoleInterface interface

implementation

uses
  VSERender2D;

const
  clFPS = $FFFFFF00;
  clNormalLine = $FF00FF00;
  clWarningLine = $FF00FFFF;
  clErrorLine = $FF0066FF;
  clScroll = $80808080;
  clCmdLineBorder = $CC80CC80;
  clBorderLine = $FF00FF00;
  clBgLines = $80333333;
  clBackground = $CC4D4D4D;
  DisplayLines = 19;
  VK_TILDE = 192;

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
  Console['help'] := HelpHandler;
  Console['cmdlist ?prefix=s'] := CmdListHandler;
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
  gleColor(clBackground);
  Render2D.DrawRect(0, 0, ScreenWidth, Temp);
  gleColor(clBorderLine);
  Render2D.DrawLine(0, Temp, ScreenWidth, Temp);
  gleColor(clBgLines);
  Temp := 0.005 * ScreenHeight;
  for i := 0 to 100 do
    Render2D.DrawLine(0, i * Temp, ScreenWidth, i * Temp);
  gleColor(clCmdLineBorder);
  Temp := 0.475 * ScreenHeight;
  Render2D.DrawLine(0, Temp, ScreenWidth, Temp);
  gleColor(clScroll);
  CaretHeight := Max(Temp * DisplayLines / FLogCache.Count, 0.01 * ScreenHeight);
  CaretPos := Min(Temp * FLogPosition / FLogCache.Count, Temp - CaretHeight);
  Temp := ScreenWidth - 0.025 * ScreenHeight;
  Render2D.DrawLine(Temp, 0, Temp, 0.475 * ScreenHeight);
  Render2D.DrawRect(Temp, CaretPos, 0.025 * ScreenHeight, CaretHeight);
  Temp := 0.025 * ScreenHeight;
  for i := 0 to Min(DisplayLines - 1, FLogCache.Count - FLogPosition - 1) do
  begin
    if LastChar(FLogCache[FLogPosition + i]) = PostfixError then gleColor(clErrorLine)
    else if LastChar(FLogCache[FLogPosition + i]) = PostfixWarning then gleColor(clWarningLine)
    else gleColor(clNormalLine);
    Render2D.TextOut(FFont, 0, Temp * i, RemovePostfix(FLogCache[FLogPosition + i]));
  end;
  gleColor(clNormalLine);
  Temp := 0.475 * ScreenHeight;
  CommandLineStart := Max(FCursor - FLineLength + 1, 0);
  Render2D.TextOut(FFont, 0, Temp, '>' + Copy(FCommandLine, CommandLineStart + 1, FLineLength - 1));
  if (Core.Time div 500) mod 2 = 0 then
    Render2D.TextOut(FFont, CharWidth * (FCursor - CommandLineStart), Temp, '_');
  gleColor(clBackground);
  Temp := ScreenWidth - 0.025 * ScreenHeight - 8 * CharWidth;
  Render2D.DrawRect(Temp, 0, 8 * CharWidth - 1, 0.025 * ScreenHeight);
  gleColor(clFPS);
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

function TConsoleInterface.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean;
begin
  Result := FActive and FBlocking;
end;

function TConsoleInterface.KeyEvent(Key: Integer; Event: TKeyEvent): Boolean;
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
  if not FActive then
  begin
    if (Event = keUp) and (Key = VK_TILDE) then Active := true;
    Result := FActive;
    Exit;
  end;
  Result := true;
  if (Event = keDown) then
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
end;

function TConsoleInterface.CharEvent(C: Char): Boolean;
begin
  Result := FActive;
  if FActive and (C in [#31..#95, #97..#126, #128..#255]) then
  begin
    Insert(C, FCommandLine, FCursor);
    Inc(FCursor);
  end;
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

procedure TConsoleInterface.UpdateFont(ScreenHeight: Single);
begin
  if FScreenHeight <> ScreenHeight then
  begin
    FFont := Render2D.CreateFont('Courier New', Round(10 * ScreenHeight / 600), true);
    FScreenHeight := ScreenHeight;
  end;
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

function TConsoleInterface.CmdListHandler(Sender: TObject; Args: array of const): Boolean;
var
  Prefix: string;
  Commands: TStringList;
  i: Integer;
begin
  Result := false;
  if Length(Args) > 1
    then Prefix := string(Args[1].VAnsiString)
    else Prefix := '';
  Commands := Console.GetCommands(Prefix);
  try
    for i := 0 to Commands.Count - 1 do
      Console.WriteLn(Console.GetCommandDescription(Commands[i]));
    Result := Commands.Count > 0;
  finally
    FAN(Commands);
  end;
end;

initialization
  RegisterModule(TConsoleInterface);

end.