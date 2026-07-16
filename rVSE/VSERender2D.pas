unit VSERender2D;

interface

uses
  Windows, AvL, avlUtils, avlMath, OpenGL, oglExtensions, VSEOpenGLExt, VSECore
  {$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  PFont = ^TFont; //internally used
  TFont = record //internally used
    Tex,  List: Cardinal;
    Width: array [0..255] of Single;
    Height: Integer;
    Size: Integer;
    Bold, AutoUpdate: Boolean;
    Name: string;
  end;
  PFloatRect = ^TFloatRect;
  TFloatRect = packed record
    Left, Top, Right, Bottom: Single;
  end;
  TVirtualScreen = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FScale, FPadding: Single;
    FPaddingVertical: Boolean;
    function GetBounds: TFloatRect;
  public
    constructor Create(ViewportWidth, ViewportHeight, Width, Height: Integer);
    procedure RescaleFont(Font: Cardinal);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bounds: TFloatRect read GetBounds;
  end;
  TRender2D = class(TModule)
  private
    FEnters: array of TVirtualScreen;
    FFonts: array of PFont;
    FMainScreen, FScreen: TVirtualScreen;
    procedure CreateFontTex(Screen: TVirtualScreen; Font: Cardinal);
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    class function Name: string; override; //internally used
    procedure OnEvent(var Event: TCoreEvent); override;
    procedure Enter(Screen: TVirtualScreen = nil); //Enter 2D mode with specified virtual screen; sets main screen as current if Screen = nil
    procedure Leave; //Leave 2D mode
    procedure SetMainScreen(Width, Height: Integer); //Set size of main virtual screen
    function  MapCursor(const Cursor: TPoint): TPoint; //Map cursor to virtual screen
    procedure Move(X, Y: Single; Relative: Boolean = true); //Move origin
    procedure SetScissor(Left, Top, Width, Height: Single); //Set scissor window
    procedure RemoveScissor; //Remove scissor window
    //Draw primitives
    procedure LineWidth(w: Single); //Set line width
    procedure DrawLine(X1, Y1, X2, Y2: Single); overload; //Draw line from X1, Y1 to X2, Y2
    procedure DrawLine(P1, P2: TPoint); overload;
    procedure DrawRectBorder(Left, Top, Width, Height: Single); overload; //Draw rect border
    procedure DrawRectBorder(const Rect: TRect); overload;
    procedure DrawRect(Left, Top, Width, Height: Single); overload; //Draw rectangle
    procedure DrawRect(const Rect: TRect); overload;
    procedure DrawRect(Left, Top, Width, Height, TexLeft, TexTop, TexWidth, TexHeight: Single); overload; //Draw rectangle with texture coordinates
    procedure DrawRect(const Rect, TexRect: TRect; TexSize: Integer); overload;
    //Font engine
    function  CreateFont(Name: string; Size: Integer; Bold: Boolean=false; AutoUpdate: Boolean = true): Cardinal; //Create font; Name - font name, Size - font size, Bold - normal/bold font, AutoUpdate: regenerate textures on resolution changes; returns font ID
    procedure DrawText(Font: Cardinal; X, Y: Single; const Text: string); //Draw text; Font - font ID, X, Y - coordinates of left upper corner of text, Text - text for draw
    function  TextWidth(Font: Cardinal; const Text: string): Integer; //Length of space, needed for drawing text
    function  CharWidth(Font: Cardinal; C: Char): Single; //Exact character width
    function  TextHeight(Font: Cardinal): Integer; //Height of space, needed for drawing text
    property  Screen: TVirtualScreen read FScreen; //current Virtual screen
  end;

var
  Render2D: TRender2D; //Render2D interface
  FontCharSet: Cardinal = DEFAULT_CHARSET;
  FontScale: Single = 4 / 3;

const
  InvalidFont: Cardinal = $FFFFFFFF;

implementation

uses
  VSETexMan;

constructor TVirtualScreen.Create(ViewportWidth, ViewportHeight, Width, Height: Integer);
begin
  inherited Create;
  FWidth := Width;
  FHeight := Height;
  FPaddingVertical := Width / Height > ViewportWidth / ViewportHeight;
  if FPaddingVertical then
  begin
    FScale := ViewportWidth / Width;
    FPadding := (ViewportHeight / FScale - Height) / 2;
  end
  else begin
    FScale := ViewportHeight / Height;
    FPadding := (ViewportWidth / FScale - Width) / 2;
  end;
end;

procedure TVirtualScreen.RescaleFont(Font: Cardinal);
begin
  Render2D.CreateFontTex(Self, Font);
end;

function TVirtualScreen.GetBounds: TFloatRect;
begin
  with Result do
    if FPaddingVertical then
    begin
      Left := 0;
      Top := -FPadding;
      Right := FWidth;
      Bottom := FHeight + FPadding;
    end
    else begin
      Left := -FPadding;
      Top := 0;
      Right := FWidth + FPadding;
      Bottom := FHeight;
    end;
end;

constructor TRender2D.Create;
begin
  inherited;
  Render2D := Self;
  FMainScreen := TVirtualScreen.Create(1, 1, 1, 1);
  SetMainScreen(800, 600);
  FScreen := FMainScreen;
end;

destructor TRender2D.Destroy;
var
  i: Integer;
begin
  Render2D := nil;
  while Length(FEnters) > 0 do Leave;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Render2D: Freeing %d fonts', [Length(FFonts)]);{$ENDIF}
  for i := 0 to High(FFonts) do
  begin
    glDeleteLists(FFonts[i]^.List, 256);
    Dispose(FFonts[i]);
  end;
  Finalize(FFonts);
  Finalize(FEnters);
  FMainScreen.Free;
  inherited;
end;

class function TRender2D.Name: string;
begin
  Result := 'Render2D';
end;

procedure TRender2D.OnEvent(var Event: TCoreEvent);
begin
  inherited;
  if (Event is TSysNotify) and ((Event as TSysNotify).Notify = snResolutionChanged) then
    with FMainScreen do SetMainScreen(Width, Height);
end;

procedure TRender2D.Enter(Screen: TVirtualScreen = nil);
var
  Entry: Integer;
begin
  if Screen = nil then
    Screen := FMainScreen;
  Entry := Length(FEnters);
  SetLength(FEnters, Entry + 1);
  FEnters[Entry] := Screen;
  if (Entry > 0) and (FEnters[Entry - 1] = Screen) then Exit;
  FScreen := Screen;
  glPushAttrib(GL_ENABLE_BIT or GL_POINT_BIT or GL_LINE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT or GL_TEXTURE_BIT or GL_SCISSOR_BIT or GL_TRANSFORM_BIT);
  glePushMatrix;
  with Screen.Bounds do
    gleOrthoMatrix2(Left, Top, Right, Bottom);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TRender2D.Leave;
var
  Entry: Integer;
begin
  if Length(FEnters) = 0 then Exit;
  Entry := High(FEnters);
  if (Entry = 0) or (FEnters[Entry] <> FEnters[Entry - 1]) then
  begin
    glePopMatrix;
    glPopAttrib;
  end;
  SetLength(FEnters, Entry);
  if Entry > 0 then
    FScreen := FEnters[Entry - 1]
  else
    FScreen := FMainScreen;
end;

procedure TRender2D.SetMainScreen(Width, Height: Integer);
var
  i: Integer;
begin
  FMainScreen.Create(Core.ResolutionX, Core.ResolutionY, Width, Height);
  for i := 0 to High(FFonts) do
    if FFonts[i]^.AutoUpdate then
      CreateFontTex(FMainScreen, i);
end;

function TRender2D.MapCursor(const Cursor: TPoint): TPoint;
begin
  if FScreen.FPaddingVertical then
    with FScreen, Cursor do
    begin
      Result.X := Round(X / FScale);
      Result.Y := Round(Y / FScale - FPadding);
    end
  else
    with FScreen, Cursor do
    begin
      Result.X := Round(X / FScale - FPadding);
      Result.Y := Round(Y / FScale);
    end;
end;

procedure TRender2D.Move(X, Y: Single; Relative: Boolean);
begin
  if not Relative then
    glLoadIdentity;
  with FScreen do
    glTranslate(Round(X * FScale) / FScale, Round(Y * FScale) / FScale, 0);
end;

procedure TRender2D.SetScissor(Left, Top, Width, Height: Single);
var
  ResY: Integer;
begin
  with FScreen do
  begin
    with FScreen.Bounds do
      ResY := Round(FScale * (Bottom - Top));
    if FPaddingVertical then
      Top := Top + FPadding
    else
      Left := Left + FPadding;
    glEnable(GL_SCISSOR_TEST);
    glScissor(Round(Left * FScale), ResY - Round((Top + Height) * FScale) - 1, Round(Width * FScale), Round(Height * FScale));
  end;
end;

procedure TRender2D.RemoveScissor;
begin
  glDisable(GL_SCISSOR_TEST);
end;

procedure TRender2D.LineWidth(w: Single);
begin
  glLineWidth(w);
  glPointSize(0.9 * w);
end;

procedure TRender2D.DrawLine(X1, Y1, X2, Y2: Single);
begin
  glBegin(GL_LINES);
    glVertex2f(X1, Y1);
    glVertex2f(X2, Y2);
  glEnd;
end;

procedure TRender2D.DrawLine(P1, P2: TPoint);
begin
  DrawLine(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TRender2D.DrawRectBorder(Left, Top, Width, Height: Single);
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(Left, Top);
    glVertex2f(Left + Width, Top);
    glVertex2f(Left + Width, Top + Height);
    glVertex2f(Left, Top + Height);
  glEnd;
  glBegin(GL_POINTS);
    glVertex2f(Left, Top);
    glVertex2f(Left + Width, Top);
    glVertex2f(Left + Width, Top + Height);
    glVertex2f(Left, Top + Height);
  glEnd;
end;

procedure TRender2D.DrawRectBorder(const Rect: TRect);
begin
  with Rect do
    DrawRectBorder(Left, Top, Right - Left, Bottom - Top);
end;

procedure TRender2D.DrawRect(Left, Top, Width, Height: Single);
begin
  glBegin(GL_QUADS);
    glVertex2f(Left, Top);
    glVertex2f(Left + Width, Top);
    glVertex2f(Left + Width, Top + Height);
    glVertex2f(Left, Top + Height);
  glEnd;
end;

procedure TRender2D.DrawRect(const Rect: TRect);
begin
  with Rect do
    DrawRect(Left, Top, Right - Left, Bottom - Top);
end;

procedure TRender2D.DrawRect(Left, Top, Width, Height, TexLeft, TexTop, TexWidth, TexHeight: Single);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(TexLeft, TexTop);
    glVertex2f(Left, Top);
    glTexCoord2f(TexLeft + TexWidth, TexTop);
    glVertex2f(Left + Width, Top);
    glTexCoord2f(TexLeft + TexWidth, TexTop + TexHeight);
    glVertex2f(Left + Width, Top + Height);
    glTexCoord2f(TexLeft, TexTop + TexHeight);
    glVertex2f(Left, Top + Height);
  glEnd;
end;

procedure TRender2D.DrawRect(const Rect, TexRect: TRect; TexSize: Integer);
begin
  with TexRect do
    DrawRect(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
             Left / TexSize, Top / TexSize, (Right - Left) / TexSize, (Bottom - Top) / TexSize);
end;

function TRender2D.CreateFont(Name: string; Size: Integer; Bold, AutoUpdate: Boolean): Cardinal;
const
  BoolStr: array[Boolean] of Char = ('N', 'B');
var
  i: Integer;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Render2D: Creating font ' + Name + ': ' + IntToStr(Size) + BoolStr[Bold]);{$ENDIF}
  Name := UpperCase(Name);
  for i := 0 to High(FFonts) do
    if (FFonts[i]^.Name = Name) and (FFonts[i]^.Size = Size) and (FFonts[i]^.Bold = Bold) then
    begin
      Result := i;
      Exit;
    end;
  Result := Length(FFonts);
  SetLength(FFonts, Result + 1);
  New(FFonts[Result]);
  FFonts[Result]^.Size := Size;
  FFonts[Result]^.Bold := Bold;
  FFonts[Result]^.AutoUpdate := AutoUpdate;
  FFonts[Result]^.Name := Name;
  FFonts[Result]^.Tex := TexMan.AddTexture('__FONT_' + Name + IntToStr(Size) + BoolStr[Bold], nil, 1, 1, GL_ALPHA8, GL_ALPHA, true, false);
  FFonts[Result]^.List := glGenLists(256);
  CreateFontTex(FScreen, Result);
end;

procedure TRender2D.DrawText(Font: Cardinal; X, Y: Single; const Text: string);
var
  i: Integer;
begin
  if (Font >= Cardinal(Length(FFonts))) or (FFonts[Font] = nil) then
    Exit;
  with FScreen do
  begin
    X := Round(X * FScale) / FScale;
    Y := Round(Y * FScale) / FScale;
  end;
  glPushAttrib(GL_COLOR_BUFFER_BIT or GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_LIST_BIT);
  glDisable(GL_CULL_FACE);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GEQUAL, 0.1);
  TexMan.Bind(FFonts[Font]^.Tex, 0);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glListBase(FFonts[Font]^.List);
  glPushMatrix;
  glTranslatef(X, Y, 0);
  for i := 1 to Length(Text) do
    glCallLists(1, GL_UNSIGNED_BYTE, @Text[i]);
  glPopMatrix;
  glPopAttrib;
end;

function TRender2D.TextWidth(Font: Cardinal; const Text: string): Integer;
var
  i: Integer;
  W: Single;
begin
  W := 0;
  if (Font < Cardinal(Length(FFonts))) and Assigned(FFonts[Font]) then
    for i := 1 to Length(Text) do
      W := W + FFonts[Font]^.Width[Byte(Text[i])];
  Result := Ceil(W);
end;

function TRender2D.CharWidth(Font: Cardinal; C: Char): Single;
begin
  if (Font < Cardinal(Length(FFonts))) and Assigned(FFonts[Font]) then
    Result := FFonts[Font]^.Width[Byte(C)]
    else Result := 0;
end;

function TRender2D.TextHeight(Font: Cardinal): Integer;
begin
  Result := 0;
  if (Font >= Cardinal(Length(FFonts))) or (FFonts[Font] = nil) then Exit;
  Result := FFonts[Font]^.Height;
end;

procedure TRender2D.CreateFontTex(Screen: TVirtualScreen; Font: Cardinal);
const
  Weight: array[Boolean] of Integer = (400, 700);
  Margin = 32;
  {$IFDEF VSE_LUMALPHA_TEXT}
  PxSize = 2;
  TexFmt = GL_LUMINANCE_ALPHA;
  {$ELSE}
  PxSize = 1;
  TexFmt = GL_ALPHA;
  {$ENDIF}
var
  i: Integer;
  FNT: HFONT;
  MDC: HDC;
  BMP: HBITMAP;
  BI: BITMAPINFO;
  Pix: PByteArray;
  Data: PByteArray;
  CS: TSize;
  s, t: Single;
  CharSize, FontTexSize: TPoint;
begin
  with FFonts[Font]^ do
  try
    MDC := CreateCompatibleDC(Core.DC);
    FNT := Windows.CreateFont(-Ceil(FontScale * Screen.FScale * Size), 0, 0, 0, Weight[Bold],
      0, 0, 0, FontCharSet, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, NONANTIALIASED_QUALITY, 0, PChar(Name));
    SelectObject(MDC, FNT);
    CharSize := Point(0, 0);
    for i := 0 to 255 do
    begin
      GetTextExtentPoint32(MDC, @Char(i), 1, CS);
      CharSize.X := Max(CharSize.X, CS.cx);
      CharSize.Y := Max(CharSize.Y, CS.cy);
    end;
    FontTexSize := Point(Max(128, Min(CeilPOT(16 * CharSize.X + Margin), glMaxTextureSize)),
                         Max(128, Min(CeilPOT(16 * CharSize.Y + Margin), glMaxTextureSize)));
    ZeroMemory(@BI, SizeOf(BI));
    with BI.bmiHeader do
    begin
      biSize := SizeOf(BITMAPINFOHEADER);
      biWidth := FontTexSize.X;
      biHeight := FontTexSize.Y;
      biPlanes := 1;
      biBitCount := 24;
      biSizeImage := biWidth * biHeight * biBitCount div 8;
    end;
    BMP := CreateDIBSection(MDC, BI, DIB_RGB_COLORS, Pointer(Pix), 0, 0);
    ZeroMemory(Pix, FontTexSize.X * FontTexSize.Y * 3);
    SelectObject(MDC, BMP);
    SetBkMode(MDC, TRANSPARENT);
    SetTextColor(MDC, $FFFFFF);
    for i := 0 to 255 do
      Windows.TextOut(MDC, i mod 16 * (FontTexSize.X div 16), i div 16 * (FontTexSize.Y div 16), @Char(i), 1);
    GetMem(Data, PxSize * FontTexSize.X * FontTexSize.Y);
    for i := 0 to FontTexSize.X * FontTexSize.Y - 1 do
    {$IFDEF VSE_LUMALPHA_TEXT}
    begin
      Data[2 * i] := 255;
      Data[2 * i + 1] := Pix[3 * i];
    end;
    {$ELSE}
      Data[i] := Pix[3 * i];
    {$ENDIF}
    glBindTexture(GL_TEXTURE_2D, Tex);
    glPixelStore(GL_UNPACK_ALIGNMENT, 1);
    glTexImage2D(GL_TEXTURE_2D, 0, TexFmt, FontTexSize.X, FontTexSize.Y, 0, TexFmt, GL_UNSIGNED_BYTE, Data);
    Height := 0;
    for i := 0 to 255 do
    begin
      glNewList(List + Cardinal(i), GL_COMPILE);
      s := (i mod 16) / 16;
      t := (i div 16) / 16;
      GetTextExtentPoint32(MDC, @Char(i), 1, CS);
      Width[i] := CS.cx / Screen.FScale;
      Height := Max(Ceil(CS.cy / Screen.FScale), Height);
      glBegin(GL_QUADS);
      glTexCoord2f(s, 1 - t);
      glVertex2f(0, 0);
      glTexCoord2f(s + CS.cx / FontTexSize.X, 1 - t);
      glVertex2f(CS.cx / Screen.FScale, 0);
      glTexCoord2f(s + CS.cx / FontTexSize.X, 1 - t - CS.cy / FontTexSize.Y);
      glVertex2f(CS.cx / Screen.FScale, CS.cy / Screen.FScale);
      glTexCoord2f(s, 1 - t - CS.cy / FontTexSize.Y);
      glVertex2f(0, CS.cy / Screen.FScale);
      glEnd;
      glTranslatef(Width[i], 0, 0);
      glEndList;
    end;
    glBindTexture(GL_TEXTURE_2D, 0);
  finally
    FreeMem(Data);
    DeleteObject(FNT);
    DeleteObject(BMP);
    DeleteDC(MDC);
  end;
end;

initialization
  RegisterModule(TRender2D);

end.
