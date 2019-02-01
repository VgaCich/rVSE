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
    Bold: Boolean;
    Name: string;
  end;
  PFloatRect = ^TFloatRect;
  TFloatRect = packed record
    Left, Top, Right, Bottom: Single;
  end;
  TRender2D = class(TModule)
  private
    FVSWidth: Integer;
    FVSHeight: Integer;
    FVSScale, FVSPadding: Single;
    FVSPaddingVertical: Boolean;
    FEnters: Integer;
    FFonts: array of PFont;
    function  GetVSBounds: TFloatRect;
    procedure SetVSWidth(Value: Integer);
    procedure SetVSHeight(Value: Integer);
    procedure ResolutionChanged;
    procedure CreateFontTex(Font: Cardinal);
  public
    constructor Create; override; //internally used
    destructor Destroy; override; //internally used
    class function Name: string; override; //internally used
    function  SysNotify(Notify: TSysNotify): Boolean; override; //internally used
    procedure Enter; //Enter 2D mode
    procedure Leave; //Leave 2D mode
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
    function  CreateFont(Name: string; Size: Integer; Bold: Boolean=false): Cardinal; //Create font; Name - font name, Size - font size, Bold - normal/bold font; returns font ID
    procedure TextOut(Font: Cardinal; X, Y: Single; const Text: string); //Draw text; Font - font ID, X, Y - coordinates of left upper corner of text, Text - text for draw
    function  TextWidth(Font: Cardinal; const Text: string): Integer; //Length of space, needed for drawing text
    function  CharWidth(Font: Cardinal; C: Char): Single; //Exact character width
    function  TextHeight(Font: Cardinal): Integer; //Height of space, needed for drawing text
    //Virtual screen
    property VSWidth: Integer read FVSWidth write SetVSWidth; //Virtual screen width
    property VSHeight: Integer read FVSHeight write SetVSHeight; //Virtual screen height
    property VSBounds: TFloatRect read GetVSBounds; //Virtual screen bounds
  end;

var
  Render2D: TRender2D; //Render2D interface

const
  InvalidFont: Cardinal = $FFFFFFFF;

implementation

uses
  VSETexMan;

constructor TRender2D.Create;
begin
  inherited;
  Render2D := Self;
  FVSWidth := 800;
  FVSHeight := 600;
  ResolutionChanged;
end;

destructor TRender2D.Destroy;
var
  i: Integer;
begin
  Render2D := nil;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Render2D: Freeing %d fonts', [Length(FFonts)]);{$ENDIF}
  for i := 0 to High(FFonts) do
  begin
    glDeleteLists(FFonts[i]^.List, 256);
    Dispose(FFonts[i]);
  end;
  Finalize(FFonts);
  inherited;
end;

class function TRender2D.Name: string;
begin
  Result := 'Render2D';
end;

function TRender2D.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result := inherited SysNotify(Notify);
  if Notify = snResolutionChanged then
    ResolutionChanged;
end;

procedure TRender2D.Enter;
begin
  Inc(FEnters);
  if FEnters > 1 then Exit;
  glPushAttrib(GL_ENABLE_BIT or GL_POINT_BIT or GL_LINE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT or GL_TEXTURE_BIT or GL_SCISSOR_BIT or GL_TRANSFORM_BIT);
  glePushMatrix;
  with VSBounds do
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
begin
  FEnters := Max(FEnters - 1, 0);
  if FEnters > 0 then Exit;
  glePopMatrix;
  glPopAttrib;
end;

function TRender2D.MapCursor(const Cursor: TPoint): TPoint;
begin
  if FVSPaddingVertical then
  begin
    Result.X := Round(Cursor.X / FVSScale);
    Result.Y := Round(Cursor.Y / FVSScale - FVSPadding);
  end
  else begin
    Result.X := Round(Cursor.X / FVSScale - FVSPadding);
    Result.Y := Round(Cursor.Y / FVSScale);
  end;
end;

procedure TRender2D.Move(X, Y: Single; Relative: Boolean);
begin
  if not Relative then
    glLoadIdentity;
  glTranslate(Round(X * FVSScale) / FVSScale, Round(Y * FVSScale) / FVSScale, 0);
end;

procedure TRender2D.SetScissor(Left, Top, Width, Height: Single);
begin
  if FVSPaddingVertical then
    Top := Top + FVSPadding
  else
    Left := Left + FVSPadding;
  glEnable(GL_SCISSOR_TEST);
  glScissor(Round(Left * FVSScale), Core.ResolutionY - Round((Top + Height) * FVSScale) - 1, Round(Width * FVSScale), Round(Height * FVSScale));
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

function TRender2D.CreateFont(Name: string; Size: Integer; Bold: Boolean): Cardinal;
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
  FFonts[Result]^.Name := Name;
  FFonts[Result]^.Tex := TexMan.AddTexture('__FONT_' + Name + IntToStr(Size) + BoolStr[Bold], nil, 1, 1, GL_ALPHA8, GL_ALPHA, true, false);
  FFonts[Result]^.List := glGenLists(256);
  CreateFontTex(Result);
end;

procedure TRender2D.TextOut(Font: Cardinal; X, Y: Single; const Text: string);
var
  i: Integer;
begin
  if (Font >= Cardinal(Length(FFonts))) or (FFonts[Font] = nil) then
    Exit;
  X := Round(X * FVSScale) / FVSScale;
  Y := Round(Y * FVSScale) / FVSScale;
  glPushAttrib(GL_COLOR_BUFFER_BIT or GL_TEXTURE_BIT or GL_LIST_BIT);
  glDisable(GL_CULL_FACE);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GEQUAL, 0.1);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glListBase(FFonts[Font]^.List);
  TexMan.Bind(FFonts[Font]^.Tex, 0);
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

function TRender2D.GetVSBounds: TFloatRect;
begin
  with Result do
    if FVSPaddingVertical then
    begin
      Left := 0;
      Top := -FVSPadding;
      Right := FVSWidth;
      Bottom := FVSHeight + FVSPadding;
    end
    else begin
      Left := -FVSPadding;
      Top := 0;
      Right := FVSWidth + FVSPadding;
      Bottom := FVSHeight;
    end;
end;

procedure TRender2D.SetVSWidth(Value: Integer);
begin
  if Value <= 0 then Exit;
  FVSWidth := Value;
  ResolutionChanged;
end;

procedure TRender2D.SetVSHeight(Value: Integer);
begin
  if Value <= 0 then Exit;
  FVSHeight := Value;
  ResolutionChanged;
end;

procedure TRender2D.ResolutionChanged;
var
  i: Integer;
begin
  FVSPaddingVertical := FVSWidth / FVSHeight > Core.ResolutionX / Core.ResolutionY;
  if FVSPaddingVertical then
  begin
    FVSScale := Core.ResolutionX / FVSWidth;
    FVSPadding := (Core.ResolutionY / FVSScale - FVSHeight) / 2;
  end
  else begin
    FVSScale := Core.ResolutionY / FVSHeight;
    FVSPadding := (Core.ResolutionX / FVSScale - FVSWidth) / 2;
  end;
  for i := 0 to High(FFonts) do CreateFontTex(i);
end;

procedure TRender2D.CreateFontTex(Font: Cardinal);
const
  Weight: array[Boolean] of Integer = (400, 700);
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
  CharSize, FontTexSize: Integer;
begin
  with FFonts[Font]^do
  try
    FNT := Windows.CreateFont(-MulDiv(Size, Ceil(FVSScale * GetDeviceCaps(Core.DC, LOGPIXELSY)), 72),
      0, 0, 0, Weight[Bold], 0, 0, 0, RUSSIAN_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS, NONANTIALIASED_QUALITY, 0, PChar(Name));
    FontTexSize := 128;
    CharSize := MulDiv(Size, Ceil(FVSScale * GetDeviceCaps(Core.DC, LOGPIXELSY)), 72) * 16 + 64;
    while (FontTexSize < CharSize) and (FontTexSize <= glMaxTextureSize) do
      FontTexSize := FontTexSize * 2;
    ZeroMemory(@BI, SizeOf(BI));
    with BI.bmiHeader do
    begin
      biSize := SizeOf(BITMAPINFOHEADER);
      biWidth := FontTexSize;
      biHeight := FontTexSize;
      biPlanes := 1;
      biBitCount := 24;
      biSizeImage := biWidth * biHeight * biBitCount div 8;
    end;
    MDC := CreateCompatibleDC(Core.DC);
    BMP := CreateDIBSection(MDC, BI, DIB_RGB_COLORS, Pointer(Pix), 0, 0);
    ZeroMemory(Pix, FontTexSize * FontTexSize * 3);
    SelectObject(MDC, BMP);
    SelectObject(MDC, FNT);
    SetBkMode(MDC, TRANSPARENT);
    SetTextColor(MDC, $FFFFFF);
    for i := 0 to 255 do
      Windows.TextOut(MDC, i mod 16 * (FontTexSize div 16), i div 16 * (FontTexSize div 16), @Char(i), 1);
    GetMem(Data, FontTexSize * FontTexSize);
    for i := 0 to FontTexSize * FontTexSize - 1 do
      Data[i] := Pix[i * 3];
    glBindTexture(GL_TEXTURE_2D, Tex);
    glPixelStore(GL_UNPACK_ALIGNMENT, 1);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, FontTexSize, FontTexSize, 0, GL_ALPHA, GL_UNSIGNED_BYTE, Data);
    Height := 0;
    for i := 0 to 255 do
    begin
      glNewList(List + Cardinal(i), GL_COMPILE);
      s := (i mod 16) / 16;
      t := (i div 16) / 16;
      GetTextExtentPoint32(MDC, @Char(i), 1, CS);
      Width[i] := CS.cx / FVSScale;
      Height := Max(Ceil(CS.cy / FVSScale), Height);
      glBegin(GL_QUADS);
      glTexCoord2f(s, 1 - t);
      glVertex2f(0, 0);
      glTexCoord2f(s + CS.cx / FontTexSize, 1 - t);
      glVertex2f(CS.cx / FVSScale, 0);
      glTexCoord2f(s + CS.cx / FontTexSize, 1 - t - CS.cy / FontTexSize);
      glVertex2f(CS.cx / FVSScale, CS.cy / FVSScale);
      glTexCoord2f(s, 1 - t - CS.cy / FontTexSize);
      glVertex2f(0, CS.cy / FVSScale);
      glEnd;
      glTranslatef(Width[i], 0, 0);
      glEndList;
    end;
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
