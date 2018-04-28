unit GameForms;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSECore, VSEGUI;

type
  {$IFDEF VSE_DEBUG}
  TLogPointsForm = class(TGUIForm)
  private
    FPoints: array of TVector3D;
    FListFont: Cardinal;
    FNameEdit: Integer;
    procedure NameEditClick(Btn: PBtn);
    procedure RemoveClick(Btn: PBtn);
    procedure SaveClick(Btn: PBtn); 
  protected
    procedure DrawForm; override;
    procedure DrawButton(const Btn: TBtn; State: TBtnState); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CharEvent(C: Char); override;
    procedure AddPoint(Point: TVector3D);
    procedure DrawPoints;
  end;
  {$ENDIF}

const
  {$IFDEF VSE_DEBUG}IDLogPoints = 'LogPoints';{$ENDIF}

implementation

uses
  VSERender2D, VSEFormManager{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

{$IFDEF VSE_DEBUG}
{ TLogPointsForm }

constructor TLogPointsForm.Create;
var
  Btn: TBtn;
begin
  with Render2D.VSBounds do
    inherited Create(Round(Right - 150), Round(Top), 150, 400);
  FCaption := 'Log points';
  FListFont := Render2D.CreateFont('Arial Narrow', 10, false);
  with Btn do
  begin
    Type_ := btPush;
    X := 10;
    Y := 320;
    Width := 130;
    Height := 30;
    Enabled := true;
    Caption := 'Name';
    OnClick := NameEditClick;
    FNameEdit := AddButton(Btn);
    Y := 360;
    Width := 60;
    Caption := 'Remove';
    OnClick := RemoveClick;
    AddButton(Btn);
    X := 80;
    Caption := 'Save';
    OnClick := SaveClick;
    AddButton(Btn);
  end;
  AddRect(Rect(10, 35, 140, 310));
end;

destructor TLogPointsForm.Destroy;
begin
  Finalize(FPoints);
  inherited;
end;

procedure TLogPointsForm.CharEvent(C: Char);
begin
  with Button[FNameEdit]^ do
    if Tag = $ED then
      case C of
        Chr(VK_BACK): Delete(Caption, Length(Caption), 1);
        Chr(VK_RETURN), Chr(VK_ESCAPE): Tag := 0;
        else Caption := Caption + C;
      end;
end;

procedure TLogPointsForm.AddPoint(Point: TVector3D);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := Point;
end;

procedure TLogPointsForm.DrawPoints;
var
  i: Integer;
begin
  glPushAttrib(GL_ENABLE_BIT or GL_POINT_BIT or GL_LINE_BIT or GL_CURRENT_BIT);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glPointSize(4);
  glLineWidth(1);
  gleColor(clLime);
  glBegin(GL_POINTS);
  for i := 0 to High(FPoints) do
    glVertex3fv(@FPoints[i]);
  glEnd;
  glBegin(GL_LINE_STRIP);
  for i := 0 to High(FPoints) do
    glVertex3fv(@FPoints[i]);
  glEnd;
  glPopAttrib;
end;

procedure TLogPointsForm.DrawForm;
var
  i: Integer;
begin
  inherited;
  Render2D.SetScissor(Left + 10, Top + 35, 130, 275);
  gleColor(clText);
  for i := 0 to Min(16, High(FPoints)) do
    with Render2D, FPoints[Max(0, High(FPoints) - 16) + i] do
      TextOut(FListFont, 12, 37 + 16 * i, Format('%s; %s; %s', [FloatToStr2(X, 1, 2), FloatToStr2(Y, 1, 2), FloatToStr2(Z, 1, 2)]));
  Render2D.RemoveScissor;    
end;

procedure TLogPointsForm.DrawButton(const Btn: TBtn; State: TBtnState);
begin
  inherited;
  if (Btn.Tag = $ED) and (Core.Time and $100 = 0) then
    with Render2D, Btn do
    begin
      SetColor(State, BtnText, Btn.Enabled);
      SetScissor(FX + X, FY + Y, Width, Height);
      TextOut(GetFont,
        X + Max((Width - TextWidth(GetFont, Caption)) div 2, 0) + TextWidth(GetFont, Caption),
        Y + (Height - TextHeight(GetFont)) div 2, '_');
      RemoveScissor;
    end;
end;

procedure TLogPointsForm.NameEditClick(Btn: PBtn);
begin
  Btn.Tag := $ED;
end;

procedure TLogPointsForm.RemoveClick(Btn: PBtn);
begin
  if Core.KeyPressed[VK_SHIFT] then
    Finalize(FPoints)
  else
    SetLength(FPoints, Length(FPoints) - 1);
end;

procedure TLogPointsForm.SaveClick(Btn: PBtn);
const
  Postfix: array[Boolean] of string = (',', '');
var
  i: Integer;
begin
  if Length(FPoints) = 0 then Exit;
  {$IFDEF VSE_CONSOLE}
  with Console do
  begin
    WriteLn(Format('Points log "%s" (%d points):' + PostfixWarning, [Button[FNameEdit].Caption, Length(FPoints)]));
    for i := 0 to High(FPoints) do
      with FPoints[i] do
        WriteLn(Format('  (X: %s; Y: %s; Z: %s)' + Postfix[i = High(FPoints)],
          [FloatToStr2(X, 1, 3), FloatToStr2(Y, 1, 3), FloatToStr2(Z, 1, 3)]));
    WriteLn;
  end;
  {$ENDIF}
  Finalize(FPoints);
end;

{$ENDIF}

end.
