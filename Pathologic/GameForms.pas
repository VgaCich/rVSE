unit GameForms;

interface

uses
  Windows, AvL, avlUtils, avlEventBus, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSECore, VSEGUI, VSEFormManager, Game, GameObjects;

type
  {$IFDEF VSE_DEBUG}
  TLogPointsForm = class(TAlignedForm)
  private
    FPoints: array of TVector3D;
    FListFont: Cardinal;
    FNameEdit: Integer;
    procedure NameEditClick(Btn: PBtn);
    procedure RemoveClick(Btn: PBtn);
    procedure SaveClick(Btn: PBtn);
    procedure GameMouseEvent(Sender: TObject; const Args: array of const); 
  protected
    procedure DrawForm(State: TBtnState); override;
    procedure DrawButton(const Btn: TBtn; State: TBtnState); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CharEvent(C: Char); override;
    procedure DrawPoints;
  end;
  TPlayerSelectForm = class(TAlignedForm)
  private
    FGame: TGame;
    procedure PlayerClick(Btn: PBtn);
  public
    constructor Create(Game: TGame);
    procedure Update; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  {$ENDIF}
  TOnCharSelect = procedure(Sender: TObject; Char: TCharacter) of object;
  TCharSelectForm = class(TAlignedForm)
  private
    FOnSelect: TOnCharSelect;
    procedure CharClick(Btn: PBtn);
  public
    constructor Create(Objects: TGameObjectsArray);
    property OnSelect: TOnCharSelect read FOnSelect write FOnSelect;
  end;


const
  {$IFDEF VSE_DEBUG}
  IDLogPoints = 'LogPoints';
  IDPlayerSelect = 'PlayerSelect';
  {$ENDIF}

implementation

uses
  VSERender2D, GameData{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

{$IFDEF VSE_DEBUG}

{ TLogPointsForm }

constructor TLogPointsForm.Create;
var
  Btn: TBtn;
begin
  with Render2D.VSBounds do
    inherited Create(0, 0, 150, 400);
  FCaption := 'Log points';
  Alignment := [faRight, faMiddle];
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
    Width := 65;
    Caption := 'Remove';
    OnClick := RemoveClick;
    AddButton(Btn);
    X := 85;
    Width := 55;
    Caption := 'Save';
    OnClick := SaveClick;
    AddButton(Btn);
  end;
  AddRect(Rect(10, 35, 140, 310));
  EventBus.AddListener(GameOnMouseEvent, GameMouseEvent);
end;

destructor TLogPointsForm.Destroy;
begin
  EventBus.RemoveListener(GameMouseEvent);
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

procedure TLogPointsForm.DrawForm(State: TBtnState);
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
const
  Cursor: array[Boolean] of Char = (' ', '_');
var
  NewBtn: TBtn;
begin
  if Btn.Tag = $ED then
  begin
    NewBtn := Btn;
    NewBtn.Caption := NewBtn.Caption + Cursor[Core.Time and $100 = 0];
    inherited DrawButton(NewBtn, State);
  end
  else
    inherited;
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

procedure TLogPointsForm.GameMouseEvent(Sender: TObject; const Args: array of const);
begin
  Assert((Length(Args) = 2) and (Args[0].VType = vtInteger) and (Args[1].VType = vtPointer));
  if FParentSet.Visible[Name] and (TMouseEvent(Args[0].VInteger) = meDown) then
  begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := TVector3D(Args[1].VPointer^);
  end;
end;

{ TPlayerSelectForm }

constructor TPlayerSelectForm.Create(Game: TGame);
var
  i: Integer;
  Btn: TBtn;
begin
  with Render2D.VSBounds do
    inherited Create(0, 0, 125, 165);
  FCaption := 'Игрок';
  Alignment := [faRight, faTop];
  FGame := Game;
  with Btn do
  begin
    Type_ := btPush;
    X := 10;
    Width := 105;
    Height := 25;
    OnClick := PlayerClick;
    for i := 0 to High(PlayerNames) do
    begin
      Y := 35 + 30 * i;
      Caption := FGame.Player[PlayerNames[i]].Character.Profile.RuName;
      Tag := i;
      AddButton(Btn);
    end;
  end;
end;

procedure TPlayerSelectForm.Update;
var
  i: Integer;
begin
  for i := 0 to High(PlayerNames) do
    Button[i].Enabled := FGame.ActivePlayer <> FGame.Player[PlayerNames[i]];
  inherited;
end;

procedure TPlayerSelectForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  inherited;
  if Event = keDown then
    case Key of
      Ord('B'): FGame.ActivePlayer := FGame.Player[SBachelor];
      Ord('C'): FGame.ActivePlayer := FGame.Player[SChangeling];
      Ord('H'): FGame.ActivePlayer := FGame.Player[SHaruspex];
      Ord('P'): FGame.ActivePlayer := FGame.Player[SPlague];
    end;
end;

procedure TPlayerSelectForm.PlayerClick(Btn: PBtn);
begin
  FGame.ActivePlayer := FGame.Player[PlayerNames[Btn.Tag]];
end;

{$ENDIF}
    
{ TCharSelectForm }

constructor TCharSelectForm.Create(Objects: TGameObjectsArray);
var
  Btn: TBtn;
  i: Integer;
begin
  inherited Create(0, 0, 220, 25);
  FCaption := 'Персонаж';
  with Btn do
  begin
    Type_ := btPush;
    X := 10;
    Width := 200;
    Height := 25;
    Enabled := true;
    OnClick := CharClick;
    i := 0;
    while Assigned(Objects.ObjOfType[TCharacter, i]) do
    begin
      Y := 35 + 30 * i;
      Self.Height := Y + 35;
      Tag := Integer(Objects.ObjOfType[TCharacter, i]);
      Caption := TCharacter(Tag).Profile.RuName;
      AddButton(Btn);
      Inc(i);
    end;
  end;
  Alignment := [faCenter, faMiddle];
end;

procedure TCharSelectForm.CharClick(Btn: PBtn);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self, TCharacter(Btn.Tag));
  Close;
end;

end.
