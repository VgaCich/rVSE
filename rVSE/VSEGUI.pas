unit VSEGUI;

interface

uses
  Windows, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore, VSERender2D;

type
  TGUIForm = class;
  TBtnType = (btPush, btCheck, btRadio); //Button type: push button, check box, radio button
  PBtn = ^TBtn;
  TGUIOnClick = procedure(Btn: PBtn) of object;
  TOnForm = procedure(Form: TGUIForm) of object;
  TBtn = record //Button
    Caption: string; //Button caption
    Type_: TBtnType; //Button type
    X, Y, Width, Height, Group, Tag: Integer; //X, Y, Width, Height: button bounds; Group: radio button group; Tag: custom information
    OnClick: TGUIOnClick; //Button click event handler, don't override if Type_=btCheck or Type_=btRadio
    Checked, Enabled: Boolean; //Checked: true if check box or radio button checked; Enabled: enable button
  end;
  TLabelAlign = (laLeft, laCenter, laRight); //Label aligning
  PLbl = ^TLbl;
  TLbl = record //Label
    Caption: string; //Label text
    X, Y, Width: Integer; //Label bounds (height depends from form font)
    Align: TLabelAlign; //Text aligning
    Color: TColor; //Text color, 0 for default
  end;
  TBtnStates = (bsHighlighted, bsPushed, bsTabStop, fsLocked = bsTabStop); //Button state - highlighted (mouse over), pushed, selected from keyboard
  TBtnState = set of TBtnStates;
  TColorSet = record
    Default, Highlighted, Activated, Disabled: TColor;
  end;
  TMenuItem = record //Menu item
    Caption: string; //Item button caption
    Tag: Integer; //Item button tag
    OnClick: TGUIOnClick; //Item button click event handler
  end;
  PFormRec = ^TFormRec;
  TFormRec = record
    Name, Parent: string;
    Form: TGUIForm;
    Visible, Locked: Boolean;
    Prev, Next: PFormRec;
  end;
  TMenuItems = array of Integer;
  TGUIFormsSet = class
  private
    FForms: PFormRec;
    function GetForm(const Name: string): TGUIForm;
    function GetVisible(const Name: string): Boolean;
    procedure SetVisible(const Name: string; const Value: Boolean);
  public
    destructor Destroy; override;
    function AddForm(const Name: string; Form: TGUIForm; const Parent: string = ''): TGUIForm; //Add form; Parent: name of parent form; Parentless forms visible by default
    procedure RemoveForm(Form: TGUIForm); //Remove form
    procedure IterateForms(Handler: TOnForm); //Call Handler for every form
    function FindForm(const Name: string): PFormRec; overload; //internally used
    function FindForm(Form: TGUIForm): PFormRec; overload; //internally used
    function FormAt(X, Y: Integer): PFormRec; //internally used
    procedure Pop(Form: PFormRec); //internally used
    function LastForm: PFormRec; //internally used
    property FirstForm: PFormRec read FForms; //internally used
    function FormName(Form: TGUIForm): string; //Name of form
    property Forms[const Name: string]: TGUIForm read GetForm; default; //Forms
    property Visible[const Name: string]: Boolean read GetVisible write SetVisible; //Form visibility
  end;
  TGUIForm = class //GUI Form
  private
    FActive, FLastActive, FLast, FTabStop: Integer; //internally used
    FCustomFont: Cardinal; //internally used
    FMovable, FClose: Boolean; //internally used
    FDragPoint: TPoint; //internally used
    FButtons: array of TBtn; //internally used
    FRects: array of TRect; //internally used
    FLabels: array of TLbl; //internally used
    function BtnState(Highlighted, Pushed, TabStop: Boolean): TBtnState;
    function IsMoving: Boolean;
    function GetName: string;
  protected
    FCaption: string; //Form caption
    FCaptHeight: Integer; //Form caption height
    FX, FY, FWidth, FHeight: Integer; //Form position and size
    FParentSet: TGUIFormsSet; //Form set
    function Font: Cardinal;
    function  GetButton(Index: Integer): PBtn; //Get button by index
    function  GetLabel(Index: Integer): PLbl; //Get label by index
    procedure CheckClick(Btn: PBtn); //CheckBox click handler
    procedure RadioClick(Btn: PBtn); //RadioButton click handler
    procedure MapCursor(var Cursor: TPoint); //Map cursor to form coordinates
    function  BtnAt(Point: TPoint): Integer; //Get button at specified coordinates
    procedure SetColor(State: TBtnState; const ColorSet: TColorSet; Enabled: Boolean = true); //Set color from color set
    procedure DrawForm(State: TBtnState); dynamic; //Override for custom form drawing
    procedure DrawButton(const Btn: TBtn; State: TBtnState); dynamic; //Override for custom buttons drawing
    procedure DrawRect(const Rect: TRect); dynamic; //Override for custom rectangles drawing
    procedure DrawLabel(const Lbl: TLbl); dynamic; //Override for custom labels drawing
  public
    constructor Create(X, Y, Width, Height: Integer); //Creates form; VertScr*: virtual screen resolution (all dimensions defined in virtual screen coordinates); X, Y, Width, Height: form bounds
    destructor Destroy; override;
    function  AddButton(Btn: TBtn): Integer; //Add button, returns button index
    function  AddLabel(const Lbl: TLbl): Integer; //Add label, returns label index
    procedure AddRect(const Rect: TRect); //Add rectangle (visual frame)
    procedure Draw; //Draw form
    procedure Update; dynamic; //Update form
    procedure Close; //Free form safely
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); dynamic; //Process mouse event
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); dynamic; //Process key event
    procedure CharEvent(C: Char); dynamic; //Process char event
    property Button[Index: Integer]: PBtn read GetButton; //Buttons array
    property Lbl[Index: Integer]: PLbl read GetLabel; //Labels array
    property Caption: string read FCaption write FCaption; //Form caption
    property Left: Integer read FX write FX; //Horisontal position
    property Top: Integer read FY write FY; //Vertical position
    property Width: Integer read FWidth write FWidth; //Form width
    property Height: Integer read FHeight write FHeight; //Form height
    property Name: string read GetName; //Form name
    property CustomFont: Cardinal read FCustomFont write FCustomFont; //Custom form font; InvalidFont: don't use
    property Movable: Boolean read FMovable write FMovable; //Form can be dragged by caption
  end;

procedure SetGUIFont(const Name: string = 'Tahoma'; Size: Integer = 12; Bold: Boolean = true);
function CreateSelect(Form: TGUIForm; X, Y, Width, Height: Integer; OnChange: TGUIOnClick; const PrevCaption, NextCaption: string): Integer; //Creates select control, returns select state label index; distinguish prev & next buttons in handler by Btn^.Tag (-1 for prev, 1 for next)
function CreateMenu(Form: TGUIForm; X, Y, BtnWidth, BtnHeight, BtnSpacing: Integer; Items: array of TMenuItem): TMenuItems; //Creates menu from buttons, returns butttons' indexes

var
{$IF not (Defined(CLRSCM_DEFAULT) or Defined(CLRSCM_WINMO) or Defined(CLRSCM_WIN95))}
  {$DEFINE CLRSCM_DEFAULT}
{$IFEND}
{$IF Defined(CLRSCM_DEFAULT)}
  BtnBackground: TColorSet = (Default: $FF80FF80; Highlighted: $FF8080FF; Activated: $FF7070E0; Disabled: $FF80FF80);
  BtnBorder: TColorSet = (Default: $FF00D000; Highlighted: $FF0000FF; Activated: $FF0000FF; Disabled: $FF20FF20);
  BtnText: TColorSet = (Default: $FF00B200; Highlighted: $FF00C000; Activated: $FF00A000; Disabled: $FF00F000);
  FormCapt: TColorSet = (Default: $FF80E600; Highlighted: $FF80DC00; Activated: $FF73CF00; Disabled: $FF80FF80);
  clFormBackground: TColor = $FF80FF80;
  clFormBorder: TColor = $FF00FF00;
  clFormCaptText: TColor = $FF00FFFF;
  clText: TColor = $FF00B200;
  clTabStop: TColor = $FF000080;
{$ELSEIF Defined(CLRSCM_WINMO) or Defined(CLRSCM_WIN95)}
  BtnBackground: TColorSet = (Default: $FFC0C0C0; Highlighted: $FFC8C8C8; Activated: $FFA0A0A0; Disabled: $FFC0C0C0);
  BtnBorder: TColorSet = (Default: $FF404040; Highlighted: $FF404040; Activated: $FF404040; Disabled: $FF808080);
  BtnText: TColorSet = (Default: $FF000000; Highlighted: $FF000000; Activated: $FF000000; Disabled: $FF808080);
  FormCapt: TColorSet = (Default: $FF800000; Highlighted: $FF8A0000; Activated: $FF880000; Disabled: $FF808080);
  clFormBackground: TColor = {$IFDEF CLRSCM_WINMO}$FFFFFFFF{$ELSE}$FFC0C0C0{$ENDIF};
  clFormBorder: TColor = $FF404040;
  clFormCaptText: TColor = $FFFFFFFF;
  clText: TColor = $FF000000;
  clTabStop: TColor = $FF000000;
{$IFEND}
  GUIFont: Cardinal;

implementation

uses
  VSECollisionCheck;

{ TGUIFormsSet }

destructor TGUIFormsSet.Destroy;
var
  Cur: PFormRec;
begin
  while Assigned(FForms) do
  begin
    Cur := FForms;
    FForms := Cur.Next;
    Cur.Form.Free;
    Dispose(Cur);
  end;
  inherited;
end;

function TGUIFormsSet.AddForm(const Name: string; Form: TGUIForm; const Parent: string): TGUIForm;
var
  Rec, Last: PFormRec;
begin
  Result := Form;
  if Assigned(FindForm(Name)) then
    raise Exception.Create('FormsSet: duplicate form name "' + Name + '"');
  New(Rec);
  Rec.Name := Name;
  Rec.Form := Form;
  Rec.Parent := Parent;
  Rec.Visible := Parent = '';
  Rec.Locked := false;
  if Rec.Visible or not Assigned(FForms) then
  begin
    Rec.Prev := nil;
    Rec.Next := FForms;
    if Assigned(FForms) then FForms.Prev := Rec;
    FForms := Rec;
  end
  else begin
    Last := LastForm;
    Rec.Prev := Last;
    Rec.Next := nil;
    if Assigned(Last) then
      Last.Next := Rec
    else
      FForms := Rec;
  end;
  Form.FParentSet := Self;
end;

procedure TGUIFormsSet.RemoveForm(Form: TGUIForm);
var
  Rec: PFormRec;
begin
  Rec := FindForm(Form);
  if not Assigned(Rec) then Exit;
  Visible[Rec.Name] := false;
  Rec.Form.FParentSet := nil;
  if Assigned(Rec.Prev) then
    Rec.Prev.Next := Rec.Next;
  if Assigned(Rec.Next) then
    Rec.Next.Prev := Rec.Prev;
  if Rec = FForms then
    FForms := Rec.Next;
  Dispose(Rec);
end;

procedure TGUIFormsSet.IterateForms(Handler: TOnForm);
var
  Cur: PFormRec;
begin
  Cur := FForms;
  while Assigned(Cur) do
  begin
    Handler(Cur.Form);
    Cur := Cur.Next;
  end;
end;

function TGUIFormsSet.FindForm(const Name: string): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    if Result.Name = Name then
      Break
    else
      Result := Result.Next;
end;

function TGUIFormsSet.FindForm(Form: TGUIForm): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    if Result.Form = Form then
      Break
    else
      Result := Result.Next;
end;

function TGUIFormsSet.FormAt(X, Y: Integer): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    with Result.Form do
      if Result.Visible and PointInRect(Render2D.MapCursor(Point(X, Y)), Rect(Left, Top, Left + Width, Top + Height)) then
        Break
      else
        Result := Result.Next;
end;

function TGUIFormsSet.LastForm: PFormRec;
begin
  Result := FForms;
  while Assigned(Result) and Assigned(Result.Next) do
    Result := Result.Next;
end;

function TGUIFormsSet.FormName(Form: TGUIForm): string;
var
  Rec: PFormRec;
begin
  Result := '';
  Rec := FindForm(Form);
  if Assigned(Rec) then
    Result := Rec.Name;
end;

procedure TGUIFormsSet.Pop(Form: PFormRec);
begin
  if not Assigned(Form) or not Assigned(Form.Prev) then Exit;
  Form.Prev.Next := Form.Next;
  if Assigned(Form.Next) then
    Form.Next.Prev := Form.Prev;
  FForms.Prev := Form;
  Form.Prev := nil;
  Form.Next := FForms;
  FForms := Form;
end;

function TGUIFormsSet.GetForm(const Name: string): TGUIForm;
var
  Form: PFormRec;
begin
  Result := nil;
  Form := nil;
  if Assigned(Self) then
    Form := FindForm(Name);
  if Assigned(Form) then
    Result := Form.Form;
end;

function TGUIFormsSet.GetVisible(const Name: string): Boolean;
var
  Form: PFormRec;
begin
  Result := false;
  Form := FindForm(Name);
  if Assigned(Form) then
    Result := Form.Visible;
end;

procedure TGUIFormsSet.SetVisible(const Name: string; const Value: Boolean);
var
  Form, Parent: PFormRec;
begin
  Form := FindForm(Name);
  if not Assigned(Form) then Exit;
  Form.Visible := Value;
  if Form.Parent <> '' then
  begin
    Parent := FindForm(Form.Parent);
    if not Assigned(Parent) then Exit;
    Parent.Locked := Value;
    if not Value then
      Pop(Parent);
  end;
end;

{ TGUIForm }

constructor TGUIForm.Create(X, Y, Width, Height: Integer);
begin
  inherited Create;
  FX := X;
  FY := Y;
  FWidth := Width;
  FHeight := Height;
  FCustomFont := InvalidFont;
  FCaptHeight := Render2D.TextHeight(Font) + 6; //TODO: Recalc on font change
  FActive := -1;
  FLastActive := -1;
  FLast := -1;
  FTabStop := -1;
  FMovable := false;
end;

destructor TGUIForm.Destroy;
begin
  if Assigned(FParentSet) then
    FParentSet.RemoveForm(Self);
  Finalize(FButtons);
  Finalize(FRects);
end;

function TGUIForm.AddButton(Btn: TBtn): Integer;
begin
  Result := Length(FButtons);
  SetLength(FButtons, Result + 1);
  if Btn.Type_ = btCheck then
    Btn.OnClick := CheckClick;
  if Btn.Type_ = btRadio then
    Btn.OnClick := RadioClick;
  FButtons[Result] := Btn;
end;

function TGUIForm.AddLabel(const Lbl: TLbl): Integer;
begin
  Result := Length(FLabels);
  SetLength(FLabels, Result + 1);
  FLabels[Result] := Lbl;
end;

procedure TGUIForm.AddRect(const Rect: TRect);
begin
  SetLength(FRects, Length(FRects) + 1);
  FRects[High(FRects)] := Rect;
end;

procedure TGUIForm.Draw;
var
  i: Integer;
  FormRec: PFormRec;
begin
  Render2D.Enter;
  Render2D.Move(FX, FY, false);
  if Assigned(FParentSet) then
    FormRec := FParentSet.FindForm(Self)
  else
    FormRec := nil;
  DrawForm(BtnState(Assigned(FParentSet) and (FParentSet.FirstForm = FormRec), IsMoving, Assigned(FormRec) and FormRec.Locked));
  for i := 0 to High(FRects) do
    DrawRect(FRects[i]);
  for i := 0 to High(FLabels) do
    DrawLabel(FLabels[i]);
  for i := 0 to High(FButtons) do
    DrawButton(FButtons[i], BtnState(i = FActive, i = FLast, i = FTabStop));
  Render2D.Leave;
end;

procedure TGUIForm.Update;
var
  Cursor: TPoint;
begin
  if FClose then
  begin
    Self.Free;
    Exit;
  end;
  Cursor := Core.MouseCursor;
  MapCursor(Cursor);
  if IsMoving then
  begin
    FX := FX + Cursor.X - FDragPoint.X;
    FY := FY + Cursor.Y - FDragPoint.Y;
  end;
  FLastActive := FActive;
  FActive := BtnAt(Cursor);
end;

procedure TGUIForm.Close;
begin
  FClose := true;
end;

procedure TGUIForm.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Cursor: TPoint;
begin
  Cursor := Point(X, Y);
  MapCursor(Cursor);
  case Event of
    meDown:
      begin
        if Button = mbLeft then
          FLast := FActive;
        FTabStop := -1;
        if PointInRect(Cursor, Rect(0, 0, FWidth, FCaptHeight)) then
          FDragPoint := Cursor;
      end;
    meUp:
      begin
        if Button = mbLeft then
        begin
          if (FActive >= 0) and (FLast = FActive) and FButtons[FActive].Enabled and Assigned(FButtons[FActive].OnClick) then
          begin
            FLast := -1;
            FButtons[FActive].OnClick(@FButtons[FActive]);
          end
            else FLast := -1;
        end;
        FTabStop := -1;
        FDragPoint := Point(0, 0);
      end;
  end;
end;

procedure TGUIForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Event = keDown) then
  begin
    case Key of
      VK_TAB: if Core.KeyPressed[VK_SHIFT]
        then Dec(FTabStop)
        else Inc(FTabStop);
      VK_UP: Dec(FTabStop);
      VK_DOWN: Inc(FTabStop);
      VK_SHIFT, VK_SPACE: ;
      else begin
        FTabStop := -1;
        Exit;
      end;
    end;
    if FTabStop < 0 then
      FTabStop := High(FButtons);
    if FTabStop > High(FButtons) then
      FTabStop := 0;
  end
  else if (Key = VK_SPACE) and FButtons[FTabStop].Enabled and Assigned(FButtons[FTabStop].OnClick) then
  begin
    FButtons[FTabStop].OnClick(@FButtons[FTabStop]);
    Exit;
  end;
end;

procedure TGUIForm.CharEvent(C: Char);
begin

end;

procedure TGUIForm.SetColor(State: TBtnState; const ColorSet: TColorSet; Enabled: Boolean = true);
var
  Color: TColor;
begin
  if Enabled then
  begin
    Color := ColorSet.Default;
    if bsHighlighted in State then
      Color := ColorSet.Highlighted;
    if bsPushed in State then
      Color := ColorSet.Activated;
  end
    else Color := ColorSet.Disabled;
  gleColor(Color);
end;

procedure TGUIForm.DrawForm(State: TBtnState);
begin
  Render2D.LineWidth(1);
  gleColor(clFormBackground);
  Render2D.DrawRect(0, 0, FWidth, FHeight);
  SetColor(State, FormCapt, not (fsLocked in State));
  Render2D.DrawRect(0, 0, FWidth, FCaptHeight);
  gleColor(clFormBorder);
  Render2D.DrawRectBorder(0, 0, FWidth, FHeight);
  gleColor(clFormCaptText);
  Render2D.TextOut(Font, (FWidth - Render2D.TextWidth(Font, FCaption)) div 2, (FCaptHeight - Render2D.TextHeight(Font)) div 2, FCaption);
end;

procedure TGUIForm.DrawButton(const Btn: TBtn; State: TBtnState);
var
  TextX, TextY: Integer;
  Text: string;
begin
  Render2D.LineWidth(2);
  case Btn.Type_ of
    btPush:
      begin
        SetColor(State, BtnBackground, Btn.Enabled);
        Render2D.DrawRect(Btn.X, Btn.Y, Btn.Width, Btn.Height);
        SetColor(State, BtnBorder, Btn.Enabled);
        Render2D.DrawRectBorder(Btn.X, Btn.Y, Btn.Width, Btn.Height);
        TextX := Max((Btn.Width - Render2D.TextWidth(Font, Btn.Caption)) div 2, 0);
      end;
    btCheck, btRadio:
      begin
        SetColor(State, BtnBorder, Btn.Enabled);
        if Btn.Checked then
          Render2D.DrawRect(Btn.X + 3, Btn.Y + 3, Btn.Height - 6, Btn.Height - 6);
        Render2D.DrawRectBorder(Btn.X, Btn.Y, Btn.Height, Btn.Height);
        TextX := Min(Btn.Height + 5, Btn.Width);
      end;
  end;
  Text := Btn.Caption;
  TextY := (Btn.Height - Render2D.TextHeight(Font)) div 2;
  while (Text <> '') and (Render2D.TextWidth(Font, Text) + TextX > Btn.Width) do
    Delete(Text, Length(Text), 1);
  SetColor(State, BtnText, Btn.Enabled);
  Render2D.TextOut(Font, Btn.X + TextX, Btn.Y + TextY, Text);
  if bsTabStop in State then
  begin
    glLineStipple(1, $F0F0);
    glEnable(GL_LINE_STIPPLE);
    gleColor(clTabStop);
    Render2D.DrawRectBorder(Btn.X, Btn.Y, Btn.Width, Btn.Height);
    glDisable(GL_LINE_STIPPLE);
  end;
end;

procedure TGUIForm.DrawRect(const Rect: TRect);
begin
  Render2D.LineWidth(2);
  gleColor(BtnBorder.Default);
  Render2D.DrawRectBorder(Rect);
end;

procedure TGUIForm.DrawLabel(const Lbl: TLbl);
var
  Text: string;
  TextX: Integer;
begin
  with Lbl do
  begin
    if Color <> 0
      then gleColor(Color)
      else gleColor(clText);
    Text := Caption;
    while (Text <> '') and (Render2D.TextWidth(Font, Text) > Width) do
      Delete(Text, Length(Text), 1);
    case Align of
      laLeft: TextX := X;
      laCenter: TextX := X + (Width - Render2D.TextWidth(Font, Text)) div 2;
      laRight: TextX := X + Width - Render2D.TextWidth(Font, Text);
    end;
    Render2D.TextOut(Font, TextX, Y, Text);
  end;
end;

function TGUIForm.BtnState(Highlighted, Pushed, TabStop: Boolean): TBtnState;
begin
  Result := [];
  if Highlighted then
    Result := Result + [bsHighlighted];
  if Pushed then
    Result := Result + [bsPushed];
  if TabStop then
    Result := Result + [bsTabStop];
end;

function TGUIForm.IsMoving: Boolean;
begin
  Result := FMovable and Core.KeyPressed[VK_LBUTTON] and (FDragPoint.X <> 0) and (FDragPoint.Y <> 0);
end;

function TGUIForm.GetName: string;
begin
  if Assigned(FParentSet) then
    Result := FParentSet.FormName(Self)
  else
    Result := ClassName;
end;

function TGUIForm.Font: Cardinal;
begin
  if FCustomFont <> InvalidFont then
    Result := FCustomFont
  else
    Result := GUIFont;
end;

function TGUIForm.GetButton(Index: Integer): PBtn;
begin
  Result := nil;
  if (Index < 0) or (Index > High(FButtons)) then Exit;
  Result := @FButtons[Index];
end;

function TGUIForm.GetLabel(Index: Integer): PLbl;
begin
  Result := nil;
  if (Index < 0) or (Index > High(FLabels)) then Exit;
  Result := @FLabels[Index];
end;

procedure TGUIForm.CheckClick(Btn: PBtn);
begin
  //Due to optimizer bug in Delphi 7.1
  if Btn^.Checked
    then Btn^.Checked := false
    else Btn^.Checked := true;
end;

procedure TGUIForm.RadioClick(Btn: PBtn);
var
  i: Integer;
begin
  for i := 0 to High(FButtons) do
    if (FButtons[i].Type_ = btRadio) and (FButtons[i].Group = Btn^.Group) then
      FButtons[i].Checked := false;
  Btn^.Checked := true;
end;

procedure TGUIForm.MapCursor(var Cursor: TPoint);
begin
  Cursor := Render2D.MapCursor(Cursor);
  Dec(Cursor.X, FX);
  Dec(Cursor.Y, FY);
end;

function TGUIForm.BtnAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FButtons) do
    with FButtons[i] do
      if PointInRect(Point, Rect(X, Y, X + Width, Y + Height)) then
      begin
        Result := i;
        Exit;
      end;
end;

{ Functions }

function CreateSelect(Form: TGUIForm; X, Y, Width, Height: Integer; OnChange: TGUIOnClick; const PrevCaption, NextCaption: string): Integer;
var
  Lbl: TLbl;
  Btn: TBtn;
begin
  Btn.Type_ := btPush;
  Btn.X := X;
  Btn.Y := Y;
  Btn.Width := Height;
  Btn.Height := Height;
  Btn.OnClick := OnChange;
  Btn.Enabled := true;
  Btn.Caption := PrevCaption;
  Btn.Tag := -1;
  Form.AddButton(Btn);
  Btn.X := X + Width - Height;
  Btn.Caption := NextCaption;
  Btn.Tag := 1;
  Form.AddButton(Btn);
  Lbl.X := X + Height;
  Lbl.Y := Y + (Height - Render2D.TextHeight(Form.Font)) div 2;
  Lbl.Width := Width - 2 * Height;
  Lbl.Align := laCenter;
  Lbl.Color := 0;
  Result := Form.AddLabel(Lbl);
  Form.AddRect(Rect(X + Height, Y, X + Width - Height, Y + Height));
end;

function CreateMenu(Form: TGUIForm; X, Y, BtnWidth, BtnHeight, BtnSpacing: Integer; Items: array of TMenuItem): TMenuItems;
var
  Btn: TBtn;
  i: Integer;
begin
  SetLength(Result, Length(Items));
  Btn.Type_ := btPush;
  Btn.X := X;
  Btn.Width := BtnWidth;
  Btn.Height := BtnHeight;
  Btn.Enabled := true;
  for i := 0 to High(Items) do
  begin
    Btn.Y := Y + i * (BtnHeight + BtnSpacing);
    Btn.Caption := Items[i].Caption;
    Btn.Tag := Items[i].Tag;
    Btn.OnClick := Items[i].OnClick;
    Result[i] := Form.AddButton(Btn);
  end;
end;

procedure SetGUIFont(const Name: string; Size: Integer; Bold: Boolean);
begin
  GUIFont := Render2D.CreateFont(Name, Size, Bold);
end;

end.
