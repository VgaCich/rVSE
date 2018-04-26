unit VSEGUI;

interface

uses
  Windows, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore, VSERender2D;

type
  TGUIForm = class;
  TFormManager = class;
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
  TBtnStates = (bsHighlighted, bsPushed, bsTabStop); //Button state - highlighted (mouse over), pushed, selected from keyboard
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
  TGUIForm = class //GUI Form
  private
    FActive, FLastActive, FLast, FTabStop: Integer; //internally used
    FMovable: Boolean; //internally used
    FDragPoint: TPoint; //internally used
    FButtons: array of TBtn; //internally used
    FRects: array of TRect; //internally used
    FLabels: array of TLbl; //internally used
    function IsMoving: Boolean;
  protected
    FCaption: string; //Form caption
    FCaptHeight: Integer; //Form caption height
    FX, FY, FWidth, FHeight: Integer; //Form position and size
    FFont: Cardinal; //Form font
    FManager: TFormManager; //Form manager
    function  GetButton(Index: Integer): PBtn; //Get button by index
    function  GetLabel(Index: Integer): PLbl; //Get label by index
    procedure CheckClick(Btn: PBtn); //CheckBox click handler
    procedure RadioClick(Btn: PBtn); //RadioButton click handler
    procedure MapCursor(var Cursor: TPoint); //Map cursor to form coordinates
    function  BtnAt(Point: TPoint): Integer; //Get button at specified coordinates
    procedure SetColor(State: TBtnState; const ColorSet: TColorSet; Enabled: Boolean = true); //Set color from color set
    procedure DrawForm; dynamic; //Override for custom form drawing
    procedure DrawButton(const Btn: TBtn; State: TBtnState); dynamic; //Override for custom buttons drawing
    procedure DrawRect(const Rect: TRect); dynamic; //Override for custom rectangles drawing
    procedure DrawLabel(const Lbl: TLbl); dynamic; //Override for custom labels drawing
  public
    constructor Create(X, Y, Width, Height: Integer; Font: Cardinal); //Creates form; VertScr*: virtual screen resolution (all dimensions defined in virtual screen coordinates); X, Y, Width, Height: form bounds; Font: form font
    destructor Destroy; override;
    function  AddButton(Btn: TBtn): Integer; //Add button, returns button index
    function  AddLabel(const Lbl: TLbl): Integer; //Add label, returns label index
    procedure AddRect(const Rect: TRect); //Add rectangle (visual frame)
    procedure Draw; //Draw form
    procedure Update; //dynamic; //Update form
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
    property Font: Cardinal read FFont write FFont; //Font
    property Movable: Boolean read FMovable write FMovable; //Form can be dragged by caption
  end;
  TFormManager = class
  private
    FForms: PFormRec;
    FCapturedMouse: TGUIForm;
    function FindForm(const Name: string): PFormRec; overload;
    function FindForm(Form: TGUIForm): PFormRec; overload;
    function FormAt(X, Y: Integer): PFormRec;
    function LastForm: PFormRec;
    function GetForm(const Name: string): TGUIForm;
    function GetVisible(const Name: string): Boolean;
    procedure SetVisible(const Name: string; const Value: Boolean);
  public
    destructor Destroy; override;
    procedure Draw; //Draw forms
    procedure Update; //Update forms
    function MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean; //Process mouse event; returns true if mouse is over form
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); //Process key event
    procedure CharEvent(C: Char); //Process char event
    procedure AddForm(const Name: string; Form: TGUIForm; const Parent: string = ''); //Add form; Parent: name of parent form; Parentless forms visible by default
    procedure RemoveForm(Form: TGUIForm); //Remove form
    procedure IterateForms(Handler: TOnForm); //Call Handler for every form
    procedure Show(const Name: string); //Show and pop form
    procedure Hide(const Name: string); //Hide form
    procedure Pop(const Name: string); //Pop form to front
    function FormName(Form: TGUIForm): string; //Name of form
    function Top: TGUIForm; //Topmost form
    property Forms[const Name: string]: TGUIForm read GetForm; default; //Forms
    property Visible[const Name: string]: Boolean read GetVisible write SetVisible; //Form visibility
  end;

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
  clFormBackground: TColor = $FF80FF80;
  clFormBorder: TColor = $FF00FF00;
  clFormCapt: TColor = $FF80E600;
  clFormCaptHl: TColor = $FF73CF00;
  clFormCaptText: TColor = $FF00FFFF;
  clText: TColor = $FF00B200;
  clTabStop: TColor = $FF000080;
{$ELSEIF Defined(CLRSCM_WINMO) or Defined(CLRSCM_WIN95)}
  BtnBackground: TColorSet = (Default: $FFC0C0C0; Highlighted: $FFC8C8C8; Activated: $FFA0A0A0; Disabled: $FFC0C0C0);
  BtnBorder: TColorSet = (Default: $FF404040; Highlighted: $FF404040; Activated: $FF404040; Disabled: $FF808080);
  BtnText: TColorSet = (Default: $FF000000; Highlighted: $FF000000; Activated: $FF000000; Disabled: $FF808080);
  clFormBackground: TColor = {$IFDEF CLRSCM_WINMO}$FFFFFFFF{$ELSE}$FFC0C0C0{$ENDIF};
  clFormBorder: TColor = $FF404040;
  clFormCapt: TColor = $FF800000;
  clFormCaptHl: TColor = $FF880000;
  clFormCaptText: TColor = $FFFFFFFF;
  clText: TColor = $FF000000;
  clTabStop: TColor = $FF000000;
{$IFEND}

implementation

uses
  VSECollisionCheck;

{TGUIForm}

constructor TGUIForm.Create(X, Y, Width, Height: Integer; Font: Cardinal);
begin
  inherited Create;
  FX := X;
  FY := Y;
  FWidth := Width;
  FHeight := Height;
  FFont := Font;
  FCaptHeight := Render2D.TextHeight(FFont) + 6;
  FActive := -1;
  FLastActive := -1;
  FLast := -1;
  FTabStop := -1;
  FMovable := false;
end;

destructor TGUIForm.Destroy;
begin
  if Assigned(FManager) then
    FManager.RemoveForm(Self);
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

  function BtnState(Index: Integer): TBtnState;
  begin
    Result := [];
    if Index = FActive then
      Result := Result + [bsHighlighted];
    if Index = FLast then
      Result := Result + [bsPushed];
    if Index = FTabStop then
      Result := Result + [bsTabStop];
  end;

var
  i: Integer;
begin
  Render2D.Enter;
  glPushMatrix;
  glTranslate(FX, FY, 0);
  DrawForm;
  for i := 0 to High(FRects) do
    DrawRect(FRects[i]);
  for i := 0 to High(FLabels) do
    DrawLabel(FLabels[i]);
  for i := 0 to High(FButtons) do
    DrawButton(FButtons[i], BtnState(i));
  glPopMatrix;
  Render2D.Leave;
end;

procedure TGUIForm.Update;
var
  Cursor: TPoint;
begin
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

procedure TGUIForm.DrawForm;
begin
  Render2D.LineWidth(1);
  gleColor(clFormBackground);
  Render2D.DrawRect(0, 0, FWidth, FHeight);
  if IsMoving or (Assigned(FManager) and (FManager.Top = Self))
    then gleColor(clFormCaptHl)
    else gleColor(clFormCapt);
  Render2D.DrawRect(0, 0, FWidth, FCaptHeight);
  gleColor(clFormBorder);
  Render2D.DrawRectBorder(0, 0, FWidth, FHeight);
  gleColor(clFormCaptText);
  Render2D.TextOut(FFont, (FWidth - Render2D.TextWidth(FFont, FCaption)) div 2, (FCaptHeight - Render2D.TextHeight(FFont)) div 2, FCaption);
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
        TextX := Max((Btn.Width - Render2D.TextWidth(FFont, Btn.Caption)) div 2, 0);
        TextY := (Btn.Height - Render2D.TextHeight(FFont)) div 2;
      end;
    btCheck, btRadio:
      begin
        SetColor(State, BtnBorder, Btn.Enabled);
        if Btn.Checked then
          Render2D.DrawRect(Btn.X + 3, Btn.Y + 3, Btn.Height - 6, Btn.Height - 6);
        Render2D.DrawRectBorder(Btn.X, Btn.Y, Btn.Height, Btn.Height);
        TextX := Min(Btn.Height + 5, Btn.Width);
        TextY := (Btn.Height - Render2D.TextHeight(FFont)) div 2;
      end;
  end;
  Text := Btn.Caption;
  while (Text <> '') and (Render2D.TextWidth(FFont, Text) + TextX > Btn.Width) do
    Delete(Text, Length(Text), 1);
  SetColor(State, BtnText, Btn.Enabled);
  Render2D.TextOut(FFont, Btn.X + TextX, Btn.Y + TextY, Text);
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
    while (Text <> '') and (Render2D.TextWidth(FFont, Text) > Width) do
      Delete(Text, Length(Text), 1);
    case Align of
      laLeft: TextX := X;
      laCenter: TextX := X + (Width - Render2D.TextWidth(FFont, Text)) div 2;
      laRight: TextX := X + Width - Render2D.TextWidth(FFont, Text);
    end;
    Render2D.TextOut(FFont, TextX, Y, Text);
  end;
end;

function TGUIForm.IsMoving: Boolean;
begin
  Result := FMovable and Core.KeyPressed[VK_LBUTTON] and (FDragPoint.X <> 0) and (FDragPoint.Y <> 0);
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

{TFormManager}

destructor TFormManager.Destroy;
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

procedure TFormManager.Draw;
var
  Form: PFormRec;
begin
  Form := LastForm;
  while Assigned(Form) do
  begin
    if Form.Visible then
      Form.Form.Draw;
    Form := Form.Prev;
  end;
end;

procedure TFormManager.Update;
var
  Form: PFormRec;
begin
  Form := FForms;
  while Assigned(Form) do
  begin
    Form.Form.Update;
    Form := Form.Next;
  end;
end;

procedure TFormManager.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if Assigned(FForms) and not FForms.Locked then
    FForms.Form.KeyEvent(Key, Event);
end;

function TFormManager.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean;
var
  Form: PFormRec;
begin
  Result := Assigned(FCapturedMouse);
  if Assigned(FCapturedMouse) then
  begin
    FCapturedMouse.MouseEvent(Button, Event, X, Y);
    if Event = meUp then
      FCapturedMouse := nil;
    Exit;
  end;
  with Render2D.MapCursor(Point(X, Y)) do
    Form := FormAt(X, Y);
  if Assigned(Form) and not Form.Locked then
  begin
    if Event = meDown then
    begin
      FCapturedMouse := Form.Form;
      Pop(Form.Name);
    end;
    Form.Form.MouseEvent(Button, Event, X, Y);
    Result := true;
  end;
end;

procedure TFormManager.CharEvent(C: Char);
begin
  if Assigned(FForms) and not FForms.Locked then
    FForms.Form.CharEvent(C);
end;

procedure TFormManager.AddForm(const Name: string; Form: TGUIForm; const Parent: string);
var
  Rec, Last: PFormRec;
begin
  if Assigned(FindForm(Name)) then
    raise Exception.Create('FormManager: duplicate form name "' + Name + '"');
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
    FForms := Rec;
  end
  else begin
    Last := LastForm;
    Rec.Prev := Last;
    Rec.Next := nil;
    Last.Next := Rec;
  end;
  Form.FManager := Self;
end;

procedure TFormManager.RemoveForm(Form: TGUIForm);
var
  Rec: PFormRec;
begin
  Rec := FindForm(Form);
  if not Assigned(Rec) then Exit;
  Rec.Form.FManager := nil;
  if Assigned(Rec.Prev) then
    Rec.Prev.Next := Rec.Next;
  if Assigned(Rec.Next) then
    Rec.Next.Prev := Rec.Prev;
  if Rec = FForms then
    FForms := Rec.Next;
  Dispose(Rec);
end;

procedure TFormManager.IterateForms(Handler: TOnForm);
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

procedure TFormManager.Show(const Name: string);
begin
  Visible[Name] := true;
  Pop(Name);
end;

procedure TFormManager.Hide(const Name: string);
begin
  Visible[Name] := false;
end;

procedure TFormManager.Pop(const Name: string);
var
  Form: PFormRec;
begin
  Form := FindForm(Name);
  if not Assigned(Form) or not Assigned(Form.Prev) then Exit;
  Form.Prev.Next := Form.Next;
  if Assigned(Form.Next) then
    Form.Next.Prev := Form.Prev;
  FForms.Prev := Form;
  Form.Prev := nil;
  Form.Next := FForms;
  FForms := Form;
end;

function TFormManager.FormName(Form: TGUIForm): string;
var
  Rec: PFormRec;
begin
  Result := '';
  Rec := FindForm(Form);
  if Assigned(Rec) then
    Result := Rec.Name;
end;

function TFormManager.Top: TGUIForm;
begin
  Result := nil;
  if Assigned(FForms) then
    Result := FForms.Form;
end;

function TFormManager.FindForm(const Name: string): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    if Result.Name = Name then
      Break
    else
      Result := Result.Next;
end;

function TFormManager.FindForm(Form: TGUIForm): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    if Result.Form = Form then
      Break
    else
      Result := Result.Next;
end;

function TFormManager.FormAt(X, Y: Integer): PFormRec;
begin
  Result := FForms;
  while Assigned(Result) do
    with Result.Form do
      if PointInRect(Point(X, Y), Rect(Left, Top, Left + Width, Top + Height)) then
        Break
      else
        Result := Result.Next;
end;

function TFormManager.LastForm: PFormRec;
begin
  Result := FForms;
  while Assigned(Result) and Assigned(Result.Next) do
    Result := Result.Next;
end;

function TFormManager.GetForm(const Name: string): TGUIForm;
var
  Form: PFormRec;
begin
  Result := nil;
  Form := FindForm(Name);
  if Assigned(Form) then
    Result := Form.Form;
end;

function TFormManager.GetVisible(const Name: string): Boolean;
var
  Form: PFormRec;
begin
  Result := false;
  Form := FindForm(Name);
  if Assigned(Form) then
    Result := Form.Visible;
end;

procedure TFormManager.SetVisible(const Name: string; const Value: Boolean);
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
      Pop(Parent.Name);
  end;
end;

{Functions}

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

end.
