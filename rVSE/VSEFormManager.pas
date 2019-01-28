unit VSEFormManager;

interface

uses
  Windows, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore, VSEGUI;

type
  TFormAlignment = (faLeft, faCenter, faRight, faTop, faMiddle, faBottom);
  TFormAlignmentSet = set of TFormAlignment;
  TAlignedForm = class(TGUIForm) //Alignable form
  protected
    FAlignment: TFormAlignmentSet;
    procedure SetAlignment(Value: TFormAlignmentSet);
  public
    constructor Create(X, Y, Width, Height: Integer);
    procedure Align; //Align form
    property Alignment: TFormAlignmentSet read FAlignment write SetAlignment; //Desired alignment
  end;
  TFormManager = class(TModule)
  private
    FFormsSet: TGUIFormsSet;
    FCapturedMouse: TGUIForm;
    FAlignedSets: TList;
    {$IFDEF VSE_CONSOLE}
    function UIColorHandler(Sender: TObject; Args: array of const): Boolean;
    function UIFontHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
    function GetForm(const Name: string): TGUIForm;
    procedure SetFormsSet(Value: TGUIFormsSet);
    function GetVisible(const Name: string): Boolean;
    procedure SetVisible(const Name: string; const Value: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
    procedure Draw; override;
    procedure Update; override;
    function MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean; override;
    function KeyEvent(Key: Integer; Event: TKeyEvent): Boolean; override;
    function CharEvent(C: Char): Boolean; override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
    procedure Show(const Name: string); //Show and pop form
    procedure Hide(const Name: string); //Hide form
    procedure Pop(const Name: string); //Pop form to front
    procedure AlignForms; //Align alignable forms
    function MouseOnForm(X, Y: Integer): Boolean; //Returns true if mouse on form or captured
    function Top: TGUIForm; //Topmost form
    property FormsSet: TGUIFormsSet read FFormsSet write SetFormsSet; //Current forms set
    property Forms[const Name: string]: TGUIForm read GetForm; default; //Forms
    property Visible[const Name: string]: Boolean read GetVisible write SetVisible; //Form visibility
  end;

var
  FormManager: TFormManager;

implementation

uses
  VSECollisionCheck, VSERender2D{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

const
  ColorNames = 'btnbg:btnbd:btntxt:frmcpt:frmbg:frmbd:frmcptxt:text:tabstop';

{ TAlignedForm }

constructor TAlignedForm.Create(X, Y, Width, Height: Integer);
begin
  inherited;
  Movable := true;
  FAlignment := [];
end;

procedure TAlignedForm.Align;
begin
  if faLeft in FAlignment then
    Left := Round(Render2D.VSBounds.Left)
  else if faCenter in FAlignment then
    Left := (Render2D.VSWidth - Width) div 2
  else if faRight in FAlignment then
    Left := Round(Render2D.VSBounds.Right) - Width;
  if faTop in FAlignment then
    Top := Round(Render2D.VSBounds.Top)
  else if faMiddle in FAlignment then
    Top := (Render2D.VSHeight - Height) div 2
  else if faBottom in FAlignment then
    Top := Round(Render2D.VSBounds.Bottom) - Height;
end;

procedure TAlignedForm.SetAlignment(Value: TFormAlignmentSet);
begin
  FAlignment := Value;
  Align;
end;

{ TFormManager }

constructor TFormManager.Create;
begin
  inherited;
  FormManager := Self;
  FAlignedSets := TList.Create;
  SetGUIFont;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['uicolor ?clr=e' + ColorNames + ' ?def=i ?hl=i ?act=i ?dis=i'] := UIColorHandler;
  Console.OnCommand['uifont name=s ?size=i8:24 ?weight=en:b'] := UIFontHandler;
  {$ENDIF}
end;

destructor TFormManager.Destroy;
begin
  FormManager := nil;
  FAN(FAlignedSets);
  inherited;
end;

class function TFormManager.Name: string;
begin
  Result := 'FormManager';
end;

procedure TFormManager.Draw;
var
  Form: PFormRec;
begin
  if not Assigned(FFormsSet) then Exit;
  Form := FFormsSet.LastForm;
  while Assigned(Form) do
  begin
    if Form.Visible then
      Form.Form.Draw;
    Form := Form.Prev;
  end;
end;

procedure TFormManager.Update;
var
  Form, NextForm: PFormRec;
begin
  if not Assigned(FFormsSet) then Exit;
  Form := FFormsSet.FirstForm;
  while Assigned(Form) do
  begin
    NextForm := Form.Next;
    if Form.Visible then
      Form.Form.Update;
    Form := NextForm;
  end;
end;

function TFormManager.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer): Boolean;
var
  Form: PFormRec;
begin
  Result := false;
  if Core.MouseCapture then Exit;
  if Assigned(FCapturedMouse) then
  begin
    FCapturedMouse.MouseEvent(Button, Event, X, Y);
    if Event = meUp then
      FCapturedMouse := nil;
    Result := true;
    Exit;
  end;
  if not Assigned(FFormsSet) then Exit;
  Form := FFormsSet.FormAt(X, Y);
  if Assigned(Form) and not Form.Locked then
    with Form^ do
    begin
      if Event = meDown then
      begin
        FCapturedMouse := Form;
        Pop(Name);
      end;
      Result := Form.MouseEvent(Button, Event, X, Y);
    end;
end;

function TFormManager.KeyEvent(Key: Integer; Event: TKeyEvent): Boolean;
begin
  Result := false;
  if not Assigned(FFormsSet) or not Assigned(FFormsSet.FirstForm) then Exit;
  if (Event = keDown) and (Key = VK_TAB) and Core.KeyPressed[VK_CONTROL] then
  begin
    FFormsSet.Pop(FFormsSet.LastForm);
    Result := true;
  end
  else
    with FFormsSet.FirstForm^ do
      if Visible and not Locked then
        Result := Form.KeyEvent(Key, Event);
end;

function TFormManager.CharEvent(C: Char): Boolean;
begin
  Result := false;
  if not Assigned(FFormsSet) or not Assigned(FFormsSet.FirstForm) then Exit;
  with FFormsSet.FirstForm^ do
    if Visible and not Locked then
      Result := Form.CharEvent(C);
end;

function TFormManager.SysNotify(Notify: TSysNotify): Boolean;
begin
  if Notify = snResolutionChanged then
  begin
    FAlignedSets.Clear;
    AlignForms;
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
begin
  if not Assigned(FFormsSet) then Exit;
  FFormsSet.Pop(FFormsSet.FindForm(Name));
end;

procedure TFormManager.AlignForms;

  procedure RealignForm(Self: TObject; Form: TGUIForm);
  begin
    if Form is TAlignedForm then
      (Form as TAlignedForm).Align;
  end;

begin
  if Assigned(FFormsSet) then
  begin
    FFormsSet.IterateForms(TOnForm(MakeMethod(@RealignForm)));
    if FAlignedSets.IndexOf(FFormsSet) < 0 then
      FAlignedSets.Add(FFormsSet);
  end;
end;

function TFormManager.MouseOnForm(X, Y: Integer): Boolean;
begin
  Result := Assigned(FCapturedMouse) or (Assigned(FFormsSet) and Assigned(FFormsSet.FormAt(X, Y)));
end;

function TFormManager.Top: TGUIForm;
begin
  Result := nil;
  if Assigned(FFormsSet) and Assigned(FFormsSet.FirstForm) then
    Result := FFormsSet.FirstForm.Form;
end;

function TFormManager.GetForm(const Name: string): TGUIForm;
begin
  Result := FFormsSet[Name];
end;

procedure TFormManager.SetFormsSet(Value: TGUIFormsSet);
begin
  FFormsSet := Value;
  if FAlignedSets.IndexOf(FFormsSet) < 0 then
    AlignForms;
end;

function TFormManager.GetVisible(const Name: string): Boolean;
begin
  Result := false;
  if Assigned(FFormsSet) then
    Result := FFormsSet.Visible[Name];
end;

procedure TFormManager.SetVisible(const Name: string; const Value: Boolean);
begin
  if Assigned(FFormsSet) then
    FFormsSet.Visible[Name] := Value;
end;

{$IFDEF VSE_CONSOLE}
type
  TColorRec = record
    Name: string;
    case IsColorSet: Boolean of
      true: (ColorSet: ^TColorSet);
      false: (Color: ^TColor);
  end;

function TFormManager.UIColorHandler(Sender: TObject; Args: array of const): Boolean;
const
  Colors: array[0..8] of TColorRec = (
    (IsColorSet: true; ColorSet: @BtnBackground),
    (IsColorSet: true; ColorSet: @BtnBorder),
    (IsColorSet: true; ColorSet: @BtnText),
    (IsColorSet: true; ColorSet: @FormCapt),
    (IsColorSet: false; Color: @clFormBackground),
    (IsColorSet: false; Color: @clFormBorder),
    (IsColorSet: false; Color: @clFormCaptText),
    (IsColorSet: false; Color: @clText),
    (IsColorSet: false; Color: @clTabStop));
begin
  Result := true;
  if Length(Args) = 1 then
    Console.WriteLn('Colors: ' + ColorNames)
  else
    with Colors[Args[1].VInteger] do
      if Length(Args) = 2 then
        if IsColorSet then
          Console.WriteLn(Format('def=$%X hl=$%X act=$%X dis=$%X', [ColorSet.Default, ColorSet.Highlighted, ColorSet.Activated, ColorSet.Disabled]))
        else
          Console.WriteLn('$' + IntToHex(Color^, 8))
      else if IsColorSet then
        if Length(Args) = 6 then
        begin
          ColorSet.Default := Args[2].VInteger;
          ColorSet.Highlighted := Args[3].VInteger;
          ColorSet.Activated := Args[4].VInteger;
          ColorSet.Disabled := Args[5].VInteger;
        end
        else begin
          Console.WriteLn('Error: 4 values needed for color set' + PostfixError);
          Result := false;
        end
      else
        Color^ := Args[2].VInteger;
end;

function TFormManager.UIFontHandler(Sender: TObject; Args: array of const): Boolean;
begin
  Result := true;
  if Length(Args) = 4 then
    SetGUIFont(string(Args[1].VAnsiString), Args[2].VInteger, Boolean(Args[3].VInteger))
  else if Length(Args) = 3 then
    SetGUIFont(string(Args[1].VAnsiString), Args[2].VInteger)
  else
    SetGUIFont(string(Args[1].VAnsiString));
end;
{$ENDIF}

initialization
  RegisterModule(TFormManager);

end.
