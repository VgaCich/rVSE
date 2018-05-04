unit VSEFormManager;

interface

uses
  Windows, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSECore, VSEGUI;

type
  TFormManager = class(TModule)
  private
    FFormsSet: TGUIFormsSet;
    FCapturedMouse: TGUIForm;
    {$IFDEF VSE_CONSOLE}
    function UIColorHandler(Sender: TObject; Args: array of const): Boolean;
    function UIFontHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
    function GetForm(const Name: string): TGUIForm;
    function GetVisible(const Name: string): Boolean;
    procedure SetVisible(const Name: string; const Value: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Name: string; override;
    procedure Draw; override;
    procedure Update; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
    procedure Show(const Name: string); //Show and pop form
    procedure Hide(const Name: string); //Hide form
    procedure Pop(const Name: string); //Pop form to front
    function MouseBusy(X, Y: Integer): Boolean; //Returns true if mouse processed by FormManager
    function Top: TGUIForm; //Topmost form
    property FormsSet: TGUIFormsSet read FFormsSet write FFormsSet; //Current forms set
    property Forms[const Name: string]: TGUIForm read GetForm; default; //Forms
    property Visible[const Name: string]: Boolean read GetVisible write SetVisible; //Form visibility
  end;

var
  FormManager: TFormManager;

implementation

uses
  VSECollisionCheck{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

const
  ColorNames = 'btnbg:btnbd:btntxt:frmbg:frmbd:frmcpt:frmcphl:frmcptxt:text:tabstop';

{ TFormManager }

constructor TFormManager.Create;
begin
  inherited;
  FormManager := Self;
  SetGUIFont;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['uicolor ?clr=e' + ColorNames + ' ?def=i ?hl=i ?act=i ?dis=i'] := UIColorHandler;
  Console.OnCommand['uifont name=s ?size=i8:24 ?weight=en:b'] := UIFontHandler;
  {$ENDIF}
end;

destructor TFormManager.Destroy;
begin
  FormManager := nil;
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

procedure TFormManager.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Form: PFormRec;
begin
  if Assigned(FCapturedMouse) then
  begin
    FCapturedMouse.MouseEvent(Button, Event, X, Y);
    if Event = meUp then
      FCapturedMouse := nil;
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
      Form.MouseEvent(Button, Event, X, Y);
    end;
end;

procedure TFormManager.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if not Assigned(FFormsSet) or not Assigned(FFormsSet.FirstForm) then Exit;
  with FFormsSet.FirstForm^ do
    if Visible and not Locked then
      Form.KeyEvent(Key, Event);
end;

procedure TFormManager.CharEvent(C: Char);
begin
  if not Assigned(FFormsSet) or not Assigned(FFormsSet.FirstForm) then Exit;
  with FFormsSet.FirstForm^ do
    if Visible and not Locked then
      Form.CharEvent(C);
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

function TFormManager.MouseBusy(X, Y: Integer): Boolean;
var
  Form: PFormRec;
begin
  if Assigned(FFormsSet) then
    Form := FFormsSet.FormAt(X, Y);
  Result := Assigned(FCapturedMouse) or Assigned(Form);
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
  Colors: array[0..9] of TColorRec = (
    (IsColorSet: true; ColorSet: @BtnBackground),
    (IsColorSet: true; ColorSet: @BtnBorder),
    (IsColorSet: true; ColorSet: @BtnText),
    (IsColorSet: false; Color: @clFormBackground),
    (IsColorSet: false; Color: @clFormBorder),
    (IsColorSet: false; Color: @clFormCapt),
    (IsColorSet: false; Color: @clFormCaptHl),
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
