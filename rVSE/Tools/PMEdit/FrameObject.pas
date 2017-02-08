unit FrameObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, PMBuild, Spin, ExtCtrls, FPSpin;

type
  TObjectFrame=class(TFrame)
    GroupObject: TGroupBox;
    TransformGroup: TGroupBox;
    MaterialCombo: TComboBox;
    IDEdit: TEdit;
    VisibleCheck: TCheckBox;
    ObjectShowAll: TButton;
    ObjectHideAll: TButton;
    TransX: TFPSpinEdit;
    NameEdit: TEdit;
    NameLabel: TLabel;
    IDLabel: TLabel;
    MaterialLabel: TLabel;
    TranslateLabel: TLabel;
    TransY: TFPSpinEdit;
    TransZ: TFPSpinEdit;
    RotYaw: TFPSpinEdit;
    RotPitch: TFPSpinEdit;
    RotRoll: TFPSpinEdit;
    SclX: TFPSpinEdit;
    SclY: TFPSpinEdit;
    SclZ: TFPSpinEdit;
    RotateLabel: TLabel;
    ScaleLabel: TLabel;
    function Fill(Obj: TObject): Boolean;
    procedure Save;
    procedure Clear;
    procedure IDEditExit(Sender: TObject);
    procedure IDEditKeyPress(Sender: TObject; var Key: Char);
    procedure MaterialComboChange(Sender: TObject);
    procedure NameEditExit(Sender: TObject);
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
    procedure ObjectHideAllClick(Sender: TObject);
    procedure ObjectShowAllClick(Sender: TObject);
    procedure TransformChange(Sender: TObject);
    procedure VisibleCheckClick(Sender: TObject);
  private
    FObject: TPMBObject;
    procedure RefreshID;
  public

  end;

implementation

uses
  FormMain;

{$R *.dfm}

function TObjectFrame.Fill(Obj: TObject): Boolean;
var
  i: Integer;
begin
  Result:=Obj is TPMBObject;
  if Result then
  begin
    FObject:=Obj as TPMBObject;
    FObject.Selected:=true;
    MaterialCombo.Items.Clear;
    for i:=0 to MainForm.Model.MaterialsCount-1 do
    begin
      MaterialCombo.Items.Add(IntToStr(MainForm.Model.Materials[i].ID));
      if FObject.Material=MainForm.Model.Materials[i] then MaterialCombo.ItemIndex:=i;
    end;
    VisibleCheck.Checked:=FObject.Visible;
    with FObject.Transform do
    begin
      TransX.ValueAsFloat:=TranslateX;
      TransY.ValueAsFloat:=TranslateY;
      TransZ.ValueAsFloat:=TranslateZ;
      RotYaw.ValueAsFloat:=Yaw*RadToBDeg;
      RotPitch.ValueAsFloat:=Pitch*RadToBDeg;
      RotRoll.ValueAsFloat:=Roll*RadToBDeg;
      SclX.ValueAsFloat:=ScaleX;
      SclY.ValueAsFloat:=ScaleY;
      SclZ.ValueAsFloat:=ScaleZ;
    end;
    RefreshID;
  end
    else FObject:=nil;
end;

procedure TObjectFrame.Save;
begin
  if not Assigned(FObject) then Exit;
  FObject.ID:=NameEdit.Text;
end;

procedure TObjectFrame.Clear;
begin
  FObject:=nil;
  NameEdit.Text:='';
  IDEdit.Text:='';
  MaterialCombo.Items.Clear;
end;

procedure TObjectFrame.IDEditExit(Sender: TObject);
var
  ID: Cardinal;
  Code: Integer;
begin
  if not Assigned(FObject) then Exit;
  Val(IDEdit.Text, ID, Code);
  if Code=0 then FObject.IID:=ID;
  RefreshID;
end;

procedure TObjectFrame.IDEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=Char(VK_RETURN) then IDEditExit(Sender);
end;

procedure TObjectFrame.MaterialComboChange(Sender: TObject);
begin
  if not Assigned(FObject) then Exit;
  if MaterialCombo.Text<>'' then FObject.Material:=MainForm.Model.FindMaterial(StrToInt(MaterialCombo.Items[MaterialCombo.ItemIndex]));
end;

procedure TObjectFrame.NameEditExit(Sender: TObject);
begin
  if not Assigned(FObject) then Exit;
  FObject.ID:=NameEdit.Text;
  RefreshID;
end;

procedure TObjectFrame.NameEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=Char(VK_RETURN) then NameEditExit(Sender);
end;

procedure TObjectFrame.ObjectHideAllClick(Sender: TObject);
begin
  if not Assigned(FObject) then Exit;
  FObject.SetVisibility(false);
  VisibleCheck.Checked:=FObject.Visible;
end;

procedure TObjectFrame.ObjectShowAllClick(Sender: TObject);
begin
  if not Assigned(FObject) then Exit;
  FObject.SetVisibility(true);
  VisibleCheck.Checked:=FObject.Visible;
end;

procedure TObjectFrame.TransformChange(Sender: TObject);
begin
  if not Assigned(FObject) or ((Sender as TFPSpinEdit).Text='') or
    ((Sender as TFPSpinEdit).Text='-') or ((Sender as TFPSpinEdit).Text=DecimalSeparator)
      then Exit;
  with FObject.Transform, Sender as TFPSpinEdit do
  begin
    if Sender=TransX then TranslateX:=ValueAsFloat;
    if Sender=TransY then TranslateY:=ValueAsFloat;
    if Sender=TransZ then TranslateZ:=ValueAsFloat;
    if Sender=RotYaw then Yaw:=ValueAsFloat*BDegToRad;
    if Sender=RotPitch then Pitch:=ValueAsFloat*BDegToRad;
    if Sender=RotRoll then Roll:=ValueAsFloat*BDegToRad;
    if Sender=SclX then ScaleX:=ValueAsFloat;
    if Sender=SclY then ScaleY:=ValueAsFloat;
    if Sender=SclZ then ScaleZ:=ValueAsFloat;
  end;
end;

procedure TObjectFrame.VisibleCheckClick(Sender: TObject);
begin
  if not Assigned(FObject) then Exit;
  FObject.Visible:=VisibleCheck.Checked;
end;

procedure TObjectFrame.RefreshID;
begin
  NameEdit.Text:=FObject.ID;
  IDEdit.Text:='$'+IntToHex(FObject.IID, 8);
  MainForm.TreeRefreshCurrentItem;
end;

end.

