unit FramePrimitive;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FPSpin, PMBuild, FramePriCube, FramePriSphere,
  FramePriCone;

type
  TPrimitiveFrame=class(TFrame)
    GroupPrimitive: TGroupBox;
    TransformGroup: TGroupBox;
    TranslateLabel: TLabel;
    RotateLabel: TLabel;
    ScaleLabel: TLabel;
    TransX: TFPSpinEdit;
    TransY: TFPSpinEdit;
    TransZ: TFPSpinEdit;
    RotYaw: TFPSpinEdit;
    RotPitch: TFPSpinEdit;
    RotRoll: TFPSpinEdit;
    SclX: TFPSpinEdit;
    SclY: TFPSpinEdit;
    SclZ: TFPSpinEdit;
    VisibleCheck: TCheckBox;
    PriCubeFrame: TPriCubeFrame;
    VarProps: TPanel;
    PriSphereFrame: TPriSphereFrame;
    GenUVCheck: TCheckBox;
    InvNormalsCheck: TCheckBox;
    DrawNormalsCheck: TCheckBox;
    PriConeFrame: TPriConeFrame;
    function  Fill(Obj: TObject): Boolean;
    procedure Clear;
    procedure DrawNormalsCheckClick(Sender: TObject);
    procedure GenUVCheckClick(Sender: TObject);
    procedure InvNormalsCheckClick(Sender: TObject);
    procedure TransformChange(Sender: TObject);
    procedure VisibleCheckClick(Sender: TObject);
  private
    FCurFrame: TFrame;
    FPri: TPMBPrimitive;
    function  GetMethod(Obj: TObject; const Name: string): TMethod;
    procedure ShowFrame(AFrame: TFrame; AObject: TPMBPrimitive);
    function  FrameFill(Obj: TPMBPrimitive): Boolean;
    procedure FrameClear;
  public

  end;

implementation

uses
  FormMain;

{$R *.dfm}

function TPrimitiveFrame.Fill(Obj: TObject): Boolean;
begin
  Result:=Obj is TPMBPrimitive;
  if Result then
  begin
    FPri:=Obj as TPMBPrimitive;
    FPri.Selected:=true;
    VisibleCheck.Checked:=FPri.Visible;
    DrawNormalsCheck.Checked:=FPri.DrawNormals;
    GenUVCheck.Checked:=FPri.TexGenUV;
    InvNormalsCheck.Checked:=FPri.InvertNormals;
    with FPri.Transform do
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
    case FPri.PriType of
      PrimitiveCube: ShowFrame(PriCubeFrame, FPri);
      PrimitiveSphere: ShowFrame(PriSphereFrame, FPri);
      PrimitiveCone: ShowFrame(PriConeFrame, FPri);
      {PrimitiveTorus: ShowFrame(PriTorusFrame, FPri);
      PrimitiveTube: ShowFrame(PriTubeFrame, FPri);}
      else ShowFrame(nil, nil);
    end;
  end
    else FPri:=nil;
end;

procedure TPrimitiveFrame.Clear;
begin
  FPri:=nil;
  FrameClear;
end;

procedure TPrimitiveFrame.DrawNormalsCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.DrawNormals:=DrawNormalsCheck.Checked;
end;

procedure TPrimitiveFrame.TransformChange(Sender: TObject);
begin
  if not Assigned(FPri) or ((Sender as TFPSpinEdit).Text='') or
    ((Sender as TFPSpinEdit).Text='-') or ((Sender as TFPSpinEdit).Text=DecimalSeparator)
      then Exit;
  with FPri.Transform, Sender as TFPSpinEdit do
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

procedure TPrimitiveFrame.VisibleCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.Visible:=VisibleCheck.Checked;
end;

function TPrimitiveFrame.GetMethod(Obj: TObject; const Name: string): TMethod;
begin
  Result.Data:=Obj;
  Result.Code:=Obj.MethodAddress(Name);
end;

procedure TPrimitiveFrame.ShowFrame(AFrame: TFrame; AObject: TPMBPrimitive);
var
  i: Integer;
begin
  if AFrame=nil then FCurFrame:=nil;
  for i:=0 to ComponentCount-1 do
    if (Components[i] is TFrame) then
      if Components[i]=AFrame then
      begin
        FCurFrame:=(Components[i] as TFrame);
        FCurFrame.Visible:=true;
        FrameFill(AObject);
      end
        else (Components[i] as TFrame).Visible:=false;
end;

function TPrimitiveFrame.FrameFill(Obj: TPMBPrimitive): Boolean;
var
  Fill: TFrameFill;
begin
  Result:=false;
  if not Assigned(FCurFrame) then Exit;
  Fill:=TFrameFill(GetMethod(FCurFrame, 'Fill'));
  if Assigned(Fill) then Result:=Fill(Obj);
end;

procedure TPrimitiveFrame.FrameClear;
var
  Clear: TFrameClear;
begin
  if not Assigned(FCurFrame) then Exit;
  Clear:=TFrameClear(GetMethod(FCurFrame, 'Clear'));
  if Assigned(Clear) then Clear;
end;

procedure TPrimitiveFrame.GenUVCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.TexGenUV:=GenUVCheck.Checked;
end;

procedure TPrimitiveFrame.InvNormalsCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.InvertNormals:=InvNormalsCheck.Checked;
end;

end.

