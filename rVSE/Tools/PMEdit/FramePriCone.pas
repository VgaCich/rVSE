unit FramePriCone;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, PMBuild, StdCtrls, ComCtrls, Spin, FPSpin;

type
  TPriConeFrame = class(TFrame)
    ConeGroup: TGroupBox;
    PageControl1: TPageControl;
    GeomPage: TTabSheet;
    TexCoordsSidePage: TTabSheet;
    Slices: TSpinEdit;
    SlicesSector: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadiusT: TFPSpinEdit;
    RadiusB: TFPSpinEdit;
    SmoothCheck: TCheckBox;
    OriginLabel: TLabel;
    SizeLabel: TLabel;
    SizeV: TSpinEdit;
    SizeU: TSpinEdit;
    OrigV: TSpinEdit;
    OrigU: TSpinEdit;
    TexCoordsBasePage: TTabSheet;
    Label7: TLabel;
    UVCenterUT: TSpinEdit;
    UVCenterVT: TSpinEdit;
    UVRadiusT: TSpinEdit;
    Label8: TLabel;
    UVCenterUB: TSpinEdit;
    UVCenterVB: TSpinEdit;
    UVRadiusB: TSpinEdit;
    function Fill(Pri: TPMBPrimitive): Boolean;
    procedure Clear;
    procedure RadiusBChange(Sender: TObject);
    procedure RadiusTChange(Sender: TObject);
    procedure SlicesChange(Sender: TObject);
    procedure SlicesSectorChange(Sender: TObject);
    procedure SmoothCheckClick(Sender: TObject);
    procedure UVBaseBChange(Sender: TObject);
    procedure UVBaseTChange(Sender: TObject);
    procedure UVSideChange(Sender: TObject);
  private
    FPri: TPMBPrimitiveCone;
  end;

implementation

{$R *.dfm}

function TPriConeFrame.Fill(Pri: TPMBPrimitive): Boolean;
begin
  Result:=Pri.PriType=PrimitiveCone;
  if Result then
  begin
    FPri:=Pri as TPMBPrimitiveCone;
    SmoothCheck.Checked:=FPri.Smooth;
    Slices.Value:=FPri.Slices;
    SlicesSector.Value:=FPri.SlicesSector;
    RadiusT.Value:=FPri.RadiusT;
    RadiusB.Value:=FPri.RadiusB;
    if FPri.TexGenUV then
    begin
      OrigU.Value:=FPri.UVSide.OrigU;
      OrigV.Value:=FPri.UVSide.OrigV;
      SizeU.Value:=FPri.UVSide.SizeU;
      SizeV.Value:=FPri.UVSide.SizeV;
      UVCenterUT.Value:=FPri.UVBaseT.CenterU;
      UVCenterVT.Value:=FPri.UVBaseT.CenterV;
      UVRadiusT.Value:=FPri.UVBaseT.Radius;
      UVCenterUB.Value:=FPri.UVBaseB.CenterU;
      UVCenterVB.Value:=FPri.UVBaseB.CenterV;
      UVRadiusB.Value:=FPri.UVBaseB.Radius;
    end;
  end
    else FPri:=nil;
end;

procedure TPriConeFrame.Clear;
begin
  FPri:=nil;
end;

procedure TPriConeFrame.RadiusBChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TFPSpinEdit).Text<>'') and
    ((Sender as TFPSpinEdit).Text<>'-') or ((Sender as TFPSpinEdit).Text=DecimalSeparator)
    then FPri.RadiusB:=RadiusB.Value;
end;

procedure TPriConeFrame.RadiusTChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TFPSpinEdit).Text<>'') and
    ((Sender as TFPSpinEdit).Text<>'-') or ((Sender as TFPSpinEdit).Text=DecimalSeparator)
    then FPri.RadiusT:=RadiusT.Value;
end;

procedure TPriConeFrame.SlicesChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.Slices:=Slices.Value;
end;

procedure TPriConeFrame.SlicesSectorChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.SlicesSector:=SlicesSector.Value;
end;

procedure TPriConeFrame.SmoothCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.Smooth:=SmoothCheck.Checked;
end;

procedure TPriConeFrame.UVBaseBChange(Sender: TObject);
var
  UVBase: TPMUVCircle;
begin
  if not Assigned(FPri) or ((Sender as TSpinEdit).Text='') or
    ((Sender as TSpinEdit).Text='-') then Exit;
  UVBase:=FPri.UVBaseB;
  if Sender=UVCenterUB then UVBase.CenterU:=UVCenterUB.Value;
  if Sender=UVCenterVB then UVBase.CenterV:=UVCenterVB.Value;
  if Sender=UVRadiusB then UVBase.Radius:=UVRadiusB.Value;
  FPri.UVBaseB:=UVBase;
end;

procedure TPriConeFrame.UVBaseTChange(Sender: TObject);
var
  UVBase: TPMUVCircle;
begin
  if not Assigned(FPri) or ((Sender as TSpinEdit).Text='') or
    ((Sender as TSpinEdit).Text='-') then Exit;
  UVBase:=FPri.UVBaseT;
  if Sender=UVCenterUT then UVBase.CenterU:=UVCenterUT.Value;
  if Sender=UVCenterVT then UVBase.CenterV:=UVCenterVT.Value;
  if Sender=UVRadiusT then UVBase.Radius:=UVRadiusT.Value;
  FPri.UVBaseT:=UVBase;
end;

procedure TPriConeFrame.UVSideChange(Sender: TObject);
var
  UVSide: TPMUVRect;
begin
  if not Assigned(FPri) or ((Sender as TSpinEdit).Text='') or
    ((Sender as TSpinEdit).Text='-') then Exit;
  UVSide:=FPri.UVSide;
  if Sender=OrigU then UVSide.OrigU:=OrigU.Value;
  if Sender=OrigV then UVSide.OrigV:=OrigV.Value;
  if Sender=SizeU then UVSide.SizeU:=SizeU.Value;
  if Sender=SizeV then UVSide.SizeV:=SizeV.Value;
  FPri.UVSide:=UVSide;
end;

end.
