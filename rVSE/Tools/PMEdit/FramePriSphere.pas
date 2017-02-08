unit FramePriSphere;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Spin, ComCtrls, PMBuild;

type
  TPriSphereFrame = class(TFrame)
    SphereGroup: TGroupBox;
    Slices: TSpinEdit;
    Stacks: TSpinEdit;
    SmoothCheck: TCheckBox;
    OriginLabel: TLabel;
    SizeLabel: TLabel;
    OrigU: TSpinEdit;
    OrigV: TSpinEdit;
    SizeU: TSpinEdit;
    SizeV: TSpinEdit;
    SpherePages: TPageControl;
    GeomPage: TTabSheet;
    TexCoordsPage: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SlicesSector: TSpinEdit;
    StacksSector: TSpinEdit;
    function Fill(Pri: TPMBPrimitive): Boolean;
    procedure Clear;
    procedure GenUVChange(Sender: TObject);
    procedure SlicesChange(Sender: TObject);
    procedure SlicesSectorChange(Sender: TObject);
    procedure SmoothCheckClick(Sender: TObject);
    procedure StacksChange(Sender: TObject);
    procedure StacksSectorChange(Sender: TObject);
  private
    FPri: TPMBPrimitiveSphere;
  end;

implementation

{$R *.dfm}

function TPriSphereFrame.Fill(Pri: TPMBPrimitive): Boolean;
begin
  Result:=Pri.PriType=PrimitiveSphere;
  if Result then
  begin
    FPri:=Pri as TPMBPrimitiveSphere;
    SmoothCheck.Checked:=FPri.Smooth;
    Slices.Value:=FPri.Slices;
    Stacks.Value:=FPri.Stacks;
    SlicesSector.Value:=FPri.SlicesSector;
    StacksSector.Value:=FPri.StacksSector;
    if FPri.TexGenUV then
    begin
      OrigU.Value:=FPri.TexUV.OrigU;
      OrigV.Value:=FPri.TexUV.OrigV;
      SizeU.Value:=FPri.TexUV.SizeU;
      SizeV.Value:=FPri.TexUV.SizeV;
    end;
  end
    else FPri:=nil;
end;

procedure TPriSphereFrame.Clear;
begin
  FPri:=nil;
end;

procedure TPriSphereFrame.GenUVChange(Sender: TObject);
var
  GenUV: TPMUVRect;
begin
  if not Assigned(FPri) or ((Sender as TSpinEdit).Text='') or
    ((Sender as TSpinEdit).Text='-') then Exit;
  GenUV:=FPri.TexUV;
  if Sender=OrigU then GenUV.OrigU:=OrigU.Value;
  if Sender=OrigV then GenUV.OrigV:=OrigV.Value;
  if Sender=SizeU then GenUV.SizeU:=SizeU.Value;
  if Sender=SizeV then GenUV.SizeV:=SizeV.Value;
  FPri.TexUV:=GenUV;
end;

procedure TPriSphereFrame.SlicesChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.Slices:=Slices.Value;
end;

procedure TPriSphereFrame.SlicesSectorChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.SlicesSector:=SlicesSector.Value;
end;

procedure TPriSphereFrame.SmoothCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.Smooth:=SmoothCheck.Checked;
end;

procedure TPriSphereFrame.StacksChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.Stacks:=Stacks.Value;
end;

procedure TPriSphereFrame.StacksSectorChange(Sender: TObject);
begin
  if Assigned(FPri) and ((Sender as TSpinEdit).Text<>'') and
    ((Sender as TSpinEdit).Text<>'-') then FPri.StacksSector:=StacksSector.Value;
end;

end.
