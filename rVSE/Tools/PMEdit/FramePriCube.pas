unit FramePriCube;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, PMBuild, ComCtrls, Spin;

type
  TPriCubeFrame = class(TFrame)
    CubeGroup: TGroupBox;
    SideTabs: TTabControl;
    SplitCheck: TCheckBox;
    OriginLabel: TLabel;
    SizeLabel: TLabel;
    OrigU: TSpinEdit;
    OrigV: TSpinEdit;
    SizeU: TSpinEdit;
    SizeV: TSpinEdit;
    TexCoordsGroup: TGroupBox;
    function Fill(Pri: TPMBPrimitive): Boolean;
    procedure Clear;
    procedure SplitCheckClick(Sender: TObject);
    procedure GenUVChange(Sender: TObject);
    procedure SideTabsChange(Sender: TObject);
  private
    FPri: TPMBPrimitiveCube;
  end;

implementation

{$R *.dfm}

function TPriCubeFrame.Fill(Pri: TPMBPrimitive): Boolean;
begin
  Result:=Pri.PriType=PrimitiveCube;
  if Result then
  begin
    FPri:=Pri as TPMBPrimitiveCube;
    SideTabsChange(nil);
  end
    else FPri:=nil;
end;

procedure TPriCubeFrame.Clear;
begin
  FPri:=nil;
end;

procedure TPriCubeFrame.SplitCheckClick(Sender: TObject);
begin
  if Assigned(FPri) then FPri.TexSplitSides[SideTabs.TabIndex]:=SplitCheck.Checked;
end;

procedure TPriCubeFrame.GenUVChange(Sender: TObject);
var
  GenUV: TPMUVRect;
begin
  if not Assigned(FPri) or ((Sender as TSpinEdit).Text='') or
    ((Sender as TSpinEdit).Text='-') then Exit;
  GenUV:=FPri.TexUV[SideTabs.TabIndex];
  if Sender=OrigU then GenUV.OrigU:=OrigU.Value;
  if Sender=OrigV then GenUV.OrigV:=OrigV.Value;
  if Sender=SizeU then GenUV.SizeU:=SizeU.Value;
  if Sender=SizeV then GenUV.SizeV:=SizeV.Value;
  FPri.TexUV[SideTabs.TabIndex]:=GenUV;
end;

procedure TPriCubeFrame.SideTabsChange(Sender: TObject);
var
  GenUV: TPMUVRect;
begin
  if not Assigned(FPri) then Exit;
  SplitCheck.Checked:=FPri.TexSplitSides[SideTabs.TabIndex];
  GenUV:=FPri.TexUV[SideTabs.TabIndex];
  OrigU.Value:=GenUV.OrigU;
  OrigV.Value:=GenUV.OrigV;
  SizeU.Value:=GenUV.SizeU;
  SizeV.Value:=GenUV.SizeV;
end;

end.
