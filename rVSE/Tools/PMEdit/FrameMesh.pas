unit FrameMesh;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PMBuild, ComCtrls, Spin, FPSpin, Import3ds;

type
  TMeshFrame=class(TFrame)
    GroupMesh: TGroupBox;
    ImportMesh: TButton;
    MeshMode: TPageControl;
    PageGeneral: TTabSheet;
    PageVertices: TTabSheet;
    VisibleCheck: TCheckBox;
    Vertices: TListBox;
    VertX: TSpinEdit;
    VertY: TSpinEdit;
    VertZ: TSpinEdit;
    VertU: TSpinEdit;
    VertV: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    MeshOpenDialog: TOpenDialog;
    Label3: TLabel;
    VertP: TSpinEdit;
    VertT: TSpinEdit;
    DrawNormalsCheck: TCheckBox;
    NormalsCheck: TCheckBox;
    UVCheck: TCheckBox;
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
    function Fill(Obj: TObject): Boolean;
    procedure Clear;
    procedure DrawNormalsCheckClick(Sender: TObject);
    procedure ImportMeshClick(Sender: TObject);
    procedure MeshModeChange(Sender: TObject);
    procedure NormalsCheckClick(Sender: TObject);
    procedure TransformChange(Sender: TObject);
    procedure UVCheckClick(Sender: TObject);
    procedure VerticesClick(Sender: TObject);
    procedure VertChange(Sender: TObject);
    procedure VisibleCheckClick(Sender: TObject);
  private
    FMesh: TPMBMesh;
    procedure Refresh;
    procedure FillVertices;
  public

  end;

implementation

{$R *.dfm}

function TMeshFrame.Fill(Obj: TObject): Boolean;
begin
  Result:=Obj is TPMBMesh;
  if Result then
  begin
    FMesh:=Obj as TPMBMesh;
    FMesh.Selected:=true;
    Refresh;
  end
    else FMesh:=nil;
end;

procedure TMeshFrame.Clear;
begin
  FMesh.HighlightVert:=-1;
  FMesh:=nil;
  Vertices.Items.Clear;
end;

procedure TMeshFrame.DrawNormalsCheckClick(Sender: TObject);
begin
  if Assigned(FMesh) then FMesh.DrawNormals:=DrawNormalsCheck.Checked;
end;

procedure TMeshFrame.ImportMeshClick(Sender: TObject);
var
  F: TFileStream;
  M: TMemoryStream;
begin
  if Assigned(FMesh) and MeshOpenDialog.Execute then
  begin
    F:=TFileStream.Create(MeshOpenDialog.FileName, fmOpenRead);
    try
      M:=TMemoryStream.Create;
      case MeshOpenDialog.FilterIndex of
        2: if not Read3ds(F, M) then Exit; 
        else M.LoadFromStream(F);
      end;
      M.Position:=0;
      FMesh.Import(M);
      Refresh;
    finally
      M.Free;
      F.Free;
    end;
  end;
end;

procedure TMeshFrame.MeshModeChange(Sender: TObject);
begin
  if Assigned(FMesh) then
    if MeshMode.ActivePage=PageVertices
      then FMesh.HighlightVert:=Vertices.ItemIndex
      else FMesh.HighlightVert:=-1;
end;

procedure TMeshFrame.NormalsCheckClick(Sender: TObject);
begin
  if Assigned(FMesh) then FMesh.HasNormals:=NormalsCheck.Checked;
end;

procedure TMeshFrame.TransformChange(Sender: TObject);
begin
  if not Assigned(FMesh) or ((Sender as TFPSpinEdit).Text='') or
    ((Sender as TFPSpinEdit).Text='-') or ((Sender as TFPSpinEdit).Text=DecimalSeparator)
      then Exit;
  with FMesh.Transform, Sender as TFPSpinEdit do
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

procedure TMeshFrame.UVCheckClick(Sender: TObject);
begin
  if Assigned(FMesh) then FMesh.HasUV:=UVCheck.Checked;
end;

procedure TMeshFrame.VerticesClick(Sender: TObject);
begin
  if not Assigned(FMesh) then Exit;
  FMesh.HighlightVert:=Vertices.ItemIndex;
  with FMesh.Verts[Vertices.ItemIndex] do
  begin
    VertX.Value:=X;
    VertY.Value:=Y;
    VertZ.Value:=Z;
    VertP.Value:=Phi;
    VertT.Value:=Theta;
    VertU.Value:=U;
    VertV.Value:=V;
  end;
end;

procedure TMeshFrame.VertChange(Sender: TObject);
var
  Vertex: TPMVertex;
begin
  if not Assigned(FMesh) or ((Sender as TSpinEdit).Text='') or ((Sender as TSpinEdit).Text='-') then Exit;
  Vertex:=FMesh.Verts[Vertices.ItemIndex];
  if Sender=VertX then Vertex.X:=(Sender as TSpinEdit).Value;
  if Sender=VertY then Vertex.Y:=(Sender as TSpinEdit).Value;
  if Sender=VertZ then Vertex.Z:=(Sender as TSpinEdit).Value;
  if Sender=VertP then Vertex.Phi:=(Sender as TSpinEdit).Value;
  if Sender=VertT then Vertex.Theta:=(Sender as TSpinEdit).Value;
  if Sender=VertU then Vertex.U:=(Sender as TSpinEdit).Value;
  if Sender=VertV then Vertex.V:=(Sender as TSpinEdit).Value;
  FMesh.Verts[Vertices.ItemIndex]:=Vertex;
  with Vertices, FMesh.Verts[Vertices.ItemIndex] do
    Items[ItemIndex]:=Format('%d (%d; %d; %d)', [ItemIndex, X, Y, Z])
end;

procedure TMeshFrame.VisibleCheckClick(Sender: TObject);
begin
  if Assigned(FMesh) then FMesh.Visible:=VisibleCheck.Checked;
end;

procedure TMeshFrame.Refresh;
begin
  VisibleCheck.Checked:=FMesh.Visible;
  NormalsCheck.Checked:=FMesh.HasNormals;
  UVCheck.Checked:=FMesh.HasUV;
  DrawNormalsCheck.Checked:=FMesh.DrawNormals;
  with FMesh.Transform do
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
  FillVertices;
end;

procedure TMeshFrame.FillVertices;
var
  i: Integer;
begin
  Vertices.Items.BeginUpdate;
  try
    Vertices.Items.Clear;
    for i:=0 to FMesh.VertsCount-1 do
      Vertices.Items.Add(Format('%d (%d; %d; %d)', [i, FMesh.Verts[i].X, FMesh.Verts[i].Y, FMesh.Verts[i].Z]));
  finally
    Vertices.Items.EndUpdate;
  end;
end;

end.

