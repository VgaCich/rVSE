unit FrameMaterial;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PMBuild, ExtCtrls, ComCtrls;

type
  TMaterialFrame=class(TFrame)
    GroupMaterial: TGroupBox;
    DiffusePaint: TPaintBox;
    AmbientPaint: TPaintBox;
    SpecularPaint: TPaintBox;
    EmissionPaint: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ColorSelect: TColorDialog;
    Shininess: TTrackBar;
    ShininessLabel: TLabel;
    Label6: TLabel;
    Texture: TComboBox;
    DiffuseAlpha: TTrackBar;
    AmbientAlpha: TTrackBar;
    SpecularAlpha: TTrackBar;
    EmissionAlpha: TTrackBar;
    DiffAlphaLabel: TLabel;
    AmbiAlphaLabel: TLabel;
    SpecAlphaLabel: TLabel;
    EmiAlphaLabel: TLabel;
    function  Fill(Obj: TObject): Boolean;
    procedure Save;
    procedure Clear;
    procedure AlphaChange(Sender: TObject);
    procedure PaintClick(Sender: TObject);
    procedure ColorPaint(Sender: TObject);
    procedure ShininessChange(Sender: TObject);
    procedure TrackKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TextureExit(Sender: TObject);
    procedure TextureKeyPress(Sender: TObject; var Key: Char);
  private
    FMaterial: TPMBMaterial;
    procedure RefreshColor(Color: TColor; ColorPaint: TPaintBox; ColorAlpha: TTrackBar);
    function  GetColor(ColorPaint: TPaintBox; ColorAlpha: TTrackBar): TColor;
    procedure RefreshAlphaLabels;
  public

  end;

implementation

{$R *.dfm}

uses
  FormMain;

resourcestring
  RSShininess = 'Shininess: ';

const
  TagAmbient=0;
  TagDiffuse=1;
  TagEmission=2;
  TagSpecular=3;
  AlphaPrefix='A=';

function TMaterialFrame.Fill(Obj: TObject): Boolean;
var
  SR: TSearchRec;
begin
  Result:=Obj is TPMBMaterial;
  if Result then
  begin
    FMaterial:=Obj as TPMBMaterial;
    FMaterial.Model.DeselectAll;
    RefreshColor(FMaterial.Ambient, AmbientPaint, AmbientAlpha);
    RefreshColor(FMaterial.Diffuse, DiffusePaint, DiffuseAlpha);
    RefreshColor(FMaterial.Emission, EmissionPaint, EmissionAlpha);
    RefreshColor(FMaterial.Specular, SpecularPaint, SpecularAlpha);
    RefreshAlphaLabels;
    Shininess.Position:=FMaterial.Shininess;
    Texture.Items.BeginUpdate;
    try
      Texture.Items.Clear;
      Texture.Items.Add('');
      if FindFirst(MainForm.TexturesDir+'*.tga', 0, SR)=0 then
        repeat
          Texture.Items.Add(ChangeFileExt(SR.Name, ''));
        until FindNext(SR)<>0;
      FindClose(SR);
    finally
      Texture.Items.EndUpdate;
    end;
    Texture.Text:=FMaterial.Texture;
  end
    else FMaterial:=nil;
end;

procedure TMaterialFrame.Save;
begin
  if not Assigned(FMaterial) then Exit;
  FMaterial.Ambient:=GetColor(AmbientPaint, AmbientAlpha);
  FMaterial.Diffuse:=GetColor(DiffusePaint, DiffuseAlpha);
  FMaterial.Emission:=GetColor(EmissionPaint, EmissionAlpha);
  FMaterial.Specular:=GetColor(SpecularPaint, SpecularAlpha);
  FMaterial.Shininess:=Shininess.Position;
  FMaterial.Texture:=Texture.Text;
end;

procedure TMaterialFrame.Clear;
begin
  FMaterial:=nil;
  RefreshColor(clBlack, DiffusePaint, DiffuseAlpha);
  RefreshColor(clBlack, AmbientPaint, AmbientAlpha);
  RefreshColor(clBlack, SpecularPaint, SpecularAlpha);
  RefreshColor(clBlack, EmissionPaint, EmissionAlpha);
  Shininess.Position:=0;
  Texture.Text:='';
end;

procedure TMaterialFrame.AlphaChange(Sender: TObject);
begin
  RefreshAlphaLabels;
  if Assigned(FMaterial) then
    case (Sender as TComponent).Tag of
      TagAmbient: FMaterial.Ambient:=GetColor(AmbientPaint, Sender as TTrackBar);
      TagDiffuse: FMaterial.Diffuse:=GetColor(DiffusePaint, Sender as TTrackBar);
      TagEmission: FMaterial.Emission:=GetColor(EmissionPaint, Sender as TTrackBar);
      TagSpecular: FMaterial.Specular:=GetColor(SpecularPaint, Sender as TTrackBar);
    end;
end;

procedure TMaterialFrame.PaintClick(Sender: TObject);
begin
  ColorSelect.Color:=(Sender as TPaintBox).Color;
  if ColorSelect.Execute then (Sender as TPaintBox).Color:=ColorSelect.Color;
  if Assigned(FMaterial) then
    case (Sender as TComponent).Tag of
      TagAmbient: FMaterial.Ambient:=GetColor((Sender as TPaintBox), AmbientAlpha);
      TagDiffuse: FMaterial.Diffuse:=GetColor((Sender as TPaintBox), DiffuseAlpha);
      TagEmission: FMaterial.Emission:=GetColor((Sender as TPaintBox), EmissionAlpha);
      TagSpecular: FMaterial.Specular:=GetColor((Sender as TPaintBox), SpecularAlpha);
    end;
end;

procedure TMaterialFrame.ColorPaint(Sender: TObject);
var
  S: string;
begin
  with (Sender as TPaintBox) do
  begin
    S:='$'+IntToHex(Color and $FF, 2)+IntToHex((Color shr 8) and $FF, 2)+IntToHex((Color shr 16) and $FF, 2);
    if (Color and $FF)+((Color shr 8) and $FF)+((Color shr 16) and $FF)>384
      then Canvas.Font.Color:=clBlack
      else Canvas.Font.Color:=clWhite;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.TextOut((Width-Canvas.TextWidth(S)) div 2, 5, S);
  end;
end;

procedure TMaterialFrame.ShininessChange(Sender: TObject);
begin
  ShininessLabel.Caption:=RSShininess+IntToStr(Shininess.Position);
  if Assigned(FMaterial) then FMaterial.Shininess:=Shininess.Position;
end;

procedure TMaterialFrame.TrackKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with (Sender as TTrackBar) do
    case Key of
      VK_LEFT: Position:=Position-1;
      VK_RIGHT: Position:=Position+1;
    end;
end;

procedure TMaterialFrame.TextureExit(Sender: TObject);
begin
  if Assigned(FMaterial) then FMaterial.Texture:=Texture.Text;
end;

procedure TMaterialFrame.TextureKeyPress(Sender: TObject; var Key: Char);
begin
  TextureExit(Sender);
end;

procedure TMaterialFrame.RefreshColor(Color: TColor; ColorPaint: TPaintBox; ColorAlpha: TTrackBar);
begin
  ColorPaint.Color:=Color and $00FFFFFF;
  ColorAlpha.Position:=(Color and $FF000000) shr 24;
end;

function TMaterialFrame.GetColor(ColorPaint: TPaintBox; ColorAlpha: TTrackBar): TColor;
begin
  Result:=TColor((Cardinal(ColorPaint.Color) and $00FFFFFF) or ((ColorAlpha.Position shl 24) and $FF000000));
end;

procedure TMaterialFrame.RefreshAlphaLabels;
begin
  AmbiAlphaLabel.Caption:=AlphaPrefix+IntToStr(AmbientAlpha.Position);
  DiffAlphaLabel.Caption:=AlphaPrefix+IntToStr(DiffuseAlpha.Position);
  EmiAlphaLabel.Caption:=AlphaPrefix+IntToStr(EmissionAlpha.Position);
  SpecAlphaLabel.Caption:=AlphaPrefix+IntToStr(SpecularAlpha.Position);
end;

end.

