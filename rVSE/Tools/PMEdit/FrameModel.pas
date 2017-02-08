unit FrameModel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PMBuild;

type
  TModelFrame=class(TFrame)
    GroupModel: TGroupBox;
    ModelShowAll: TButton;
    ModelHideAll: TButton;
    function  Fill(Obj: TObject): Boolean;
    procedure Clear;
    procedure ModelHideAllClick(Sender: TObject);
    procedure ModelShowAllClick(Sender: TObject);
  private
    FModel: TPMBModel;
  public

  end;

implementation

{$R *.dfm}

function TModelFrame.Fill(Obj: TObject): Boolean;
begin
  Result:=Obj is TPMBModel;
  if Result then
  begin
    FModel:=Obj as TPMBModel;
    FModel.DeselectAll;
  end
    else FModel:=nil;
end;

procedure TModelFrame.Clear;
begin
  FModel:=nil;
end;

procedure TModelFrame.ModelHideAllClick(Sender: TObject);
begin
  if Assigned(FModel) then FModel.SetVisibility(false);
end;

procedure TModelFrame.ModelShowAllClick(Sender: TObject);
begin
  if Assigned(FModel) then FModel.SetVisibility(true);
end;

end.

