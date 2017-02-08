unit FormPrefs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SDDialog;

type
  TPrefsForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupTextures: TGroupBox;
    LabelPath: TLabel;
    EditTexPath: TEdit;
    TexPathBrowse: TButton;
    TexPathSelect: TSDDialog;
    CheckAssoc: TCheckBox;
    procedure TexPathBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrefsForm: TPrefsForm;

implementation

{$R *.dfm}

procedure TPrefsForm.TexPathBrowseClick(Sender: TObject);
begin
  //TexPathSelect.Path:=EditTexPath.Text;
  if TexPathSelect.Execute
    then EditTexPath.Text:=TexPathSelect.Path;
end;

end.
