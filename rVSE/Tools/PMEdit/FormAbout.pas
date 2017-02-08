unit FormAbout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutForm = class(TForm)
    OKButton: TButton;
    VSLogo: TImage;
    ProgName: TLabel;
    Copyright: TLabel;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

end.
 
