unit GlobalVariables;

interface

uses
  IniFiles, Dialogs, SysUtils, Forms;

const
  AppVersion='1.0';

  //Имена ключей и секций для сохранения опций
  KeyLeft='Left';
  KeyTop='Top';
  KeyWidth='Width';
  KeyHeight='Height';

var
  Config: TIniFile;
  ExePath: string;

procedure SaveFormState(Form: TForm);
procedure RestoreFormState(Form: TForm);

implementation

procedure SaveFormState(Form: TForm);
begin
  with Config, Form do
  begin
    WriteInteger(Name, KeyLeft, Left);
    WriteInteger(Name, KeyTop, Top);
    WriteInteger(Name, KeyWidth, Width);
    WriteInteger(Name, KeyHeight, Height);
  end;
end;

procedure RestoreFormState(Form: TForm);
begin
  with Config, Form do
  begin
    SetBounds(ReadInteger(Name, KeyLeft, Left),
              ReadInteger(Name, KeyTop, Top),
              ReadInteger(Name, KeyWidth, Width),
              ReadInteger(Name, KeyHeight, Height));
  end;
end;

initialization
  ExePath:=ExtractFilePath(Application.ExeName);
  Config:=TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

finalization
  Config.Free;
end.
