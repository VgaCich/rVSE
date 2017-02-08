unit SDDialog;

interface

uses
  Windows, SysUtils, Classes, ShellAPI, ShlObj, Forms;

type
  TSDDialog = class(TComponent)
    function Execute: boolean;
  private
    FHandle: HWND;
    FPath: string;
    FTitle: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent);override;
    property Path: string read FPath;
    { Public declarations }
  published
    property Title: string read FTitle write FTitle;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Dialogs', [TSDDialog]);
end;

constructor TSDDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle:=Application.Handle;
end;

function TSDDialog.Execute: boolean;
var
  BI: TBrowseInfo;
  StartPIDL, ResPIDL: PItemIDList;
  Path: PChar;
begin
  SHGetSpecialFolderLocation(FHandle, CSIDL_DESKTOP, StartPIDL);
  with BI do
  begin
    hwndOwner:=FHandle;
    pszDisplayName:=nil;
    lpszTitle:=PChar(FTitle);
    ulFlags:=BIF_RETURNONLYFSDIRS;
    pidlRoot:=StartPIDL;
    lpfn:=nil;
    iImage:=1;
  end;
  Path:=StrAlloc(MAX_PATH);
  ResPIDL:=SHBrowseForFolder(BI);
  if SHGetPathFromIDList(ResPIDL, Path)
  then begin
    FPath:=string(Path);
    Result:=true;
  end else Result:=false;
  StrDispose(Path);
end;

end.
 
