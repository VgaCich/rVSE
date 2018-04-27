unit GameForms;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors, OpenGL, oglExtensions,
  VSEOpenGLExt, VSECore, VSEGUI;

type
  {$IFDEF VSE_DEBUG}
  TLogPointsForm = class(TGUIForm)
  private
    {$IFDEF VSE_CONSOLE}function LogPointsHandler(Sender: TObject; Args: array of const): Boolean;{$ENDIF} 
  public
    constructor Create;
  end;
  {$ENDIF}

const
  {$IFDEF VSE_DEBUG}IDLogPoints = 'LogPoints';{$ENDIF}

implementation

uses
  VSERender2D, VSEFormManager{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF};

{$IFDEF VSE_DEBUG}
{ TLogPointsForm }

constructor TLogPointsForm.Create;
begin
  with Render2D.VSBounds do
    inherited Create(Round(Right - 150), Round(Top), 150, 400);
  FCaption := 'Log points';
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['logpoints val=eoff:on'] := LogPointsHandler;
  {$ENDIF}
end;

{$IFDEF VSE_CONSOLE}
function TLogPointsForm.LogPointsHandler(Sender: TObject; Args: array of const): Boolean;
begin
  Result := true;
  FormManager.Visible[FormManager.FormName(Self)] := Boolean(Args[1].VInteger);
end;
{$ENDIF}

{$ENDIF}

end.
