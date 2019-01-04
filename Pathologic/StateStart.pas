unit StateStart;

interface

uses
  Windows, Messages, AvL, avlUtils, avlVectors, OpenGL, VSEOpenGLExt,
  oglExtensions, VSECore, VSEImageCodec;

type
  TLoadThread = class(TThread)
  protected
    procedure Execute; override;
  public
    TexName: string;
    TexImage: TImage;
    Progress: Integer;
    OnLoad: TThreadMethod;
    constructor Create;
    destructor Destroy; override;
  end;
  TStateStart = class(TGameState)
  private
    FFont: Cardinal;
    FLoadThread: TLoadThread;
  protected
    function GetName: string; override;
    procedure DrawSegs(X, Y: Single; Segs: Integer);
    procedure LoadComplete(Sender: TObject);
    procedure LoadTex;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function Activate: Cardinal; override;
    procedure Deactivate; override;
    function SysNotify(Notify: TSysNotify): Boolean; override;
  end;

const
  SIDStart = 'Start';

implementation

uses
  VSETexMan, VSERender2D, VSEGUI
  {$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF},
  StateMenu;

const
  SLoading = 'Загрузка...';
  SRVSE = 'reduced VS Engine';

{TLoadThread}

constructor TLoadThread.Create;
begin
  inherited Create(true);
  TexImage := TImage.Create;
end;

destructor TLoadThread.Destroy;
begin
  FAN(TexImage);
  inherited;
end;

procedure TLoadThread.Execute;
var
  i: Integer;
  List: TStringList;
  Data: TStream;
begin
  inherited;
  Progress := 0;
  List := Core.GetFileText('Preload.txt');
  try
    for i := 0 to List.Count - 1 do
    try
      TexName := List[i];
      Data := Core.GetFile(GetTexFileName(TexName));
      try
        TexImage.Load(Data);
      finally
        FAN(Data);
      end;
      Progress := 100 * i div (List.Count - 1);
      if Assigned(OnLoad) then
        Synchronize(OnLoad);
    except
      {$IFDEF VSE_LOG}LogException('when loading ' + TexName);{$ENDIF}
    end;
  finally
    List.Free;
  end;
end;

{TStateStart}

constructor TStateStart.Create;
begin
  inherited Create;
  FFont := Render2D.CreateFont(UIFont, 20, true);
end;

destructor TStateStart.Destroy;
begin
  FAN(FLoadThread);
  inherited;
end;

procedure TStateStart.Draw;
var
  Top, Width: Single;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  Render2D.Enter;
  DrawBackground;
  gleColor($80000000);
  Render2D.DrawRect(250, 300, 300, 180);
  gleColor(VSEGUI.clText);
  glPushMatrix;
  Render2D.Move(200, 230);
  glScalef(0.5, 0.5, 1);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, SRVSE)/2, 430, SRVSE);
  DrawSegs(300, 200, $426);
  DrawSegs(400, 200, $2C0);
  DrawSegs(500, 200, $079);
  glScalef(0.7, 0.7, 1);
  DrawSegs(300, 370, $473);
  glPopMatrix;
  Render2D.TextOut(FFont, 20, 535, SLoading);
  if Assigned(FLoadThread) then
  begin
    Top := Render2D.TextHeight(FFont) + 540;
    Width := Render2D.TextWidth(FFont, SLoading);
    Render2D.LineWidth(2);
    Render2D.DrawRectBorder(20, Top, Width, 20);
    Render2D.DrawRect(22, Top + 2, (Width - 4) * FLoadThread.Progress / 100, 16);
  end;
  Render2D.Leave;
end;

function TStateStart.Activate: Cardinal;
begin
  Result := inherited Activate;
  glClearColor(0, 0, 0, 1);
  ShowCursor(false);
  FLoadThread := TLoadThread.Create;
  FLoadThread.OnLoad := LoadTex;
  FLoadThread.OnTerminate := LoadComplete;
  FLoadThread.Resume;
end;

procedure TStateStart.Deactivate;
begin
  inherited;
  FLoadThread.WaitFor;
  FAN(FLoadThread);
  ShowCursor(true);
end;

function TStateStart.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result := inherited SysNotify(Notify);
  if (Notify = snMinimize) or (Notify = snConsoleActive) then
    Result := true;
end;

function TStateStart.GetName: string;
begin
  Result := SIDStart;
end;

procedure TStateStart.DrawSegs(X, Y: Single; Segs: Integer);
const
  SegsCoord: array [0..10, 0..1] of TPoint=(
    ((X: 0; Y: 0),    (X: 100; Y: 0)),
    ((X: 100; Y: 0),  (X: 100; Y: 100)),
    ((X: 100; Y: 100), (X: 100; Y: 200)),
    ((X: 100; Y: 200),  (X: 0; Y: 200)),
    ((X: 0; Y: 200),   (X: 0; Y: 100)),
    ((X: 0; Y: 100),    (X: 0; Y: 0)),
    ((X: 0; Y: 100),  (X: 100; Y: 100)),
    ((X: 100; Y: 0),   (X: 0; Y: 100)),
    ((X: 0; Y: 0),   (X: 100; Y: 100)),
    ((X: 100; Y: 100),  (X: 0; Y: 200)),
    ((X: 0; Y: 100),  (X: 100; Y: 200)));
var
  i: Integer;
begin
  glPushMatrix;
  glLineWidth(7);
  glPointSize(6);
  glTranslatef(X, Y, 0);
  for i := 0 to 10 do
    if Segs and (1 shl i) <> 0 then
      Render2D.DrawLine(SegsCoord[i, 0], SegsCoord[i, 1]);
  glBegin(GL_POINTS);
    for i := 0 to 10 do
      if Segs and (1 shl i) <> 0 then
      begin
        glVertex2iv(@SegsCoord[i, 0]);
        glVertex2iv(@SegsCoord[i, 1]);
      end;
  glEnd;
  glPopMatrix;
end;

procedure TStateStart.LoadComplete(Sender: TObject);
begin
  Core.SwitchState(SIDMenu);
end;

procedure TStateStart.LoadTex;
begin
  with FLoadThread do
    TexMan.SetFilter(TexMan.AddTexture(TexName, TexImage, true, true), tfAnisotropic);
end;

end.

