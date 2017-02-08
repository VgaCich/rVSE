unit StateLoad;

interface

uses
  Windows, Messages, AvL, avlUtils, avlVectors, OpenGL, VSEOpenGLExt, oglExtensions,
  VSECore, SynTex, SynTexFilters, StateGame;

type
  TLoadStage=procedure of object;
  TStateLoad=class(TGameState)
  protected
    FFont: Cardinal;
    FGame: TStateGame;
    FLoadStage: TLoadStage;
    FLevelName, FStageName: string;
    function GetName: string; override;
    procedure Delay;
    procedure LoadTerrain;
    procedure StartGame;
    procedure STStore(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
    procedure SetStage(Stage: TLoadStage; const Name: string);
  public
    constructor Create;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
    property LevelName: string read FLevelName write FLevelName;
  end;

implementation

uses VSEMemPak, VSETexMan, VSERender2D {$IFDEF VSE_LOG}, VSELog{$ENDIF}, StateMenu;

const
  SLoad='Loading level %s: %s...';
  STitle='Assault Lite';

constructor TStateLoad.Create;
begin
  inherited Create;
  FFont:=Render2D.CreateFont(UIFont, 20, false);
  FGame:=TStateGame(Core.GetState(Core.FindState('Game')));
end;

procedure TStateLoad.Draw;
var
  S: string;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  S:=Format(SLoad, [FLevelName, FStageName]);
  Render2D.Enter;
  gleColor(clLime);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, S)/2, 500, S);
  Render2D.TextOut(FFont, 400-Render2D.TextWidth(FFont, STitle)/2, 250, STitle);
  Render2D.Leave;
end;

procedure TStateLoad.Update;
begin
  inherited;
  if Assigned(FLoadStage) then
  begin
    FLoadStage;
    Core.ResetUpdateTimer;
  end;
end;

function TStateLoad.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  SetStage(Delay, '');
end;

function TStateLoad.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if (Notify=snMinimize) or (Notify=snConsoleActive) then Result:=true;
end;

function TStateLoad.GetName: string;
begin
  Result:='Load';
end;

procedure TStateLoad.Delay;
begin
  SetStage(LoadTerrain, 'landscape');
end;

procedure TStateLoad.LoadTerrain;
var
  STCode: TStream;
  ST: TSynTex;
  STF: TSynTexFilters;
begin
  STCode:=GetFile(FLevelName+'.stc');
  ST:=nil;
  STF:=nil;
  if STCode=nil then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'Level %s textures synthesizing code not found', [FLevelName]);{$ENDIF}
    Core.StopEngine(StopUserError);
    Exit;
  end;
  try
    ST:=TSynTex.Create(512);
    STF:=TSynTexFilters.Create(ST);
    ST.Code:=STCode;
    ST.OnStore:=STStore;
    if not ST.Synthesize then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Level %s textures synthesizing failed', [FLevelName]);{$ENDIF}
      Core.StopEngine(StopUserError);
      Exit;
    end;
  finally
    FAN(STF);
    FAN(ST);
    FAN(STCode);
  end;
  FGame.Terrain.Texture:=TexMan.GetTex('Grass');
  FGame.NewGame;
  SetStage(StartGame, 'start');
end;

procedure TStateLoad.StartGame;
begin
  Core.SwitchState('Game');
  FLoadStage:=nil;
end;

procedure TStateLoad.STStore(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
begin
  if Name='Terrain'
    then FGame.Terrain.Load(Reg, TexSize, 1, 1/4);
end;

procedure TStateLoad.SetStage(Stage: TLoadStage; const Name: string);
begin
  FLoadStage:=Stage;
  FStageName:=Name;
end;

end.
