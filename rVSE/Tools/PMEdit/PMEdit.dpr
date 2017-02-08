program PMEdit;

uses
  SysUtils,
  Forms,
  GlobalVariables in 'GlobalVariables.pas',
  FormMain in 'FormMain.pas' {MainForm},
  FrameModel in 'FrameModel.pas' {ModelFrame: TFrame},
  FrameObject in 'FrameObject.pas' {ObjectFrame: TFrame},
  FrameMesh in 'FrameMesh.pas' {MeshFrame: TFrame},
  FrameMaterial in 'FrameMaterial.pas' {MaterialFrame: TFrame},
  FramePrimitive in 'FramePrimitive.pas' {PrimitiveFrame: TFrame},
  FormAbout in 'FormAbout.pas' {AboutForm},
  FormPrefs in 'FormPrefs.pas' {PrefsForm},
  FramePriCube in 'FramePriCube.pas' {PriCubeFrame: TFrame},
  FramePriSphere in 'FramePriSphere.pas' {PriSphereFrame: TFrame},
  FramePriCone in 'FramePriCone.pas' {PriConeFrame: TFrame};

{$R *.res}

begin
  DecimalSeparator:='.';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPrefsForm, PrefsForm);
  Application.Run;
end.

