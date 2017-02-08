unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, ComCtrls, ToolWin, ImgList, GlobalVariables,
  FrameObject, FrameModel, FramePrimitive, FrameMesh, FrameMaterial,
  devTabs, PMBuild, eXgine, OpenGL, StdCtrls, XPMan, Registry, ShlObj;

const
  ViewportsCount=8;

type
  TFrameFill=function(Obj: TObject): Boolean of object;
  TFrameSave=procedure of object;
  TFrameClear=procedure of object;
  TViewportView=record
    PanX, PanY, PanZ: Single;
    Yaw, Pitch, Roll: Single;
    Scale: Single;
  end;
  TMainForm=class(TForm)
    Viewport: TPanel;
    Tools: TPanel;
    StatusBar: TStatusBar;
    SplitterViewTools: TSplitter;
    Tree: TPanel;
    Properties: TPanel;
    MainMenu: TMainMenu;
    MMFile: TMenuItem;
    MMOpen: TMenuItem;
    MMSave: TMenuItem;
    MMSaveAs: TMenuItem;
    N1: TMenuItem;
    MMExit: TMenuItem;
    MMHelp: TMenuItem;
    ElementsTree: TTreeView;
    SplitterTools: TSplitter;
    ModelFrame: TModelFrame;
    ObjectFrame: TObjectFrame;
    MaterialFrame: TMaterialFrame;
    PrimitiveFrame: TPrimitiveFrame;
    MeshFrame: TMeshFrame;
    ImageListMenus: TImageList;
    MenuNew: TPopupMenu;
    TreeToolBar: TToolBar;
    TreeTBAdd: TToolButton;
    TreeTBRemove: TToolButton;
    MNMaterial: TMenuItem;
    MNObject: TMenuItem;
    MNMesh: TMenuItem;
    MNPrimitive: TMenuItem;
    MMAbout: TMenuItem;
    ViewportMode: TdevTabs;
    MMPreferences: TMenuItem;
    PMOpenDialog: TOpenDialog;
    PMSaveDialog: TSaveDialog;
    ImageListTree: TImageList;
    MMNew: TMenuItem;
    MNCube: TMenuItem;
    MNSphere: TMenuItem;
    MNCone: TMenuItem;
    MNTorus: TMenuItem;
    MNTube: TMenuItem;
    ViewportControl: TPanel;
    ViewReset: TButton;
    XPManifest: TXPManifest;
    ViewX: TCheckBox;
    ViewY: TCheckBox;
    ViewZ: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    PanSpeed: TTrackBar;
    ZoomSpeed: TTrackBar;
    PanSpeedLabel: TLabel;
    ZoomSpeedLabel: TLabel;
    procedure ElementsTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ElementsTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure ElementsTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ElementsTreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ElementsTreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ElementsTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuNewPopup(Sender: TObject);
    procedure MMExitClick(Sender: TObject);
    procedure MNMaterialClick(Sender: TObject);
    procedure MNObjectClick(Sender: TObject);
    procedure MNMeshClick(Sender: TObject);
    procedure MNPrimitiveClick(Sender: TObject);
    procedure MMAboutClick(Sender: TObject);
    procedure MMNewClick(Sender: TObject);
    procedure MMOpenClick(Sender: TObject);
    procedure MMPreferencesClick(Sender: TObject);
    procedure MMSaveAsClick(Sender: TObject);
    procedure MMSaveClick(Sender: TObject);
    procedure PanSpeedChange(Sender: TObject);
    procedure TreeTBAddClick(Sender: TObject);
    procedure TreeTBRemoveClick(Sender: TObject);
    procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewportMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewportResize(Sender: TObject);
    procedure ViewResetClick(Sender: TObject);
    procedure ViewXYClick(Sender: TObject);
    procedure ViewZClick(Sender: TObject);
    procedure ZoomSpeedChange(Sender: TObject);
  private
    FTexturesDir, FBaseCaption: string;
    FOGLFont: eXgine.TFont;
    FModelFileName: string;
    FModel: TPMBModel;
    FCurFrame: TFrame;
    FViews: array[0..ViewportsCount-1] of TViewportView;
    FRotating, FPanning, FZooming: Boolean;
    FLastCursorPos: TPoint;
    function CheckAssoc: Boolean;
    function GetAssocCmd: string;
    function  GetMethod(Obj: TObject; const Name: string): TMethod;
    procedure SetAssoc(Assoc: Boolean);
    procedure SetModelFileName(const Value: string);
  public
    function  LoadTexture(const TexName: string): Cardinal;
    procedure BindTexture(ID: Cardinal);
    procedure ShowStatus(AStatus: string);
    procedure ClearStatus;
    procedure ShowFrame(AFrame: TFrame; AObject: TObject);
    function  FrameFill(Obj: TObject): Boolean;
    procedure FrameSave;
    procedure FrameClear;
    procedure FillTree;
    function  TreeAddObject(ParentNode: TTreeNode; Obj: TPMBObject): TTreeNode;
    function  TreeAddPrimitive(ParentNode: TTreeNode; Primitive: TPMBPrimitive): TTreeNode;
    function  TreeAddMesh(ParentNode: TTreeNode; Mesh: TPMBMesh): TTreeNode;
    function  TreeAddMaterial(ParentNode: TTreeNode; Mat: TPMBMaterial): TTreeNode;
    procedure TreeRefreshCurrentItem;
    procedure UpdateView(Sender: TObject; var Done: Boolean);
    procedure Render;
    property Model: TPMBModel read FModel;
    property TexturesDir: string read FTexturesDir;
    property ModelFileName: string read FModelFileName write SetModelFileName;
  end;

var
  MainForm: TMainForm;

implementation

uses FormPrefs, FormAbout;

const
  SConfTexturesDir = 'TexturesDir';
  SConfSettings = 'Settings';
  ViewFree=0;
  ViewFront=1;
  ViewLeft=2;
  ViewTop=3;
  ViewBack=4;
  ViewRight=5;
  ViewBottom=6;
  ViewUnwrap=7;
  DefaultViews: array[0..ViewportsCount-1] of TViewportView = (
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 0; Pitch: 0; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 0; Pitch: 0; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 90; Pitch: 0; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 0; Pitch: 90; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 180; Pitch: 0; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: -90; Pitch: 0; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: -10; Yaw: 0; Pitch: -90; Roll: 0; Scale: 1),
    (PanX: 0; PanY: 0; PanZ: 0; Yaw: 0; Pitch: 0; Roll: 0; Scale: 1));

resourcestring
  RSReallyRemove = 'Really remove?';
  RSModelCleared = 'Model cleared';
  RSTreeMaterial = 'Material: %d';
  RSTreeMesh = 'Mesh';
  RSTreePrimitive = 'Primitive: %s';
  RSTreeObject = 'Object: %s ($%s)';
  RSTreeModel = 'Model';
  RSModelSaved = 'Model saved (%d bytes)';
  RSReady = 'Ready';
  RSCannotBindEXgineToViewport = 'Cannot bind eXgine to viewport';
  RSModelLoaded = 'Model loaded (%d bytes)';

{$R *.dfm}

procedure Render;
begin
  MainForm.Render;
end;

function FlSize(const FileName: string): Integer;
var
  F: TFileStream;
begin
  F:=TFileStream.Create(FileName, fmOpenRead);
  try
    Result:=F.Size;
  finally
    F.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FBaseCaption:=Caption;
  RestoreFormState(Self);
  Tools.Width:=Config.ReadInteger(Name, 'SplitterViewTools', Tools.Width);
  Properties.Height:=Config.ReadInteger(Name, 'SplitterTools', Properties.Height);
  PanSpeed.Position:=Config.ReadInteger(Name, 'PanSpeed', PanSpeed.Position);
  PanSpeedChange(nil);
  ZoomSpeed.Position:=Config.ReadInteger(Name, 'ZoomSpeed', ZoomSpeed.Position);
  ZoomSpeedChange(nil);
  FTexturesDir:=IncludeTrailingBackslash(Config.ReadString(SConfSettings, SConfTexturesDir, ''));
  Application.Title:=Caption;
  Application.OnIdle:=UpdateView;
  ClearStatus;
  ShowFrame(nil, nil);
  for i:=0 to ViewportsCount-1 do FViews[i]:=DefaultViews[i];
  if not wnd.Create(Viewport.Handle) then ShowMessage(RSCannotBindEXgineToViewport);
  eX.SetProc(PROC_RENDER, @FormMain.Render);
  ogl.VSync(true);
  FOGLFont:=ogl.FontCreate('Courier New', 10);
  glViewport(0, 0, Viewport.Width, Viewport.Height);
  MMNewClick(Self);
  if (ParamCount>0) and (FileExists(ParamStr(1))) then
  begin
    ModelFileName:=ExpandFileName(ParamStr(1));
    MMOpenClick(nil);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Application.OnIdle:=nil;
  FModel.Free;
  SaveFormState(Self);
  Config.WriteInteger(Name, SplitterViewTools.Name, Tools.Width);
  Config.WriteInteger(Name, SplitterTools.Name, Properties.Height);
  Config.WriteInteger(Name, 'PanSpeed', PanSpeed.Position);
  Config.WriteInteger(Name, 'ZoomSpeed', ZoomSpeed.Position);
  Config.WriteString(SConfSettings, SConfTexturesDir, FTexturesDir);
end;

procedure TMainForm.MMExitClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.LoadTexture(const TexName: string): Cardinal;
begin
  Result:=tex.Load(PChar(FTexturesDir+TexName+'.tga'));
end;

procedure TMainForm.BindTexture(ID: Cardinal);
begin
  if ID<>0
    then tex.Enable(ID)
    else tex.Disable;
end;

function TMainForm.CheckAssoc: Boolean;
var
  Reg: TRegistry;
begin
  Result:=false;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    if not (Reg.OpenKeyReadOnly('\.vpm') and
      (Reg.ReadString('')='vpmfile') and
      Reg.OpenKeyReadOnly('\vpmfile\shell\open\command') and
      (Reg.ReadString('')=GetAssocCmd)) then Exit;
    Result:=true;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TMainForm.ShowStatus(AStatus: string);
begin
  StatusBar.Panels[0].Text:=AStatus;
end;

procedure TMainForm.ClearStatus;
begin
  ShowStatus(RSReady);
end;

procedure TMainForm.ElementsTreeChange(Sender: TObject; Node: TTreeNode);
var
  Frame: TFrame;
begin
  if TObject(Node.Data) is TPMBModel then Frame:=ModelFrame
  else if TObject(Node.Data) is TPMBObject then Frame:=ObjectFrame
  else if TObject(Node.Data) is TPMBPrimitive then Frame:=PrimitiveFrame
  else if TObject(Node.Data) is TPMBMesh then Frame:=MeshFrame
  else if TObject(Node.Data) is TPMBMaterial then Frame:=MaterialFrame
  else Exit;
  ShowFrame(Frame, Node.Data);
  TreeTBAdd.Enabled:=(Frame=ModelFrame) or (Frame=ObjectFrame);
  TreeTBRemove.Enabled:=Frame<>ModelFrame;
end;

procedure TMainForm.ElementsTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  FrameSave;
end;

procedure TMainForm.ElementsTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Element, DstElement: TObject;
begin
  Element:=TObject(ElementsTree.Selected.Data);
  DstElement:=TObject(ElementsTree.GetNodeAt(X, Y).Data);
  if Element is TPMBObject then
  begin
    if DstElement is TPMBObject
      then TPMBObject(Element).Relink(TPMBObject(DstElement));
    if DstElement is TPMBModel
      then TPMBObject(Element).Relink(TPMBModel(DstElement));
  end
  else if Element is TPMBPrimitive
    then TPMBPrimitive(Element).Relink(TPMBObject(DstElement))
  else if Element is TPMBMesh
    then TPMBMesh(Element).Relink(TPMBObject(DstElement))
  else Exit;
  FillTree;
end;

procedure TMainForm.ElementsTreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  DstNode: TTreeNode;
  Element, DstElement: TObject;
begin
  Accept:=false;
  if not ((Sender=ElementsTree) and (Source=ElementsTree)) then Exit;
  DstNode:=ElementsTree.GetNodeAt(X, Y);
  if not Assigned(DstNode) then Exit;
  Element:=TObject(ElementsTree.Selected.Data);
  DstElement:=TObject(DstNode.Data);
  if Element is TPMBObject then
  begin
    if not ((DstElement is TPMBObject) or (DstElement is TPMBModel)) then Exit;
  end
  else if (Element is TPMBMesh) or (Element is TPMBPrimitive) then
  begin
    if not (DstElement is TPMBObject) then Exit;
  end
  else Exit;
  Accept:=true;
end;

procedure TMainForm.ElementsTreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_DELETE then TreeTBRemoveClick(Sender);
end;

procedure TMainForm.ElementsTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node:=ElementsTree.GetNodeAt(X, Y);
  if Assigned(Node)
    then ElementsTree.Selected:=Node;
end;

procedure TMainForm.MNMaterialClick(Sender: TObject);
begin
  ElementsTree.Selected:=TreeAddMaterial(ElementsTree.Selected, TPMBMaterial.Create(TPMBModel(ElementsTree.Selected.Data)));
end;

procedure TMainForm.MNObjectClick(Sender: TObject);
var
  M: TPMBModel;
  P: TPMBObject;
begin
  with ElementsTree.Selected do
  begin
    if TObject(Data) is TPMBModel then
    begin
      M:=TPMBModel(Data);
      P:=nil;
    end
    else if TObject(Data) is TPMBObject then
    begin
      M:=TPMBObject(Data).Model;
      P:=TPMBObject(Data);
    end
    else Exit;
  end;
  ElementsTree.Selected:=TreeAddObject(ElementsTree.Selected, TPMBObject.Create(M, P));
end;

procedure TMainForm.MNMeshClick(Sender: TObject);
begin
  ElementsTree.Selected:=TreeAddMesh(ElementsTree.Selected, TPMBMesh.Create(TPMBObject(ElementsTree.Selected.Data)));
end;

procedure TMainForm.MNPrimitiveClick(Sender: TObject);
var
  Obj: TPMBObject;
  Pri: TPMBPrimitive;
begin
  Obj:=TPMBObject(ElementsTree.Selected.Data);
  case (Sender as TMenuItem).Tag of
    PrimitiveCube: Pri:=TPMBPrimitiveCube.Create(Obj);
    PrimitiveSphere: Pri:=TPMBPrimitiveSphere.Create(Obj);
    PrimitiveCone: Pri:=TPMBPrimitiveCone.Create(Obj);
    PrimitiveTorus: Pri:=TPMBPrimitiveTorus.Create(Obj);
    PrimitiveTube: Pri:=TPMBPrimitiveTube.Create(Obj);
  end;
  ElementsTree.Selected:=TreeAddPrimitive(ElementsTree.Selected, Pri);
end;

procedure TMainForm.MMAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.MMOpenClick(Sender: TObject);
begin
  if Sender<>nil then
    if PMOpenDialog.Execute
      then ModelFileName:=PMOpenDialog.FileName
      else Exit;
  try
    FModel.LoadFromFile(ModelFileName);
  except
    MMNewClick(nil);
    raise;
  end;
  ShowStatus(Format(RSModelLoaded, [FlSize(ModelFileName)]));
  FillTree;
end;

procedure TMainForm.MMPreferencesClick(Sender: TObject);
var
  Assoc: Boolean;
begin
  Assoc:=CheckAssoc;
  PrefsForm.CheckAssoc.Checked:=Assoc;
  PrefsForm.EditTexPath.Text:=TexturesDir;
  if PrefsForm.ShowModal=mrOk then
  begin
    FTexturesDir:=IncludeTrailingBackslash(PrefsForm.EditTexPath.Text);
    if PrefsForm.CheckAssoc.Checked<>Assoc then SetAssoc(not Assoc);
  end;
end;

procedure TMainForm.ShowFrame(AFrame: TFrame; AObject: TObject);
var
  i: Integer;
begin
  if AFrame=nil then FCurFrame:=nil;
  for i:=0 to ComponentCount-1 do
    if (Components[i] is TFrame) then
      if Components[i]=AFrame then
      begin
        FCurFrame:=(Components[i] as TFrame);
        FCurFrame.Visible:=true;
        FrameFill(AObject);
      end
        else (Components[i] as TFrame).Visible:=false;
end;

function TMainForm.FrameFill(Obj: TObject): Boolean;
var
  Fill: TFrameFill;
begin
  Result:=false;
  if not Assigned(FCurFrame) then Exit;
  Fill:=TFrameFill(GetMethod(FCurFrame, 'Fill'));
  if Assigned(Fill) then Result:=Fill(Obj);
end;

procedure TMainForm.FrameSave;
var
  Save: TFrameSave;
begin
  if not Assigned(FCurFrame) then Exit;
  Save:=TFrameSave(GetMethod(FCurFrame, 'Save'));
  if Assigned(Save) then Save;
end;

procedure TMainForm.FrameClear;
var
  Clear: TFrameClear;
begin
  if not Assigned(FCurFrame) then Exit;
  Clear:=TFrameClear(GetMethod(FCurFrame, 'Clear'));
  if Assigned(Clear) then Clear;
end;

procedure TMainForm.FillTree;
var
  Node: TTreeNode;
  i: Integer;
begin
  FrameClear;
  ElementsTree.Items.BeginUpdate;
  try
    ElementsTree.Items.Clear;
    Node:=ElementsTree.Items.AddObject(nil, RSTreeModel, FModel);
    Node.ImageIndex:=0;
    Node.SelectedIndex:=0;
    for i:=0 to FModel.ObjectsCount-1 do TreeAddObject(Node, FModel.Objects[i]);
    for i:=0 to FModel.MaterialsCount-1 do TreeAddMaterial(Node, FModel.Materials[i]);
  finally
    ElementsTree.Items.EndUpdate;
    ElementsTree.FullExpand;
    ElementsTree.Selected:=ElementsTree.Items[0];
  end;
end;

function TMainForm.GetAssocCmd: string;
begin
  Result:='"'+ExpandFileName(Application.ExeName)+'" "%L"';
end;

procedure TMainForm.MenuNewPopup(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to MenuNew.Items.Count-1 do MenuNew.Items[i].Visible:=false;
  if not Assigned(ElementsTree.Selected) then Exit; 
  with ElementsTree.Selected do
  begin
    if TObject(Data) is TPMBModel then
    begin
      MNMaterial.Visible:=true;
      MNObject.Visible:=true;
    end
    else if TObject(Data) is TPMBObject then
    begin
      MNObject.Visible:=true;
      MNMesh.Visible:=true;
      for i:=0 to Count-1 do
        if TObject(Item[i].Data) is TPMBMesh then
        begin
          MNMesh.Visible:=false;
          Break;
        end;
      MNPrimitive.Visible:=true;
    end;
  end;
end;

procedure TMainForm.MMNewClick(Sender: TObject);
begin
  FModel.Free;
  FModel:=TPMBModel.Create;
  FModel.OnLoadTex:=LoadTexture;
  FModel.OnBindTex:=BindTexture;
  FillTree;
  ModelFileName:='';
  ShowStatus(RSModelCleared);
end;

function TMainForm.TreeAddObject(ParentNode: TTreeNode; Obj: TPMBObject): TTreeNode;
var
  i: Integer;
begin
  Result:=ElementsTree.Items.AddChildObject(ParentNode, Format(RSTreeObject, [Obj.ID, IntToHex(Obj.IID, 8)]), Obj);
  Result.ImageIndex:=1;
  Result.SelectedIndex:=1;
  for i:=0 to Obj.PrimitivesCount-1 do TreeAddPrimitive(Result, Obj.Primitives[i]);
  if Assigned(Obj.Mesh) then TreeAddMesh(Result, Obj.Mesh);
  for i:=0 to Obj.ObjectsCount-1 do TreeAddObject(Result, Obj.Objects[i]);
end;

function TMainForm.TreeAddPrimitive(ParentNode: TTreeNode; Primitive: TPMBPrimitive): TTreeNode;
begin
  Result:=ElementsTree.Items.AddChildObject(ParentNode, Format(RSTreePrimitive, [PriTypeToString(Primitive.PriType)]), Primitive);
  Result.ImageIndex:=2;
  Result.SelectedIndex:=2;
end;

function TMainForm.TreeAddMesh(ParentNode: TTreeNode; Mesh: TPMBMesh): TTreeNode;
begin
  Result:=ElementsTree.Items.AddChildObject(ParentNode, RSTreeMesh, Mesh);
  Result.ImageIndex:=3;
  Result.SelectedIndex:=3;
end;

function TMainForm.TreeAddMaterial(ParentNode: TTreeNode; Mat: TPMBMaterial): TTreeNode;
begin
  Result:=ElementsTree.Items.AddChildObject(ParentNode, Format(RSTreeMaterial, [Mat.ID]), Mat);
  Result.ImageIndex:=4;
  Result.SelectedIndex:=4;
end;

procedure TMainForm.TreeRefreshCurrentItem;
begin
  with ElementsTree.Selected do
  begin
    if TObject(Data) is TPMBObject then Text:=Format(RSTreeObject, [TPMBObject(Data).ID, IntToHex(TPMBObject(Data).IID, 8)])
    else if TObject(Data) is TPMBPrimitive then Text:=Format(RSTreePrimitive, [PriTypeToString(TPMBPrimitive(Data).PriType)])
    else if TObject(Data) is TPMBMaterial then Text:=Format(RSTreeMaterial, [TPMBMaterial(Data).ID])
  end;
end;

procedure TMainForm.MMSaveAsClick(Sender: TObject);
begin
  PMSaveDialog.FileName:=ModelFileName;
  if PMSaveDialog.Execute then
  begin
    ModelFileName:=PMSaveDialog.FileName;
    FModel.SaveToFile(ModelFileName);
    ShowStatus(Format(RSModelSaved, [FlSize(ModelFileName)]));
  end;
end;

procedure TMainForm.MMSaveClick(Sender: TObject);
begin
  PMSaveDialog.FileName:=ModelFileName;
  if ModelFileName='' then
    if PMSaveDialog.Execute
      then ModelFileName:=PMSaveDialog.FileName
      else Exit;
  FModel.SaveToFile(ModelFileName);
  ShowStatus(Format(RSModelSaved, [FlSize(ModelFileName)]));
end;

procedure TMainForm.UpdateView(Sender: TObject; var Done: Boolean);
begin
  eX.Update;
  eX.Render;
  Done:=GetForegroundWindow<>Handle;
end;

procedure TMainForm.Render;
var
  SX, SY: Single;
  S: string;
begin
  glClearColor(0, 0, 0, 1);
  ogl.Clear(true, true);
  if Viewport.Width/Viewport.Height>1 then
  begin
    SX:=Viewport.Width/Viewport.Height;
    SY:=1;
  end
  else begin
    SX:=1;
    SY:=Viewport.Height/Viewport.Width;
  end;
  case ViewportMode.TabIndex of
    ViewFree:
      begin
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        gluPerspective(45, Viewport.Width/Viewport.Height, 0.001, 100);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
      end;
    ViewFront, ViewLeft, ViewTop, ViewBack, ViewRight, ViewBottom:
      begin
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity;
        glOrtho(-5*SX, 5*SX, -5*SY, 5*SY, 0.001, 100);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
      end;
    ViewUnwrap:
      begin
        ogl.Set2D(-0.1*SX, -0.1*SY, 1.2*SX, 1.2*SY);
        glTranslate((SX-1)/2, (SY-1)/2, 0);
      end;
  end;
  ogl.LightColor(GL_LIGHT0, 1, 1, 1);
  ogl.LightPos(GL_LIGHT0, 0, 0, 15);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glCullFace(GL_FRONT_AND_BACK);
  glEnable(GL_DEPTH_TEST);
  with FViews[ViewportMode.TabIndex] do
  begin
    glTranslate(PanX, PanY, PanZ);
    glRotate(Roll, 0, 0, -1);
    glRotate(Pitch, 1, 0, 0);
    glRotate(Yaw, 0, 1, 0);
    glScale(Scale, Scale, Scale);
  end;
  tex.Filter(FT_ANISOTROPY);
  case ViewportMode.TabIndex of
    ViewFree, ViewFront, ViewLeft, ViewTop, ViewBack, ViewRight, ViewBottom:
      begin
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);
        glDisable(GL_COLOR_MATERIAL);
        glDisable(GL_CULL_FACE);
        FModel.Draw;
        glDisable(GL_LIGHTING);
      end;
    ViewUnwrap: if Assigned(ElementsTree.Selected) then
      with ElementsTree.Selected do
      begin
        glDisable(GL_LIGHTING);
        glDisable(GL_COLOR_MATERIAL);
        glDisable(GL_DEPTH_TEST);
        if TObject(Data) is TPMBObject then TPMBObject(Data).DrawUV
        else if TObject(Data) is TPMBPrimitive then TPMBPrimitive(Data).DrawUV
        else if TObject(Data) is TPMBMesh then TPMBMesh(Data).DrawUV
        else if TObject(Data) is TPMBMaterial then TPMBMaterial(Data).DrawUV;
      end;
  end;
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);
  tex.Disable;
  case ViewportMode.TabIndex of
    ViewFree:
      begin
        glLoadIdentity;
        glTranslate(-0.35*Viewport.Width/Viewport.Height, -0.35, -1);
        glRotate(FViews[ViewportMode.TabIndex].Roll, 0, 0, -1);
        glRotate(FViews[ViewportMode.TabIndex].Pitch, 1, 0, 0);
        glRotate(FViews[ViewportMode.TabIndex].Yaw, 0, 1, 0);
        glBegin(GL_LINES);
          glColor3f(1, 0, 0);
          glVertex3f(0, 0, 0);
          glVertex3f(0.05, 0, 0);
          glColor3f(0, 1, 0);
          glVertex3f(0, 0, 0);
          glVertex3f(0, 0.05, 0);
          glColor3f(0, 0, 1);
          glVertex3f(0, 0, 0);
          glVertex3f(0, 0, 0.05);
        glEnd;
      end;
    ViewFront, ViewLeft, ViewTop, ViewBack, ViewRight, ViewBottom:
      begin
        glLoadIdentity;
        glTranslate(-5*SX+0.5, -5*SY+0.5, -1);
        glRotate(FViews[ViewportMode.TabIndex].Roll, 0, 0, -1);
        glRotate(FViews[ViewportMode.TabIndex].Pitch, 1, 0, 0);
        glRotate(FViews[ViewportMode.TabIndex].Yaw, 0, 1, 0);
        glBegin(GL_LINES);
          glColor3f(1, 0, 0);
          glVertex3f(0, 0, 0);
          glVertex3f(0.5, 0, 0);
          glColor3f(0, 1, 0);
          glVertex3f(0, 0, 0);
          glVertex3f(0, 0.5, 0);
          glColor3f(0, 0, 1);
          glVertex3f(0, 0, 0);
          glVertex3f(0, 0, 0.5);
        glEnd;
      end;
  end;
  ogl.Set2D(0, 0, wnd.Width, wnd.Height);
  glColor3f(1, 1, 1);
  S:='FPS: '+IntToStr(ogl.FPS);
  ogl.TextOut(FOGLFont, wnd.Width-ogl.TextLen(FOGLFont, PChar(S))-5, 5, PChar(S));
  if ViewportMode.TabIndex<>ViewUnwrap then
    with FViews[ViewportMode.TabIndex] do
    begin
      ogl.TextOut(FOGLFont, 5, 5, PChar(Format('Pos:  %5.3g; %5.3g; %5.3g', [PanX, PanY, PanZ])));
      ogl.TextOut(FOGLFont, 5, 20, PChar(Format('Rot:  %5.3g; %5.3g; %5.3g', [Yaw, Pitch, Roll])));
      ogl.TextOut(FOGLFont, 5, 35, PChar(Format('Zoom: %5.3g', [Scale])));
    end;
end;

procedure TMainForm.TreeTBAddClick(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  MenuNew.Popup(P.X, P.Y);
end;

procedure TMainForm.TreeTBRemoveClick(Sender: TObject);
begin
  if not (TObject(ElementsTree.Selected.Data) is TPMBModel) then
  begin
    if MessageDlg(RSReallyRemove, mtConfirmation, [mbYes, mbNo], 0)=mrNo then Exit;
    FrameClear;
    TObject(ElementsTree.Selected.Data).Free;
    ElementsTree.Selected.Delete;
  end;
end;

procedure TMainForm.ViewportResize(Sender: TObject);
begin
  glViewport(0, 0, Viewport.Width, Viewport.Height);
end;

function TMainForm.GetMethod(Obj: TObject; const Name: string): TMethod;
begin
  Result.Data:=Obj;
  Result.Code:=Obj.MethodAddress(Name);
end;

procedure TMainForm.PanSpeedChange(Sender: TObject);
begin
  PanSpeedLabel.Caption:=IntToStr(PanSpeed.Position);
end;

procedure TMainForm.SetAssoc(Assoc: Boolean);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    if Assoc then
    begin
      Reg.OpenKey('\.vpm', true);
      Reg.WriteString('', 'vpmfile');
      Reg.OpenKey('\vpmfile', true);
      Reg.WriteString('', 'PrimitiveModel file');
      Reg.OpenKey('DefaultIcon', true);
      Reg.WriteString('', ExpandFileName(Application.ExeName));
      Reg.OpenKey('\vpmfile\shell\open\command', true);
      Reg.WriteString('', GetAssocCmd);
    end
    else begin
      Reg.DeleteKey('\.vpm');
      Reg.DeleteKey('\vpmfile');
    end;
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TMainForm.SetModelFileName(const Value: string);
begin
  FModelFileName:=Value;
  StatusBar.Panels[1].Text:=Value;
  Caption:=FBaseCaption;
  if Value<>'' then Caption:=Caption+' ['+ExtractFileName(Value)+']';
end;

procedure TMainForm.ViewportMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then FRotating:=true;
  if Button=mbRight then FZooming:=true;
  if Button=mbMiddle then FPanning:=true;
  FLastCursorPos.X:=X;
  FLastCursorPos.Y:=Y;
end;

procedure TMainForm.ViewportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ViewportMode.TabIndex=ViewUnwrap then Exit;
  with FViews[ViewportMode.TabIndex] do
  begin
    if FRotating and (ViewportMode.TabIndex=ViewFree) then
    begin
      if ViewX.Checked then Yaw:=Yaw+(X-FLastCursorPos.X);
      if ViewY.Checked then Pitch:=Pitch+(Y-FLastCursorPos.Y);
      if ViewZ.Checked then Roll:=Roll+(Y-FLastCursorPos.Y);
    end;
    if FPanning then
    begin
      if ViewX.Checked then PanX:=PanX+0.002*PanSpeed.Position*(X-FLastCursorPos.X);
      if ViewY.Checked then PanY:=PanY-0.002*PanSpeed.Position*(Y-FLastCursorPos.Y);
      if ViewZ.Checked then PanZ:=PanZ-0.002*PanSpeed.Position*(Y-FLastCursorPos.Y);
    end;
    if FZooming
      then Scale:=Scale+0.001*ZoomSpeed.Position*Scale*(X-FLastCursorPos.X);
  end;
  FLastCursorPos.X:=X;
  FLastCursorPos.Y:=Y;
end;

procedure TMainForm.ViewportMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then FRotating:=false;
  if Button=mbRight then FZooming:=false;
  if Button=mbMiddle then FPanning:=false;
end;

procedure TMainForm.ViewResetClick(Sender: TObject);
begin
  FViews[ViewportMode.TabIndex]:=DefaultViews[ViewportMode.TabIndex];
end;

procedure TMainForm.ViewXYClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then ViewZ.Checked:=false;
end;

procedure TMainForm.ViewZClick(Sender: TObject);
begin
  ViewX.Checked:=not ViewZ.Checked;
  ViewY.Checked:=not ViewZ.Checked;
end;

procedure TMainForm.ZoomSpeedChange(Sender: TObject);
begin
  ZoomSpeedLabel.Caption:=IntToStr(ZoomSpeed.Position);
end;

end.

