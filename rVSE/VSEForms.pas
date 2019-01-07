unit VSEForms;

interface

uses
  Windows, Messages, AvL, avlUtils, VSECore, VSEOpenGLExt, VSEGUI, VSEFormManager;

type
  TOnMessageBox = procedure(Sender: TObject; BtnNum: Integer) of object; //MessageBox button click handler; BtnNum: bumber of pressed button
  TMessageBox = class(TAlignedForm) //MessageBox form
  private
    FHandler: TOnMessageBox;
    procedure Click(Btn: PBtn);
  public
    constructor Create(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox = nil); //Creates MessageBox; Caption: Form caption, Prompt: message text, Buttons: buttons' captions, Handler: button click handler
  end;
  TOptionsForm = class(TAlignedForm)
  protected
    FLResolution, FLRefreshRate, FLColorDepth, FCFullscreen, FCVSync,
      FCurrentResolution, FCurrentRefreshRate, FColorDepth, FBOK, FBCancel: Integer;
    FResolutions: TResolutions;
    FNeedRestart: Boolean;
    procedure DrawForm(State: TBtnState); override;
    procedure ResClick(Btn: PBtn);
    procedure RefrClick(Btn: PBtn);
    procedure DepthClick(Btn: PBtn);
    procedure OKClick(Btn: PBtn); dynamic;
    procedure CancelClick(Btn: PBtn);
    procedure RestartClick(Sender: TObject; BtnNum: Integer);
  public
    constructor Create(Width, Height: Integer);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  {$IFNDEF FORMS_NO_BINDMAN}
  TBindManCfgForm=class(TAlignedForm) // Keys configuration form
  private
    FLabels, FButtons: array of Integer;
    FPageLabel, FPage, FPages, FActive: Integer;
    procedure ChangePage(Btn: PBtn);
    procedure KeyBtnClick(Btn: PBtn);
    procedure CloseClick(Btn: PBtn);
    procedure DefaultClick(Btn: PBtn);
    procedure SetKey(Key: Integer);
  public
    constructor Create(Width, Height: Integer; const DefaultCapt, CloseCapt: string);
    destructor Destroy; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure Refresh;
  end;
  {$ENDIF}
  TTextView = class(TAlignedForm)
  protected
    FCurPage, FLines, FPages, FLPage: Integer;
    FText: TStringList;
    procedure DrawForm(State: TBtnState); override;
    procedure ChangePage(Btn: PBtn);
    procedure Close(Btn: PBtn);
  public
    constructor Create(Width, Height: Integer; const Caption, CloseCaption: string; Text: TStringList);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;

procedure ShowMessage(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox = nil; const Parent: string = ''); //Shows Message Box; Parent: parent form, Caption: Form caption, Prompt: message text, Buttons: buttons' captions, Handler: button click handler

implementation

uses
  VSERender2D{$IFNDEF FORMS_NO_BINDMAN}, VSEBindMan{$ENDIF};

{ TMessageBox }

constructor TMessageBox.Create(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox);
const
  BtnWidth = 75;
  WndHeight = 90;
var
  i, WndWidth: Integer;
  Lbl: TLbl;
  Btn: TBtn;
begin
  WndWidth := Min(Max(Render2D.TextWidth(GUIFont, Prompt) + 25, Length(Buttons) * (BtnWidth + 10) + 10), Render2D.VSWidth);
  inherited Create(0, 0, WndWidth, WndHeight);
  Alignment := [faCenter, faMiddle];
  FCaption := Caption;
  FHandler := Handler;
  with Lbl do
  begin
    Align := laCenter;
    Color := 0;
    X := 10;
    Y := 30;
    Width := WndWidth - 20;
    Caption := Prompt;
  end;
  AddLabel(Lbl);
  WndWidth := Max(0, WndWidth - Length(Buttons) * BtnWidth - High(Buttons) * 10) div 2;
  with Btn do
  begin
    Type_ := btPush;
    Y := 55;
    Width := BtnWidth;
    Height := 25;
    OnClick := Click;
    Enabled := true;
    for i := 0 to High(Buttons) do
    begin
      X := WndWidth + i * (BtnWidth + 10);
      Tag := i;
      Caption := Buttons[i];
      AddButton(Btn);
    end;
  end;
end;

procedure TMessageBox.Click(Btn: PBtn);
begin
  if Assigned(FHandler) then
    FHandler(Self, Btn.Tag);
  Close;
end;

{TOptionsForm}

constructor TOptionsForm.Create(Width, Height: Integer);
var
  Btn: TBtn;
  Lbl: TLbl;
  i: Integer;
begin
  inherited Create(0, 0, Width, Height);
  Alignment := [faCenter, faMiddle];
  FCaption := 'Options';
  FResolutions := gleGetResolutions;
  FLResolution := CreateSelect(Self, 10, 60, 190, 20, ResClick, '-', '+');
  FLRefreshRate := CreateSelect(Self, 10, 110, 190, 20, RefrClick, '-', '+');
  FLColorDepth := CreateSelect(Self, 10, 160, 190, 20, DepthClick, '-', '+');
  with Btn do
  begin
    Enabled := true;
    X := 10;
    Width := 190;
    Tag := 0;
    Y := 200;
    Height := 20;
    Type_ := btCheck;
    OnClick := nil;
    Caption := 'Fullscreen';
    Checked := Core.Fullscreen;
    FCFullscreen := AddButton(Btn);
    Y := 230;
    Caption := 'V. Sync';
    Checked := Core.VSync;
    FCVSync := AddButton(Btn);
    X := FWidth - 190;
    Y := FHeight - 35;
    Width := 85;
    Height := 25;
    Type_ := btPush;
    Caption := 'OK';
    OnClick := OKClick;
    FBOK := AddButton(Btn);
    X := X + Width + 10;
    Caption := 'Cancel';
    OnClick := CancelClick;
    FBCancel := AddButton(Btn);
  end;
  with Lbl do
  begin
    Align := laCenter;
    Color := 0;
    X := 10;
    Y := 38;
    Width := 190;
    Caption := 'Resolution';
    AddLabel(Lbl);
    Y := 88;
    Caption := 'Refresh rate';
    AddLabel(Lbl);
    Y := 138;
    Caption := 'Color depth';
    AddLabel(Lbl);
  end;
  FNeedRestart := false;
  FColorDepth := Core.ColorDepth;
  FCurrentResolution := -1;
  for i := 0 to High(FResolutions) do
    if (FResolutions[i].Width = Core.ResolutionX) and (FResolutions[i].Height = Core.ResolutionY) then
      FCurrentResolution := i;
  if FCurrentResolution = -1 then
  begin
    FCurrentResolution := Length(FResolutions);
    SetLength(FResolutions, FCurrentResolution + 1);
    with FResolutions[FCurrentResolution] do
    begin
      Width := Core.ResolutionX;
      Height := Core.ResolutionY;
      SetLength(RefreshRates, 1);
      RefreshRates[0] := Core.RefreshRate;
    end;
  end;
  FCurrentRefreshRate := -1;
  for i := 0 to High(FResolutions[FCurrentResolution].RefreshRates) do
    if FResolutions[FCurrentResolution].RefreshRates[i] = Core.RefreshRate then
      FCurrentRefreshRate := i;
  if FCurrentRefreshRate = -1 then
  begin
    FCurrentRefreshRate := Length(FResolutions[FCurrentResolution].RefreshRates);
    SetLength(FResolutions[FCurrentResolution].RefreshRates, FCurrentRefreshRate + 1);
    FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate] := Core.RefreshRate;
  end;
end;

destructor TOptionsForm.Destroy;
begin
  Finalize(FResolutions);
  inherited Destroy;
end;

procedure TOptionsForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key = VK_ESCAPE) and (Event = keUp) then
    CancelClick(nil)
  else
    inherited KeyEvent(Key, Event);
end;

procedure TOptionsForm.DrawForm(State: TBtnState);
begin
  Lbl[FLResolution].Caption := Format('%dx%d', [FResolutions[FCurrentResolution].Width, FResolutions[FCurrentResolution].Height]);
  Lbl[FLRefreshRate].Caption := IntToStr(FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]);
  Lbl[FLColorDepth].Caption := IntToStr(FColorDepth);
  inherited;
end;

procedure TOptionsForm.ResClick(Btn: PBtn);
begin
  FCurrentResolution := Max(0, Min(FCurrentResolution + Btn.Tag, High(FResolutions)));
  FCurrentRefreshRate := 0;
end;

procedure TOptionsForm.RefrClick(Btn: PBtn);
begin
  FCurrentRefreshRate := Max(0, Min(FCurrentRefreshRate + Btn.Tag, High(FResolutions[FCurrentResolution].RefreshRates)));
end;

procedure TOptionsForm.DepthClick(Btn: PBtn);
const
  Depth: array[-1..1] of Integer = (16, 0, 32);
begin
  FColorDepth := Depth[Btn.Tag];
end;

procedure TOptionsForm.OKClick(Btn: PBtn);
begin
  FNeedRestart := FNeedRestart or (Core.ColorDepth <> FColorDepth);
  with FResolutions[FCurrentResolution] do
    if (Core.ResolutionX <> Width) or (Core.ResolutionY <> Height) or (Core.RefreshRate <> RefreshRates[FCurrentRefreshRate]) then
      Core.SetResolution(Width, Height, RefreshRates[FCurrentRefreshRate], Button[FCFullscreen].Checked, true)
    else
      Core.Fullscreen := Button[FCFullscreen].Checked;
  Core.VSync := Button[FCVSync].Checked;
  Core.ColorDepth := FColorDepth;
  if FNeedRestart then
    ShowMessage(Caption, 'Выбранные настройки требуют перезапуска. Перезапустить игру?', ['Да', 'Нет'], RestartClick, Name)
  else
    Close;
  FNeedRestart := false;
end;

procedure TOptionsForm.CancelClick(Btn: PBtn);
begin
  FNeedRestart := false;
  Close;
end;

procedure TOptionsForm.RestartClick(Sender: TObject; BtnNum: Integer);
begin
  if BtnNum = 0 then
    Core.StopEngine(StopNeedRestart);
  Close;
end;

{ TBindManCfgForm }

{$IFNDEF FORMS_NO_BINDMAN}
const
  PageLabel = '%d/%d';

constructor TBindManCfgForm.Create(Width, Height: Integer; const DefaultCapt, CloseCapt: string);
const
  BHeight = 25;
var
  Btn: TBtn;
  Lbl: TLbl;
  i: Integer;
begin
  inherited Create(0, 0, Width, Height);
  Alignment := [faCenter, faMiddle];
  Caption := 'Controls';
  FActive := -1;
  SetLength(FLabels, Min(Length(BindMan.Bindings), (Height - 20 - Render2D.TextHeight(Font)) div (BHeight + 10)));
  SetLength(FButtons, Length(FLabels));
  FPages := High(BindMan.Bindings) div Max(Length(FLabels), 1);
  with Btn do
  begin
    Type_ := btPush;
    X := 2*FWidth div 3;
    Width := FWidth - X - 10;
    Height := BHeight;
    OnClick := KeyBtnClick;
    Enabled := true;
  end;
  with Lbl do
  begin
    X := 10;
    Width := Btn.X - 20;
    Align := laLeft;
    Color := 0;
  end;
  for i := 0 to High(FLabels) do
  begin
    with Btn do
    begin
      Y := 20 + Render2D.TextHeight(Font) + i * (BHeight + 10);
      Tag := i;
    end;
    Lbl.Y := 25 + Render2D.TextHeight(Font) + i * (BHeight + 10);
    FButtons[i] := AddButton(Btn);
    FLabels[i] := AddLabel(Lbl);
  end;
  Refresh;
  if FPages > 0 then
  begin
    FPageLabel := CreateSelect(Self, 10, Height - 40, Min(Width div 2, Width - 280), 30, ChangePage, '<', '>');
    Self.Lbl[FPageLabel]^.Caption := Format(PageLabel, [FPage + 1, FPages + 1]);
  end;
  with Btn do
  begin
    Width := 120;
    Y := FHeight - 40;
    X := FWidth - 260;
    Caption := DefaultCapt;
    OnClick := DefaultClick;
    AddButton(Btn);
    X := FWidth - 130;
    Caption := CloseCapt;
    OnClick := CloseClick;
    AddButton(Btn);
  end;
end;

procedure TBindManCfgForm.ChangePage(Btn: PBtn);
begin
  FPage := Min(FPages, Max(0, FPage + Btn^.Tag));
  Lbl[FPageLabel].Caption := Format(PageLabel, [FPage + 1, FPages + 1]);
  Refresh;
end;

procedure TBindManCfgForm.Refresh;
var
  i: Integer;
begin
  for i := 0 to High(FLabels) do
  begin
    if FPage * Length(FLabels) + i > High(BindMan.Bindings) then
    begin
      Lbl[FLabels[i]]^.Caption := '';
      with Button[FButtons[i]]^ do
      begin
        Caption := '';
        Enabled := false;
      end;
      Continue;
    end;
    Lbl[FLabels[i]]^.Caption := BindMan.Bindings[FPage * Length(FLabels) + i].Description;
    with Button[FButtons[i]]^ do
    begin
      Caption := KeyToStr(BindMan.Bindings[FPage * Length(FLabels) + i].Key);
      Enabled := true;
    end;
  end;
end;

procedure TBindManCfgForm.KeyBtnClick(Btn: PBtn);
begin
  FActive := FPage * Length(FLabels) + Btn^.Tag;
  Btn^.Caption := '???';
end;

procedure TBindManCfgForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if FActive > -1 then
  begin
    if Event <> keUp then Exit;
    if Key = VK_ESCAPE then
    begin
      FActive := -1;
      Refresh;
    end
      else if Key = VK_BACK
        then SetKey(0)
        else SetKey(Key);
  end
  else begin
    if (Event = keUp) and (Key = VK_ESCAPE)
      then CloseClick(nil)
      else inherited KeyEvent(Key, Event);
  end;
end;

procedure TBindManCfgForm.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if FActive > -1 then
  begin
    if not (Event in [meDown, meWheel]) then Exit;
    if Event = meWheel then
    begin
      if Button > 0
        then SetKey(VK_MWHEELUP)
        else SetKey(VK_MWHEELDOWN);
    end
      else SetKey(MBtnMap[Button]);
  end
    else inherited MouseEvent(Button, Event, X, Y);
end;

procedure TBindManCfgForm.CloseClick(Btn: PBtn);
begin
  BindMan.SaveBindings;
  Close;
end;

destructor TBindManCfgForm.Destroy;
begin
  Finalize(FLabels);
  Finalize(FButtons);
  inherited Destroy;
end;

procedure TBindManCfgForm.DefaultClick(Btn: PBtn);
begin
  Settings.EraseSection(SSectionBindings);
  BindMan.ResetKeys;
  Refresh;
end;

procedure TBindManCfgForm.SetKey(Key: Integer);
var
  i: Integer;
begin
  if Key in [VK_SNAPSHOT] then Exit;
  for i := 0 to High(BindMan.Bindings) do
    if (i <> FActive) and (BindMan.Bindings[i].Key = Key)
      then BindMan.Bindings[i].Key := BindMan.Bindings[FActive].Key;
  BindMan.Bindings[FActive].Key := Key;
  FActive := -1;
  Refresh;
end;
{$ENDIF}

{ TTextView }

constructor TTextView.Create(Width, Height: Integer; const Caption, CloseCaption: string; Text: TStringList);
var
  Line: Integer;
  Src, Dst: string;
  Btn: TBtn;
begin
  inherited Create(0, 0, Width, Height);
  Alignment := [faCenter, faMiddle];
  FCaption := Caption;
  FText := Text;
  FLines := (Height - 70) div 16;
  Line := 0;
  while Line < FText.Count do
  begin
    Src := {$IFNDEF FORMS_NO_BINDMAN}ProcessKeyTags(FText[Line]){$ELSE}FText[Line]{$ENDIF};
    Dst := '';
    while (Src <> '') and (Render2D.TextWidth(Font, Src) > Width - 20) do
    begin
      Dst := Src[Length(Src)] + Dst;
      Delete(Src, Length(Src), 1);
    end;
    FText[Line] := Src;
    if Dst <> '' then
      FText.Insert(Line + 1, Dst);
    Inc(Line);
  end;
  FPages := (FText.Count - 1) div FLines;
  FCurPage := 0;
  with Btn do
  begin
    Type_ := btPush;
    X := FWidth - 95;
    Y := FHeight - 35;
    Width := 85;
    Height := 25;
    Enabled := true;
    Caption := CloseCaption;
    OnClick := Close;
    AddButton(Btn);
  end;
  if FPages > 0 then
    FLPage := CreateSelect(Self, 10, Height - 35, Min(150, Width - 115), 25, ChangePage, '<', '>');
end;

destructor TTextView.Destroy;
begin
  FAN(FText);
  inherited Destroy;
end;

procedure TTextView.KeyEvent(Key: Integer; Event: TKeyEvent);
var
  Btn: TBtn;
begin
  if Event = keDown then
    case Key of
      VK_LEFT:
        begin
          Btn.Tag := -1;
          ChangePage(@Btn);
        end;
      VK_RIGHT:
        begin
          Btn.Tag := 1;
          ChangePage(@Btn);
        end;
    end
  else if Key = VK_ESCAPE then
    Close(nil);
  inherited KeyEvent(Key, Event);
end;

procedure TTextView.DrawForm(State: TBtnState);
var
  i, Left: Integer;
  S: string;
begin
  if FPages > 0 then
    Lbl[FLPage].Caption := Format('%d/%d', [FCurPage + 1, FPages + 1]);
  inherited;
  gleColor(clText);
  for i := 0 to FLines - 1 do
    if FLines * FCurPage + i < FText.Count then
    begin
      S := FText[FLines * FCurPage + i];
      Left := 10;
      if (S <> '') and (S[1] = #9) then
      begin
        S := Copy(S, 2, MaxInt);
        Inc(Left, (Width - Render2D.TextWidth(Font, S) - 20) div 2);
      end;
      Render2D.TextOut(Font, Left, 35 + 16 * i, S);
    end;
end;

procedure TTextView.ChangePage(Btn: PBtn);
begin
  FCurPage := Max(0, Min(FCurPage + Btn.Tag, FPages));
end;

procedure TTextView.Close(Btn: PBtn);
begin
  inherited Close;
end;

{ Functions }

procedure ShowMessage(const Caption, Prompt: string; const Buttons: array of string; Handler: TOnMessageBox = nil; const Parent: string = '');
begin
  if not Assigned(FormManager.FormsSet) then Exit;
  FormManager.Show(FormManager.FormsSet.AddForm('MessageBox:' + Prompt, TMessageBox.Create(Caption, Prompt, Buttons, Handler), Parent).Name);
end;

end.