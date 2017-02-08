object PrefsForm: TPrefsForm
  Left = 191
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 134
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  DesignSize = (
    377
    134)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 213
    Top = 100
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 293
    Top = 100
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupTextures: TGroupBox
    Left = 8
    Top = 8
    Width = 362
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Textures'
    TabOrder = 2
    DesignSize = (
      362
      65)
    object LabelPath: TLabel
      Left = 8
      Top = 16
      Width = 26
      Height = 13
      Caption = 'Path:'
    end
    object EditTexPath: TEdit
      Left = 8
      Top = 32
      Width = 321
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object TexPathBrowse: TButton
      Left = 328
      Top = 32
      Width = 25
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = TexPathBrowseClick
    end
  end
  object CheckAssoc: TCheckBox
    Left = 8
    Top = 80
    Width = 145
    Height = 17
    Caption = 'Associate with *.vpm'
    TabOrder = 3
  end
  object TexPathSelect: TSDDialog
    Left = 272
    Top = 32
  end
end
