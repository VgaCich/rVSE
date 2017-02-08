object ObjectFrame: TObjectFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 282
  TabOrder = 0
  object GroupObject: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 282
    Align = alClient
    Caption = 'Object'
    TabOrder = 0
    DesignSize = (
      225
      282)
    object NameLabel: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object IDLabel: TLabel
      Left = 128
      Top = 16
      Width = 15
      Height = 13
      Caption = 'ID:'
    end
    object MaterialLabel: TLabel
      Left = 8
      Top = 40
      Width = 42
      Height = 13
      Caption = 'Material:'
    end
    object TransformGroup: TGroupBox
      Left = 2
      Top = 136
      Width = 221
      Height = 144
      Align = alBottom
      Caption = 'Transform'
      TabOrder = 0
      object TranslateLabel: TLabel
        Left = 8
        Top = 16
        Width = 92
        Height = 13
        Caption = 'Translate (X, Y, Z):'
      end
      object RotateLabel: TLabel
        Left = 8
        Top = 56
        Width = 122
        Height = 13
        Caption = 'Rotate (Yaw, Pitch, Roll):'
      end
      object ScaleLabel: TLabel
        Left = 8
        Top = 96
        Width = 72
        Height = 13
        Caption = 'Scale (X, Y, Z):'
      end
      object TransX: TFPSpinEdit
        Left = 8
        Top = 32
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 0
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
      object TransY: TFPSpinEdit
        Left = 80
        Top = 32
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 1
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
      object TransZ: TFPSpinEdit
        Left = 152
        Top = 32
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 2
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
      object RotYaw: TFPSpinEdit
        Left = 8
        Top = 72
        Width = 65
        Height = 22
        MaxValue = 127
        MinValue = -128
        TabOrder = 3
        Value = 0
        ValueDivider = 1
        OnChange = TransformChange
      end
      object RotPitch: TFPSpinEdit
        Left = 80
        Top = 72
        Width = 65
        Height = 22
        MaxValue = 127
        MinValue = -128
        TabOrder = 4
        Value = 0
        ValueDivider = 1
        OnChange = TransformChange
      end
      object RotRoll: TFPSpinEdit
        Left = 152
        Top = 72
        Width = 65
        Height = 22
        MaxValue = 127
        MinValue = -128
        TabOrder = 5
        Value = 0
        ValueDivider = 1
        OnChange = TransformChange
      end
      object SclX: TFPSpinEdit
        Left = 8
        Top = 112
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 6
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
      object SclY: TFPSpinEdit
        Left = 80
        Top = 112
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 7
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
      object SclZ: TFPSpinEdit
        Left = 152
        Top = 112
        Width = 65
        Height = 22
        MaxValue = 32767
        MinValue = -32768
        TabOrder = 8
        Value = 0
        ValueDivider = 512
        OnChange = TransformChange
      end
    end
    object MaterialCombo: TComboBox
      Left = 56
      Top = 40
      Width = 161
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = MaterialComboChange
    end
    object IDEdit: TEdit
      Left = 152
      Top = 16
      Width = 65
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnExit = IDEditExit
      OnKeyPress = IDEditKeyPress
    end
    object VisibleCheck: TCheckBox
      Left = 8
      Top = 64
      Width = 57
      Height = 17
      Caption = 'Visible'
      TabOrder = 3
      OnClick = VisibleCheckClick
    end
    object ObjectShowAll: TButton
      Left = 64
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Show all'
      TabOrder = 4
      OnClick = ObjectShowAllClick
    end
    object ObjectHideAll: TButton
      Left = 144
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Hide all'
      TabOrder = 5
      OnClick = ObjectHideAllClick
    end
    object NameEdit: TEdit
      Left = 56
      Top = 16
      Width = 65
      Height = 21
      MaxLength = 4
      TabOrder = 6
      OnExit = NameEditExit
      OnKeyPress = NameEditKeyPress
    end
  end
end
