object PrimitiveFrame: TPrimitiveFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 282
  TabOrder = 0
  object GroupPrimitive: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 282
    Align = alClient
    Caption = 'Primitive'
    TabOrder = 0
    DesignSize = (
      225
      282)
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
    object VisibleCheck: TCheckBox
      Left = 8
      Top = 120
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Visible'
      TabOrder = 1
      OnClick = VisibleCheckClick
    end
    object VarProps: TPanel
      Left = 2
      Top = 15
      Width = 221
      Height = 66
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      inline PriCubeFrame: TPriCubeFrame
        Left = 0
        Top = 0
        Width = 221
        Height = 66
        Align = alClient
        TabOrder = 0
        inherited CubeGroup: TGroupBox
          Width = 221
          Height = 66
          inherited TexCoordsGroup: TGroupBox
            Width = 217
            inherited SideTabs: TTabControl
              Width = 213
            end
          end
        end
      end
      inline PriSphereFrame: TPriSphereFrame
        Left = 0
        Top = 0
        Width = 221
        Height = 66
        Align = alClient
        TabOrder = 1
        inherited SphereGroup: TGroupBox
          Width = 221
          Height = 66
          inherited SpherePages: TPageControl
            Width = 217
            Height = 49
          end
        end
      end
      inline PriConeFrame: TPriConeFrame
        Left = 0
        Top = 0
        Width = 221
        Height = 66
        Align = alClient
        TabOrder = 2
        inherited ConeGroup: TGroupBox
          Width = 221
          Height = 66
          inherited PageControl1: TPageControl
            Width = 217
            Height = 49
            inherited TexCoordsSidePage: TTabSheet
              inherited SizeV: TSpinEdit
                OnChange = nil
              end
              inherited SizeU: TSpinEdit
                OnChange = nil
              end
              inherited OrigV: TSpinEdit
                OnChange = nil
              end
              inherited OrigU: TSpinEdit
                OnChange = nil
              end
            end
          end
        end
      end
    end
    object GenUVCheck: TCheckBox
      Left = 8
      Top = 88
      Width = 129
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Generate TexCoords'
      TabOrder = 3
      OnClick = GenUVCheckClick
    end
    object InvNormalsCheck: TCheckBox
      Left = 8
      Top = 104
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Invert normals'
      TabOrder = 4
      OnClick = InvNormalsCheckClick
    end
    object DrawNormalsCheck: TCheckBox
      Left = 104
      Top = 120
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Draw normals'
      TabOrder = 5
      OnClick = DrawNormalsCheckClick
    end
  end
end
