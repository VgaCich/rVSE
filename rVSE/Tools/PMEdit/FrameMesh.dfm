object MeshFrame: TMeshFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 293
  TabOrder = 0
  object GroupMesh: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 293
    Align = alClient
    Caption = 'Mesh'
    TabOrder = 0
    object MeshMode: TPageControl
      Left = 2
      Top = 15
      Width = 221
      Height = 276
      ActivePage = PageGeneral
      Align = alClient
      TabOrder = 0
      OnChange = MeshModeChange
      object PageGeneral: TTabSheet
        Caption = 'General'
        object ImportMesh: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Import'
          TabOrder = 0
          OnClick = ImportMeshClick
        end
        object VisibleCheck: TCheckBox
          Left = 8
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Visible'
          TabOrder = 1
          OnClick = VisibleCheckClick
        end
        object NormalsCheck: TCheckBox
          Left = 8
          Top = 64
          Width = 73
          Height = 17
          Caption = 'Normals'
          TabOrder = 2
          OnClick = NormalsCheckClick
        end
        object UVCheck: TCheckBox
          Left = 88
          Top = 64
          Width = 41
          Height = 17
          Caption = 'UV'
          TabOrder = 3
          OnClick = UVCheckClick
        end
        object TransformGroup: TGroupBox
          Left = 0
          Top = 104
          Width = 213
          Height = 144
          Align = alBottom
          Caption = 'Transform'
          TabOrder = 4
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
      end
      object PageVertices: TTabSheet
        Caption = 'Vertices'
        ImageIndex = 1
        DesignSize = (
          213
          248)
        object Label1: TLabel
          Left = 0
          Top = 123
          Width = 81
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Coords (X, Y, Z):'
        end
        object Label2: TLabel
          Left = 0
          Top = 203
          Width = 87
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'TexCoords (U, V):'
        end
        object Label3: TLabel
          Left = 0
          Top = 162
          Width = 97
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Normal (Phi, Theta):'
        end
        object Vertices: TListBox
          Left = 0
          Top = 0
          Width = 213
          Height = 121
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          ExtendedSelect = False
          ItemHeight = 13
          TabOrder = 0
          OnClick = VerticesClick
        end
        object VertX: TSpinEdit
          Left = 0
          Top = 139
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 127
          MinValue = -128
          TabOrder = 1
          Value = 0
          OnChange = VertChange
        end
        object VertY: TSpinEdit
          Left = 72
          Top = 139
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 127
          MinValue = -128
          TabOrder = 2
          Value = 0
          OnChange = VertChange
        end
        object VertZ: TSpinEdit
          Left = 144
          Top = 139
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 127
          MinValue = -128
          TabOrder = 3
          Value = 0
          OnChange = VertChange
        end
        object VertU: TSpinEdit
          Left = 0
          Top = 219
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = VertChange
        end
        object VertV: TSpinEdit
          Left = 72
          Top = 219
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 255
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = VertChange
        end
        object VertP: TSpinEdit
          Left = 0
          Top = 179
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 127
          MinValue = -128
          TabOrder = 6
          Value = 0
          OnChange = VertChange
        end
        object VertT: TSpinEdit
          Left = 72
          Top = 179
          Width = 65
          Height = 22
          Anchors = [akLeft, akBottom]
          MaxValue = 255
          MinValue = 0
          TabOrder = 7
          Value = 0
          OnChange = VertChange
        end
        object DrawNormalsCheck: TCheckBox
          Left = 144
          Top = 177
          Width = 65
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Draw'
          TabOrder = 8
          OnClick = DrawNormalsCheckClick
        end
      end
    end
  end
  object MeshOpenDialog: TOpenDialog
    DefaultExt = '.pmesh'
    Filter = 'PM Mesh files|*.pmesh|3ds files|*.3ds|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 112
    Top = 48
  end
end
