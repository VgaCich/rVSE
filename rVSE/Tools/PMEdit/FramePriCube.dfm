object PriCubeFrame: TPriCubeFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 163
  TabOrder = 0
  object CubeGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 163
    Align = alClient
    Caption = 'Cube'
    TabOrder = 0
    object TexCoordsGroup: TGroupBox
      Left = 2
      Top = 15
      Width = 221
      Height = 146
      Align = alTop
      Caption = 'TexCoords'
      TabOrder = 0
      object SideTabs: TTabControl
        Left = 2
        Top = 15
        Width = 217
        Height = 129
        Align = alClient
        TabOrder = 0
        Tabs.Strings = (
          'Up-Down'
          'Right-Left'
          'Front-Back')
        TabIndex = 0
        OnChange = SideTabsChange
        object SizeLabel: TLabel
          Left = 8
          Top = 80
          Width = 54
          Height = 13
          Caption = 'Size (U, V):'
        end
        object OriginLabel: TLabel
          Left = 8
          Top = 40
          Width = 63
          Height = 13
          Caption = 'Origin (U, V):'
        end
        object SplitCheck: TCheckBox
          Left = 8
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Split'
          TabOrder = 0
          OnClick = SplitCheckClick
        end
        object OrigU: TSpinEdit
          Left = 8
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = GenUVChange
        end
        object OrigV: TSpinEdit
          Left = 80
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = GenUVChange
        end
        object SizeU: TSpinEdit
          Left = 8
          Top = 96
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = GenUVChange
        end
        object SizeV: TSpinEdit
          Left = 80
          Top = 96
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = GenUVChange
        end
      end
    end
  end
end
