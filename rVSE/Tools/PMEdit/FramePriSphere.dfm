object PriSphereFrame: TPriSphereFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 138
  TabOrder = 0
  object SphereGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 138
    Align = alClient
    Caption = 'Sphere'
    TabOrder = 0
    object SpherePages: TPageControl
      Left = 2
      Top = 15
      Width = 221
      Height = 121
      ActivePage = GeomPage
      Align = alClient
      TabOrder = 0
      object GeomPage: TTabSheet
        Caption = 'Geometry'
        object Label1: TLabel
          Left = 0
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Slices:'
        end
        object Label2: TLabel
          Left = 0
          Top = 48
          Width = 35
          Height = 13
          Caption = 'Stacks:'
        end
        object Label3: TLabel
          Left = 40
          Top = 0
          Width = 33
          Height = 13
          Caption = 'Count:'
        end
        object Label4: TLabel
          Left = 112
          Top = 0
          Width = 35
          Height = 13
          Caption = 'Sector:'
        end
        object Slices: TSpinEdit
          Left = 40
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = SlicesChange
        end
        object SmoothCheck: TCheckBox
          Left = 40
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Smooth'
          TabOrder = 1
          OnClick = SmoothCheckClick
        end
        object Stacks: TSpinEdit
          Left = 40
          Top = 48
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = StacksChange
        end
        object SlicesSector: TSpinEdit
          Left = 112
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = SlicesSectorChange
        end
        object StacksSector: TSpinEdit
          Left = 112
          Top = 48
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = StacksSectorChange
        end
      end
      object TexCoordsPage: TTabSheet
        Caption = 'TexCoords'
        ImageIndex = 1
        object OriginLabel: TLabel
          Left = 8
          Top = 0
          Width = 63
          Height = 13
          Caption = 'Origin (U, V):'
        end
        object SizeLabel: TLabel
          Left = 8
          Top = 40
          Width = 54
          Height = 13
          Caption = 'Size (U, V):'
        end
        object SizeV: TSpinEdit
          Left = 80
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = GenUVChange
        end
        object SizeU: TSpinEdit
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
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = GenUVChange
        end
        object OrigU: TSpinEdit
          Left = 8
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = GenUVChange
        end
      end
    end
  end
end
