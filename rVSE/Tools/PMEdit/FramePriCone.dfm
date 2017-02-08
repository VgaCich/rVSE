object PriConeFrame: TPriConeFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 145
  TabOrder = 0
  object ConeGroup: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 145
    Align = alClient
    Caption = 'Cone'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 15
      Width = 221
      Height = 128
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
          Left = 40
          Top = 0
          Width = 33
          Height = 13
          Caption = 'Count:'
        end
        object Label3: TLabel
          Left = 112
          Top = 0
          Width = 35
          Height = 13
          Caption = 'Sector:'
        end
        object Label4: TLabel
          Left = 40
          Top = 40
          Width = 22
          Height = 13
          Caption = 'Top:'
        end
        object Label5: TLabel
          Left = 112
          Top = 40
          Width = 38
          Height = 13
          Caption = 'Bottom:'
        end
        object Label6: TLabel
          Left = 0
          Top = 56
          Width = 36
          Height = 13
          Caption = 'Radius:'
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
        object SlicesSector: TSpinEdit
          Left = 112
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = SlicesSectorChange
        end
        object RadiusT: TFPSpinEdit
          Left = 40
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 2
          Value = 0
          ValueDivider = 255
          OnChange = RadiusTChange
        end
        object RadiusB: TFPSpinEdit
          Left = 112
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          ValueDivider = 255
          OnChange = RadiusBChange
        end
        object SmoothCheck: TCheckBox
          Left = 40
          Top = 80
          Width = 97
          Height = 17
          Caption = 'Smooth'
          TabOrder = 4
          OnClick = SmoothCheckClick
        end
      end
      object TexCoordsSidePage: TTabSheet
        Caption = 'TC Side'
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
          OnChange = UVSideChange
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
          OnChange = UVSideChange
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
          OnChange = UVSideChange
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
          OnChange = UVSideChange
        end
      end
      object TexCoordsBasePage: TTabSheet
        Caption = 'TC Base'
        ImageIndex = 2
        object Label7: TLabel
          Left = 8
          Top = 0
          Width = 136
          Height = 13
          Caption = 'Top (Center (U, V), Radius):'
        end
        object Label8: TLabel
          Left = 8
          Top = 40
          Width = 152
          Height = 13
          Caption = 'Bottom (Center (U, V), Radius):'
        end
        object UVCenterUT: TSpinEdit
          Left = 8
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = UVBaseTChange
        end
        object UVCenterVT: TSpinEdit
          Left = 80
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = UVBaseTChange
        end
        object UVRadiusT: TSpinEdit
          Left = 152
          Top = 16
          Width = 65
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = UVBaseTChange
        end
        object UVCenterUB: TSpinEdit
          Left = 8
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = UVBaseBChange
        end
        object UVCenterVB: TSpinEdit
          Left = 80
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = UVBaseBChange
        end
        object UVRadiusB: TSpinEdit
          Left = 152
          Top = 56
          Width = 65
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = UVBaseBChange
        end
      end
    end
  end
end
