object ModelFrame: TModelFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 282
  TabOrder = 0
  object GroupModel: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 282
    Align = alClient
    Caption = 'Model'
    TabOrder = 0
    object ModelShowAll: TButton
      Left = 8
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Show all'
      TabOrder = 0
      OnClick = ModelShowAllClick
    end
    object ModelHideAll: TButton
      Left = 88
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Hide all'
      TabOrder = 1
      OnClick = ModelHideAllClick
    end
  end
end
