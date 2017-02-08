object MaterialFrame: TMaterialFrame
  Left = 0
  Top = 0
  Width = 225
  Height = 295
  Constraints.MinHeight = 295
  TabOrder = 0
  object GroupMaterial: TGroupBox
    Left = 0
    Top = 0
    Width = 225
    Height = 295
    Align = alClient
    Caption = 'Material'
    TabOrder = 0
    DesignSize = (
      225
      295)
    object AmbientPaint: TPaintBox
      Left = 56
      Top = 64
      Width = 155
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      OnClick = PaintClick
      OnPaint = ColorPaint
    end
    object SpecularPaint: TPaintBox
      Tag = 3
      Left = 56
      Top = 112
      Width = 155
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      OnClick = PaintClick
      OnPaint = ColorPaint
    end
    object EmissionPaint: TPaintBox
      Tag = 2
      Left = 56
      Top = 160
      Width = 155
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      OnClick = PaintClick
      OnPaint = ColorPaint
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 38
      Height = 13
      Caption = 'Diffuse:'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 43
      Height = 13
      Caption = 'Ambient:'
    end
    object Label3: TLabel
      Left = 8
      Top = 112
      Width = 45
      Height = 13
      Caption = 'Specular:'
    end
    object Label4: TLabel
      Left = 8
      Top = 160
      Width = 44
      Height = 13
      Caption = 'Emission:'
    end
    object ShininessLabel: TLabel
      Left = 8
      Top = 208
      Width = 48
      Height = 13
      Caption = 'Shininess:'
    end
    object Label6: TLabel
      Left = 8
      Top = 248
      Width = 42
      Height = 13
      Caption = 'Texture:'
    end
    object DiffusePaint: TPaintBox
      Tag = 1
      Left = 56
      Top = 16
      Width = 155
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      OnClick = PaintClick
      OnPaint = ColorPaint
    end
    object DiffAlphaLabel: TLabel
      Left = 8
      Top = 40
      Width = 33
      Height = 13
      Caption = 'A=255'
    end
    object AmbiAlphaLabel: TLabel
      Left = 8
      Top = 88
      Width = 33
      Height = 13
      Caption = 'A=255'
    end
    object SpecAlphaLabel: TLabel
      Left = 8
      Top = 136
      Width = 33
      Height = 13
      Caption = 'A=255'
    end
    object EmiAlphaLabel: TLabel
      Left = 8
      Top = 184
      Width = 33
      Height = 13
      Caption = 'A=255'
    end
    object DiffuseAlpha: TTrackBar
      Tag = 1
      Left = 40
      Top = 40
      Width = 179
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      LineSize = 0
      Max = 255
      PageSize = 0
      Frequency = 0
      TabOrder = 2
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = AlphaChange
      OnKeyDown = TrackKeyDown
    end
    object Shininess: TTrackBar
      Left = 8
      Top = 224
      Width = 211
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      LineSize = 0
      Max = 255
      TabOrder = 0
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = ShininessChange
      OnKeyDown = TrackKeyDown
    end
    object Texture: TComboBox
      Left = 8
      Top = 264
      Width = 207
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnExit = TextureExit
      OnKeyPress = TextureKeyPress
      OnSelect = TextureExit
    end
    object EmissionAlpha: TTrackBar
      Tag = 2
      Left = 40
      Top = 184
      Width = 179
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      LineSize = 0
      Max = 255
      PageSize = 0
      Frequency = 0
      TabOrder = 3
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = AlphaChange
      OnKeyDown = TrackKeyDown
    end
    object SpecularAlpha: TTrackBar
      Tag = 3
      Left = 40
      Top = 136
      Width = 179
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      LineSize = 0
      Max = 255
      PageSize = 0
      Frequency = 0
      TabOrder = 4
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = AlphaChange
      OnKeyDown = TrackKeyDown
    end
    object AmbientAlpha: TTrackBar
      Left = 40
      Top = 88
      Width = 179
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      LineSize = 0
      Max = 255
      PageSize = 0
      Frequency = 0
      TabOrder = 5
      ThumbLength = 15
      TickStyle = tsNone
      OnChange = AlphaChange
      OnKeyDown = TrackKeyDown
    end
  end
  object ColorSelect: TColorDialog
    Options = [cdFullOpen]
    Left = 80
    Top = 232
  end
end
