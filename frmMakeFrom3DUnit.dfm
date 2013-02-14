object frmMakeFrom3D: TfrmMakeFrom3D
  Left = 299
  Top = 122
  Width = 687
  Height = 750
  Caption = 'Rooftops and Columns'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object imgMain: TImage
    Left = 276
    Top = 8
    Width = 394
    Height = 394
  end
  object grpSymmetry: TGroupBox
    Left = 8
    Top = 0
    Width = 257
    Height = 129
    Caption = 'Symmetry'
    TabOrder = 0
    object lblSides: TLabel
      Left = 158
      Top = 30
      Width = 80
      Height = 16
      Caption = 'Sides/Spars:'
    end
    object rbCone: TRadioButton
      Left = 10
      Top = 22
      Width = 90
      Height = 20
      Caption = 'Cone'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbConeClick
    end
    object edtPolygonSides: TEdit
      Left = 158
      Top = 59
      Width = 60
      Height = 24
      TabOrder = 1
      Text = '4'
      OnChange = edtPolygonSidesChange
    end
    object udPolygonSides: TUpDown
      Left = 218
      Top = 59
      Width = 18
      Height = 24
      Associate = edtPolygonSides
      Min = 3
      Max = 16
      Position = 4
      TabOrder = 2
      Wrap = False
    end
    object rbCylindrical: TRadioButton
      Left = 10
      Top = 43
      Width = 100
      Height = 21
      Caption = 'Cylindrical'
      TabOrder = 3
      OnClick = rbCylindricalClick
    end
    object cbPolygonal: TCheckBox
      Left = 158
      Top = 90
      Width = 89
      Height = 21
      Caption = 'Polygonal'
      TabOrder = 4
      OnClick = cbPolygonalClick
    end
    object rbSpherical: TRadioButton
      Left = 10
      Top = 65
      Width = 100
      Height = 21
      Caption = 'Spherical'
      TabOrder = 5
      OnClick = rbSphericalClick
    end
    object rbRectangular: TRadioButton
      Left = 8
      Top = 88
      Width = 113
      Height = 17
      Caption = 'Rectangular'
      TabOrder = 6
      OnClick = rbRectangularClick
    end
  end
  object grpShape: TGroupBox
    Left = 8
    Top = 130
    Width = 257
    Height = 407
    Caption = 'Shape'
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 22
      Width = 99
      Height = 16
      Caption = 'Elasticity (0-100):'
    end
    object Label1: TLabel
      Left = 10
      Top = 126
      Width = 92
      Height = 16
      Caption = 'Height (0-1000):'
    end
    object Label24: TLabel
      Left = 10
      Top = 272
      Width = 111
      Height = 16
      Caption = 'Top radius (0-100):'
    end
    object Bevel1: TBevel
      Left = 20
      Top = 112
      Width = 218
      Height = 5
      Shape = bsTopLine
    end
    object Bevel2: TBevel
      Left = 20
      Top = 260
      Width = 218
      Height = 5
      Shape = bsTopLine
    end
    object Label25: TLabel
      Left = 10
      Top = 343
      Width = 128
      Height = 16
      Caption = 'Bottom radius (0-100):'
    end
    object Label35: TLabel
      Left = 10
      Top = 192
      Width = 108
      Height = 16
      Caption = 'Thickness (0-100):'
    end
    object edtElasticity: TEdit
      Left = 158
      Top = 22
      Width = 60
      Height = 24
      TabOrder = 0
      Text = '0'
      OnChange = edtElasticityChange
    end
    object udElasticity: TUpDown
      Left = 218
      Top = 22
      Width = 18
      Height = 24
      Associate = edtElasticity
      Min = 0
      Position = 0
      TabOrder = 1
      Wrap = False
      OnClick = udElasticityClick
    end
    object tbElasticity: TTrackBar
      Left = 5
      Top = 51
      Width = 244
      Height = 31
      Max = 100
      Orientation = trHorizontal
      PageSize = 5
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbElasticityChange
    end
    object cbTop: TCheckBox
      Left = 69
      Top = 82
      Width = 70
      Height = 21
      Caption = 'Top'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
      OnClick = cbTopClick
    end
    object cbSides: TCheckBox
      Left = 158
      Top = 82
      Width = 80
      Height = 21
      Caption = 'Sides'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 4
      OnClick = cbSidesClick
    end
    object edtCenterHeight: TEdit
      Left = 158
      Top = 126
      Width = 60
      Height = 24
      TabOrder = 5
      Text = '100'
      OnChange = edtCenterHeightChange
    end
    object udCenterHeight: TUpDown
      Left = 218
      Top = 126
      Width = 18
      Height = 24
      Associate = edtCenterHeight
      Min = 0
      Max = 1000
      Position = 100
      TabOrder = 6
      Wrap = False
      OnClick = udCenterHeightClick
    end
    object tbCenterHeight: TTrackBar
      Left = 5
      Top = 155
      Width = 244
      Height = 31
      Max = 1000
      Orientation = trHorizontal
      PageSize = 50
      Frequency = 100
      Position = 100
      SelEnd = 0
      SelStart = 0
      TabOrder = 7
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbCenterHeightChange
    end
    object edtInnerRadius: TEdit
      Left = 158
      Top = 272
      Width = 60
      Height = 24
      TabOrder = 8
      Text = '0'
      OnChange = edtInnerRadiusChange
    end
    object udInnerRadius: TUpDown
      Left = 218
      Top = 272
      Width = 18
      Height = 24
      Associate = edtInnerRadius
      Min = 0
      Position = 0
      TabOrder = 9
      Wrap = False
      OnClick = udInnerRadiusClick
    end
    object tbInnerRadius: TTrackBar
      Left = 5
      Top = 302
      Width = 244
      Height = 30
      Max = 100
      Orientation = trHorizontal
      PageSize = 5
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 10
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbInnerRadiusChange
    end
    object edtOuterRadius: TEdit
      Left = 158
      Top = 343
      Width = 60
      Height = 24
      TabOrder = 11
      Text = '100'
      OnChange = edtOuterRadiusChange
    end
    object udOuterRadius: TUpDown
      Left = 218
      Top = 343
      Width = 18
      Height = 24
      Associate = edtOuterRadius
      Min = 0
      Position = 100
      TabOrder = 12
      Wrap = False
      OnClick = udOuterRadiusClick
    end
    object tbOuterRadius: TTrackBar
      Left = 5
      Top = 372
      Width = 244
      Height = 31
      Max = 100
      Orientation = trHorizontal
      PageSize = 5
      Frequency = 10
      Position = 100
      SelEnd = 0
      SelStart = 0
      TabOrder = 13
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbOuterRadiusChange
    end
    object edtThickness: TEdit
      Left = 158
      Top = 192
      Width = 60
      Height = 24
      TabOrder = 14
      Text = '0'
      OnChange = edtThicknessChange
    end
    object udThickness: TUpDown
      Left = 218
      Top = 192
      Width = 19
      Height = 24
      Associate = edtThickness
      Min = 0
      Position = 0
      TabOrder = 15
      Wrap = False
      OnClick = udThicknessClick
    end
    object tbThickness: TTrackBar
      Left = 5
      Top = 224
      Width = 244
      Height = 31
      Max = 100
      Orientation = trHorizontal
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 16
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbThicknessChange
    end
  end
  object PageControl1: TPageControl
    Left = 276
    Top = 406
    Width = 395
    Height = 276
    ActivePage = TabSheet1
    TabIndex = 0
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Model Color'
      object Label6: TLabel
        Left = 10
        Top = 10
        Width = 26
        Height = 16
        Caption = 'Red'
      end
      object Label7: TLabel
        Left = 10
        Top = 49
        Width = 37
        Height = 16
        Caption = 'Green'
      end
      object Label8: TLabel
        Left = 10
        Top = 89
        Width = 27
        Height = 16
        Caption = 'Blue'
      end
      object Label9: TLabel
        Left = 10
        Top = 128
        Width = 25
        Height = 16
        Caption = 'Hue'
      end
      object Label10: TLabel
        Left = 10
        Top = 167
        Width = 60
        Height = 16
        Caption = 'Saturation'
      end
      object Label11: TLabel
        Left = 10
        Top = 207
        Width = 65
        Height = 16
        Caption = 'Luminance'
      end
      object edtMR: TEdit
        Left = 98
        Top = 10
        Width = 61
        Height = 24
        TabOrder = 0
        Text = '0'
        OnChange = edtMRChange
      end
      object edtMG: TEdit
        Left = 98
        Top = 49
        Width = 61
        Height = 24
        TabOrder = 1
        Text = '0'
        OnChange = edtMGChange
      end
      object edtMB: TEdit
        Left = 98
        Top = 89
        Width = 61
        Height = 24
        TabOrder = 2
        Text = '0'
        OnChange = edtMBChange
      end
      object edtMH: TEdit
        Left = 98
        Top = 128
        Width = 61
        Height = 24
        TabOrder = 3
        Text = '0'
        OnChange = edtMHChange
      end
      object edtMS: TEdit
        Left = 98
        Top = 167
        Width = 61
        Height = 24
        TabOrder = 4
        Text = '0'
        OnChange = edtMSChange
      end
      object edtMV: TEdit
        Left = 98
        Top = 207
        Width = 61
        Height = 24
        TabOrder = 5
        Text = '0'
        OnChange = edtMVChange
      end
      object udMR: TUpDown
        Left = 159
        Top = 10
        Width = 18
        Height = 24
        Associate = edtMR
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 6
        Wrap = False
        OnClick = udMRClick
      end
      object udMG: TUpDown
        Left = 159
        Top = 49
        Width = 18
        Height = 24
        Associate = edtMG
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 7
        Wrap = False
        OnClick = udMGClick
      end
      object udMB: TUpDown
        Left = 159
        Top = 89
        Width = 18
        Height = 24
        Associate = edtMB
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 8
        Wrap = False
        OnClick = udMBClick
      end
      object udMH: TUpDown
        Left = 159
        Top = 128
        Width = 18
        Height = 24
        Associate = edtMH
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 9
        Wrap = False
        OnClick = udMHClick
      end
      object udMS: TUpDown
        Left = 159
        Top = 167
        Width = 18
        Height = 24
        Associate = edtMS
        Min = 0
        Position = 0
        TabOrder = 10
        Wrap = False
        OnClick = udMSClick
      end
      object udMV: TUpDown
        Left = 159
        Top = 207
        Width = 18
        Height = 24
        Associate = edtMV
        Min = 0
        Position = 0
        TabOrder = 11
        Wrap = False
        OnClick = udMVClick
      end
      object tbMR: TTrackBar
        Left = 187
        Top = 10
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 12
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMRChange
      end
      object tbMG: TTrackBar
        Left = 187
        Top = 49
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 13
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMGChange
      end
      object tbMB: TTrackBar
        Left = 187
        Top = 89
        Width = 129
        Height = 30
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 14
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMBChange
      end
      object tbMH: TTrackBar
        Left = 187
        Top = 128
        Width = 129
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 15
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMHChange
      end
      object tbMS: TTrackBar
        Left = 187
        Top = 167
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 16
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMSChange
      end
      object tbMV: TTrackBar
        Left = 187
        Top = 207
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 17
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbMVChange
      end
      object pnlModelColor: TPanel
        Left = 325
        Top = 10
        Width = 50
        Height = 228
        BevelOuter = bvLowered
        TabOrder = 18
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Light Color'
      ImageIndex = 1
      object Label12: TLabel
        Left = 10
        Top = 10
        Width = 26
        Height = 16
        Caption = 'Red'
      end
      object Label13: TLabel
        Left = 10
        Top = 49
        Width = 37
        Height = 16
        Caption = 'Green'
      end
      object Label14: TLabel
        Left = 10
        Top = 89
        Width = 27
        Height = 16
        Caption = 'Blue'
      end
      object Label15: TLabel
        Left = 10
        Top = 128
        Width = 25
        Height = 16
        Caption = 'Hue'
      end
      object Label16: TLabel
        Left = 10
        Top = 167
        Width = 60
        Height = 16
        Caption = 'Saturation'
      end
      object Label17: TLabel
        Left = 10
        Top = 207
        Width = 65
        Height = 16
        Caption = 'Luminance'
      end
      object edtLR: TEdit
        Left = 98
        Top = 10
        Width = 61
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = edtLRChange
      end
      object edtLG: TEdit
        Left = 98
        Top = 49
        Width = 61
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = edtLGChange
      end
      object edtLB: TEdit
        Left = 98
        Top = 89
        Width = 61
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = edtLBChange
      end
      object edtLH: TEdit
        Left = 98
        Top = 128
        Width = 61
        Height = 21
        TabOrder = 3
        Text = '0'
        OnChange = edtLHChange
      end
      object edtLS: TEdit
        Left = 98
        Top = 167
        Width = 61
        Height = 21
        TabOrder = 4
        Text = '0'
        OnChange = edtLSChange
      end
      object edtLV: TEdit
        Left = 98
        Top = 207
        Width = 61
        Height = 21
        TabOrder = 5
        Text = '0'
        OnChange = edtLVChange
      end
      object udLR: TUpDown
        Left = 159
        Top = 10
        Width = 18
        Height = 26
        Associate = edtLR
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 6
        Wrap = False
        OnClick = udLRClick
      end
      object udLG: TUpDown
        Left = 159
        Top = 49
        Width = 18
        Height = 26
        Associate = edtLG
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 7
        Wrap = False
        OnClick = udLGClick
      end
      object udLB: TUpDown
        Left = 159
        Top = 89
        Width = 18
        Height = 25
        Associate = edtLB
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 8
        Wrap = False
        OnClick = udLBClick
      end
      object udLH: TUpDown
        Left = 159
        Top = 128
        Width = 18
        Height = 26
        Associate = edtLH
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 9
        Wrap = False
        OnClick = udLHClick
      end
      object udLS: TUpDown
        Left = 159
        Top = 167
        Width = 18
        Height = 26
        Associate = edtLS
        Min = 0
        Position = 0
        TabOrder = 10
        Wrap = False
        OnClick = udLSClick
      end
      object udLV: TUpDown
        Left = 159
        Top = 207
        Width = 18
        Height = 26
        Associate = edtLV
        Min = 0
        Position = 0
        TabOrder = 11
        Wrap = False
        OnClick = udLVClick
      end
      object tbLR: TTrackBar
        Left = 187
        Top = 10
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 12
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLRChange
      end
      object tbLG: TTrackBar
        Left = 187
        Top = 49
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 13
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLGChange
      end
      object tbLB: TTrackBar
        Left = 187
        Top = 89
        Width = 129
        Height = 30
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 14
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLBChange
      end
      object tbLH: TTrackBar
        Left = 187
        Top = 128
        Width = 129
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 15
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLHChange
      end
      object tbLS: TTrackBar
        Left = 187
        Top = 167
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 16
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLSChange
      end
      object tbLV: TTrackBar
        Left = 187
        Top = 207
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 17
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbLVChange
      end
      object pnlLightColor: TPanel
        Left = 325
        Top = 10
        Width = 50
        Height = 228
        BevelOuter = bvLowered
        TabOrder = 18
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Ambient Color'
      ImageIndex = 2
      object Label18: TLabel
        Left = 10
        Top = 10
        Width = 26
        Height = 16
        Caption = 'Red'
      end
      object Label19: TLabel
        Left = 10
        Top = 49
        Width = 37
        Height = 16
        Caption = 'Green'
      end
      object Label20: TLabel
        Left = 10
        Top = 89
        Width = 27
        Height = 16
        Caption = 'Blue'
      end
      object Label21: TLabel
        Left = 10
        Top = 128
        Width = 25
        Height = 16
        Caption = 'Hue'
      end
      object Label22: TLabel
        Left = 10
        Top = 167
        Width = 60
        Height = 16
        Caption = 'Saturation'
      end
      object Label23: TLabel
        Left = 10
        Top = 207
        Width = 65
        Height = 16
        Caption = 'Luminance'
      end
      object edtAR: TEdit
        Left = 98
        Top = 10
        Width = 61
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = edtARChange
      end
      object edtAG: TEdit
        Left = 98
        Top = 49
        Width = 61
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = edtAGChange
      end
      object edtAB: TEdit
        Left = 98
        Top = 89
        Width = 61
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = edtABChange
      end
      object edtAH: TEdit
        Left = 98
        Top = 128
        Width = 61
        Height = 21
        TabOrder = 3
        Text = '0'
        OnChange = edtAHChange
      end
      object edtAS: TEdit
        Left = 98
        Top = 167
        Width = 61
        Height = 21
        TabOrder = 4
        Text = '0'
        OnChange = edtASChange
      end
      object edtAV: TEdit
        Left = 98
        Top = 207
        Width = 61
        Height = 21
        TabOrder = 5
        Text = '0'
        OnChange = edtAVChange
      end
      object udAR: TUpDown
        Left = 159
        Top = 10
        Width = 18
        Height = 26
        Associate = edtAR
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 6
        Wrap = False
        OnClick = udARClick
      end
      object udAG: TUpDown
        Left = 159
        Top = 49
        Width = 18
        Height = 26
        Associate = edtAG
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 7
        Wrap = False
        OnClick = udAGClick
      end
      object udAB: TUpDown
        Left = 159
        Top = 89
        Width = 18
        Height = 25
        Associate = edtAB
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 8
        Wrap = False
        OnClick = udABClick
      end
      object udAH: TUpDown
        Left = 159
        Top = 128
        Width = 18
        Height = 26
        Associate = edtAH
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 9
        Wrap = False
        OnClick = udAHClick
      end
      object udAS: TUpDown
        Left = 159
        Top = 167
        Width = 18
        Height = 26
        Associate = edtAS
        Min = 0
        Position = 0
        TabOrder = 10
        Wrap = False
        OnClick = udASClick
      end
      object udAV: TUpDown
        Left = 159
        Top = 207
        Width = 18
        Height = 26
        Associate = edtAV
        Min = 0
        Position = 0
        TabOrder = 11
        Wrap = False
        OnClick = udAVClick
      end
      object tbAR: TTrackBar
        Left = 187
        Top = 10
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 12
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbARChange
      end
      object tbAG: TTrackBar
        Left = 187
        Top = 49
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 13
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbAGChange
      end
      object tbAB: TTrackBar
        Left = 187
        Top = 89
        Width = 129
        Height = 30
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 14
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbABChange
      end
      object tbAH: TTrackBar
        Left = 187
        Top = 128
        Width = 129
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 15
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbAHChange
      end
      object tbAS: TTrackBar
        Left = 187
        Top = 167
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 16
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbASChange
      end
      object tbAV: TTrackBar
        Left = 187
        Top = 207
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 17
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbAVChange
      end
      object pnlAmbientColor: TPanel
        Left = 325
        Top = 10
        Width = 50
        Height = 228
        BevelOuter = bvLowered
        TabOrder = 18
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Outline Color'
      ImageIndex = 3
      object Label26: TLabel
        Left = 10
        Top = 10
        Width = 26
        Height = 16
        Caption = 'Red'
      end
      object Label27: TLabel
        Left = 10
        Top = 49
        Width = 37
        Height = 16
        Caption = 'Green'
      end
      object Label28: TLabel
        Left = 10
        Top = 89
        Width = 27
        Height = 16
        Caption = 'Blue'
      end
      object Label29: TLabel
        Left = 10
        Top = 128
        Width = 25
        Height = 16
        Caption = 'Hue'
      end
      object Label30: TLabel
        Left = 10
        Top = 167
        Width = 60
        Height = 16
        Caption = 'Saturation'
      end
      object Label31: TLabel
        Left = 10
        Top = 207
        Width = 65
        Height = 16
        Caption = 'Luminance'
      end
      object edtOR: TEdit
        Left = 98
        Top = 10
        Width = 61
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = edtORChange
      end
      object edtOG: TEdit
        Left = 98
        Top = 49
        Width = 61
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = edtOGChange
      end
      object edtOB: TEdit
        Left = 98
        Top = 89
        Width = 61
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = edtOBChange
      end
      object edtOH: TEdit
        Left = 98
        Top = 128
        Width = 61
        Height = 21
        TabOrder = 3
        Text = '0'
        OnChange = edtOHChange
      end
      object edtOS: TEdit
        Left = 98
        Top = 167
        Width = 61
        Height = 21
        TabOrder = 4
        Text = '0'
        OnChange = edtOSChange
      end
      object edtOV: TEdit
        Left = 98
        Top = 207
        Width = 61
        Height = 21
        TabOrder = 5
        Text = '0'
        OnChange = edtOVChange
      end
      object udOR: TUpDown
        Left = 159
        Top = 10
        Width = 18
        Height = 26
        Associate = edtOR
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 6
        Wrap = False
        OnClick = udORClick
      end
      object udOG: TUpDown
        Left = 159
        Top = 49
        Width = 18
        Height = 26
        Associate = edtOG
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 7
        Wrap = False
        OnClick = udOGClick
      end
      object udOB: TUpDown
        Left = 159
        Top = 89
        Width = 18
        Height = 25
        Associate = edtOB
        Min = 0
        Max = 255
        Position = 0
        TabOrder = 8
        Wrap = False
        OnClick = udOBClick
      end
      object udOH: TUpDown
        Left = 159
        Top = 128
        Width = 18
        Height = 26
        Associate = edtOH
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 9
        Wrap = False
        OnClick = udOHClick
      end
      object udOS: TUpDown
        Left = 159
        Top = 167
        Width = 18
        Height = 26
        Associate = edtOS
        Min = 0
        Position = 0
        TabOrder = 10
        Wrap = False
        OnClick = udOSClick
      end
      object udOV: TUpDown
        Left = 159
        Top = 207
        Width = 18
        Height = 26
        Associate = edtOV
        Min = 0
        Position = 0
        TabOrder = 11
        Wrap = False
        OnClick = udOVClick
      end
      object tbOR: TTrackBar
        Left = 187
        Top = 10
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 12
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbORChange
      end
      object tbOG: TTrackBar
        Left = 187
        Top = 49
        Width = 129
        Height = 31
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 13
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbOGChange
      end
      object tbOB: TTrackBar
        Left = 187
        Top = 89
        Width = 129
        Height = 30
        Max = 255
        Orientation = trHorizontal
        Frequency = 32
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 14
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbOBChange
      end
      object tbOH: TTrackBar
        Left = 187
        Top = 128
        Width = 129
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 15
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbOHChange
      end
      object tbOS: TTrackBar
        Left = 187
        Top = 167
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 16
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbOSChange
      end
      object tbOV: TTrackBar
        Left = 187
        Top = 207
        Width = 129
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 17
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbOVChange
      end
      object pnlOutlineColor: TPanel
        Left = 325
        Top = 10
        Width = 50
        Height = 198
        BevelOuter = bvLowered
        TabOrder = 18
      end
      object cbUseOutlineColor: TCheckBox
        Left = 325
        Top = 217
        Width = 55
        Height = 21
        Caption = 'Use'
        Checked = True
        State = cbChecked
        TabOrder = 19
        OnClick = cbUseOutlineColorClick
      end
    end
  end
  object btnOk: TBitBtn
    Left = 495
    Top = 690
    Width = 81
    Height = 27
    TabOrder = 3
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 591
    Top = 690
    Width = 81
    Height = 27
    TabOrder = 4
    Kind = bkCancel
  end
  object cbRemoveHiddenSurfaces: TCheckBox
    Left = 276
    Top = 691
    Width = 189
    Height = 21
    Caption = 'Remove hidden surfaces'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbRemoveHiddenSurfacesClick
  end
  object PageControl2: TPageControl
    Left = 8
    Top = 544
    Width = 257
    Height = 169
    ActivePage = TabSheet5
    TabIndex = 0
    TabOrder = 6
    object TabSheet5: TTabSheet
      Caption = 'Rotate'
      object Label5: TLabel
        Left = 8
        Top = 88
        Width = 35
        Height = 16
        Caption = 'Z Rot.'
      end
      object Label4: TLabel
        Left = 8
        Top = 48
        Width = 36
        Height = 16
        Caption = 'Y Rot.'
      end
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 35
        Height = 16
        Caption = 'X Rot.'
      end
      object udZRot: TUpDown
        Left = 110
        Top = 88
        Width = 18
        Height = 24
        Associate = edtZRot
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 0
        Wrap = False
        OnClick = udZRotClick
      end
      object udYRot: TUpDown
        Left = 110
        Top = 48
        Width = 18
        Height = 24
        Associate = edtYRot
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 1
        Wrap = False
        OnClick = udYRotClick
      end
      object udXRot: TUpDown
        Left = 110
        Top = 8
        Width = 18
        Height = 24
        Associate = edtXRot
        Min = 0
        Max = 360
        Position = 0
        TabOrder = 2
        Wrap = False
        OnClick = udXRotClick
      end
      object tbZRot: TTrackBar
        Left = 138
        Top = 88
        Width = 106
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 3
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbZRotChange
      end
      object tbYRot: TTrackBar
        Left = 138
        Top = 48
        Width = 106
        Height = 31
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 4
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbYRotChange
      end
      object tbXRot: TTrackBar
        Left = 138
        Top = 8
        Width = 106
        Height = 30
        Max = 360
        Orientation = trHorizontal
        Frequency = 45
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 5
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbXRotChange
      end
      object edtZRot: TEdit
        Left = 59
        Top = 88
        Width = 51
        Height = 24
        TabOrder = 6
        Text = '0'
        OnChange = edtZRotChange
      end
      object edtYRot: TEdit
        Left = 59
        Top = 48
        Width = 51
        Height = 24
        TabOrder = 7
        Text = '0'
        OnChange = edtYRotChange
      end
      object edtXRot: TEdit
        Left = 59
        Top = 8
        Width = 51
        Height = 24
        TabOrder = 8
        Text = '0'
        OnChange = edtXRotChange
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Move'
      ImageIndex = 1
      object Label32: TLabel
        Left = 8
        Top = 48
        Width = 44
        Height = 16
        Caption = 'Y trans.'
      end
      object Label33: TLabel
        Left = 8
        Top = 8
        Width = 43
        Height = 16
        Caption = 'X trans.'
      end
      object Label34: TLabel
        Left = 8
        Top = 88
        Width = 43
        Height = 16
        Caption = 'Z trans.'
      end
      object edtYTrans: TEdit
        Left = 59
        Top = 48
        Width = 51
        Height = 24
        TabOrder = 3
        Text = '50'
        OnChange = edtYTransChange
      end
      object udYTrans: TUpDown
        Left = 110
        Top = 48
        Width = 19
        Height = 24
        Associate = edtYTrans
        Min = 0
        Position = 50
        TabOrder = 4
        Wrap = False
        OnClick = udYTransClick
      end
      object tbYTrans: TTrackBar
        Left = 138
        Top = 48
        Width = 106
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 50
        SelEnd = 0
        SelStart = 0
        TabOrder = 5
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbYTransChange
      end
      object edtXTrans: TEdit
        Left = 59
        Top = 8
        Width = 51
        Height = 24
        TabOrder = 0
        Text = '50'
        OnChange = edtXTransChange
      end
      object edtZTrans: TEdit
        Left = 59
        Top = 88
        Width = 51
        Height = 24
        TabOrder = 6
        Text = '50'
        OnChange = edtZTransChange
      end
      object udXTrans: TUpDown
        Left = 110
        Top = 8
        Width = 19
        Height = 24
        Associate = edtXTrans
        Min = 0
        Position = 50
        TabOrder = 1
        Wrap = False
        OnClick = udXTransClick
      end
      object udZTrans: TUpDown
        Left = 110
        Top = 88
        Width = 19
        Height = 24
        Associate = edtZTrans
        Min = 0
        Position = 50
        TabOrder = 7
        Wrap = False
        OnClick = udZTransClick
      end
      object tbXTrans: TTrackBar
        Left = 138
        Top = 16
        Width = 106
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 50
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbXTransChange
      end
      object tbZTrans: TTrackBar
        Left = 138
        Top = 88
        Width = 106
        Height = 31
        Max = 100
        Orientation = trHorizontal
        Frequency = 10
        Position = 50
        SelEnd = 0
        SelStart = 0
        TabOrder = 8
        ThumbLength = 15
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = tbZTransChange
      end
    end
  end
end
