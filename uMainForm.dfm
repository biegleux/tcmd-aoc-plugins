object MainForm: TMainForm
  Left = 207
  Top = 59
  Width = 878
  Height = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseMove = FormMouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 870
    Height = 611
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object PageControl: TPageControl
      Left = 0
      Top = 0
      Width = 863
      Height = 604
      ActivePage = GeneralTabSheet
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Lucida Blackletter'
      Font.Style = []
      HotTrack = True
      MultiLine = True
      ParentFont = False
      TabOrder = 0
      object GeneralTabSheet: TTabSheet
        Caption = 'General'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Lucida Blackletter'
        Font.Style = []
        ParentFont = False
        object GeneralScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object GeneralPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Lucida Blackletter'
            Font.Style = []
            ParentFont = False
            OnPaint = GeneralPaintBoxPaint
          end
          object GSLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 96
            Height = 17
            Caption = 'Game Settings:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel1: TJvLabel
            Left = 24
            Top = 32
            Width = 63
            Height = 16
            Caption = 'Game Type:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel2: TJvLabel
            Left = 24
            Top = 48
            Width = 56
            Height = 16
            Caption = 'Map Style:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel3: TJvLabel
            Left = 24
            Top = 64
            Width = 47
            Height = 16
            Caption = 'Location:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel4: TJvLabel
            Left = 24
            Top = 80
            Width = 45
            Height = 16
            Caption = 'Players:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel5: TJvLabel
            Left = 24
            Top = 96
            Width = 50
            Height = 16
            Caption = 'Duration:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel6: TJvLabel
            Left = 24
            Top = 112
            Width = 53
            Height = 16
            Caption = 'Difficulty:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel7: TJvLabel
            Left = 24
            Top = 128
            Width = 59
            Height = 16
            Caption = 'Population:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel8: TJvLabel
            Left = 24
            Top = 144
            Width = 51
            Height = 16
            Caption = 'Map Size:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel9: TJvLabel
            Left = 24
            Top = 160
            Width = 33
            Height = 16
            Caption = 'Speed:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel10: TJvLabel
            Left = 24
            Top = 176
            Width = 84
            Height = 16
            Caption = 'Lock Diplomacy:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel11: TJvLabel
            Left = 24
            Top = 192
            Width = 65
            Height = 16
            Caption = 'Reveal Map:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object POVLabel: TLabel
            Left = 112
            Top = 224
            Width = 23
            Height = 14
            Caption = 'pov'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            Transparent = True
          end
          object TeamsLabel: TJvLabel
            Left = 12
            Top = 284
            Width = 47
            Height = 17
            Caption = 'Teams:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object MapPaintBox: TPaintBox
            Left = 176
            Top = 40
            Width = 306
            Height = 153
            OnPaint = MapPaintBoxPaint
          end
          object PlayersLabel: TLabel
            Left = 112
            Top = 80
            Width = 49
            Height = 14
            Caption = 'players'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object DurationLabel: TLabel
            Left = 112
            Top = 96
            Width = 59
            Height = 14
            Caption = 'duration'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object DifficultyLabel: TLabel
            Left = 112
            Top = 112
            Width = 62
            Height = 14
            Caption = 'difficulty'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object PopulationLabel: TLabel
            Left = 112
            Top = 128
            Width = 72
            Height = 14
            Caption = 'population'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object MapSizeLabel: TLabel
            Left = 112
            Top = 144
            Width = 57
            Height = 14
            Caption = 'map-size'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object SpeedLabel: TLabel
            Left = 112
            Top = 160
            Width = 36
            Height = 14
            Caption = 'speed'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object LockDiplomacyLabel: TLabel
            Left = 112
            Top = 176
            Width = 98
            Height = 14
            Caption = 'lock-diplomacy'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object LocationLabel: TLabel
            Left = 112
            Top = 64
            Width = 53
            Height = 14
            Caption = 'location'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            Transparent = True
          end
          object MapStyleLabel: TLabel
            Left = 112
            Top = 48
            Width = 65
            Height = 14
            Caption = 'map-style'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object GameTypeLabel: TLabel
            Left = 112
            Top = 32
            Width = 68
            Height = 14
            Caption = 'game-type'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object JvLabel13: TJvLabel
            Left = 24
            Top = 224
            Width = 31
            Height = 16
            Caption = 'POV:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object ScenarioFileLabel: TLabel
            Left = 112
            Top = 256
            Width = 81
            Height = 14
            Caption = 'scenario-file'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            ShowAccelChar = False
            Transparent = True
          end
          object JvLabel14: TJvLabel
            Left = 24
            Top = 240
            Width = 73
            Height = 16
            Caption = 'Game Version:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel15: TJvLabel
            Left = 24
            Top = 256
            Width = 67
            Height = 16
            Caption = 'Scenario File:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object RevealMapLabel: TLabel
            Left = 112
            Top = 192
            Width = 75
            Height = 14
            Caption = 'reveal-map'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object GameVersionLabel: TLabel
            Left = 112
            Top = 240
            Width = 88
            Height = 14
            Caption = 'game-version'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object JvLabel12: TJvLabel
            Left = 24
            Top = 208
            Width = 41
            Height = 16
            Caption = 'Victory:'
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object VictoryLabel: TLabel
            Left = 112
            Top = 208
            Width = 47
            Height = 14
            Caption = 'victory'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
        end
      end
      object ChatTabSheet: TTabSheet
        Caption = 'Chat'
        ImageIndex = 1
        object ChatScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object ChatPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object ChatLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 37
            Height = 17
            Caption = 'Chat:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object ChatWB: TEmbeddedWB
            Left = 24
            Top = 32
            Width = 657
            Height = 249
            TabOrder = 0
            RegisterAsDropTarget = False
            DisableCtrlShortcuts = 'N'
            UserInterfaceOptions = [DontUse3DBorders]
            DisabledPopupMenus = [rcmAll]
            About = ' EmbeddedWB http://bsalsa.com/'
            EnableMessageHandler = False
            PrintOptions.Margins.Left = 19.050000000000000000
            PrintOptions.Margins.Right = 19.050000000000000000
            PrintOptions.Margins.Top = 19.050000000000000000
            PrintOptions.Margins.Bottom = 19.050000000000000000
            PrintOptions.Header = '&w&bPage &p of &P'
            PrintOptions.HTMLHeader.Strings = (
              '<HTML></HTML>')
            PrintOptions.Footer = '&u&b&d'
            PrintOptions.Orientation = poPortrait
            VisualEffects.DisableSounds = True
            ControlData = {
              4C000000E3300000F21300000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126203000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
      object TributesTabSheet: TTabSheet
        Caption = 'Tributes'
        ImageIndex = 2
        object TributesScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object TributingPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object TributingLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 62
            Height = 17
            Caption = 'Tributes:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object TributesWB: TEmbeddedWB
            Left = 24
            Top = 32
            Width = 657
            Height = 249
            TabOrder = 0
            Silent = False
            RegisterAsDropTarget = False
            DisableCtrlShortcuts = 'N'
            UserInterfaceOptions = [DontUse3DBorders]
            DisabledPopupMenus = [rcmAll]
            About = ' EmbeddedWB http://bsalsa.com/'
            EnableMessageHandler = False
            PrintOptions.Margins.Left = 19.050000000000000000
            PrintOptions.Margins.Right = 19.050000000000000000
            PrintOptions.Margins.Top = 19.050000000000000000
            PrintOptions.Margins.Bottom = 19.050000000000000000
            PrintOptions.Header = '&w&bPage &p of &P'
            PrintOptions.HTMLHeader.Strings = (
              '<HTML></HTML>')
            PrintOptions.Footer = '&u&b&d'
            PrintOptions.Orientation = poPortrait
            VisualEffects.DisableSounds = True
            ControlData = {
              4C000000E3300000F21300000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126203000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
      object ResearchesTabSheet: TTabSheet
        Caption = 'Researches'
        ImageIndex = 3
        object ResearchesScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object ResearchesPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object ResearchesLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 74
            Height = 17
            Caption = 'Researches:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object ResPaintBox: TPaintBox
            Left = 24
            Top = 32
            Width = 105
            Height = 105
            OnMouseMove = ResPaintBoxMouseMove
            OnPaint = ResPaintBoxPaint
          end
        end
      end
      object ExtraStatsTabSheet: TTabSheet
        Caption = 'Extra stats'
        ImageIndex = 4
        object ExtraStatsScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object ExtraStatsPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object ESLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 79
            Height = 17
            Caption = 'Extra stats:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object UnitsPaintBox: TPaintBox
            Left = 24
            Top = 32
            Width = 105
            Height = 105
            OnPaint = UnitsPaintBoxPaint
          end
          object BuildingsPaintBox: TPaintBox
            Left = 152
            Top = 32
            Width = 105
            Height = 105
            OnPaint = BuildingsPaintBoxPaint
          end
        end
      end
      object ScenarioInfoTabSheet: TTabSheet
        Caption = 'Scenario Information'
        ImageIndex = 5
        object ScenarioInfoScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object ScenarioPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object SILabel: TJvLabel
            Left = 12
            Top = 12
            Width = 139
            Height = 17
            Caption = 'Scenario Information:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object ScBgPaintBox: TPaintBox
            Left = 24
            Top = 32
            Width = 603
            Height = 518
            OnPaint = ScBgPaintBoxPaint
          end
          object BtnPaintBox1: TPaintBox
            Left = 88
            Top = 456
            Width = 117
            Height = 63
            OnClick = BtnPaintBox1Click
            OnPaint = BtnPaintBox1Paint
          end
          object BtnPaintBox2: TPaintBox
            Left = 216
            Top = 456
            Width = 117
            Height = 63
            OnClick = BtnPaintBox1Click
            OnPaint = BtnPaintBox1Paint
          end
          object BtnPaintBox3: TPaintBox
            Left = 344
            Top = 456
            Width = 117
            Height = 63
            OnClick = BtnPaintBox1Click
            OnPaint = BtnPaintBox1Paint
          end
          object BtnPaintBox4: TPaintBox
            Left = 472
            Top = 456
            Width = 117
            Height = 63
            OnClick = BtnPaintBox1Click
            OnPaint = BtnPaintBox1Paint
          end
          object JvLabel17: TJvLabel
            Left = 234
            Top = 480
            Width = 81
            Height = 17
            Caption = 'Instructions'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            OnClick = JvLabel19Click
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel18: TJvLabel
            Left = 380
            Top = 480
            Width = 38
            Height = 17
            Caption = 'Hints'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            OnClick = JvLabel19Click
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel20: TJvLabel
            Left = 504
            Top = 480
            Width = 44
            Height = 17
            Caption = 'Scouts'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            OnClick = JvLabel19Click
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object JvLabel19: TJvLabel
            Left = 120
            Top = 480
            Width = 50
            Height = 17
            Caption = 'General'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            OnClick = JvLabel19Click
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object HeaderLabel: TJvLabel
            Left = 293
            Top = 52
            Width = 75
            Height = 23
            Caption = 'Caption'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -19
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -19
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object ScenarioMapPaintBox: TPaintBox
            Left = 64
            Top = 88
            Width = 306
            Height = 153
            OnPaint = MapPaintBoxPaint
          end
          object ScContentLabel: TLabel
            Left = 64
            Top = 88
            Width = 97
            Height = 16
            Caption = 'ScContentLabel'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
          end
        end
      end
      object CommentTabSheet: TTabSheet
        Caption = 'Comment'
        ImageIndex = 6
        object CommentScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object CommentPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object MemoShape: TShape
            Left = 23
            Top = 63
            Width = 435
            Height = 195
          end
          object CLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 169
            Height = 17
            Caption = 'Comment in recorded game:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object CheckBox2PaintBox: TPaintBox
            Left = 344
            Top = 40
            Width = 15
            Height = 17
            OnClick = CheckBox2PaintBoxClick
            OnPaint = CheckBox2PaintBoxPaint
          end
          object UpdateBtnPaintBox: TPaintBox
            Left = 392
            Top = 264
            Width = 65
            Height = 28
            OnClick = UpdateBtnPaintBoxClick
            OnMouseDown = UpdateBtnPaintBoxMouseDown
            OnMouseUp = UpdateBtnPaintBoxMouseUp
            OnPaint = UpdateBtnPaintBoxPaint
          end
          object CheckBox1PaintBox: TPaintBox
            Left = 24
            Top = 40
            Width = 15
            Height = 17
            OnClick = CheckBox1PaintBoxClick
            OnPaint = CheckBox1PaintBoxPaint
          end
          object ACLabel: TLabel
            Left = 44
            Top = 42
            Width = 88
            Height = 14
            Caption = 'add comment'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object CBLabel: TLabel
            Left = 364
            Top = 42
            Width = 91
            Height = 14
            Caption = 'create backup'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object UpdateLabel: TLabel
            Left = 401
            Top = 271
            Width = 46
            Height = 14
            Caption = 'update'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Georgia'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
            OnClick = UpdateBtnPaintBoxClick
            OnMouseDown = UpdateBtnPaintBoxMouseDown
            OnMouseUp = UpdateBtnPaintBoxMouseUp
          end
          object CommentMemo: TMemo
            Left = 24
            Top = 64
            Width = 433
            Height = 193
            BorderStyle = bsNone
            Color = 1789036
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -15
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
      end
      object AboutTabSheet: TTabSheet
        Caption = 'About'
        ImageIndex = 7
        object AboutScrollBox: TScrollBox
          Left = 0
          Top = 0
          Width = 855
          Height = 574
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnHighlight
          ParentColor = False
          TabOrder = 0
          object AboutPaintBox: TPaintBox
            Left = 0
            Top = 0
            Width = 855
            Height = 574
            Align = alClient
            OnPaint = GeneralPaintBoxPaint
          end
          object APaintBox: TPaintBox
            Left = 24
            Top = 32
            Width = 603
            Height = 518
            OnPaint = ScBgPaintBoxPaint
          end
          object JvLabel16: TJvLabel
            Left = 212
            Top = 52
            Width = 128
            Height = 23
            Caption = 'about-caption'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWhite
            Font.Height = -19
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clBlack
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -19
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object AboutLabel: TJvLabel
            Left = 12
            Top = 12
            Width = 45
            Height = 17
            Caption = 'About:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clActiveCaption
            Font.Height = -12
            Font.Name = 'Lucida Blackletter'
            Font.Style = [fsBold]
            ParentFont = False
            ShadowColor = clWhite
            ShadowSize = 1
            Transparent = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -12
            HotTrackFont.Name = 'Lucida Blackletter'
            HotTrackFont.Style = []
          end
          object AboutContentLabel: TLabel
            Left = 64
            Top = 104
            Width = 521
            Height = 169
            AutoSize = False
            Caption = 'credits-note'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object CopyrightLabel: TLabel
            Left = 128
            Top = 400
            Width = 94
            Height = 16
            Caption = 'copyright-note'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
          end
          object Label3: TLabel
            Left = 72
            Top = 416
            Width = 182
            Height = 16
            Caption = 'Latest release can be found at'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
          end
          object PluginURLLabel: TLabel
            Left = 256
            Top = 416
            Width = 60
            Height = 15
            Cursor = crHandPoint
            Hint = 'Click here to visit the homepage.'
            Caption = 'plugin-url'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -12
            Font.Name = 'Georgia'
            Font.Style = [fsUnderline]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            OnClick = PluginURLLabelClick
          end
          object PluginInfoLabel: TLabel
            Left = 68
            Top = 384
            Width = 68
            Height = 16
            Caption = 'plugin-info'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Georgia'
            Font.Style = []
            ParentFont = False
          end
          object OSPaintBox: TPaintBox
            Left = 480
            Top = 400
            Width = 88
            Height = 32
            Cursor = crHandPoint
            Hint = 'Click here to enter the site of Open Source Initiative.'
            ParentShowHint = False
            ShowHint = True
            OnClick = OSPaintBoxClick
            OnPaint = OSPaintBoxPaint
          end
        end
      end
    end
  end
  object MapPopupMenu: TPopupMenu
    Left = 796
    Top = 121
    object SaveMapAs: TMenuItem
      Caption = '&Save map as...'
      OnClick = SaveMapAsClick
    end
  end
  object ChatPopupMenu: TPopupMenu
    Left = 796
    Top = 154
    object SaveasHTML: TMenuItem
      Caption = '&Save chat as HTML...'
      OnClick = SaveasHTMLClick
    end
  end
  object ImageList: TImageList
    Left = 796
    Top = 82
  end
end
