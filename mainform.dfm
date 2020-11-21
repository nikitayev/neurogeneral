object MainFormExtrapolation: TMainFormExtrapolation
  Left = 0
  Top = 0
  Caption = #1040#1083#1077#1082#1089#1077#1081' '#1053#1080#1082#1080#1090#1072#1077#1074' (nikitayev@mail.ru) '#1085#1077#1081#1088#1086#1089#1077#1090#1080' 2016'
  ClientHeight = 841
  ClientWidth = 1584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesktopCenter
  ScreenSnap = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcData: TPageControl
    Left = 0
    Top = 0
    Width = 1584
    Height = 841
    ActivePage = tsData
    Align = alClient
    TabOrder = 0
    object tsData: TTabSheet
      Caption = #1044#1072#1085#1085#1099#1077' '#1076#1083#1103' '#1086#1073#1091#1095#1077#1085#1080#1103
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sgDataTeachingSrc: TStringGrid
        Left = 0
        Top = 0
        Width = 1576
        Height = 813
        Align = alClient
        DefaultDrawing = False
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking]
        ParentDoubleBuffered = False
        ParentFont = False
        PopupMenu = pmMainPopupMenu
        TabOrder = 0
        OnDrawCell = sgDataTeachingSrcDrawCell
        OnMouseLeave = sgDataTeachingSrcMouseLeave
        OnMouseMove = sgDataTeachingSrcMouseMove
        OnMouseUp = sgDataTeachingSrcMouseUp
      end
    end
    object tsPrediction: TTabSheet
      Caption = #1055#1088#1077#1076#1089#1082#1072#1079#1072#1085#1080#1077
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter2: TSplitter
        Left = 1573
        Top = 0
        Height = 813
        Align = alRight
        Beveled = True
        ExplicitLeft = 544
        ExplicitTop = 192
        ExplicitHeight = 100
      end
      object sgDataPredictionSrc: TStringGrid
        Left = 0
        Top = 0
        Width = 1253
        Height = 813
        Align = alClient
        FixedCols = 0
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goThumbTracking]
        ParentFont = False
        PopupMenu = pmSecondPopupMenu
        TabOrder = 0
      end
      object sgDataPredictionResult: TStringGrid
        Left = 1253
        Top = 0
        Width = 320
        Height = 813
        Align = alRight
        FixedCols = 0
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goThumbTracking]
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsPreferences: TTabSheet
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vleNeuroPreferences: TValueListEditor
        Left = 0
        Top = 0
        Width = 1576
        Height = 813
        Align = alClient
        DefaultRowHeight = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        ParentFont = False
        Strings.Strings = (
          #1053#1077#1081#1088#1086#1085#1086#1074' '#1074#1086' '#1074#1090#1086#1088#1086#1084' '#1089#1083#1086#1077'=8'
          #1053#1077#1081#1088#1086#1085#1086#1074' '#1074#1086' '#1090#1088#1077#1090#1100#1077#1084' '#1089#1083#1086#1077'=8'
          #1057#1083#1086#1105#1074'=3'
          #1056#1077#1089#1090#1072#1088#1090#1086#1074'=1000'
          #1052#1072#1082#1089#1080#1084#1091#1084' '#1080#1090#1077#1088#1072#1094#1080#1081'=300'
          #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1099#1081' '#1096#1072#1075'=0.001'
          #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1072#1103' '#1086#1096#1080#1073#1082#1072'=0.001')
        TabOrder = 0
        TitleCaptions.Strings = (
          #1055#1072#1088#1072#1084#1077#1090#1088
          #1047#1085#1072#1095#1077#1085#1080#1077)
        ColWidths = (
          258
          1312)
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 176
    Top = 152
    object NInsertData: TMenuItem
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100' '#1076#1072#1085#1085#1099#1077
      OnClick = NInsertDataClick
    end
    object miINS: TMenuItem
      Caption = #1053#1077#1081#1088#1086#1089#1077#1090#1080
      object N5: TMenuItem
        Caption = #1054#1073#1091#1095#1080#1090#1100
        OnClick = NTeachingClick
      end
      object N6: TMenuItem
        Action = INSFileOpen
      end
      object N7: TMenuItem
        Action = INSFileSaveAs
      end
    end
    object NPreferences: TMenuItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object NHelp: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
    end
  end
  object pmMainPopupMenu: TPopupMenu
    Left = 172
    Top = 288
    object N1: TMenuItem
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1089#1090#1086#1083#1073#1077#1094' '#1079#1072#1074#1080#1089#1080#1084#1099#1093' '#1087#1077#1088#1077#1084#1077#1085#1085#1099#1093
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1089#1090#1086#1083#1073#1077#1094' '#1080#1079#1084#1077#1085#1103#1077#1084#1099#1093' '#1085#1077#1079#1072#1074#1080#1089#1080#1084#1099#1093' '#1087#1077#1088#1077#1084#1077#1085#1085#1099#1093
      OnClick = N2Click
    end
    object N3: TMenuItem
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1079#1072#1082#1088#1077#1087#1083#1105#1085#1085#1099#1077' '#1089#1090#1086#1083#1073#1094#1099
      OnClick = N3Click
    end
    object N4: TMenuItem
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1086#1089#1085#1086#1074#1085#1091#1102' '#1089#1090#1088#1086#1082#1091
      OnClick = N4Click
    end
  end
  object ActionList: TActionList
    Left = 300
    Top = 152
    object INSFileOpen: TFileOpen
      Category = 'File'
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083' '#1089' '#1085#1077#1081#1088#1086#1089#1077#1090#1100#1102
      Dialog.DefaultExt = 'ins'
      Dialog.Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Hint = #1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083' '#1089' '#1085#1077#1081#1088#1086#1089#1077#1090#1100#1102
      ImageIndex = 7
      OnAccept = INSFileOpenAccept
    end
    object INSFileSaveAs: TFileSaveAs
      Category = 'File'
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1085#1077#1081#1088#1086#1089#1077#1090#1100' '#1074' '#1092#1072#1081#1083
      Dialog.DefaultExt = 'ins'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1085#1077#1081#1088#1086#1089#1077#1090#1100' '#1074' '#1092#1072#1081#1083
      ImageIndex = 30
      OnAccept = INSFileSaveAsAccept
    end
  end
  object pmSecondPopupMenu: TPopupMenu
    Left = 300
    Top = 288
    object nInsertCalculatedData: TMenuItem
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100' '#1076#1083#1103' '#1088#1072#1089#1095#1105#1090#1072
      OnClick = NInsertAtPredictionClick
    end
    object N11: TMenuItem
      Caption = #1047#1072#1087#1086#1083#1085#1080#1090#1100' '#1074#1072#1088#1080#1072#1085#1090' 1'
      OnClick = N11Click
    end
    object N9: TMenuItem
      Caption = #1056#1072#1089#1089#1095#1080#1090#1072#1090#1100
      OnClick = NSolveClick
    end
  end
end
