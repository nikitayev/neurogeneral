object MainFormExtrapolation: TMainFormExtrapolation
  Left = 0
  Top = 0
  Caption = #1040#1083#1077#1082#1089#1077#1081' '#1053#1080#1082#1080#1090#1072#1077#1074' (nikitayev@mail.ru) '#1085#1077#1081#1088#1086#1089#1077#1090#1080' 2016'
  ClientHeight = 460
  ClientWidth = 800
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
    Width = 800
    Height = 460
    ActivePage = tsData
    Align = alClient
    TabOrder = 0
    object tsData: TTabSheet
      Caption = #1044#1072#1085#1085#1099#1077' '#1076#1083#1103' '#1086#1073#1091#1095#1077#1085#1080#1103
      object Splitter1: TSplitter
        Left = 469
        Top = 0
        Height = 432
        Align = alRight
        Beveled = True
        ExplicitLeft = 544
        ExplicitTop = 192
        ExplicitHeight = 100
      end
      object sgDataTeachingSrc: TStringGrid
        Left = 0
        Top = 0
        Width = 469
        Height = 432
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
        TabOrder = 0
      end
      object sgDataTeachingResult: TStringGrid
        Left = 472
        Top = 0
        Width = 320
        Height = 432
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
    object tsPrediction: TTabSheet
      Caption = #1055#1088#1077#1076#1089#1082#1072#1079#1072#1085#1080#1077
      ImageIndex = 1
      object Splitter2: TSplitter
        Left = 789
        Top = 0
        Height = 432
        Align = alRight
        Beveled = True
        ExplicitLeft = 544
        ExplicitTop = 192
        ExplicitHeight = 100
      end
      object sgDataPredictionSrc: TStringGrid
        Left = 0
        Top = 0
        Width = 469
        Height = 432
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
        PopupMenu = PopupMenuResult
        TabOrder = 0
      end
      object sgDataPredictionResult: TStringGrid
        Left = 469
        Top = 0
        Width = 320
        Height = 432
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
        PopupMenu = PopupMenuResult
        TabOrder = 1
      end
    end
    object tsPreferences: TTabSheet
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      ImageIndex = 2
      object vleNeuroPreferences: TValueListEditor
        Left = 0
        Top = 0
        Width = 792
        Height = 432
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
          #1053#1077#1081#1088#1086#1085#1086#1074' '#1074#1086' '#1074#1090#1086#1088#1086#1084' '#1089#1083#1086#1077'=60'
          #1053#1077#1081#1088#1086#1085#1086#1074' '#1074#1086' '#1090#1088#1077#1090#1100#1077#1084' '#1089#1083#1086#1077'=10'
          #1057#1083#1086#1105#1074'=3'
          #1056#1077#1089#1090#1072#1088#1090#1086#1074'=1000'
          #1052#1072#1082#1089#1080#1084#1091#1084' '#1080#1090#1077#1088#1072#1094#1080#1081'=100'
          #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1099#1081' '#1096#1072#1075'=0.1'
          #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1072#1103' '#1086#1096#1080#1073#1082#1072'=0.00001')
        TabOrder = 0
        TitleCaptions.Strings = (
          #1055#1072#1088#1072#1084#1077#1090#1088
          #1047#1085#1072#1095#1077#1085#1080#1077)
        ColWidths = (
          258
          528)
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 248
    Top = 192
    object NInsertData: TMenuItem
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100' '#1076#1072#1085#1085#1099#1077
      OnClick = NInsertDataClick
    end
    object NInsertResults: TMenuItem
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090
      OnClick = NInsertResultsClick
    end
    object NInsertAtPrediction: TMenuItem
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100' '#1074' '#1087#1088#1077#1076#1089#1082#1072#1079#1072#1085#1080#1077
      OnClick = NInsertAtPredictionClick
    end
    object NTeaching: TMenuItem
      Caption = #1054#1073#1091#1095#1077#1085#1080#1077
      object NTeachingStart: TMenuItem
        Caption = #1057#1090#1072#1088#1090
        OnClick = NTeachingClick
      end
      object NTeachingSaveTo: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
        OnClick = NTeachingSaveToClick
      end
      object NTeachingLoadFrom: TMenuItem
        Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100'...'
        OnClick = NTeachingLoadFromClick
      end
    end
    object NSolve: TMenuItem
      Caption = #1054#1073#1088#1072#1073#1086#1090#1072#1090#1100
      OnClick = NSolveClick
    end
    object NPreferences: TMenuItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object NHelp: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
    end
  end
  object PopupMenuResult: TPopupMenu
    Left = 588
    Top = 216
    object NCopyResults: TMenuItem
      Caption = #1057#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1099
      OnClick = NCopyResultsClick
    end
  end
  object SaveDialog: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 548
    Top = 88
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'neuro'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 548
    Top = 136
  end
end
