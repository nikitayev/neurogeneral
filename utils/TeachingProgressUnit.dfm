object TeachingProgressForm: TTeachingProgressForm
  Left = 0
  Top = 0
  Caption = 'TeachingProgressForm'
  ClientHeight = 137
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object leTotalRestarts: TLabeledEdit
    Left = 144
    Top = 8
    Width = 200
    Height = 21
    EditLabel.Width = 84
    EditLabel.Height = 13
    EditLabel.Caption = #1042#1089#1077#1075#1086' '#1088#1077#1089#1090#1072#1088#1090#1086#1074
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object leRestartsFinished: TLabeledEdit
    Left = 144
    Top = 29
    Width = 200
    Height = 21
    EditLabel.Width = 111
    EditLabel.Height = 13
    EditLabel.Caption = #1056#1077#1089#1090#1072#1088#1090#1086#1074' '#1089#1086#1074#1077#1088#1096#1077#1085#1086
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object leIsTerminated: TLabeledEdit
    Left = 144
    Top = 50
    Width = 200
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = #1060#1083#1072#1075' '#1087#1088#1077#1088#1099#1074#1072#1085#1080#1103
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object leEBest: TLabeledEdit
    Left = 144
    Top = 71
    Width = 200
    Height = 21
    EditLabel.Width = 80
    EditLabel.Height = 13
    EditLabel.Caption = #1051#1091#1095#1096#1072#1103' '#1086#1096#1080#1073#1082#1072
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object btCancel: TButton
    Left = 64
    Top = 98
    Width = 265
    Height = 25
    Caption = #1055#1088#1077#1088#1074#1072#1090#1100' '#1087#1088#1086#1094#1077#1089#1089' '#1086#1073#1091#1095#1077#1085#1080#1103
    TabOrder = 4
    OnClick = btCancelClick
  end
end
