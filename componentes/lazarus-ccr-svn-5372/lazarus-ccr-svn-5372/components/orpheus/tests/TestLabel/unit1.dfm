object Form1: TForm1
  Left = 192
  Top = 114
  Width = 386
  Height = 340
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object OvcLabel1: TOvcLabel
    Left = 64
    Top = 32
    Width = 257
    Height = 49
    Caption = 'Fancy Raised Label'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -26
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentColor = False
  end
  object OvcLabel2: TOvcLabel
    Left = 64
    Top = 112
    Width = 257
    Height = 49
    Appearance = apSunken
    Caption = 'Fancy Sunken Label'
    Color = clBtnFace
    ColorScheme = csCustom
    CustomSettings.HighlightDirection = sdDownRight
    CustomSettings.ShadowDirection = sdUpLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -26
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentColor = False
  end
end
