object MainForm: TMainForm
  Left = 458
  Top = 254
  BorderStyle = bsDialog
  Caption = 'AudioHash v1.03 by Yaron Gur / Inmatrix.com'
  ClientHeight = 362
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object LabeOutput: TTntLabel
    Left = 283
    Top = 14
    Width = 70
    Height = 13
    Alignment = taCenter
    Caption = 'Output format :'
    OnMouseDown = FormMouseDown
  end
  object LabelTip: TTntLabel
    Left = 248
    Top = 142
    Width = 140
    Height = 110
    AutoSize = False
    WordWrap = True
    OnMouseDown = FormMouseDown
  end
  object FolderList: TTntListBox
    Left = 12
    Top = 12
    Width = 227
    Height = 293
    ItemHeight = 13
    TabOrder = 4
  end
  object AddFolderButton: TTntButton
    Left = 248
    Top = 66
    Width = 140
    Height = 30
    Caption = 'Add folder'
    TabOrder = 1
    OnClick = AddFolderButtonClick
  end
  object ClearButton: TTntButton
    Left = 248
    Top = 105
    Width = 140
    Height = 30
    Caption = 'Clear list'
    TabOrder = 2
    OnClick = ClearButtonClick
  end
  object OutputFormatCB: TTntComboBox
    Left = 248
    Top = 31
    Width = 140
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 0
    Text = 'XML'
    Items.Strings = (
      'Comma separated CSV'
      'XML')
  end
  object HashButton: TTntButton
    Left = 248
    Top = 255
    Width = 140
    Height = 50
    Caption = 'Start Hashing'
    TabOrder = 3
    OnClick = HashButtonClick
  end
  object ProgressBar: TProgressBar
    Left = 12
    Top = 336
    Width = 376
    Height = 17
    Max = 10000
    Smooth = True
    TabOrder = 5
    OnMouseDown = FormMouseDown
  end
  object PanelStatus: TTntPanel
    Left = 12
    Top = 314
    Width = 376
    Height = 21
    BevelOuter = bvNone
    TabOrder = 6
    OnMouseDown = FormMouseDown
  end
  object FolderBrowser: TFolderBrowser
    BrowseFlags = [bfUseNewUI]
    Left = 204
    Top = 20
  end
  object XPManifest: TXPManifest
    Left = 294
    Top = 180
  end
end
