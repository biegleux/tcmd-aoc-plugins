object UploadForm: TUploadForm
  Left = 362
  Top = 249
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Upload'
  ClientHeight = 86
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object UploadLabel: TLabel
    Left = 16
    Top = 16
    Width = 51
    Height = 13
    Caption = 'Uploading:'
  end
  object ThLabel: TLabel
    Left = 16
    Top = 56
    Width = 36
    Height = 13
    Caption = 'Thanks'
  end
  object ProgressBar: TProgressBar
    Left = 16
    Top = 32
    Width = 193
    Height = 17
    TabOrder = 0
  end
end
