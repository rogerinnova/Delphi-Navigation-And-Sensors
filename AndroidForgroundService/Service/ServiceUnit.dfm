object FBServiceModule: TFBServiceModule
  Left = 0
  Top = 0
  ClientHeight = 199
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object NotificationCenter: TNotificationCenter
    Left = 120
    Top = 8
  end
end
