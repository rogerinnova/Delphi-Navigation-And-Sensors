object FBServiceModule: TFBServiceModule
  OldCreateOrder = False
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnBind = AndroidServiceBind
  OnUnBind = AndroidServiceUnBind
  OnRebind = AndroidServiceRebind
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object NotificationCenter: TNotificationCenter
    Left = 120
    Top = 8
  end
end
