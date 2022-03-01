object SvcDMExho: TSvcDMExho
  OldCreateOrder = False
  OnCreate = AndroidServiceCreate
  OnBind = AndroidServiceBind
  OnHandleMessage = AndroidServiceHandleMessage
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
end
