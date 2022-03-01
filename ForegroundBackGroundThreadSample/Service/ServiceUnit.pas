unit ServiceUnit;

interface

uses
  System.Android.Service, System.Classes, System.Notification, System.Sensors,
  System.Sensors.Components, System.UITypes, System.Generics.Collections,
  // Androidapi.Looper,
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os;

type
//  TLaunchActiveIntent = Procedure(AIntent: JIntent) of Object;

  TISThreadChangedEvent = Procedure(AThread: TThread) of Object;

  TFBServiceModule = class(TAndroidService)
    NotificationCenter: TNotificationCenter;

    procedure AndroidServiceCreate(Sender: TObject);
    function AndroidServiceStartCommand(const Sender: TObject;
      const Intent: JIntent; Flags, StartId: Integer): Integer;
    function AndroidServiceBind(const Sender: TObject; const AnIntent: JIntent)
      : JIBinder;
    procedure AndroidServiceRebind(const Sender: TObject;
      const AnIntent: JIntent);
    function AndroidServiceUnBind(const Sender: TObject;
      const AnIntent: JIntent): Boolean;
    procedure ThreadUpdate(AThread: TThread);
    procedure AndroidServiceDestroy(Sender: TObject);
    function AndroidServiceHandleMessage(const Sender: TObject;
      const AMessage: JMessage): Boolean;
  private const
    NotificationId = -1;
    NotificationChannelId = 'channel_id_foreground_thread_tracking';
  private
    SomeText: string;
    FServiceThread: TThread;
    FIsUpdatingThreadData: Boolean;
    FIsRunningInForeground: Boolean;
    FNotificationManager: JNotificationManager;
    FOnThreadDataUpdated, FOnThreadChangedUpdateForDb: TISThreadChangedEvent;
    FOnLogStartStop: TNotifyEvent;
//    FStartIntent: TLaunchActiveIntent;
    Procedure NotificationNonLocation;
    function GetIntent(const ClassName: string): JIntent;
    function GetNotification: JNotification;
    procedure SetOnLogStartStop(const Value: TNotifyEvent);
  public const
    ActivityClassName = 'com.embarcadero.firemonkey.FMXNativeActivity';
    ServiceClassName =
      'com.embarcadero.services.ForegroundBackgroundSampleService';
    IntentExtraStopLocationTracking =
      'com.embarcadero.intent.extra.STOP_LOCATION_TRACKING';
  public
    DbAccess: TObject;
    procedure StopTimerThread;
    Procedure RestartTimerThread;
    procedure StartThreadTrackingService;
    Procedure DoCloseQueryFunction(Sender: TObject; var CanClose: Boolean);
    Function TimerThreadRunning: Boolean;
    property OnThreadDataUpdated: TISThreadChangedEvent
      read FOnThreadDataUpdated write FOnThreadDataUpdated;
    property OnThreadChangedUpdateForDb: TISThreadChangedEvent
      read FOnThreadChangedUpdateForDb write FOnThreadChangedUpdateForDb;
//    property StartIntent: TLaunchActiveIntent read FStartIntent
//      write FStartIntent;
    property OnLogStartStop: TNotifyEvent read FOnLogStartStop
      write SetOnLogStartStop;
    property IsUpdatingThreadData: Boolean Read FIsUpdatingThreadData;
  end;

  RTimerSampleRecord = Record
    SampleTime, CalcTime: TDateTime;
    Procedure Create(ASample, ACalc: TDateTime);
    Function AsText: String;
  end;

  TTimeThreadDataSource = Class(TObject)
  Private
    FOnThreadUpdate: TISThreadChangedEvent;
    FOnCloseQuery, FOnStartStopProc: TNotifyEvent;
    FLatestList, FOlderList: TList<RTimerSampleRecord>;
    Procedure DropEverySecond(AList: TList<RTimerSampleRecord>; AEndSize: Integer;
      AKeepFirst: Boolean);
    Procedure RationizeLists;
    procedure SetOnThreadUpdate(const Value: TISThreadChangedEvent);
    procedure SetOnCloseQuery(const Value: TNotifyEvent);
    procedure SetOnStartStopProc(const Value: TNotifyEvent);
  public
    AboutText, ProgressText: string;
    Constructor Create;
    Destructor Destroy; override;
    procedure ServiceStartStopNotify(Sender: TObject);
    procedure ThreadFormClose(Sender: TObject; var Action: TCloseAction);
    procedure ThreadFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    Procedure ThreadUpdateEvent(AThread: TThread);
    Procedure MemoText(AMemo: TStrings);
    Property OnThreadUpdate: TISThreadChangedEvent read FOnThreadUpdate
      write SetOnThreadUpdate;
    Property OnCloseQuery: TNotifyEvent read FOnCloseQuery
      write SetOnCloseQuery;
    Property OnStartStopProc: TNotifyEvent read FOnStartStopProc
      write SetOnStartStopProc;
  End;

  TISTestServiceThread = Class(TThread)
  Private
    FDm: TFBServiceModule;
    FTimeDataAccess: TTimeThreadDataSource;
    FOnUpdateData: TISThreadChangedEvent;
    FUpdateCountDb, FUpdateCountDisplay: Integer;
    function SetUpDbAccess: TObject;
    procedure SetISThreadChangedEvent(const Value: TISThreadChangedEvent);
  Protected
    procedure Execute; override;
  Public
    FCalTime: TDateTime;
    CurrentText: String;
    Constructor Create(ADm: TFBServiceModule);
    Destructor Destroy; override;
    Property TimeDataAccess: TTimeThreadDataSource read FTimeDataAccess;
    Property OnUpdateData: TISThreadChangedEvent read FOnUpdateData
      write SetISThreadChangedEvent;
  End;

Function ThreadDataSource: TTimeThreadDataSource;

Const
  CIgnorFirstXLocations = 3;
  GET_TIME = 455;
  SERVICE_TIME = 544;

var
  FBServiceModule: TFBServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses
  System.SysUtils, // Androidapi.JNI.Widget,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Support
  {$Ifdef AccessOnlineDb}
  ,GpsDbBusObjects
  {$Endif};

Var
  LThreadDataSource: TTimeThreadDataSource;

Function ThreadDataSource: TTimeThreadDataSource;
Begin
  if LThreadDataSource = nil then
    TTimeThreadDataSource.Create;
  Result := LThreadDataSource;
End;

procedure TFBServiceModule.AndroidServiceCreate(Sender: TObject);
{$IFDEF ISD102T_DELPHI}
var
  NotificationChannel: TChannel;
{$Endif}
begin

  SomeText := 'Location Text';
  FServiceThread := TISTestServiceThread.Create(self);

{$IFDEF ISD102T_DELPHI}
  // Creates the notification channel that is used by the ongoing
  // notification that presents location updates to the user.
  NotificationChannel := NotificationCenter.CreateChannel;
  NotificationChannel.Id := NotificationChannelId;
  NotificationChannel.Title := 'Foreground location tracking';
  NotificationChannel.Importance := TImportance.Default;

  NotificationCenter.CreateOrUpdateChannel(NotificationChannel);
  // The Run-Time Library does not allow all customizations needed for the
  // ongoing notification used in this demo application.
  // For the mentioned reason, this demo application uses the native APIs for
  // handling notifications.
  FNotificationManager := TJNotificationManager.Wrap
    (TAndroidHelper.Context.getSystemService
    (TJContext.JavaClass.NOTIFICATION_SERVICE));
 {$Else}
  FNotificationManager := TJNotificationManager.Wrap
    (TAndroidHelper.Context.getSystemService
    (TJContext.JavaClass.NOTIFICATION_SERVICE));
{$Endif}
end;

procedure TFBServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  FServiceThread.Free;
  // FreeAndNil(FISLocationSensor);
end;

function TFBServiceModule.AndroidServiceHandleMessage(const Sender: TObject;
  const AMessage: JMessage): Boolean;
var
  LMessage: JMessage;
  LBundle: JBundle;
begin
  case AMessage.what of
    GET_TIME:
    begin
      LBundle := TJBundle.Create;  // we can not send String because is not parcelable
      LMessage := TJMessage.Create;
      LMessage.what := SERVICE_TIME;
      LBundle.putString(TAndroidHelper.StringToJString('Key'), TAndroidHelper.StringToJString('This is a service text !!!'));
      LMessage.obj := LBundle;
      AMessage.replyTo.send(LMessage);
      Result := True;
    end;
  else
    Result := False;
  end;

end;

function TFBServiceModule.AndroidServiceStartCommand(const Sender: TObject;
  const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  // Checks if the intent object contains an extra indicating that the user tapped on the 'Stop location tracking' notification action.
  // if Intent.getBooleanExtra(StringToJString(IntentExtraStopLocationTracking),
  // False) then
  // StopLocationTracking;
  if FServiceThread <> nil then
    if not FServiceThread.Started then
      FServiceThread.Start;

  Result := TJService.JavaClass.START_NOT_STICKY;
end;

function TFBServiceModule.AndroidServiceBind(const Sender: TObject;
  const AnIntent: JIntent): JIBinder;
begin
  // Called when the native activity starts to be visible
  // (goes back to the foreground state) and binds to this service.
  // The native activity started to be visible and, therefore,
  // this service is no longer needed to run in the foreground
  // to avoid being affected by the 'background location limits'
  // introduced as part of Android 8.0.

  if Assigned(FOnLogStartStop) then
    FOnLogStartStop(self);

  JavaService.stopForeground(True);

  FIsRunningInForeground := False;

  Result := GetBinder;
end;

procedure TFBServiceModule.AndroidServiceRebind(const Sender: TObject;
  const AnIntent: JIntent);
begin
  // Called when the native activity starts to be visible
  // (goes back to the foreground state) and binds once again
  // to this service.
  if Assigned(FOnLogStartStop) then
    FOnLogStartStop(self);

  JavaService.stopForeground(True);

  FIsRunningInForeground := False;
end;

function TFBServiceModule.AndroidServiceUnBind(const Sender: TObject;
  const AnIntent: JIntent): Boolean;
begin
  // Called when the native activity stops to be visible
  // (goes to the background state) and unbinds from this service.
  // The native activity stopped to be visible and, therefore, this service
  // needs to run in the foreground, otherwise,
  // it is affected by the background location limits introduced as part
  // of Android 8.0. Running a service in the foreground
  // requires an ongoing notification to be present to the user in order
  // to indicate that the application is actively running.
  JavaService.startForeground(NotificationId, GetNotification);

  if Assigned(FOnLogStartStop) then
    FOnLogStartStop(self);

  FIsRunningInForeground := True;
  Result := True;
end;

// constructor TFBServiceModule.Create(AOwner: TComponent);
// begin
// inherited;
// if FISLocationSensor <> nil then
// FreeAndNil(FISLocationSensor);
// end;

// destructor TFBServiceModule.Destroy;
// begin
// if FISLocationSensor <> nil then
// FreeAndNil(FISLocationSensor);
// inherited;
// end;

procedure TFBServiceModule.DoCloseQueryFunction(Sender: TObject;
  var CanClose: Boolean);
begin
  if DbAccess <> nil then
    TTimeThreadDataSource(DbAccess).ThreadFormCloseQuery(Sender, CanClose);
  JavaService.stopSelf;
end;

function TFBServiceModule.GetIntent(const ClassName: string): JIntent;
begin
  Result := TJIntent.JavaClass.init;
  Result.setClassName(TAndroidHelper.Context.getPackageName,
    TAndroidHelper.StringToJString(ClassName));
end;

function TFBServiceModule.GetNotification: JNotification;

  function GetNotificationIconId: Integer;
  begin
    // Gets the notification icon's resource id. Otherwise, fall backs to
    // the application icon's resource id.
    Result := TAndroidHelper.Context.getResources.getIdentifier
      (StringToJString('drawable/ic_notification'), nil,
      TAndroidHelper.Context.getPackageName);

    if Result = 0 then
      Result := TAndroidHelper.Context.getApplicationInfo.icon;
  end;

  function GetActivityPendingIntent: JPendingIntent;
  var
    Intent: JIntent;
  begin
    // Gets the intent used to start the native activity after the user
    // taps on the ongoing notification that presents
    // location updates to the user.
    Intent := GetIntent(ActivityClassName);

    Result := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0,
      Intent, 0);
  end;

//  function GetServicePendingIntent: JPendingIntent;
//  var
//    Intent: JIntent;
//  begin
    // Gets the intent used to stop this service after the user taps on
    // the 'Stop location tracking' notification action
    // from the ongoing notification that presents location updates to
    // the user.
//    Intent := GetIntent(ServiceClassName);
//    Intent.putExtra(StringToJString(IntentExtraStopLocationTracking), True);
//
//    Result := TJPendingIntent.JavaClass.getService(TAndroidHelper.Context, 0,
//      Intent, TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
//  end;

var
  NotificationTitle: string;
  NotificationContent: string;
  ResultBuilder: JNotificationCompat_Builder;
begin
  // Checks if the location sensor is actually providing location updates.
  if FIsUpdatingThreadData then
  begin
    NotificationTitle := 'Service On';
    // NotificationContent := string.Format('(%f, %f)',
    // [FLocation.Latitude, FLocation.Longitude]);
    NotificationContent := SomeText;
  end
  else
  begin
    NotificationTitle := 'Service Off';
    NotificationContent := SomeText;
  end;

  { Result := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context,
    StringToJString(NotificationChannelId))
    .addAction(TAndroidHelper.Context.getApplicationInfo.icon,
    StrToJCharSequence('Stop location tracking'), GetServicePendingIntent)
    .setPriority(TJNotification.JavaClass.PRIORITY_HIGH).setOngoing(True)
    .setSmallIcon(GetNotificationIconId).setContentIntent
    (GetActivityPendingIntent).setContentTitle
    (StrToJCharSequence(NotificationTitle))
    .setContentText(StrToJCharSequence(NotificationContent))
    .setTicker(StrToJCharSequence(NotificationContent))
    .setWhen(TJDate.Create.getTime).build;
  }
{$IFDEF ISD102T_DELPHI}
  ResultBuilder := TJNotificationCompat_Builder.JavaClass.init
    (TAndroidHelper.Context, StringToJString(NotificationChannelId));
{$ELSE}
  ResultBuilder := TJNotificationCompat_Builder.JavaClass.init
    (TAndroidHelper.Context);
{$ENDIF}
//  ResultBuilder.addAction(TAndroidHelper.Context.getApplicationInfo.icon,
//    StrToJCharSequence('Stop location tracking'), GetServicePendingIntent);
  ResultBuilder.setPriority(TJNotification.JavaClass.PRIORITY_HIGH);
  ResultBuilder.setOngoing(True);
  ResultBuilder.setSmallIcon(GetNotificationIconId);
  ResultBuilder.setContentIntent(GetActivityPendingIntent);
  ResultBuilder.setContentTitle(StrToJCharSequence(NotificationTitle));
  ResultBuilder.setContentText(StrToJCharSequence(NotificationContent));
  ResultBuilder.setTicker(StrToJCharSequence(NotificationContent));
  ResultBuilder.setWhen(TJDate.Create.getTime);

  Result := ResultBuilder.build;
end;

procedure TFBServiceModule.NotificationNonLocation;
begin
  FNotificationManager.notify(NotificationId, GetNotification);
end;

procedure TFBServiceModule.RestartTimerThread;
begin
  if FServiceThread = nil then
  Begin

  End
  Else if FServiceThread.Suspended then
    FServiceThread.Resume;
end;

procedure TFBServiceModule.SetOnLogStartStop(const Value: TNotifyEvent);
begin
  FOnLogStartStop := Value;
end;

procedure TFBServiceModule.StartThreadTrackingService;
begin
  // Starting this service turns it into a started service and, therefore,
  // it can run in the foreground for undefined
  // time and provide real-time location updates. After calling the
  // 'startService' procedure, this service becomes
  // a bound and started service. That being said, this service is
  // destroyed only after the native activity unbinds
  // from it and this service calls the 'stopSelf' procedure.
  if Assigned(FOnLogStartStop) then
    FOnLogStartStop(self);
  TAndroidHelper.Context.startService(GetIntent(ServiceClassName));
end;

procedure TFBServiceModule.StopTimerThread;
begin
  if FServiceThread = nil then
  begin

  end
  else
    FServiceThread.Suspend;
end;

procedure TFBServiceModule.ThreadUpdate(AThread: TThread);
Var
  LThread: TISTestServiceThread;
begin
  if Not(AThread is TISTestServiceThread) then
    Exit;

  FIsUpdatingThreadData := True;

  if Assigned(FOnThreadChangedUpdateForDb) then
    FOnThreadChangedUpdateForDb(AThread);

  // If this service is running in the foreground, this service updates its
  // ongoing notification to present the updated location.
  // If this service is running in the background, this service sends
  // the updated location to the native activity.

  if FIsRunningInForeground then // Service
  begin
    FNotificationManager.notify(NotificationId, GetNotification);
  end
  else
  begin
    if Assigned(FOnThreadDataUpdated) then
      FOnThreadDataUpdated(AThread);
  end;
end;

function TFBServiceModule.TimerThreadRunning: Boolean;
begin
  Result := False;
  if FServiceThread = nil then
    Exit
  Else
    Result := not FServiceThread.Suspended;
end;

// procedure TFBServiceModule.StopLocationTracking;
// begin
// FIsUpdatingLocation := False;
// If FISLocationSensor <> nil then
// FISLocationSensor.StopGps;

// Stopping this service turns it into only a bound service and, therefore, it is destroyed after the native activity
// unbinds from it.
// JavaService.stopSelf;
// end;

{ TTestServiceThread }

constructor TISTestServiceThread.Create(ADm: TFBServiceModule);
begin
  if ADm = nil then
    raise Exception.Create('No TLocationTrackingServiceModule');

  inherited Create(True);
  FDm := ADm;
  FreeOnTerminate := True;
  FDm.SomeText := 'TLocationTrackingServiceModule Created';
  CurrentText := FDm.SomeText;
  OnUpdateData := FDm.ThreadUpdate;
  FUpdateCountDb := 200;
  FUpdateCountDisplay := 10;
end;

destructor TISTestServiceThread.Destroy;
begin
  FDm := nil;
  inherited;
end;

procedure TISTestServiceThread.Execute;
Const
  WtimeMilliSec = 1000;
Var
  Count: Integer;
  CheckClose: Boolean;

begin
  if Terminated then
    Exit;
  Count := 0;
  FCalTime := Now;
  CurrentText := 'Starting at ' + FormatDateTime('dd hh:nn:ss', Now);

  FTimeDataAccess := nil;
  if Assigned(FOnUpdateData) then
    FOnUpdateData(self);
  while Not Terminated do
  begin
    Inc(Count);
    Sleep(WtimeMilliSec);
    FCalTime := FCalTime + WtimeMilliSec / (60 * 60 * 24 * 1000);

    CurrentText := inttostr(Count) + ' : ' + FormatDateTime('dd hh:nn:ss ', Now)
      + ' [' + FormatDateTime('hh:nn:ss.zzz]', Now - FCalTime);

    if FDm <> nil then
    begin
      FDm.SomeText := CurrentText;
      if FDm.DbAccess = nil then
        FDm.DbAccess := SetUpDbAccess; // as TTimeThreadDataSource;
    end;

    if (FDm = nil) then
      Count := 1;

    if (Count mod FUpdateCountDisplay) = 0 then
      if Assigned(FOnUpdateData) then
        FOnUpdateData(self);
    if (Count mod FUpdateCountDb) = 0 then
      if FTimeDataAccess <> nil then
        FTimeDataAccess.OnThreadUpdate(self);

    if (Count mod 200) = 0 then
      if FDm <> nil then
        FDm.NotificationNonLocation;

  end;
  if FTimeDataAccess <> nil then
    FTimeDataAccess.ThreadFormCloseQuery(self, CheckClose);
end;

procedure TISTestServiceThread.SetISThreadChangedEvent
  (const Value: TISThreadChangedEvent);
begin
  FOnUpdateData := Value;
end;

function TISTestServiceThread.SetUpDbAccess: TObject;
Var
  LDbAccess: TTimeThreadDataSource;

begin
  Result := nil;
  if FDm = nil then
    Exit;
  // if FDm.LocationSensor = nil then
  // Exit;

  Try
    LDbAccess := ThreadDataSource;
    Result := LDbAccess;
    FDm.DbAccess := LDbAccess;
    FDm.OnThreadChangedUpdateForDb := LDbAccess.ThreadUpdateEvent;

  {$Ifdef AccessOnlineDb}
    TGpsSaveDb.Create(True, False);
  {$Endif}
  Except
    FreeAndNil(Result);
  End;
end;

{ TTimeThreadDataSource }

constructor TTimeThreadDataSource.Create;
begin
  if LThreadDataSource <> nil then
    raise Exception.Create
      ('TTimeThreadDataSource.Create >> LThreadDataSource<>nil');
  LThreadDataSource := self;
  FLatestList := TList<RTimerSampleRecord>.Create;
  FOlderList := TList<RTimerSampleRecord>.Create;

end;

destructor TTimeThreadDataSource.Destroy;
begin
  FLatestList.Free;
  FOlderList.Free;
  if LThreadDataSource = self then
    LThreadDataSource := nil;
  inherited;
end;

procedure TTimeThreadDataSource.DropEverySecond(AList: TList<RTimerSampleRecord>;
  AEndSize: Integer; AKeepFirst: Boolean);
Var
  i:Integer;
begin
 if AList.Count<AEndSize then
    Exit;

 if AKeepFirst then
    i:=1
    else
    i:=0;
 while i<AList.Count-1  do
   begin
    AList.Delete(i);
    Inc(i);
   end;
end;

procedure TTimeThreadDataSource.MemoText(AMemo: TStrings);
Var
  i: Integer;
begin
  AMemo.Clear;
  // for i:=0 to 20 do
  // Begin
  // AMemo.Add('Some Text');
  // End;
  for i := 0 to FOlderList.Count - 1 do
  Begin
    AMemo.Add(FOlderList[i].AsText);
  End;
  for i := 0 to FLatestList.Count - 1 do
  Begin
    AMemo.Add(FLatestList[i].AsText);
  End;
  AMemo.Add('End');
end;

procedure TTimeThreadDataSource.RationizeLists;
Var
  RatTime, NextOldTime: TDateTime;
Const
  cOldCount = 10;
  cNewCount = 16;

begin
  if FLatestList.Count < 5 then
    Exit;

  RatTime := Now - 2 / 24;

  if FOlderList.Count < 1 then
  Begin
    FOlderList.Add(FLatestList[0]);  //Save First Value
    FLatestList.Delete(0);
  End;

  NextOldTime := FOlderList[FOlderList.Count - 1].SampleTime + 1 / 24;
  While FLatestList[0].SampleTime < RatTime do
  begin
    if FLatestList[0].SampleTime > NextOldTime then
    Begin
      FOlderList.Add(FLatestList[0]);
      NextOldTime := FLatestList[0].SampleTime + 1 / 24;
    End;
    FLatestList.Delete(0);
  end;

  if FOlderList.Count > cOldCount then
    DropEverySecond(FOlderList, cOldCount, True);
  if FLatestList.Count > cOldCount then
    DropEverySecond(FLatestList, cNewCount, False);

end;

procedure TTimeThreadDataSource.ServiceStartStopNotify(Sender: TObject);
begin
  if Assigned(FOnStartStopProc) then
    FOnStartStopProc(Sender);
end;

procedure TTimeThreadDataSource.SetOnCloseQuery(const Value: TNotifyEvent);
begin
  FOnCloseQuery := Value;
end;

procedure TTimeThreadDataSource.SetOnStartStopProc(const Value: TNotifyEvent);
begin
  FOnStartStopProc := Value;
end;

procedure TTimeThreadDataSource.SetOnThreadUpdate
  (const Value: TISThreadChangedEvent);
begin
  FOnThreadUpdate := Value;
end;

procedure TTimeThreadDataSource.ThreadFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self);
end;

procedure TTimeThreadDataSource.ThreadFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self);
  CanClose := True;
end;

procedure TTimeThreadDataSource.ThreadUpdateEvent(AThread: TThread);
Var
  LThread: TISTestServiceThread;
  Sample: RTimerSampleRecord;
begin
  if not(AThread is TISTestServiceThread) then
    Exit;
  if (LThreadDataSource = nil) then Exit;
  if (LThreadDataSource <> self) then Exit;


  LThread := TISTestServiceThread(AThread);
  Sample.Create(Now, LThread.FCalTime);
  FLatestList.Add(Sample);
  RationizeLists;

  // do stuff
  if Assigned(FOnThreadUpdate) then
    FOnThreadUpdate(AThread);
end;

{ RTimerSampleRecord }
function RTimerSampleRecord.AsText: String;
begin
  Result := FormatDateTime('ddd hh.nn.ss', SampleTime) +
    FormatDateTime(' [hh.nn.ss.zzz]', SampleTime - CalcTime);
end;

procedure RTimerSampleRecord.Create(ASample, ACalc: TDateTime);
begin
  SampleTime := ASample;
  CalcTime := ACalc;
end;

end.
