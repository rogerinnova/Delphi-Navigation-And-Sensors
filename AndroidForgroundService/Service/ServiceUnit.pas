unit ServiceUnit;

interface

uses
  System.Android.Service, System.Classes, System.Notification, System.Sensors,
  System.Sensors.Components, System.UITypes,
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os, IsMobileSensors, IsNavUtils;

type
  TLocationUpdated = procedure(const NewLocation: RNavigateLongLat) of object;
  TLaunchActiveIntent = Procedure(AIntent: JIntent) of Object;

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
    procedure LocationSensorLocationChanged(ALocSensor: TIsLocationSensor);
    procedure AndroidServiceDestroy(Sender: TObject);
  private const
    NotificationId = -1;
    NotificationChannelId = 'channel_id_foreground_location_tracking';
  private
    SomeText: string;
    FServiceThread: TThread;
    FIsUpdatingLocation: Boolean;
    FIsRunningInForeground: Boolean;
    FNotificationManager: JNotificationManager;
    FLocation: RNavigateLongLat;
    FLocationUpdated: TLocationUpdated;
    FOnLocationUpdateForDb: TISLocationChangedEvent;
    FStartIntent: TLaunchActiveIntent;
    FISLocationSensor: TIsLocationSensor;
    procedure SendTextViaIntent(const AText: string);
    Procedure NotificationNonLocation;
    function GetIntent(const ClassName: string): JIntent;
    function GetNotification: JNotification;
  public const
    ActivityClassName = 'com.embarcadero.firemonkey.FMXNativeActivity';
    ServiceClassName = 'com.embarcadero.services.ForegroundBackgroundService';
    IntentExtraStopLocationTracking =
      'com.embarcadero.intent.extra.STOP_LOCATION_TRACKING';
  public
    DbAccess: TObject;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function LocationSensor: TIsLocationSensor;
    Procedure DoCloseQueryFunction(Sender: TObject; var CanClose: Boolean);
    property LocationUpdated: TLocationUpdated read FLocationUpdated
      write FLocationUpdated;
    property OnLocationUpdateForDb: TISLocationChangedEvent
      read FOnLocationUpdateForDb write FOnLocationUpdateForDb;
    property StartIntent: TLaunchActiveIntent read FStartIntent
      write FStartIntent;
    property IsUpdatingLocation: Boolean Read FIsUpdatingLocation;
    procedure StartLocationTracking;
    procedure StopLocationTracking;
  end;

  TTestServiceThread = Class(TThread)
  Private
    FDm: TFBServiceModule;
    // FGpsDAtaAccess:TGpsDataSource;
    function SetUpDbAccess: TObject;
  Protected
    procedure Execute; override;
  Public
    Constructor Create(ADm: TFBServiceModule);
    Destructor Destroy; override;
  End;

var
  FBServiceModule: TFBServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses
{$IfDef AccessOnlineDb}
  GpsDbBusObjects,
{$Endif}
  GpsUserDataAccess,
  System.SysUtils, // Androidapi.JNI.Widget,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Support;

procedure TFBServiceModule.AndroidServiceCreate(Sender: TObject);
var
  NotificationChannel: TChannel;
begin
  SomeText := 'Location Text';
  FServiceThread := TTestServiceThread.Create(self);

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
end;

procedure TFBServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  FServiceThread.Free;
  FreeAndNil(FISLocationSensor);
end;

function TFBServiceModule.AndroidServiceStartCommand(const Sender: TObject;
  const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  // Checks if the intent object contains an extra indicating that the user tapped on the 'Stop location tracking' notification action.
  if Intent.getBooleanExtra(StringToJString(IntentExtraStopLocationTracking),
    False) then
    StopLocationTracking;
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

  FIsRunningInForeground := True;
  Result := True;
end;

constructor TFBServiceModule.Create(AOwner: TComponent);
begin
  inherited;
  if FISLocationSensor <> nil then
    FreeAndNil(FISLocationSensor);
end;

destructor TFBServiceModule.Destroy;
begin
  if FISLocationSensor <> nil then
    FreeAndNil(FISLocationSensor);
  inherited;
end;

procedure TFBServiceModule.DoCloseQueryFunction(Sender: TObject;
  var CanClose: Boolean);
begin
  if DbAccess <> nil then
    TGpsDataSource(DbAccess).NavCloseQuery(Sender, CanClose);
  // TGpsDataSource(DbAccess).SetOnGpsStartStopProc(nil);
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

  function GetServicePendingIntent: JPendingIntent;
  var
    Intent: JIntent;
  begin
    // Gets the intent used to stop this service after the user taps on
    // the 'Stop location tracking' notification action
    // from the ongoing notification that presents location updates to
    // the user.
    Intent := GetIntent(ServiceClassName);
    Intent.putExtra(StringToJString(IntentExtraStopLocationTracking), True);

    Result := TJPendingIntent.JavaClass.getService(TAndroidHelper.Context, 0,
      Intent, TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
  end;

var
  NotificationTitle: string;
  NotificationContent: string;
  ResultBuilder: JNotificationCompat_Builder;
begin
  // Checks if the location sensor is actually providing location updates.
  if FIsUpdatingLocation then
  begin
    NotificationTitle := 'Long/Lat Service On';
    // NotificationContent := string.Format('(%f, %f)',
    // [FLocation.Latitude, FLocation.Longitude]);
    NotificationContent := SomeText + ' : ' + FLocation.LocatationText(2);
  end
  else
  begin
    NotificationTitle := 'Long/Lat Service Off';
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
  ResultBuilder := TJNotificationCompat_Builder.JavaClass.init
    (TAndroidHelper.Context, StringToJString(NotificationChannelId));
  ResultBuilder.addAction(TAndroidHelper.Context.getApplicationInfo.icon,
    StrToJCharSequence('Stop location tracking'), GetServicePendingIntent);
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

function TFBServiceModule.LocationSensor: TIsLocationSensor;
begin
  if FISLocationSensor = nil then
  Begin
    FISLocationSensor := TIsLocationSensor.Create;
    FISLocationSensor.OnLocChange := LocationSensorLocationChanged;
    FISLocationSensor.DoRunningAveLoc:=true;
    // FISLocationSensor.OnBeforeAverageReset :=???

  End;
  Result := FISLocationSensor;
end;

// procedure TFBServiceModule.LocationSensorLocationChanged(Sender: TObject;
// const OldLocation, NewLocation: TLocationCoord2D);
procedure TFBServiceModule.LocationSensorLocationChanged
  (ALocSensor: TIsLocationSensor);
begin
  FIsUpdatingLocation := True;
  FLocation := ALocSensor.CurrentLocation;
  if ALocSensor <> nil then
    if Assigned(FOnLocationUpdateForDb) then
      FOnLocationUpdateForDb(ALocSensor);

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
    if Assigned(FLocationUpdated) then
      FLocationUpdated(FLocation);
  end;
end;

procedure TFBServiceModule.NotificationNonLocation;
begin
  FNotificationManager.notify(NotificationId, GetNotification);
end;

procedure TFBServiceModule.SendTextViaIntent(const AText: string);
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setType(StringToJString('text/pas'));
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  if FIsRunningInForeground then // Service Forground
    Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT,
      StringToJString('IsRunningInForeground ' + AText))
  Else
    Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(AText));

  If TAndroidHelper.Context.getPackageManager.queryIntentActivities(Intent,
    TJPackageManager.JavaClass.MATCH_DEFAULT_ONLY).size > 0 then
  Begin
    if FIsRunningInForeground then // Service Forground
      TAndroidHelper.Context.startActivity(Intent)
    Else if Assigned(FStartIntent) then
      FStartIntent(Intent)
    else
      Exit;
  End
  else
    Intent := nil;
end;

procedure TFBServiceModule.StartLocationTracking;
begin
  // Starting this service turns it into a started service and, therefore,
  // it can run in the foreground for undefined
  // time and provide real-time location updates. After calling the
  // 'startService' procedure, this service becomes
  // a bound and started service. That being said, this service is
  // destroyed only after the native activity unbinds
  // from it and this service calls the 'stopSelf' procedure.

  TAndroidHelper.Context.startService(GetIntent(ServiceClassName));

  LocationSensor.StartNav;
end;

procedure TFBServiceModule.StopLocationTracking;
begin
  FIsUpdatingLocation := False;
  If FISLocationSensor <> nil then
    FISLocationSensor.StopGps;

  // Stopping this service turns it into only a bound service and, therefore, it is destroyed after the native activity
  // unbinds from it.
  // JavaService.stopSelf;
end;

{ TTestServiceThread }

constructor TTestServiceThread.Create(ADm: TFBServiceModule);
begin
  if ADm = nil then
    raise Exception.Create('No TLocationTrackingServiceModule');

  inherited Create(True);
  FDm := ADm;
  FreeOnTerminate := True;
  FDm.SomeText := 'TLocationTrackingServiceModule Created';
end;

destructor TTestServiceThread.Destroy;
begin
  FDm := nil;
  inherited;
end;

procedure TTestServiceThread.Execute;
Const
  WtimeMilliSec = 1000;
Var
  Count: Integer;
  CheckClose: Boolean;
  ThisTime,CalcTime: TDateTime;
  LDbAccess: TGpsDataSource;
begin
  if Terminated then
    Exit;
  Count := 0;
  CalcTime:=Now;
  LDbAccess := nil;
  while Not Terminated do
  begin
    ThisTime := Now;
    Inc(Count);
    Sleep(WtimeMilliSec);
    if LDbAccess = nil then
      LDbAccess := SetUpDbAccess as TGpsDataSource;

    if (Count = 5) or ((Count Mod 50) = 0) then
      if (FDm <> nil) then
        FDm.SendTextViaIntent('Service Thread Time is ' +
          FormatDateTime('hh:nn:ss', CalcTime))
      else
        Count := 1;

    if (Count mod 20) = 0 then
      if FDm <> nil then
        FDm.NotificationNonLocation;


    CalcTime := CalcTime-ThisTime + Now;
    ThisTime := Now;

    if FDm <> nil then
     Begin
      FDm.SomeText := inttostr(Count) + ' : ' + FormatDateTime('dd hh:nn:ss ',
        Now) + ' [' + FormatDateTime('dd hh:nn:ss]', CalcTime);
     if LDbAccess <> nil then
      LDbAccess.ProgressText := FDm.SomeText;
     End;

    CalcTime := CalcTime-ThisTime + Now;  //to allow for break points
  end;
  if LDbAccess <> nil then
    LDbAccess.NavCloseQuery(self, CheckClose);
end;

function TTestServiceThread.SetUpDbAccess: TObject;
Var
  LDbAccess: TGpsDataSource;

begin
  Result := nil;
  if FDm = nil then
    Exit;
  if FDm.LocationSensor = nil then
    Exit;

  Try
    LDbAccess := GPSDataSource;
    Result := LDbAccess;
    FDm.DbAccess := LDbAccess;
    FDm.OnLocationUpdateForDb := LDbAccess.AddChangedLoc;
    FDm.LocationSensor.OnGPSStartStop := LDbAccess.DoGpsStartStopProc;
{$IfDef AccessOnlineDb}
    TGpsSaveDb.Create(True, False);
{$Endif}
  Except
    FreeAndNil(Result);
  End;
end;

end.
