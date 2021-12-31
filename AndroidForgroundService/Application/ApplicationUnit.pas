unit ApplicationUnit;

interface

uses
  System.Android.Service, System.Classes, System.Messaging,
  System.Permissions,
  System.Sensors,
  System.Generics.Collections, System.Types, System.UITypes,
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, FMX.Forms, FMX.Dialogs,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Platform, FMX.StdCtrls,
  ServiceUnit, IsNavUtils, FMX.Objects;

type
  TLocationTrackingForm = class(TForm)
    ToolBarHeader: TToolBar;
    LabelHeader: TLabel;
    ButtonStopLocationTracking: TButton;
    ButtonStartLocationTracking: TButton;
    BtnSendIntent: TButton;
    Image1: TImage;
    Timer1: TTimer;
    LblPicTitle: TLabel;
    ToolBarZoomPic: TToolBar;
    SpBtnZoomInPict: TSpeedButton;
    SpBtnZoomOut: TSpeedButton;
    SpBtnPicLoc1: TSpeedButton;
    SpBtnPicLocEnd: TSpeedButton;
    SpBtnShowAllProgress: TSpeedButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartLocationTrackingClick(Sender: TObject);
    procedure ButtonStopLocationTrackingClick(Sender: TObject);
    procedure BtnSendIntentClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure SpBtnZoomClick(Sender: TObject);
    procedure SpBtnMapLocClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormActivate(Sender: TObject);
  private const
    LocationPermission = 'android.permission.ACCESS_FINE_LOCATION';
  private
    ServiceConnection: TLocalServiceConnection;
    Service: TFBServiceModule; // Managed by HandleApplicationEvent
    // Service>Not Null TApplicationEvent.WillBecomeForeground
    // Service >>Nil TApplicationEvent.EnteredBackground:

    FDisplayChoice: integer;
    FScaleMap, FCenterOffsetMap: TPointf;
    FOriginMap: RNavigateLongLat;
    FZoomOut: integer;
    FCurrentMapList: TList<RNavigateLongLat>;
    fTotalDistance: Double;
    FDoneFirstTime:boolean;
    Function PermissionsOK:boolean;
    Function ProcessHardwareBack(AKey: Word): Word;
    procedure LogErrorInForm(AError: String);
    procedure SendTextViaIntent(const AText: string);
    procedure ServiceConnected(const LocalService: TAndroidBaseService);
    procedure ServiceDisconnected;
    function HandleApplicationEvent(ApplicationEvent: TApplicationEvent;
      Context: TObject): Boolean;
    procedure StartLocationTracking;
    procedure StopLocationTracking;
    procedure DoButtons;
    procedure ServiceLocationUpdated(const NewLocation: RNavigateLongLat);
    // Allow the service to launch an intent  ??only while active >>Service in Background
    Procedure LaunchServiceIntent(AIntent: JIntent);
    Procedure ImageChanged(Sender: TObject);
  end;

var
  LocationTrackingForm: TLocationTrackingForm;

implementation

{$R *.fmx}

uses
  System.SysUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget,
  IsPermissions,
  FMX.Platform.Android,
  FMX.DialogService, GpsUserDataAccess, IsFmxGraphics;

procedure TLocationTrackingForm.FormActivate(Sender: TObject);
begin
  if not PermissionsOK then
    raise Exception.Create('Need to set permisions to allow location');
  If FDoneFirstTime then Exit;


  StartLocationTracking;
  FDoneFirstTime:=Service<>nil;

end;

procedure TLocationTrackingForm.FormCreate(Sender: TObject);
var
  ApplicationEventService: IFMXApplicationEventService;
begin
  ServiceConnection := TLocalServiceConnection.Create;
  ServiceConnection.OnConnected := ServiceConnected;
  ServiceConnection.OnDisconnected := ServiceDisconnected;

  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, ApplicationEventService) then
    ApplicationEventService.SetApplicationEventHandler(HandleApplicationEvent);
end;

procedure TLocationTrackingForm.FormDestroy(Sender: TObject);
begin
  ServiceConnection.Free;
end;

procedure TLocationTrackingForm.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  try
    if Key = vkHardwareBack then
      Key := ProcessHardwareBack(Key);
    // ResetNoAction;
  Except
    On E: Exception do
    Begin
      LogErrorInForm('Error FormKeyUp::' + E.Message);
    End;
  End;
end;

procedure TLocationTrackingForm.BtnSendIntentClick(Sender: TObject);
begin
  SendTextViaIntent('{Some random text} const AText: string;');
end;

procedure TLocationTrackingForm.ButtonStartLocationTrackingClick
  (Sender: TObject);
begin
  if Not PermissionsOK then Exit;

    StartLocationTracking;
end;

procedure TLocationTrackingForm.ButtonStopLocationTrackingClick
  (Sender: TObject);
begin
  StopLocationTracking;
end;

procedure TLocationTrackingForm.DoButtons;
begin
  if Service <> nil then
  Begin
    if Service.IsUpdatingLocation then
    Begin
      ButtonStopLocationTracking.Visible := True;
      ButtonStartLocationTracking.Visible := false;
      BtnSendIntent.Visible := false;
    End
    else
    begin
      ButtonStopLocationTracking.Visible := false;
      ButtonStartLocationTracking.Visible := True;
      BtnSendIntent.Visible := True;
    end;

  End;
end;

procedure TLocationTrackingForm.SendTextViaIntent(const AText: string);
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setType(StringToJString('text/pas'));
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(AText));
  if MainActivity.getPackageManager.queryIntentActivities(Intent,
    TJPackageManager.JavaClass.MATCH_DEFAULT_ONLY).size > 0 then
    MainActivity.startActivity(Intent)
  else
    ShowMessage('Receiver not found');
end;

procedure TLocationTrackingForm.ServiceConnected(const LocalService
  : TAndroidBaseService);
begin
  // Called when the connection between the native activity and the service
  // has been established. It is used to obtain the
  // binder object that allows the direct interaction between
  // the native activity and the service.
  Service := TFBServiceModule(LocalService);
  Service.LocationUpdated := ServiceLocationUpdated;
  Service.StartIntent := LaunchServiceIntent;
  Service.StartLocationTracking;
  OnCloseQuery := Service.DoCloseQueryFunction;
end;

procedure TLocationTrackingForm.ServiceDisconnected;
begin
  // Called when the connection between the native activity and
  // the service has been unexpectedly lost (e.g. when the user
  // manually stops the service using the 'Settings' application).
  Service := nil;
end;

function TLocationTrackingForm.HandleApplicationEvent(ApplicationEvent
  : TApplicationEvent; Context: TObject): Boolean;
begin
  // It is important to note that a FireMonkey application for Android
  // generally consists of a single activity, which is the
  // native activity mentioned in this demo application. When the native
  // activity starts to be visible (goes to the foreground
  // state), it binds to the service, and when the native activity stops
  // to be visible (goes to the background state), it
  // unbinds from the service. This is needed to allow the service to be
  // aware of the native activity's lifecycle changes.
  // The 'WillBecomeForeground' and 'EnteredBackground' enum cases are
  // equivalent to the 'onStart' and 'onStop' activity callbacks.
  case ApplicationEvent of
    // FinishedLaunching, BecameActive, WillBecomeInactive, EnteredBackground,
    // WillBecomeForeground, WillTerminate, LowMemory, TimeChange, OpenURL
    TApplicationEvent.WillTerminate:
      Begin
        Result := false; // for debugging
      End;
    TApplicationEvent.WillBecomeForeground:
      begin
        // Binding the native activity to the service turns the service into
        // a bound service and, therefore, allows the native
        // activity to directly interact with it using the binder object
        // passed as parameter in the 'ServiceConnected' procedure.
        Try
        ServiceConnection.BindService(TFBServiceModule.ServiceClassName);
        Except
        On E:Exception do
           LogErrorInForm(E.Message);
        End;
        Result := True;
      end;
    TApplicationEvent.EnteredBackground:
      begin
        if Service <> nil then
        begin
          // Unbinding the native activity from the service ensures
          // that the native activity is no longer a bound client and the
          // service can be destroyed by the system if there is no other bound client.
          // The native activity is the only bound client
          // used in this demo application. If the service is also a started service,
          // the system will destroy the service only after
          // a call to the 'stopSelf' procedure.
          ServiceConnection.UnbindService;

          Service := nil;
        end;

        Result := True;
      end;
  else
    Result := false;
  end
end;

procedure TLocationTrackingForm.ImageChanged(Sender: TObject);

begin
  if FCurrentMapList = nil then
    Exit;

  TIsGraphics.SetNewImageBitMap(Image1, $FFFFFF);
  if FOriginMap.NotNull then
    LblPicTitle.Text := FOriginMap.LocatationText(2);

  FScaleMap := TNavGraphics.GetScale(Image1.Bitmap.Canvas, FCurrentMapList,
    FOriginMap, FCenterOffsetMap);
  if FScaleMap.X > 0 then
  Begin
    if FZoomOut > 1 then
      FScaleMap := FScaleMap / FZoomOut
    Else if FZoomOut < 0 then
      FScaleMap := -FScaleMap * FZoomOut;
    TNavGraphics.DrawLocationsOnCanvas(Image1.Bitmap.Canvas, FCurrentMapList,
      FOriginMap, FScaleMap, FCenterOffsetMap, 1,
      'Total ' + TextMeters(fTotalDistance, 2, 0, 2), 0.0)
  End;
end;

procedure TLocationTrackingForm.ImageMouseDown(Sender: TObject;
Button: TMouseButton; Shift: TShiftState; X, Y: Single);
Var
  Loc: TPointf;
begin
  if Sender = Image1 then
  Begin
    Loc := TIsGraphics.LocationAsDraw(TPointf.Create(X, Y),
      Image1.Bitmap.Canvas);
    TNavGraphics.RelocateNavPointFromMapRef(FOriginMap, FScaleMap, Loc,
      FCenterOffsetMap);
    // Dec(FLastCount, 2);
    // End
    // Else if Sender = Image2 then
    // Begin
    // Loc := TIsGraphics.LocationAsDraw(TPointf.create(X, Y),Image2.Bitmap.Canvas);
    // TNavGraphics.RelocateNavPointFromMapRef(FOriginMapMesr, FScaleMapMesr, Loc,
    // FCenterOffsetMapMesr);
    // Dec(FLastCountMeasure, 2);
  End;
  ImageChanged(Sender);
end;

procedure TLocationTrackingForm.LaunchServiceIntent(AIntent: JIntent);
begin
  MainActivity.startActivity(AIntent);
end;

procedure TLocationTrackingForm.LogErrorInForm(AError: String);
begin
  SendTextViaIntent(AError);
end;

function TLocationTrackingForm.PermissionsOK: boolean;
begin
  // Tracking the user's location requires the 'ACCESS_FINE_LOCATION'
  // dangerous permission to be granted at runtime.
  Result:= TPermissionsService.DefaultService.IsPermissionGranted('android.permission.ACCESS_BACKGROUND_LOCATION');
//  Result:= TPermissionsService.DefaultService.IsPermissionGranted(LocationPermission);

  if not Result then
    ISRequestPermission([LocationPermission,
      'android.permission.ACCESS_BACKGROUND_LOCATION'], True, True,
      Procedure(Const AllGood: Boolean;
        Const AAllGranted, APartialGranted: PmsmSet; Const AFailed: String)
      Begin
        if AFailed <> '' then
          TDialogService.ShowMessage('Permission Fail:' + AFailed);
        if AllGood then
        begin
          if AFailed <> '' then
            BtnSendIntent.Text := AFailed
           else
            StartLocationTracking;
        End;
      End, nil);
end;

function TLocationTrackingForm.ProcessHardwareBack(AKey: Word): Word;
begin
  Result := 0;
  TDialogService.MessageDialog(
  // MessageDlg(
  'Do you wish to Exit GPS', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
    Procedure(Const AResult: TModalResult)
    begin
      if AResult = mrYes then
      Begin
        // UpdateTimer.Enabled := false; // stop timer
        Close;
      End;
    end)
  // ProcessHardwareBackDlgRtn)
end;

procedure TLocationTrackingForm.StartLocationTracking;
begin
  if Service <> nil then
    Service.StartLocationTracking;
end;

procedure TLocationTrackingForm.StopLocationTracking;
begin
  if Service <> nil then
    Service.StopLocationTracking;
end;

procedure TLocationTrackingForm.Timer1Timer(Sender: TObject);
Var
  LDataAccess: TGpsDataSource;
begin
  if Service = nil then
    Exit;
  if Service.DbAccess = nil then
    Exit;
  DoButtons;
  LDataAccess := TGpsDataSource(Service.DbAccess);

  Inc(FDisplayChoice);
  fTotalDistance := LDataAccess.TotalDistance;
  if FDisplayChoice > 3 then
    FDisplayChoice := 0;
//  case FDisplayChoice of
//    0:
//      FCurrentMapList := LDataAccess.ListOfAllProgress;
//    1:
      FCurrentMapList := LDataAccess.AdjustedListOfGPSPlots;
//    2:
//      FCurrentMapList := LDataAccess.ListOfAllProgress;
//    3:
//      FCurrentMapList := LDataAccess.AdjustedListOfGPSPlots;
//  end;

  ImageChanged(nil);
end;

procedure TLocationTrackingForm.ServiceLocationUpdated(const NewLocation
  : RNavigateLongLat);
var
  Text: string;
  SvData: TGpsDataSource;
begin
  Text := NewLocation.LocatationText(1);
  // When the native activity is visible, location updates are presented
  // to the user in toast messages of short duration.
  TJToast.JavaClass.makeText(TAndroidHelper.Context, StrToJCharSequence(Text),
    TJToast.JavaClass.LENGTH_LONG).show;

  //If the app is bound to the service you can access service data.
  if Service = nil then
    LabelHeader.Text := 'No Service'
  else
  begin
    SvData := TGpsDataSource(Service.DbAccess);
    if SvData <> nil then
      LabelHeader.Text := FormatFloat('###0.00 ', SvData.SpeedKph) + ' kmph';
  end;
end;

procedure TLocationTrackingForm.SpBtnMapLocClick(Sender: TObject);
begin
  if FCurrentMapList = nil then
    Exit;

  if FCurrentMapList.Count > 0 then
    if Sender = SpBtnPicLoc1 then
      FOriginMap := FCurrentMapList[0]
    Else if Sender = SpBtnPicLocEnd then
      FOriginMap := FCurrentMapList[FCurrentMapList.Count - 1];

  ImageChanged(Sender);
end;

procedure TLocationTrackingForm.SpBtnZoomClick(Sender: TObject);
begin
  if Sender = SpBtnZoomOut then
    Inc(FZoomOut, 5)
  Else
    Dec(FZoomOut, 5);

  ImageChanged(Sender);
end;

end.
