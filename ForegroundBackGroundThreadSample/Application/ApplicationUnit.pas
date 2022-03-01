unit ApplicationUnit;

interface

uses
  System.Android.Service, System.Classes, System.Messaging,
{$IFDEF ISD102T_DELPHI}
  System.Permissions,
{$ENDIF}
  System.Sensors,
  System.Generics.Collections, System.Types, System.UITypes,
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, FMX.Forms, FMX.Dialogs,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  FMX.Platform, FMX.StdCtrls, // IsNavUtils,
  ServiceUnit, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TLocationTrackingForm = class(TForm)
    ToolBarHeader: TToolBar;
    LabelHeader: TLabel;
    ButtonStopTimeTracking: TButton;
    ButtonStartTimeTracking: TButton;
    BtnSendIntent: TButton;
    TimerUpdateForm: TTimer;
    LblPicTitle: TLabel;
    ToolBarZoomPic: TToolBar;
    SpBtnZoomInPict: TSpeedButton;
    SpBtnZoomOut: TSpeedButton;
    SpBtnPicLoc1: TSpeedButton;
    SpBtnPicLocEnd: TSpeedButton;
    SpBtnShowAllProgress: TSpeedButton;
    DataMemo: TMemo;
    BtnBindExternal: TButton;
    BtnSendText: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartTimeTrackingClick(Sender: TObject);
    procedure BtnSendIntentClick(Sender: TObject);
    procedure TimerUpdateFormTimer(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    // procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
    // Shift: TShiftState; X, Y: Single);
    procedure FormActivate(Sender: TObject);
    procedure ButtonStopTimeTrackingClick(Sender: TObject);
    procedure BtnBindExternalClick(Sender: TObject);
    procedure BtnSendTextClick(Sender: TObject);
  private const
    LocationPermission = 'android.permission.ACCESS_FINE_LOCATION';
  private
    FNewData: Boolean;
    FToastText: string;
    ServiceConnection: TLocalServiceConnection; // TLocalServiceConnection;
    Service: TFBServiceModule; // Managed by HandleApplicationEvent
    // Service>Not Null TApplicationEvent.WillBecomeForeground
    // Service >>Nil TApplicationEvent.EnteredBackground:


    FDisplayChoice: integer;
    // FScaleMap, FCenterOffsetMap: TPointf;
    // FOriginMap: RNavigateLongLat;
    // FZoomOut: integer;
    // FCurrentMapList: TList<RNavigateLongLat>;
    // fTotalDistance: Double;
    FDoneFirstTime: Boolean;

{$IFNDEF ISD102T_DELPHI}  // action via Timer

    FRemoteServiceConnection: TRemoteServiceConnection;
    Procedure MakeServiceConnection;
    procedure OnRemoteServiceConnected(const ServiceMessenger: JMessenger);
    procedure OnHandleRemoteMessage(const AMessage: JMessage);
    // Procedure IdleUpdate(Sender:TObject; Var Done:Boolean);
{$ENDIF}
    Function PermissionsOK: Boolean;
    Function ProcessHardwareBack(AKey: Word): Word;
    procedure LogErrorInForm(AError: String);
    procedure SendTextViaIntent(const AText: string);
    procedure ServiceConnected(const LocalService: TAndroidBaseService);
    procedure ServiceDisconnected;
    function HandleApplicationEvent(ApplicationEvent: TApplicationEvent;
      Context: TObject): Boolean;
    procedure StartTimerTracking;
    procedure StopTimerTracking;
    procedure DoButtons;
    procedure ServiceThreadDataUpdated(AThread: TThread);
    // Allow the service to launch an intent  ??only while active >>Service in Background
    // Procedure LaunchServiceIntent(AIntent: JIntent);
    // Procedure ImageChanged(Sender: TObject);
    procedure OnHandleMessage(const AMessage: JMessage);
  end;

var
  LocationTrackingForm: TLocationTrackingForm;

implementation

{$R *.fmx}

uses
  System.SysUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget,
  IsPermissions,
  FMX.Platform.Android, // IsFmxGraphics, GpsUserDataAccess,
  FMX.DialogService;

procedure TLocationTrackingForm.FormActivate(Sender: TObject);
begin
  if not PermissionsOK then
    raise Exception.Create('Need to set permisions to allow location');
  If FDoneFirstTime then
    Exit;

  StartTimerTracking;

  FDoneFirstTime := Service <> nil;
end;

procedure TLocationTrackingForm.FormCreate(Sender: TObject);
var
  ApplicationEventService: IFMXApplicationEventService;
begin
  ServiceConnection := TLocalServiceConnection.Create;
  ServiceConnection.OnConnected := ServiceConnected;
  ServiceConnection.OnDisconnected := ServiceDisconnected;
//  ServiceConnection.OnHandleMessage := OnHandleMessage;

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

procedure TLocationTrackingForm.BtnBindExternalClick(Sender: TObject);
begin
{$IFNDEF ISD102T_DELPHI}  // action via Timer
  if FRemoteServiceConnection=nil then MakeServiceConnection;
  Try
  FRemoteServiceConnection.BindService('com.embarcadero.AppRemoteHost',
    'com.embarcadero.services.RemoteService');
  Except
      On e:Exception do
        FToastText:=e.Message;
  End;
{$Endif}
end;

procedure TLocationTrackingForm.BtnSendIntentClick(Sender: TObject);
begin
  SendTextViaIntent('{Some random text} const AText: string;');
end;

procedure TLocationTrackingForm.BtnSendTextClick(Sender: TObject);
var
  LMessage: JMessage;
const
  GET_STRING = 123;
begin
{$IFNDEF ISD102T_DELPHI}  // action via Timer
  LMessage := TJMessage.JavaClass.obtain(nil, GET_STRING);
  LMessage.replyTo := FRemoteServiceConnection.LocalMessenger;
  FRemoteServiceConnection.ServiceMessenger.send(LMessage);
{$Endif}
end;

procedure TLocationTrackingForm.ButtonStartTimeTrackingClick(Sender: TObject);
begin
  if Not PermissionsOK then
    Exit;

  StartTimerTracking;
end;

procedure TLocationTrackingForm.ButtonStopTimeTrackingClick(Sender: TObject);
begin
  if TimerUpdateForm.Enabled then
    TimerUpdateForm.Enabled := False
  else if (Service <> nil) then
    if Service.TimerThreadRunning then
      Service.StopTimerThread;
end;

procedure TLocationTrackingForm.DoButtons;
begin
  if Service <> nil then
  Begin
    if Service.IsUpdatingThreadData then
    Begin
      ButtonStopTimeTracking.Visible := True;
      ButtonStartTimeTracking.Visible := False;
      BtnSendIntent.Visible := False;
    End
    else
    begin
      ButtonStopTimeTracking.Visible := False;
      ButtonStartTimeTracking.Visible := True;
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
  Service.OnThreadDataUpdated := ServiceThreadDataUpdated;
  // Service.StartIntent := LaunchServiceIntent;
  Service.StartThreadTrackingService;
  OnCloseQuery := Service.DoCloseQueryFunction;
end;

procedure TLocationTrackingForm.ServiceDisconnected;
begin
  // Called when the connection between the native activity and
  // the service has been unexpectedly lost (e.g. when the user
  // manually stops the service using the 'Settings' application).
  Service := nil;
end;

procedure TLocationTrackingForm.ServiceThreadDataUpdated(AThread: TThread);
var
  TimeThread: TISTestServiceThread;
begin
  // if not (AThread is TISTestServiceThread) then
  // Exit;         Always Exits  as only a pointer is passed

  if AThread <> nil then
    Try
      TimeThread := TISTestServiceThread(AThread);
      FToastText := TimeThread.Currenttext;
    Except
      on E: Exception do
        FToastText := E.Message;
    End;
  FNewData := True;
  // Cannot update the Form in this Thread
  // Toast will not ruun
  // Want to keep thread delay minimal

{$IFDEF ISD102T_DELPHI}
  // action via Timer
{$ELSE}
  if Assigned(Application.OnIdle) then
    // already working
  else; // Application.OnIdle:= IdleUpdate;
{$ENDIF}
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
        Result := False; // for debugging
      End;
    TApplicationEvent.BecameActive:
      begin
{$IFDEF ISD102T_DELPHI}
        Result := False; // for debugging
{$ELSE}
        Try
          ServiceConnection.BindService(TFBServiceModule.ServiceClassName);
        Except
          On E: Exception do
            LogErrorInForm(E.Message);
        End;
        Result := True;
{$ENDIF}
      end;

    TApplicationEvent.WillBecomeForeground:
      begin
        // Binding the native activity to the service turns the service into
        // a bound service and, therefore, allows the native
        // activity to directly interact with it using the binder object
        // passed as parameter in the 'ServiceConnected' procedure.
        ServiceConnection.BindService(TFBServiceModule.ServiceClassName);

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
    Result := False;
  end
end;

(*
  {$IFNDEF ISD102T_DELPHI}  //action via Timer
  procedure TLocationTrackingForm.IdleUpdate(Sender: TObject; Var Done: Boolean);
  begin
  Done:=true;
  if Service = nil then
  Exit;
  if Service.DbAccess = nil then
  Exit;
  if not FNewData then
  Exit;
  //  No looper in berlin timer
  //      'java.lang.RuntimeException: Can'''t create handler inside thread that has not called Looper.prepare()'
  FNewData := false;
  if FToastText <> '' then
  Try
  // When the native activity is visible, location updates are presented
  // to the user in toast messages of short duration.
  TJToast.JavaClass.makeText(TAndroidHelper.Context,
  StrToJCharSequence(FToastText), TJToast.JavaClass.LENGTH_LONG).show;
  FToastText:='';
  Except
  on e:Exception do
  begin
  FToastText :=  E.Message;
  FNewData := true;
  end;
  End;
  end;
  {$EndIf} *)

{ procedure TLocationTrackingForm.ImageChanged(Sender: TObject);

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
}
// procedure TLocationTrackingForm.LaunchServiceIntent(AIntent: JIntent);
// begin
// MainActivity.startActivity(AIntent);
// end;

procedure TLocationTrackingForm.LogErrorInForm(AError: String);
begin
  SendTextViaIntent(AError);
end;

{$IFNDEF ISD102T_DELPHI}  // action via Timer
procedure TLocationTrackingForm.MakeServiceConnection;
begin
  if FRemoteServiceConnection=nil then
    Try
        FRemoteServiceConnection := TRemoteServiceConnection.Create;
        FRemoteServiceConnection.OnConnected := OnRemoteServiceConnected;
        FRemoteServiceConnection.OnHandleMessage := OnHandleRemoteMessage;
    Except
      On e:Exception do
        FToastText:=e.Message;
    End;
end;
{$Endif}
procedure TLocationTrackingForm.OnHandleMessage(const AMessage: JMessage);
var
  LStr: JString;

  LBundle: JBundle;
begin
  case AMessage.what of
    SERVICE_TIME:
      begin
        LBundle := TJBundle.Wrap(AMessage.obj);
        LStr := LBundle.getString(TAndroidHelper.StringToJString('Key'));
        TJToast.JavaClass.makeText(TAndroidHelper.Context,
          LStr.subSequence(0, LStr.length),
          TJToast.JavaClass.LENGTH_SHORT).show;
      end;
  else
//    FServiceConnection.Handler.Super.handleMessage(AMessage);
  end;
end;

{$IFNDEF ISD102T_DELPHI}
procedure TLocationTrackingForm.OnHandleRemoteMessage(const AMessage: JMessage);
const
  SERVICE_STRING = 321;
var
  LStr: JString;
  LBundle: JBundle;
begin
  case AMessage.what of
    SERVICE_STRING:
    begin
      LBundle := TJBundle.Wrap(AMessage.obj);
      LStr := LBundle.getString(TAndroidHelper.StringToJString('Key'));
      TJToast.JavaClass.makeText(TAndroidHelper.Context, LStr.subSequence(0, LStr.length),
        TJToast.JavaClass.LENGTH_SHORT).show;
    end;
  else
    FRemoteServiceConnection.Handler.Super.handleMessage(AMessage);
  end;
end;

procedure TLocationTrackingForm.OnRemoteServiceConnected(
  const ServiceMessenger: JMessenger);
begin
  BtnSendText.Enabled:=True;
end;
{$Endif}

function TLocationTrackingForm.PermissionsOK: Boolean;
begin
{$IFDEF ISD102T_DELPHI}
  // Tracking the user's location requires the 'ACCESS_FINE_LOCATION'
  // dangerous permission to be granted at runtime.
  Result := TPermissionsService.DefaultService.IsPermissionGranted
    (LocationPermission);

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
            StartTimerTracking;
        End;
      End, nil);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TLocationTrackingForm.ProcessHardwareBack(AKey: Word): Word;
begin
  Result := 0;
  TDialogService.MessageDialog(
  // MessageDlg(
  'Do you wish to Exit Time Thread Tracking', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
    Procedure(Const AResult: TModalResult)
    begin
      if AResult = mrYes then
      Begin
        Close;
      End;
    end)
end;

procedure TLocationTrackingForm.StartTimerTracking;
begin
  if Service <> nil then
    Service.StartThreadTrackingService;
end;

procedure TLocationTrackingForm.StopTimerTracking;
begin
  if Service <> nil then
    Service.StopTimerThread;
end;

procedure TLocationTrackingForm.TimerUpdateFormTimer(Sender: TObject);
Var
  LDataAccess: TTimeThreadDataSource;
  LMessage: JMessage;

begin
  if Service = nil then
    Exit;
  if Service.DbAccess = nil then
    Exit;
  if not FNewData then
    Exit;
  // No looper in berlin timer
  // 'java.lang.RuntimeException: Can'''t create handler inside thread that has not called Looper.prepare()'
  FNewData := False;
  Try
{$IFDEF ISD102T_DELPHI}
    if FToastText <> '' then
      Try
        // When the native activity is visible, location updates are presented
        // to the user in toast messages of short duration.
        TJToast.JavaClass.makeText(TAndroidHelper.Context,
          StrToJCharSequence(FToastText), TJToast.JavaClass.LENGTH_LONG).show;
        FToastText := '';
      Except
        on E: Exception do
        begin
          FToastText := E.Message;
          FNewData := True;
        end;
      End;
{$ELSE}
    Try
      LMessage := TJMessage.JavaClass.obtain(nil, GET_TIME);
//      LMessage.replyTo := ServiceConnection.LocalMessenger;
//      ServiceConnection.ServiceMessenger.send(LMessage);
    Except
        on E: Exception do
        begin
          FToastText := E.Message;
          FNewData := True;
        end;
    End;
{$ENDIF}
    DoButtons;

    LDataAccess := TTimeThreadDataSource(Service.DbAccess);
    LDataAccess.MemoText(DataMemo.Lines);
    DataMemo.GoToTextEnd;

    Inc(FDisplayChoice);
    // fTotalDistance := LDataAccess.TotalDistance;
    // if FDisplayChoice > 3 then
    // FDisplayChoice := 0;
    // case FDisplayChoice of
    // 0:
    // FCurrentMapList := LDataAccess.ListOfAllProgress;
    // 1:
    // FCurrentMapList := LDataAccess.AdjustedListOfGPSPlots;
    // 2:
    // FCurrentMapList := LDataAccess.ListOfAllProgress;
    // 3:
    // FCurrentMapList := LDataAccess.AdjustedListOfGPSPlots;
    // end;

    // ImageChanged(nil);

  Except
    on E: Exception do
    begin
      FToastText := E.Message;
      FNewData := True;
    end;
  End;
end;

end.
