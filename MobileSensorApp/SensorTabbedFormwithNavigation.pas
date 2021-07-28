unit SensorTabbedFormwithNavigation;

interface

{ $I InnovaMultiPlatLibDefs.inc }

uses
  System.SysUtils, Fmx.Objects, System.Types, System.UITypes, System.Classes,
  System.math,
  IsFmxGraphics, IsPermissions,
{$IFDEF Android}
  Posix.Unistd,
{$ENDIF}
  System.Variants, System.ioutils, System.Generics.Collections,
  Fmx.Types, Fmx.Controls, Fmx.Forms, Fmx.Graphics, {Fmx.Dialogs,}
  Fmx.DialogService, Fmx.TabControl,
  Fmx.StdCtrls, Fmx.Controls.Presentation,
  Fmx.Gestures, System.Actions, Fmx.ActnList, IsMobileSensors, IsNavUtils,
  Fmx.ScrollBox, Fmx.Memo, Fmx.WebBrowser, Fmx.Edit, Fmx.EditBox, Fmx.SpinBox,
  Fmx.Memo.Types;

type
  TTabbedwithNavigationForm = class(TForm)
    TbCtrlMain: TTabControl;
    TbItemGenNav: TTabItem;
    TbCtrlGenNav: TTabControl;
    TbItemNavScrn: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TbItemHoldValues: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
    TbItemMeasure: TTabItem;
    TabAbout: TTabItem;
    ToolBar4: TToolBar;
    TiTestValue1: TTabItem;
    ToolBar5: TToolBar;
    lblTestValues: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    TbItemRecords: TTabItem;
    LblSpeed: TLabel;
    LblPos: TLabel;
    LblStartPos: TLabel;
    LblDistance: TLabel;
    LblDistance2: TLabel;
    LblStartPos2: TLabel;
    LblPos2: TLabel;
    LblSpeed2: TLabel;
    LblHeading: TLabel;
    LblBearing: TLabel;
    LblBearing2: TLabel;
    LblHeading2: TLabel;
    LblT1p3: TLabel;
    BtnNext2: TSpeedButton;
    ToolBar6: TToolBar;
    BtnBack3: TSpeedButton;
    LblTestStart: TLabel;
    LblTestEnd: TLabel;
    LblTestDistance: TLabel;
    LblBackAgain: TLabel;
    LblTestBearing: TLabel;
    LblReversBearing: TLabel;
    MmoTrkLocations: TMemo;
    MmoPreviusLocations: TMemo;
    TbCtrlMeasure: TTabControl;
    TbItemRunningAverage: TTabItem;
    TBarTopHomeIn: TToolBar;
    LblHomeInCaption: TLabel;
    SpBtnHomeInNext: TSpeedButton;
    LblHomeInMain: TLabel;
    LblHomeInBlack: TLabel;
    LblBtmHomein: TLabel;
    LblBtm2HomeIn: TLabel;
    LblHomeInMinor: TLabel;
    LblBtm1HomeIn: TLabel;
    TbItemAveDetailst: TTabItem;
    ToolBar8: TToolBar;
    LblSnapTitle: TLabel;
    SpBtnAveSnapNxt: TSpeedButton;
    SpBtnAveSnapBck: TSpeedButton;
    LblCurentLocationText: TLabel;
    LblAverageLocationText: TLabel;
    LblMetersPerSecondDetail: TLabel;
    LblAveTimeSampling: TLabel;
    LblNoOfSamples: TLabel;
    TbItemPastSamples: TTabItem;
    ToolBar9: TToolBar;
    LblAveSnapTitle: TLabel;
    SpBtnAveRsltsBck: TSpeedButton;
    TBarBotHomeIn: TToolBar;
    LblStartStopAverage: TLabel;
    SpBtnStartStopHomeIn: TSpeedButton;
    UpdateTimer: TTimer;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    PreviousTabAction2: TPreviousTabAction;
    NextTabAction2: TNextTabAction;
    MmoAbout: TMemo;
    TIMap: TTabItem;
    WebBrowser1: TWebBrowser;
    LblLocationDev: TLabel;
    LblAveSpeed: TLabel;
    LblAveHding: TLabel;
    LblAveAlt: TLabel;
    MmoSamplingDetails: TMemo;
    LblAveSnapBottom: TLabel;
    SpBtnDtlsStopStart: TSpeedButton;
    SpBtnRestDetails: TSpeedButton;
    SpBtnResetAll: TSpeedButton;
    ToolBar3: TToolBar;
    SpBtnSaveAve: TSpeedButton;
    SpBtnSaveAllLoc: TSpeedButton;
    TbItemPicture: TTabItem;
    ToolBarPicture: TToolBar;
    LblPicTitle: TLabel;
    SpBtnPicBack: TSpeedButton;
    SpBtnPicNext: TSpeedButton;
    PnlMap: TPanel;
    Image1: TImage;
    LblTotalDist: TLabel;
    ToolBarZoomPic: TToolBar;
    SpBtnZoomInPict: TSpeedButton;
    SpBtnZoomOut: TSpeedButton;
    TbItemMeasPict: TTabItem;
    PnlMeasPict: TPanel;
    Image2: TImage;
    ToolBarMeasPictTop: TToolBar;
    LblMeasurePicTop: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ToolBarZoomMeasure: TToolBar;
    SpBtnZoomInMeasure: TSpeedButton;
    SpBtnZoomOutMeasure: TSpeedButton;
    SpBtnPicLoc1: TSpeedButton;
    SpBtnPicLocEnd: TSpeedButton;
    SpBtnMsrLocStart: TSpeedButton;
    SpBtnMsrLocEnd: TSpeedButton;
    SpBtnStartGPS: TSpeedButton;
    SpBtnStartMotion: TSpeedButton;
    SpBtnStopAll: TSpeedButton;
    SpBtnShowAllProgress: TSpeedButton;
    SpBtnSetTestValues: TSpeedButton;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure TbCtrlChangeUpdate(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure SpBtnStartStopHomeInClick(Sender: TObject);
    procedure SpBtnRestDetailsClick(Sender: TObject);
    procedure SpBtnResetAllClick(Sender: TObject);
    procedure SaveAverageData(Sender: TObject);
    procedure SaveAllLocClick(Sender: TObject);
    procedure SpBtnZoomClick(Sender: TObject);
    procedure SpBtnZoomMeasureClick(Sender: TObject);
    procedure SpBtnMapLocClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SpBtnStartMotionClick(Sender: TObject);
    procedure RemoveAllSensors(Sender: TObject);
    procedure SpBtnShowAllProgressClick(Sender: TObject);
    procedure SpBtnSetTestValuesClick(Sender: TObject);
  private
    { Private declarations }
    FTestNo: integer;
    FTrackOnWebPage: Boolean; // update Web page frequently
    FLastWebQuery, FStartTime, FNextPastSampleUpdate: TdateTime;
    FSampleResultArray: Array of RRunAveLocRec;
    // Array of running Average Samples for map
    FListOfProgress, // After Glitch Suppession
    FListOfAllProgress: TList<RNavigateLongLat>; // No Glitch Suppession
    // List of records for Map
    FListOfSampleTimes: TList<TdateTime>;
    FTotalDistance: Double;
    FLastLocation, FLastSignificantLocation: RNavigateLongLat;
    FLastSigLocIndex, FPrevSigLocIndex: integer;
    FLoctnSensorData: TIsLocationSensor;
    FMotionSensorData: TIsMotionSensor;
    FOrientationSensorData: TIsOrientationSensor;
    FManagerData: TIsSensorManager;
    FSecondsSinceStartCounter, fSecondsCounter: integer;
    // Count of Seconds doing Average
    FLastCountMeasure, FZoomOutMeasure, FLastCount, FZoomOut: integer;
    FOriginMapOfPos: RNavigateLongLat;
    FScaleMapOfPos, FCenterOffsetMapOfPos: TPointF;
    FOriginMapMesr: RNavigateLongLat;
    FScaleMapMesr, FCenterOffsetMapMesr: TPointF;
    FLastTrkVal: String;
    FShowAllProgress, FDoneTestPage, FTestValuesLoading: Boolean;
    FFailedPermissions, FCompileStgr: string;
    FAllLocationStringList: TStringList; // Flags Glitch Suppession
    Function SecondsAsText(ASeconds: integer): String;
    Function CountAsText(ACount: integer): String;
    Function SaveFileDirectory(ADateTimeDir: Boolean): String;
    Procedure AddToMmoTrkLocations(AText: String);
    Procedure LocChanged(ASensor: TIsLocationSensor);
    Procedure EndRunnibgAverage(ASensor: TIsLocationSensor);
    Procedure MotionChangedEvent(ASensor: TIsMotionSensor);
    Procedure OrientationChangedEvent(ASensor: TIsOrientationSensor);
    Function SaveStringListAsFile(AList: TStringList; AFileName: String;
      ADateTimeDir: Boolean = false): String;
    Function SaveImageBitMapAsFile(AImage: TImage; AFileName: String;
      ADateTimeDir: Boolean = false): String;
    // Procedure UpdateProgressMemo;
    Procedure SetTestPage;
    Procedure SetAboutPage;
    Procedure SetGeneralNav;
    Procedure SetHoldNav;
    Procedure SetMapOfPositions;
    Procedure SetMapOfMeasure;
    // Procedure SetNavRecords;
    Procedure SetRunningAverage;
    Procedure SetRunningAverageDetails;
    Procedure SetPastSamples;
    Function ProcessHardwareBack(AKey: Word): Word;
    Procedure SetSpeedStartStopHomeButton(ADoingRunningLocation: Boolean);
    procedure StartGps;
    procedure StartMotion;
    procedure ResetDistanceTraveled;
  public
    { Public declarations }
    Procedure LoadMap;
  end;

var
  TabbedwithNavigationForm: TTabbedwithNavigationForm;

Const
  CIgnorFirstXLocations = 3;

implementation

{$R *.fmx}
{$IFDEF VER310}
{$ELSE}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}
{$ENDIF}

Const
  StableCount = 4;

procedure TTabbedwithNavigationForm.AddToMmoTrkLocations(AText: String);

begin
  if FLastTrkVal = AText then
    exit;

  FLastTrkVal := AText;
  MmoTrkLocations.Lines.Add(AText + FormatDateTime(' >hh:mm:ss', Now));
  if MmoTrkLocations.Lines.Count > 30 then
  begin
    while MmoTrkLocations.Lines.Count > 20 do
      MmoTrkLocations.Lines.Delete(0);
    MmoTrkLocations.Lines[0] := ('Culling ' + FormatDateTime('hh:nn:ss', Now));
  end;
end;

function TTabbedwithNavigationForm.CountAsText(ACount: integer): String;
begin
  if ACount < 1000 then
    Result := IntToStr(ACount)
  Else
    Result := FormatFloat('0.0', ACount / 1000) + 'K';
end;

procedure TTabbedwithNavigationForm.EndRunnibgAverage
  (ASensor: TIsLocationSensor);
Var
  NxtLocInArray: integer;
  Sample: RRunAveLocRec;

begin
  if ASensor <> FLoctnSensorData then
    exit;

  if FLoctnSensorData.NoOfConsistenLocSamples > 5 then
  Begin
    // Snapshot before moving sammple
    NxtLocInArray := Length(FSampleResultArray);
    SetLength(FSampleResultArray, NxtLocInArray + 1);
    Sample := FLoctnSensorData.RunningLocaltionSample;
    FSampleResultArray[NxtLocInArray] := Sample;
  End
  Else
    MmoTrkLocations.Lines.Add(FormatDateTime('ddd hh:nn', Now) +
      ' Change samples= ' + IntToStr(FLoctnSensorData.NoOfConsistenLocSamples));
  SetRunningAverage;
  fSecondsCounter := 0;
end;

procedure TTabbedwithNavigationForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Try
    if FLoctnSensorData <> nil then
      if FLoctnSensorData.Started then
        FLoctnSensorData.Started := false;
    RemoveAllSensors(Nil);
  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
  CanClose := True;
end;

procedure TTabbedwithNavigationForm.FormCreate(Sender: TObject);

begin
  FStartTime := Now;
  FSecondsSinceStartCounter := 0;
  FZoomOut := 5;
  // need to create first
  FAllLocationStringList := TStringList.Create;
  FListOfProgress := TList<RNavigateLongLat>.Create;
  FListOfAllProgress := TList<RNavigateLongLat>.Create;
  FListOfSampleTimes := TList<TdateTime>.Create;
  try { This defines the default active tab at runtime }
    TbCtrlMain.ActiveTab := TbItemGenNav;
    TbCtrlGenNav.ActiveTab := TbItemNavScrn;
    TbCtrlMeasure.ActiveTab := TbItemRunningAverage;

    // try All
    // Pmsion = (Gps, Camera, DataAcc, Network, WiFi, BlueTooth);
    PermissionsGranted([Pmsion.Gps { , Pmsion.Camera } , Pmsion.DataAcc { ,
        Pmsion.Network, Pmsion.WiFi, Pmsion.BlueTooth } ], false, false,
      Procedure(Const AllGood: Boolean;
        Const AAllGranted, APartialGranted: PmsmSet; Const AFailed: String)
      Begin
        if AllGood then
        begin
          FFailedPermissions := AFailed;
          StartGps;
          // StartMotion;
          SetTestPage;
          SetAboutPage;
          // TabControl2Change(nil);
        End;
        if AFailed <> '' then
          TDialogService.ShowMessage('Permission Fail:' + AFailed);
      End);
    ResetDistanceTraveled;

    SpBtnStartStopHomeInClick(nil);

  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.FormDestroy(Sender: TObject);
begin
  try
    if FLoctnSensorData <> nil then
      if FLoctnSensorData.Started then
        FLoctnSensorData.Started := false;
    FLoctnSensorData.Free;
    FMotionSensorData.Free;
    FOrientationSensorData.Free;
    FManagerData.Free;
    FListOfProgress.Free;
    FListOfAllProgress.Free;
    FListOfSampleTimes.Free;
    FAllLocationStringList.Free;
  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

Procedure TTabbedwithNavigationForm.FormKeyUp(Sender: TObject; var Key: Word;
var KeyChar: Char; Shift: TShiftState);
begin
  try
    if Key = vkHardwareBack then
      Key := ProcessHardwareBack(Key);
    // ResetNoAction;
  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.FormShow(Sender: TObject);
begin
  Try
    if FLoctnSensorData <> nil then
      if not FLoctnSensorData.Started then
        FLoctnSensorData.StartNav;
  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.GestureDone(Sender: TObject;
const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  try
    case EventInfo.GestureID of
      sgiLeft:
        begin
          if TbCtrlMain.ActiveTab <> TbCtrlMain.Tabs[TbCtrlMain.TabCount - 1]
          then
            TbCtrlMain.ActiveTab := TbCtrlMain.Tabs[TbCtrlMain.TabIndex + 1];
          Handled := True;
        end;

      sgiRight:
        begin
          if TbCtrlMain.ActiveTab <> TbCtrlMain.Tabs[0] then
            TbCtrlMain.ActiveTab := TbCtrlMain.Tabs[TbCtrlMain.TabIndex - 1];
          Handled := True;
        end;
    end;
    // ResetNoAction;
  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.ImageMouseDown(Sender: TObject;
Button: TMouseButton; Shift: TShiftState; X, Y: Single);
Var
  Loc: TPointF;
begin
  if Sender = Image1 then
  Begin
    Loc := TIsGraphics.LocationAsDraw(TPointf.create(X, Y),Image1.Bitmap.Canvas);
    TNavGraphics.RelocateNavPointFromMapRef(FOriginMapOfPos, FScaleMapOfPos,
      Loc, FCenterOffsetMapOfPos);
    Dec(FLastCount, 2);
  End
  Else if Sender = Image2 then
  Begin
    Loc := TIsGraphics.LocationAsDraw(TPointf.create(X, Y),Image2.Bitmap.Canvas);
    TNavGraphics.RelocateNavPointFromMapRef(FOriginMapMesr, FScaleMapMesr, Loc,
      FCenterOffsetMapMesr);
    Dec(FLastCountMeasure, 2);
  End;
  TbCtrlChangeUpdate(Sender);
end;

procedure TTabbedwithNavigationForm.LoadMap;
var
  CurrentMapPos: RNavigateLongLat;
begin
  If FLoctnSensorData = nil then
    exit;
  if FTrackOnWebPage then
  begin
    if FLastWebQuery > (Now - 30 / 24 / 60 / 60) then // 30 seconds
      exit;
  end
  else if FLastWebQuery > (Now - 10 / 24 / 60) then // 10 Minutes
    exit;

  FLastWebQuery := Now;
  // 145.45 dec degrees
  if FTrackOnWebPage then
    CurrentMapPos := FLoctnSensorData.CurrentLocation
  else //show position off maps
  if FOriginMapOfPos.NotNull then
    CurrentMapPos:= FOriginMapOfPos
  else
  if FOriginMapMesr.NotNull then
     CurrentMapPos:= FOriginMapMesr
  else
  case FTestNo of
    1 .. 5:
      CurrentMapPos := FListOfProgress[FListOfProgress.Count - 1];
  else
    CurrentMapPos := FLoctnSensorData.CurrentLocation;
  end;
  CurrentMapPos.GoogleScale := 17;
{$IFDEF msWindows}
  CurrentMapPos.GoGoogle;
{$ELSE}
  WebBrowser1.Navigate(Current.GoogleLink);
  (*
    ENUSLat := Current.Latitude.ToString(ffGeneral, 5, 2,
    TFormatSettings.Create('en-US'));
    ENUSLong := Current.Longitude.ToString(ffGeneral, 5, 2,
    TFormatSettings.Create('en-US'));
    { convert the location to latitude and longitude }
    { and track the location via Google Maps }
    WebBrowser1.Navigate(Format(LGoogleMapsURL, [ENUSLat, ENUSLong]));
    // https://maps.google.com/maps?q= -38.55,145.55
    // https://www.google.com/maps/dir/-38.0497722,145.1384916/-38.0498333,145.1385556/@-38.04983,145.1384232,21z
    // https://www.google.com/maps/place/38%C2%B002'59.4%22S+145%C2%B008'18.8%22E/@-38.0498333,145.1363669,17z/data=!3m1!4b1!4m5!3m4!1s0x0:0x0!8m2!3d-38.04983!4d145.13856
  *)

{$ENDIF}
end;

procedure TTabbedwithNavigationForm.ResetDistanceTraveled;
begin
  FLastSignificantLocation.SetToNull;
  FPrevSigLocIndex := -1;
  FLastSigLocIndex := FPrevSigLocIndex;
  FTotalDistance := 0.0;
end;

procedure TTabbedwithNavigationForm.StartMotion;
begin
  if FMotionSensorData <> nil then
  Begin
    if FLoctnSensorData <> nil then
      FLoctnSensorData.EndAverageMotionSensor := FMotionSensorData;
    exit;
  End;
  FMotionSensorData := TIsMotionSensor.Create;
  FMotionSensorData.OnMotionEvent := MotionChangedEvent;
  if FLoctnSensorData <> nil then
    FLoctnSensorData.EndAverageMotionSensor := FMotionSensorData;
  FMotionSensorData.UpdateInterval := 1000000 * 5; // 30; // 1000000usec=secs
  FormShow(nil);
end;

procedure TTabbedwithNavigationForm.StartGps;
begin
  if FLoctnSensorData <> nil then
  Begin
    if FMotionSensorData <> nil then
      FLoctnSensorData.EndAverageMotionSensor := FMotionSensorData;
    exit;
  End;
  FLoctnSensorData := TIsLocationSensor.Create;
  FLoctnSensorData.OnLocChange := LocChanged;
  FLoctnSensorData.OnBeforeAverageReset := EndRunnibgAverage;
  FLoctnSensorData.MetersToTriggerLocationChange := 2 * GPSAccuracy;
  FormShow(nil);
  SetGeneralNav;
end;

procedure TTabbedwithNavigationForm.LocChanged(ASensor: TIsLocationSensor);
var
  Current: RNavigateLongLat;
  ThisDist, PrevDist, GlitchDist: Double;
  NextAdd, LastTxtIndex: integer;
begin
  if FTestValuesLoading then
    exit
  Else
    case FTestNo of
      1 .. 4:
        exit;
    end;
  try
    if FLoctnSensorData <> ASensor then
      FLoctnSensorData := ASensor;
    Current := FLoctnSensorData.CurrentLocation;
    if Current.NotNull then
    Begin
      FListOfAllProgress.Add(Current);
      FListOfSampleTimes.Add(Now);
      AddToMmoTrkLocations(' ' + Current.LocatationText(1));
      LastTxtIndex := FAllLocationStringList.Add
        (FormatDateTime('dd/mm/yy hh:nn,ss.zzz,', Now) + 'Seconds' +
        Current.LocatationCsv(8) + ',' + FormatFloat('0.0',
        Current.MetresFrom(FLastLocation)) + ',meters Prev Total=,' +
        FormatFloat('0.0', FTotalDistance));
      // Glitch Dist >>>   <<<< Dist
      NextAdd := FListOfProgress.Count;
      if FLastSignificantLocation.NotNull then
      Begin
        ThisDist := Current.MetresFrom(FLastSignificantLocation);
        if ThisDist > GPSAccuracy then
        Begin
          if (FLastSigLocIndex = NextAdd - 1) and (FPrevSigLocIndex > -1) then
          Begin
            PrevDist := FListOfProgress[FLastSigLocIndex]
              .MetresFrom(FListOfProgress[FPrevSigLocIndex]);
            GlitchDist := Current.MetresFrom(FListOfProgress[FPrevSigLocIndex]);
            if (GlitchDist < PrevDist / 3) and SameValue(PrevDist, ThisDist,
              GlitchDist) then
            Begin
              AddToMmoTrkLocations('    Glitch ?????');
              if LastTxtIndex > 0 then
                FAllLocationStringList[LastTxtIndex - 1] :=
                  FAllLocationStringList[LastTxtIndex - 1] + ',GLITCH ???';
              Dec(NextAdd);
              FListOfProgress[NextAdd] := Current; // Drop Old Value
              FTotalDistance := FTotalDistance - PrevDist;
              ThisDist := GlitchDist;
              FLastSigLocIndex := FPrevSigLocIndex;
              FAllLocationStringList[LastTxtIndex] := FAllLocationStringList
                [LastTxtIndex] + ',New Total After GLITCH ???,' +
                FormatFloat('0.0', FTotalDistance);
            End
            Else
              FListOfProgress.Add(Current);
          End
          Else
            FListOfProgress.Add(Current);
          FTotalDistance := FTotalDistance + ThisDist;
          FLastSignificantLocation := Current;
          FPrevSigLocIndex := FLastSigLocIndex;
          FLastSigLocIndex := NextAdd;
        End;
      End
      Else
      begin
        if FListOfProgress.Count > CIgnorFirstXLocations then
        Begin
          FLastSignificantLocation := Current;
          FAllLocationStringList[LastTxtIndex] := FAllLocationStringList
            [LastTxtIndex] + ',First Reference at ' +
            IntToStr(FListOfProgress.Count);
        End;
        FPrevSigLocIndex := FLastSigLocIndex;
        FLastSigLocIndex := FListOfProgress.Add(Current);
      end;
    End;
    TbCtrlChangeUpdate(ASensor);
    FLastLocation := Current;
  Except
    On E: Exception do
    Begin
      AddToMmoTrkLocations('LocChanged Error::' + E.Message);
    End;
  End;
end;

procedure TTabbedwithNavigationForm.MotionChangedEvent
  (ASensor: TIsMotionSensor);
begin
  try
    if FMotionSensorData <> ASensor then
      FMotionSensorData := ASensor;
    if ASensor.ChangedLinear then
      if FLoctnSensorData <> nil then
        if FLoctnSensorData.DoRunningAveLoc then
          AddToMmoTrkLocations('MotionChangedEvent' +
            FormatDateTime('mm ddd hh:nn:ss', Now));
    TbCtrlChangeUpdate(ASensor);
  Except
    On E: Exception do
    Begin
      AddToMmoTrkLocations('MotionChangedEvent Error::' + E.Message);
    End;
  End;

end;

procedure TTabbedwithNavigationForm.OrientationChangedEvent
  (ASensor: TIsOrientationSensor);
begin
  try
    if FOrientationSensorData <> ASensor then
      FOrientationSensorData := ASensor;
    AddToMmoTrkLocations('OrientationChangedEvent');
    TbCtrlChangeUpdate(ASensor);
  Except
    On E: Exception do
    Begin
      AddToMmoTrkLocations('OrientationChangedEvent Error::' + E.Message);
    End;
  End;

end;

function TTabbedwithNavigationForm.ProcessHardwareBack(AKey: Word): Word;
begin
  Result := 0;
  if (TbCtrlMain.ActiveTab = TbItemGenNav) then
  begin
    if (TbCtrlGenNav.ActiveTab = TbItemNavScrn) then
      TDialogService.MessageDialog(
      // MessageDlg(
      'Do you wish to Exit GPS', TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
        Procedure(Const AResult: TModalResult)
        begin
          if AResult = mrYes then
          Begin
            UpdateTimer.Enabled := false; // stop timer
            Close;
          End;
        end)
      // ProcessHardwareBackDlgRtn)
    Else
      TbCtrlGenNav.Previous
  end
  Else if (TbCtrlMain.ActiveTab = TbItemMeasure) then
  Begin
    if (TbCtrlMeasure.ActiveTab = TbItemRunningAverage) then
      TbCtrlMain.Previous
    Else
      TbCtrlMeasure.Previous;
  end
  else
    TbCtrlMain.Previous;
end;

procedure TTabbedwithNavigationForm.SaveAllLocClick(Sender: TObject);
Var
  i, Last: integer;
  FileList: TStringList;
  FwdAz, RevAz: Double;
  ReferencePoint: RNavigateLongLat;
  s: string;
begin
  FileList := TStringList.Create;
  Try
    FileList.AddStrings(FAllLocationStringList);
    FileList.Insert(0,
      'Date Time,,,Longitude,,Latitude,Mtrs From Last,,Mtrs Traveled');
    SaveStringListAsFile(FileList, 'AllLocationData.csv', True);

    FileList.Clear;
    if (FListOfAllProgress.Count > 0) then
      ReferencePoint := FListOfAllProgress[0];
    if FOriginMapOfPos.NotNull then
      ReferencePoint := FOriginMapOfPos;

    FileList.Add('Date Time,,,Longitude,,Latitude,Mtrs From Last');
    // s := FileList[0];
    // FileList.Add(s);
    for i := 0 to FListOfAllProgress.Count - 1 do
    Begin
      if i < FListOfSampleTimes.Count then
        Last := FileList.Add(FormatDateTime('mmm dd hh:nn:ss,',
          FListOfSampleTimes[i]))
      Else
        Last := FileList.Add(',');
      FileList[Last] := FileList[Last] + FListOfAllProgress[i].LocatationCsv(5)
        + ',' + FormatFloat('0.00',
        FListOfAllProgress[i].MetresFrom(ReferencePoint)) + ', metres';

    End;
    SaveStringListAsFile(FileList, 'TimeAndLocationData.csv', True);
    s := FileList[0];
    FileList.Add(s);
    FileList.Clear;

    FileList.Add(',,Longitude,,Latitude,From Ref Point');
    // s := FileList[0];
    // FileList.Add(s);
    for i := 0 to FListOfProgress.Count - 1 do
    Begin
      FileList.Add(FListOfProgress[i].LocatationCsv(5) + ',' +
        FormatFloat('0.00', FListOfProgress[i].MetresFrom(ReferencePoint)) +
        ',Metres');

    End;
    // s := FileList[0];
    // FileList.Add(s);
    SaveStringListAsFile(FileList, 'AdjustedLocationData.csv', True);
    FileList.Clear;
  Finally;
    FileList.Free;
  End;

  SaveImageBitMapAsFile(Image1, 'ProgressImage.Bmp', True);
  if FLoctnSensorData <> nil then
    if FLoctnSensorData.DoRunningAveLoc then
      SaveAverageData(nil);
end;

procedure TTabbedwithNavigationForm.SaveAverageData(Sender: TObject);
Var
  i: integer;
  FileList: TStringList;
  FwdAz, RevAz: Double;
  ReferencePoint: RNavigateLongLat;
begin
  FileList := TStringList.Create;
  Try
    if High(FSampleResultArray) > 0 then
      ReferencePoint := FSampleResultArray[0].NavLoc;
    if FOriginMapMesr.NotNull then
      ReferencePoint := FOriginMapMesr;

    FileList.Add
      ('Date Time,Frm Last,Longitude,Samples,Latitude,From Reference Point,,Azimuth Degrees');
    for i := 0 to High(FSampleResultArray) do
    begin
      if i > 0 then
        FileList.Add('    ' + FormatDateTime('ddd hh:mm  ss.zzz,',
          FSampleResultArray[i].Time) + FormatFloat('0.000,',
          FSampleResultArray[i].NavLoc.MetresFromPrecision(FSampleResultArray
          [i - 1].NavLoc, FwdAz, RevAz)) + ',Meters,' +
          IntToStr(FSampleResultArray[i].NumberInAverage) + ',' + 'Samples');
      FileList.Add(FSampleResultArray[i].NavLoc.LocatationCsv(8) +
        ',From First,' + FormatFloat('0.000,',
        FSampleResultArray[i].NavLoc.MetresFromPrecision(ReferencePoint, FwdAz,
        RevAz)) + ',Fwd Az=,' + FormatFloat('000.0', FwdAz / Pi * 180));
    end;
    if SaveStringListAsFile(FileList, 'averagedata.csv', True) = '' then
      SaveImageBitMapAsFile(Image2, 'AverageImage.Bmp', True);
  Finally
    FileList.Free;
  End;
end;

function TTabbedwithNavigationForm.SaveFileDirectory(ADateTimeDir
  : Boolean): String;
begin
  // Result := TPath.Combine(TPath.GetSharedDocumentsPath, 'GPSSaveDir');
  // It seems the Microsoft based concept of shared files is getting too difficult on Android. My problem is a "feature" of Sydney
  // TPath.GetSharedDocumentsPath currently uses "getExternalStoragePublicDirectory" which has been deprecated in API level 29 see:
  // https://developer.android.com/reference/android/os/Environment#getExternalStoragePublicDirectory(java.lang.String)
{$IFDEF ISD104S_DELPHI}
  Result := TPath.Combine(TPath.GetDocumentsPath, 'GPSSaveDir');
{$ELSE}
  Result := TPath.Combine(TPath.GetSharedDocumentsPath, 'GPSSaveDir');
{$ENDIF}
  if ADateTimeDir then
    Result := Result + FormatDateTime('mmm_dd_hh', Now);
  if Not DirectoryExists(Result) then
    ForceDirectories(Result);
  if Not DirectoryExists(Result) then
  begin
    MmoSamplingDetails.Lines.Add('Documents Directory Create fail');
    Result := TPath.GetDocumentsPath;
    if Not DirectoryExists(Result) then
      Result := TPath.GetDocumentsPath;
  end;
end;

Function TTabbedwithNavigationForm.SaveImageBitMapAsFile(AImage: TImage;
AFileName: String; ADateTimeDir: Boolean = false): String;
Var
  LFileName, DirName, Ext, error: String;
  i: integer;
  FileStrm: TFileStream;
begin
  error := '';
  if AImage = nil then
    exit;
  if (AImage.Bitmap = nil) or (AImage.Bitmap.Size.cx < 1) then
    error := 'No data in bitmap'
  else
    try
      i := 1;
      if AFileName = '' then
        LFileName := 'Save.Bmp'
      else
        LFileName := AFileName;
      DirName := SaveFileDirectory(ADateTimeDir);
      Ext := ExtractFileExt(LFileName);
      LFileName := TPath.Combine(DirName, LFileName);
      while FileExists(LFileName) do
      Begin
        If not DeleteFile(LFileName) then
        Begin
          LFileName := ChangeFileExt(LFileName, IntToStr(i) + Ext);
          Inc(i); // 1234.ext
        End;
      End;
      FileStrm := TFileStream.Create(LFileName, fmCreate);
      Try
        AImage.Bitmap.SaveToStream(FileStrm);
      Finally
        FileStrm.Free;
      End;
    Except
      On E: Exception do
        error := E.Message;
    end;
  Result := error;
end;

Function TTabbedwithNavigationForm.SaveStringListAsFile(AList: TStringList;
AFileName: String; ADateTimeDir: Boolean = false): String;
Var
  LFileName, DirName, Ext, error, s: String;
  i: integer;
begin
  Try
    s := AList[0];
    i := 1;
    if AFileName = '' then
      LFileName := 'Save.Txt'
    else
      LFileName := AFileName;
    DirName := SaveFileDirectory(ADateTimeDir);
    Ext := ExtractFileExt(LFileName);
    LFileName := TPath.Combine(DirName, LFileName);
    while FileExists(LFileName) do
    Begin
      If not DeleteFile(LFileName) then
      Begin
        LFileName := ChangeFileExt(LFileName, IntToStr(i) + Ext);
        Inc(i); // 1234.ext
      End;
    End;
    // AList.Add(s);
    AList.SaveToFile(LFileName);
    error := '';
  Except
    On E: Exception do
      error := E.Message;
  end;
  Result := error;
end;

function TTabbedwithNavigationForm.SecondsAsText(ASeconds: integer): String;
begin
  if ASeconds < 360 then
    Result := IntToStr(ASeconds) + ' Secs'
  Else if ASeconds < 3600 then
    Result := FormatFloat('0.0', ASeconds / 60) + ' Mins'
  Else
    Result := FormatFloat('0.00', ASeconds / 60 / 60) + ' Hrs';
end;

procedure TTabbedwithNavigationForm.SetAboutPage;
begin
  FCompileStgr := LibCompilerString;
  if FManagerData = nil then
    FManagerData := TIsSensorManager.CurrentIsSensorManager;
  MmoAbout.Text := FCompileStgr + FManagerData.TextList;
end;

procedure TTabbedwithNavigationForm.SetGeneralNav;
Var
  Hdg, Trv: Double;
  Current, Start: RNavigateLongLat;

begin
  if FLoctnSensorData = nil then
  Begin
    LblSpeed.Text := FormatDateTime('hh:nn:ss', Now - FStartTime);
    LblHeading.Text := ' GPS Off ';
    if FMotionSensorData <> nil then
      LblHeading.Text := LblHeading.Text + 'Motion On'
    Else
      LblHeading.Text := LblHeading.Text + 'Motion OFF';

    LblPos.Text := 'Press Start or go to About Page';
  End
  Else if FLoctnSensorData.Started then
  Begin
    LblSpeed.Text := FormatFloat('###0.00 ',
      FLoctnSensorData.SpeedKilometersPerHour) + ' kmph';
    Hdg := FLoctnSensorData.TrueHeading;
    if FTestNo > 0 then
    Begin
      Current := FListOfAllProgress[16];
      Start := FListOfAllProgress[0];
    End
    else
    Begin
      Current := FLoctnSensorData.CurrentLocation;
      Start := FLoctnSensorData.StartLocation;
    End;
    LblHeading.Text := TextDegreeMinuteSecondFrmDec(Hdg, 4);
    LblPos.Text := 'At ' + Current.LocatationText(1);
    LblStartPos.Text := 'Start ' + Start.LocatationText(1);
    Trv := Current.MetresFrom(Start);
    LblDistance.Text := 'Frm Start =' + TextMeters(Trv, 2, 0, 2) + '(' +
      SecondsAsText(FSecondsSinceStartCounter) + ')';
    // FormatFloat('###0.00 ', Trv)
    // + 'Meters';
    LblTotalDist.Text := 'Traveled =' + TextMeters(FTotalDistance, 2, 0, 2) +
      '(' + SecondsAsText(fSecondsCounter) + ')';
    // FormatFloat('###0.00 ', FTotalDistance)
    // + 'Meters';
    LblBearing.Text := TextDegreeMinuteSecondFrmDec(Current.HeadingFrom(Start),
      4) + '(' + SecondsAsText(Round((Now - FStartTime) * 24 * 60 * 60)) + ')';
  End;
end;

procedure TTabbedwithNavigationForm.SetHoldNav;
Var
  Hdg, Trv: Double;
  Current, Start: RNavigateLongLat;

begin
  if FLoctnSensorData <> nil then
    if FLoctnSensorData.Started then
    Begin
      LblSpeed2.Text := FormatFloat('###0.00 ',
        FLoctnSensorData.SpeedKilometersPerHour) + 'K/H';
      Hdg := FLoctnSensorData.TrueHeading;
      Current := FLoctnSensorData.CurrentLocation;
      Start := FLoctnSensorData.StartLocation;
      LblHeading2.Text := TextDegreeMinuteSecondFrmDec(Hdg, 4);
      LblPos2.Text := 'At ' + Current.LocatationText(1);
      LblStartPos2.Text := 'Start ' + Start.LocatationText(1);
      Trv := Current.MetresFrom(Start);
      LblDistance2.Text := 'Frm Start =' + FormatFloat('###0.00 ', Trv)
        + 'Meters';
      LblBearing2.Text := TextDegreeMinuteSecondFrmDec
        (Current.HeadingFrom(Start), 4);
    End;
end;

procedure TTabbedwithNavigationForm.SetMapOfMeasure;
Var
  ListOfPos: TList<RNavigateLongLat>;
  i: integer;
  TotalDistance, AverageDistance: Double;
  ReDraw: Boolean;
  // LOrigin: RNavigateLongLat;

begin
  if FOriginMapMesr.NotNull then
    LblMeasurePicTop.Text:=FOriginMapMesr.LocatationText(2)
  else
    LblMeasurePicTop.Text := 'Plot ' + IntToStr(Length(FSampleResultArray)) +
      ' Averages';
  ReDraw := TIsGraphics.SetNewImageBitMap(Image2, $FFFFFF) or
    (High(FSampleResultArray) > FLastCountMeasure);
  if ReDraw then
  Begin
    TotalDistance := 0.0;
    ListOfPos := TList<RNavigateLongLat>.Create;
    Try
      LblMeasurePicTop.Text:=FOriginMapMesr.LocatationText(2);
      for i := 0 to High(FSampleResultArray) do
      Begin
        ListOfPos.Add(FSampleResultArray[i].NavLoc);
        if i > 0 then
          TotalDistance := TotalDistance + FSampleResultArray[i]
            .NavLoc.MetresFrom(FSampleResultArray[i - 1].NavLoc);
      end;
      FLastCountMeasure := ListOfPos.Count;

      FScaleMapMesr := TNavGraphics.GetScale(Image2.Bitmap.Canvas, ListOfPos,
        FOriginMapMesr, FCenterOffsetMapMesr);
      if FScaleMapMesr.X > 0 then
      Begin
        if FZoomOutMeasure > 1 then
          FScaleMapMesr := FScaleMapMesr / FZoomOutMeasure
        Else if FZoomOutMeasure < 0 then
          FScaleMapMesr := -FScaleMapMesr * FZoomOutMeasure;
        if FLastCountMeasure > 1 then
          AverageDistance := TotalDistance / (FLastCountMeasure - 1)
        Else
          AverageDistance := 0.0;
        if FLastCountMeasure > 0 then
        begin
          TNavGraphics.DrawLocationsOnCanvas(Image2.Bitmap.Canvas, ListOfPos,
            FOriginMapMesr, FScaleMapMesr, FCenterOffsetMapMesr, 1,
            'Average of Averages ' + TextMeters(AverageDistance, 2, 0, 2),
            0.0 { Non Jitter Plot } );
        end;
      End;
    Finally
      ListOfPos.Free;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.SetMapOfPositions;
Var
  ReDraw: Boolean;
  // LOrigin: RNavigateLongLat;
begin
  if FListOfProgress = nil then
  Begin
    ReDraw := True;
    exit;
  End;

  ReDraw := TIsGraphics.SetNewImageBitMap(Image1, $FFFFFF) or
    (FListOfProgress.Count > FLastCount);
  if ReDraw then
  Begin
     if FOriginMapOfPos.NotNull then
        LblPicTitle.Text:=FOriginMapOfPos.LocatationText(2);
    // FOriginMapOfPos.SetToNull;
    FLastCount := FListOfProgress.Count;
    FScaleMapOfPos := TNavGraphics.GetScale(Image1.Bitmap.Canvas,
      FListOfProgress, FOriginMapOfPos, FCenterOffsetMapOfPos);

    if FScaleMapOfPos.X > 0 then
    Begin
      if FZoomOut > 1 then
        FScaleMapOfPos := FScaleMapOfPos / FZoomOut
      Else if FZoomOut < 0 then
        FScaleMapOfPos := -FScaleMapOfPos * FZoomOut;
      if FLastCount > 1 then
      begin
        if FShowAllProgress then
          TNavGraphics.DrawLocationsOnCanvas(Image1.Bitmap.Canvas,
            FListOfAllProgress, FOriginMapOfPos, FScaleMapOfPos,
            FCenterOffsetMapOfPos, 1, 'Total ' + TextMeters(FTotalDistance, 2,
            0, 2), 0.0)
        else
          TNavGraphics.DrawLocationsOnCanvas(Image1.Bitmap.Canvas,
            FListOfProgress, FOriginMapOfPos, FScaleMapOfPos,
            FCenterOffsetMapOfPos, 1, 'Total ' + TextMeters(FTotalDistance, 2,
            0, 2), GPSAccuracy);
      end;
    End;
    FLastCount := FListOfProgress.Count;
  End;
end;

procedure TTabbedwithNavigationForm.SetPastSamples;
Var
  i: integer;
  FwdAz, RevAz: Double;
  TimeNow: TdateTime;
begin
  TimeNow := Now;
  if TimeNow < FNextPastSampleUpdate then
    exit;

  FNextPastSampleUpdate := TimeNow + 1 / 24 / 60;
  MmoPreviusLocations.Lines.Clear;
  MmoPreviusLocations.Lines.Add( // Formatdatetime('dd hh:mm:ss', Now) +
  ' No Samples =' + IntToStr(Length(FSampleResultArray)));
  for i := 0 to High(FSampleResultArray) do
  begin
    if i > 0 then
      MmoPreviusLocations.Lines.Add('    ' + FormatDateTime('ddd hh.mm  ',
        FSampleResultArray[i].Time) + FormatFloat('0.00',
        FSampleResultArray[i].NavLoc.MetresFromPrecision(FSampleResultArray
        [i - 1].NavLoc, FwdAz, RevAz)) + 'Meters(#' +
        IntToStr(FSampleResultArray[i].NumberInAverage) + ')');
    MmoPreviusLocations.Lines.Add(FSampleResultArray[i]
      .NavLoc.LocatationText(3));
  end;
  // MmoPreviusLocations.scroll
end;

procedure TTabbedwithNavigationForm.SetRunningAverage;
var
  ThisValue: RNavigateLongLat;
  // SampleIsGood: Boolean;
  NxtLocInArray: integer;
begin
  if FLoctnSensorData = nil then
    exit;
  // SampleIsGood := False;

  if FLoctnSensorData.NoOfConsistenLocSamples > 0 then
  begin
    LblHomeInBlack.Text := FLoctnSensorData.AverageLocation(2);
    LblHomeInMain.Text := IntToStr(FLoctnSensorData.NoOfConsistenLocSamples) +
      ' Samples (' + SecondsAsText(fSecondsCounter) + ')';
    if FLoctnSensorData.NoOfConsistenLocSamples > StableCount then
    Begin
      LblHomeInMinor.Text := 'Stable ' +
        FLoctnSensorData.AverageLocationDev(True);
      // SampleIsGood := True;
    End
    else
      LblHomeInMinor.Text := FLoctnSensorData.AverageLocationDev(True);
  end
  else
  begin
    LblHomeInBlack.Text := FLoctnSensorData.CurrentLocation.LocatationText(1);

    LblHomeInMain.Text := IntToStr(FLoctnSensorData.NoOfConsistenLocSamples) +
      '  (' + IntToStr(FLoctnSensorData.NoOfConsistentAltSamples) + ' in ' +
      SecondsAsText(fSecondsCounter) + ')';
    if FLoctnSensorData.DoRunningAveLoc then
      LblHomeInMinor.Text := 'Please Wait - Homing in on Location'
    else
      LblHomeInMinor.Text := 'Select Start to Home in on Location';
    LblHomeInMain.Text := 'Single Sample';
  end;

  NxtLocInArray := Length(FSampleResultArray);
  if NxtLocInArray < 1 then
  Begin
    LblBtmHomein.Text := 'No Locations Stored';
    LblBtm1HomeIn.Text := FLoctnSensorData.SampleTimes;
    LblBtm2HomeIn.Text := 'Last Location';
  End
  else
  begin
    ThisValue := FLoctnSensorData.RunningLocationValue;
    if not ThisValue.NotNull then
      ThisValue := FLoctnSensorData.CurrentLocation;
    LblBtmHomein.Text := 'Frm Start ' + FormatFloat('0.0',
      ThisValue.MetresFrom(FSampleResultArray[0].NavLoc)) + ' meters';
    LblBtm1HomeIn.Text := 'Frm Last ' + FormatFloat('0.0',
      ThisValue.MetresFrom(FSampleResultArray[NxtLocInArray - 1].NavLoc)) +
      ' meters';
    LblBtm2HomeIn.Text := FSampleResultArray[NxtLocInArray - 1]
      .NavLoc.LocatationText(2);
  end;

  SetSpeedStartStopHomeButton(FLoctnSensorData.DoRunningAveLoc);
end;

procedure TTabbedwithNavigationForm.SetRunningAverageDetails;
Var
  ss: string;
begin
  if FLoctnSensorData = nil then
    exit;
  FDoneTestPage := false;
  MmoSamplingDetails.Lines.Clear;
  MmoSamplingDetails.Lines.Text :=
  // +#13#10 +
    FLoctnSensorData.lastError;

  LblMetersPerSecondDetail.Text := FormatFloat('0.00 kmph',
    FLoctnSensorData.SpeedKilometersPerHour);
  if FLoctnSensorData.HasAveAlt then
    LblAveAlt.Text := 'Ave Altitude = ' + FLoctnSensorData.AverageAltitude
  Else
    LblAveAlt.Text := 'Current Altitude = ' + FormatFloat('0.0',
      FLoctnSensorData.AltitudeMeters) + ' meters';

  LblAveHding.Text := 'Hdg ' + FLoctnSensorData.AverageHeadinge;
  LblAveSpeed.Text := 'Ave ' + FLoctnSensorData.AverageSpeed;
  LblNoOfSamples.Text := 'Samples Loc=' +
    CountAsText(FLoctnSensorData.NoOfLocSamples) + '( ' +
    CountAsText(FLoctnSensorData.NoOfTimeSamples) + ')';
  if FLoctnSensorData.HasAveLoc then
  Begin
    LblAverageLocationText.Text := 'Avg ' + FLoctnSensorData.AverageLocation(3);
    LblLocationDev.Text := FLoctnSensorData.AverageLocationDev(True);
  End
  Else
  Begin
    LblAverageLocationText.Text := 'AverageLocation';
    LblLocationDev.Text := 'Location Varience';
  End;
  LblCurentLocationText.Text := 'Last ' + FLoctnSensorData.CurrentLocation.
    LocatationText(1);
  LblAveTimeSampling.Text := FLoctnSensorData.SampleTimes;;
  ss := 'Smpls L' + IntToStr(FLoctnSensorData.NoOfConsistenLocSamples);
  if FLoctnSensorData.NoOfConsistentAltSamples > 0 then
    ss := ss + ' Alt' + IntToStr(FLoctnSensorData.NoOfConsistentAltSamples);
  ss := ss + '(' + SecondsAsText(fSecondsCounter) + ')';
  LblAveSnapBottom.Text := ss;
end;

procedure TTabbedwithNavigationForm.SetSpeedStartStopHomeButton
  (ADoingRunningLocation: Boolean);
begin
  if ADoingRunningLocation then
  Begin
    SpBtnStartStopHomeIn.Text := 'Stop';
    SpBtnStartStopHomeIn.TextSettings.FontColorForState.Normal :=
      TAlphaColorRec.Tomato;
    SpBtnDtlsStopStart.Text := 'Stop';
    SpBtnDtlsStopStart.TextSettings.FontColorForState.Normal :=
      TAlphaColorRec.Tomato;
  End
  else
  begin
    SpBtnStartStopHomeIn.Text := 'Start';
    SpBtnStartStopHomeIn.TextSettings.FontColorForState.Normal :=
      TAlphaColorRec.Seagreen;
    SpBtnDtlsStopStart.Text := 'Start';
    SpBtnDtlsStopStart.TextSettings.FontColorForState.Normal :=
      TAlphaColorRec.Seagreen;
  end;
end;

procedure TTabbedwithNavigationForm.SetTestPage;
  Function DoTestWrite(Const ATstList: TStringList; Const Test: String;
  AFileDir: String): String;
  var
    Ext, FileName: String;
    i: integer;
  begin
    i := 1;
    try
      FileName := TPath.Combine(AFileDir, 'Save.Txt');
      Ext := ExtractFileExt(FileName);

      if Not DirectoryExists(AFileDir) then
        ForceDirectories(AFileDir);
      while FileExists(FileName) do
      Begin
        If not DeleteFile(FileName) then
        Begin
          FileName := ChangeFileExt(FileName, IntToStr(i) + Ext);
          Inc(i); // 1234.ext
        End;
      End;
      ATstList.SaveToFile(FileName);

      if FileExists(FileName) then
        Result := 'Passed:' + Test + '::' + ExtractFileName(FileName)
      else
        Result := 'Failed:' + Test + '::' + FileName;

    Except
      On E: Exception Do
      Begin
        Result := 'Exception::' + Test + #13#10 + E.Message + #13#10 + FileName;
        // TDialogService.ShowMessage('Data Write Fail:' + Test + '::' + FileName);
      end
    end;

  end;

Var
  TstStart, TstEnd: RNavigateLongLat;
  TstList: TStringList;
  LFileDataDir: String;
begin
  if FDoneTestPage then
    exit;
  FDoneTestPage := True;
  MmoSamplingDetails.Lines.Add(FFailedPermissions);
  TstList := TStringList.Create;
  Try
    TstList.Add('Testing File save');
    TstList.Add('Testing File Line2');
    TstList.Add(FormatDateTime('dd mmm yy hh:nn:ss', Now));
    LFileDataDir := TPath.Combine(TPath.GetHomePath, 'GPSSaveDir');
    if FTestNo > 0 then
      MmoSamplingDetails.Lines.Add('FTestNo Value:' + IntToStr(FTestNo - 1));
    MmoSamplingDetails.Lines.Add(DoTestWrite(TstList, 'GetHomePath',
      LFileDataDir));
    LFileDataDir := TPath.Combine(TPath.GetTempPath, 'GPSSaveDir');
    MmoSamplingDetails.Lines.Add(DoTestWrite(TstList, 'GetTempPath',
      LFileDataDir));
    LFileDataDir := TPath.Combine(TPath.GetPicturesPath, 'GPSSaveDir');
    MmoSamplingDetails.Lines.Add(DoTestWrite(TstList, 'GetPicturesPath',
      LFileDataDir));
    LFileDataDir := TPath.Combine(TPath.GetSharedDocumentsPath, 'GPSSaveDir');
    MmoSamplingDetails.Lines.Add(DoTestWrite(TstList, 'GetSharedDocumentsPath',
      LFileDataDir));
  Finally
    freeAndNil(TstList);
  End;

  TstStart.CreateDec(147.0, 40.0);
  TstEnd.CreateDec(149.0, 38.5);
  LblTestStart.Text := 'From ' + TstStart.LocatationText(0);
  LblTestEnd.Text := 'To ' + TstEnd.LocatationText(0);
  LblTestBearing.Text := 'Bearing ' + TextDegreeMinuteSecondFrmDec
    (TstEnd.HeadingFrom(TstStart), 4);
  LblTestDistance.Text := FormatFloat('###0.00 ', TstEnd.MetresFrom(TstStart)) +
    ' Meters';
  LblReversBearing.Text := 'Back ' + TextDegreeMinuteSecondFrmDec
    (TstStart.HeadingFrom(TstEnd), 4);
  LblBackAgain.Text := FormatFloat('###0.00 ', TstStart.MetresFrom(TstEnd)) +
    ' Meters';
end;

procedure TTabbedwithNavigationForm.SpBtnMapLocClick(Sender: TObject);
begin
  if FListOfProgress.Count > 0 then
    if Sender = SpBtnPicLoc1 then
      FOriginMapOfPos := FListOfProgress[0]
    Else if Sender = SpBtnPicLocEnd then
      FOriginMapOfPos := FListOfProgress[FListOfProgress.Count - 1];

  if Length(FSampleResultArray) > 0 then
    if Sender = SpBtnMsrLocStart then
      FOriginMapMesr := FSampleResultArray[0].NavLoc
    Else If FListOfProgress.Count > 1 Then
      if Sender = SpBtnMsrLocEnd then
        FOriginMapMesr := FSampleResultArray[High(FSampleResultArray)].NavLoc;

  // if FLastCount > 0 then
  if (Sender = SpBtnPicLoc1) Or (Sender = SpBtnPicLocEnd) then
    FLastCount := 0;
  // if FLastCountMeasure > 0 then
  if (Sender = SpBtnMsrLocStart) Or (Sender = SpBtnMsrLocEnd) then
    FLastCountMeasure := 0;

  TbCtrlChangeUpdate(Sender);
end;

procedure TTabbedwithNavigationForm.SpBtnResetAllClick(Sender: TObject);
begin
  StartGps;
  FAllLocationStringList.Add('Reset Counters'); // Keep Total Record
  MmoTrkLocations.Lines.Clear;
  ResetDistanceTraveled;
  FListOfProgress.Clear;
  FLastCount := 0;
  if FLoctnSensorData = nil then
    exit;
  FLoctnSensorData.Restart;
end;

procedure TTabbedwithNavigationForm.SpBtnRestDetailsClick(Sender: TObject);
begin
  if FLoctnSensorData = nil then
    exit;
  if FLoctnSensorData.DoRunningAveLoc then
  begin
    FLoctnSensorData.DoRunningAveLoc := false;
    Sleep(1000);
  end;
  FLoctnSensorData.DoRunningAveLoc := True;
  SetSpeedStartStopHomeButton(FLoctnSensorData.DoRunningAveLoc);
  fSecondsCounter := 0;
end;

procedure TTabbedwithNavigationForm.SpBtnSetTestValuesClick(Sender: TObject);
// Insert a set of test values for maps
Var
  OriginValue, SampleValue: RNavigateLongLat;
  SampleMeasure: RRunAveLocRec;
  TestDist: Double;
  NxtLocInMsrArray, TstCount: integer;
  StartTime: TdateTime;
begin
  FTestValuesLoading := True;
  RemoveAllSensors(nil);

  StartTime := Now;
  Try
    case FTestNo of
      0:
        ;
      1 .. 4:
        Begin
          FListOfProgress.Clear;
          FListOfAllProgress.Clear;
          SetLength(FSampleResultArray, 0);
        End;
    end;
    NxtLocInMsrArray := Length(FSampleResultArray);
    SetLength(FSampleResultArray, NxtLocInMsrArray + 17);
    if FListOfProgress.Count > 0 then
      OriginValue := FListOfProgress[FListOfProgress.Count - 1]
    Else
      case FTestNo of
        0:
          OriginValue := FListOfProgress[FListOfProgress.Count - 1];
        1:
          OriginValue.CreateDec(139.779, 35.54939); // Tokyo Airport
        2:
          OriginValue.CreateDec(-74.0445, 40.6892); // Statue of Liberty
        3:
          OriginValue.CreateDec(145.66, -37.999); // Bunyip State Park
        4:
          OriginValue.CreateDec(-43.214872, -22.9519916); // Christ the Redeemer
      else
        FTestNo := -1;
      end;
    FListOfProgress.Add(OriginValue);
    FListOfAllProgress.Add(OriginValue);

    for TstCount := 0 to 16 do
    begin
      SampleValue := OriginValue.LocationAtMtrsRad(1000.00,
        TstCount * 2 * Pi / 16 + Pi / 4);
      if TstCount = 4 then
        TestDist := SampleValue.MetresFrom(OriginValue);
      FListOfProgress.Add(SampleValue);
      FListOfAllProgress.Add(SampleValue);
      SampleMeasure.NavLoc := SampleValue;
      SampleMeasure.Time := StartTime + TstCount / (24 * 60 * 60);
      SampleMeasure.JitterStr := 'Test Value';
      SampleMeasure.NumberInAverage := 10;
      FSampleResultArray[NxtLocInMsrArray + TstCount] := SampleMeasure;
    end;
    StartGps;
  Finally
    FTestValuesLoading := false;
  End;
  FDoneTestPage := false;
  Inc(FTestNo);
end;

procedure TTabbedwithNavigationForm.SpBtnShowAllProgressClick(Sender: TObject);
begin
  FShowAllProgress := Not FShowAllProgress;
  if FShowAllProgress then
    SpBtnShowAllProgress.Text := 'Exit'
  Else
    SpBtnShowAllProgress.Text := 'All';

  FLastCount := 0;
  TbCtrlChangeUpdate(Sender);
end;

procedure TTabbedwithNavigationForm.SpBtnStartMotionClick(Sender: TObject);
begin
  StartMotion;
end;

procedure TTabbedwithNavigationForm.SpBtnStartStopHomeInClick(Sender: TObject);
begin
  if FLoctnSensorData = nil then
    exit;
  FLoctnSensorData.DoRunningAveLoc := Not FLoctnSensorData.DoRunningAveLoc;
  SetSpeedStartStopHomeButton(FLoctnSensorData.DoRunningAveLoc);
  fSecondsCounter := 0;
end;

procedure TTabbedwithNavigationForm.RemoveAllSensors(Sender: TObject);
begin
  freeAndNil(FLoctnSensorData);
  freeAndNil(FMotionSensorData);
  freeAndNil(FOrientationSensorData);
  freeAndNil(FManagerData);
end;

procedure TTabbedwithNavigationForm.SpBtnZoomClick(Sender: TObject);
begin
  if Sender = SpBtnZoomOut then
    Inc(FZoomOut, 5)
  Else
    Dec(FZoomOut, 5);
  FLastCount := 0;
  TbCtrlChangeUpdate(Sender);
end;

procedure TTabbedwithNavigationForm.SpBtnZoomMeasureClick(Sender: TObject);
begin
  if Sender = SpBtnZoomOutMeasure then
    Inc(FZoomOutMeasure, 5)
  Else
    Dec(FZoomOutMeasure, 5);
  FLastCountMeasure := 0;
  TbCtrlChangeUpdate(Sender);
end;

procedure TTabbedwithNavigationForm.TbCtrlChangeUpdate(Sender: TObject);
begin
  Try
    // if Sender is TIsLocationSensor then
    // AddToMmoTrkLocations('Change Event ' +
    // Formatdatetime('hh:nn:ss.nnn', Now));

    if FLoctnSensorData = nil then
      SetGeneralNav
    else if FLoctnSensorData.Started then
    Begin
      If TbCtrlMain.ActiveTab <> TIMap then
        FLastWebQuery := 0.0;
      If TbCtrlMain.ActiveTab = TIMap then
        LoadMap
      else If TbCtrlMain.ActiveTab = TbItemGenNav then
      Begin
        If TbCtrlGenNav.ActiveTab = TbItemNavScrn Then
          SetGeneralNav
        else If TbCtrlGenNav.ActiveTab = TbItemPicture Then
        Begin
          // If (Sender <> FSensorData) and (Sender <> UpdateTimer) Then
          SetMapOfPositions
        End
        else If TbCtrlGenNav.ActiveTab = TbItemHoldValues Then
          If (Sender <> FLoctnSensorData) and (Sender <> UpdateTimer) Then
            SetHoldNav
            // else If TbCtrlGenNav.ActiveTab = TbItemRecords then
            // SetNavRecords
              ;
      End
      else If TbCtrlMain.ActiveTab = TbItemMeasure then
      Begin
        If TbCtrlMeasure.ActiveTab = TbItemRunningAverage then
          SetRunningAverage
        else If TbCtrlMeasure.ActiveTab = TbItemAveDetailst then
          SetRunningAverageDetails
        else If TbCtrlMeasure.ActiveTab = TbItemPastSamples then
          SetPastSamples
        else If TbCtrlMeasure.ActiveTab = TbItemMeasPict then
          SetMapOfMeasure;
      End
      else If TbCtrlMain.ActiveTab = TiTestValue1 then
        SetTestPage;
      // If TbCtrlMain.ActiveTab = TIMap then LoadMap;
      // If TbCtrlMain.ActiveTab = TabAbout then SetAboutPage;
    end;

  Except
    On E: Exception do
    Begin
      LblHeading.Text := 'Error::' + E.Message;
    End;
  End;
end;

procedure TTabbedwithNavigationForm.UpdateTimerTimer(Sender: TObject);
// Var
// NxtLocInArray: Integer;
// SampleIsGood: Boolean;

begin
  UpdateTimer.Enabled := false;
  Try
    if True then

      Inc(FSecondsSinceStartCounter);
    if FLoctnSensorData <> nil then
      if FLoctnSensorData.DoRunningAveLoc then
        Inc(fSecondsCounter);
    Try
      TbCtrlChangeUpdate(Sender);
      { Dec(FCountDownToOffGps);
        if FSensorData = nil then
        exit;

        if FCountDownForSample = CountDownStart then
        FSensorData.ResetAverages;
        Dec(FCountDownForSample);
        // Temp
        LblBtm1HomeIn.Text := 'Sensor Count ' +
        IntToStr(FSensorData.NoOfTimeSamples);
        LblBtmHomein.Text := FSensorData.SampleTimes;
        // Temp

        if (FCountDownToOffGps < 0) and (FCountDownForSample < 0) then
        begin
        FSensorData.Started := False;
        FCountDownToOffGps := -5; // do not allow to roll over to positive
        FCountDownForSample := -5;
        UpdateTimer.Enabled := True;
        exit;
        end;

        ?????

        TbCtrlChangeUpdate(UpdateTimer);
        { }
    Except
      on E: Exception do
        MmoTrkLocations.Lines.Add('Error::' + E.Message);
    End;
  Finally
    UpdateTimer.Interval := 985; // 1000; allow time to process
    UpdateTimer.Enabled := True;
  End;
end;

end.
