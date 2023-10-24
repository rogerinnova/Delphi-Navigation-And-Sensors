unit GpsUserDataAccess;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.math,
{$IFDEF UseAppForm}
  Fmx.Objects,
  SensorTabbedFormwithNavigation,
{$ENDIF}
  System.Variants, System.ioutils, System.Generics.Collections,
  System.Actions, IsMobileSensors, IsNavUtils;

Type
  TLocalGpsStartRec = record
    LastSampleNo: integer;
    ResetTime: TDateTime;
    Procedure SetData(ALastSample: integer);
  end;

  TGpsDataSource = Class
  private
{$IFDEF UseAppForm}
    FAppForm: TTabbedwithNavigationForm;
{$ELSE}
    FAppForm: TObject;
    FListOfSampleTimes: TList<TDateTime>;
    FListOfAllProgress: TList<RNavigateLongLat>;
    FBestListOfProgress: TList<RNavigateLongLat>;
{$ENDIF}
    FListOfGPSResets: TList<TLocalGpsStartRec>;
    FOnGpsStartStopProc: TISLocationChangedEvent;
    // FListOfSampleTimes: TList<TdateTime>;
    // FListOfAllProgress: TList<RNavigateLongLat>;
    FTotalDistance: Double;
    FOnCloseQuery: TNotifyEvent;
    FAboutText: String;
    FSamplingDetails: String;
    FProgressText: String;
    FAltitude: Double;
    FTrueHeading: Double;
    FSpeedMs,FSpeedKmh: Double;
    procedure SetProgressText(const Value: String);
    procedure SetOnCloseQuery(const Value: TNotifyEvent);
    function GetTotalDistance: Double;
    function GetAboutText: String;
    function GetSamplingDetails: String;
    function GetListOfAllProgress: TList<RNavigateLongLat>;
    function GetListOfSampleTimes: TList<TDateTime>;
    function ListOfGPSResetTimes: TList<TLocalGpsStartRec>;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure NavFormClose(Sender: TObject; var Action: TCloseAction);
    procedure NavCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetOnGpsStartStopProc(const Value: TISLocationChangedEvent);
    Procedure LoadForm;
    Procedure AddChangedLoc(ASensor: TIsLocationSensor);
    Procedure DoGpsStartStopProc(ASensor: TIsLocationSensor);
{$IFDEF UseAppForm}
{$ELSE}
    function AdjustedListOfGPSPlots: TList<RNavigateLongLat>;
{$ENDIF}
    Property ProgressText: String read FProgressText write SetProgressText;
    Property OnGpsStartStopProc: TISLocationChangedEvent
      read FOnGpsStartStopProc write SetOnGpsStartStopProc;
    Property TotalDistance: Double read GetTotalDistance;
    Property ListOfSampleTimes: TList<TDateTime> read GetListOfSampleTimes;
    Property ListOfAllProgress: TList<RNavigateLongLat>
      read GetListOfAllProgress;
    Property OnCloseQuery: TNotifyEvent read FOnCloseQuery
      write SetOnCloseQuery;
    Property AboutText: String read GetAboutText;
    Property SamplingDetails: String read GetSamplingDetails;
    Property SpeedMps:Double read FSpeedMs;
    Property SpeedKph:Double read FSpeedKmh;
    Property TrueHeading:Double read FTrueHeading;
    Property Altitude:Double read FAltitude;
  End;

Function GPSDataSource: TGpsDataSource;

Const
  CIgnorFirstXLocations = 3;

implementation

Var
  LGPSDataSource: TGpsDataSource;

Function GPSDataSource: TGpsDataSource;
Begin
  if LGPSDataSource = nil then
    TGpsDataSource.Create;
  Result := LGPSDataSource;
End;
{ TGpsDataSource }

procedure TGpsDataSource.AddChangedLoc(ASensor: TIsLocationSensor);
// Var
// Count:Integer;
begin
{$IFDEF UseAppForm}
  Raise Exception.Create('UseAppForm TTabbedwithNavigationForm in Service App');
{$ELSE}
  if FAboutText = '' then
    if ASensor <> nil then
    begin
      FAboutText := ASensor.PlatformAllSensorText;
      if FAboutText = CNoSendorText then
        FAboutText := '';
    end;
  // Count:=ListOfSampleTimes.Count;
  ListOfSampleTimes.Add(Now);
  // Count:=ListOfSampleTimes.Count;
  ListOfAllProgress.Add(ASensor.CurrentLocation);
{$ENDIF}
  FAltitude:=ASensor.AltitudeMeters;
  FTrueHeading:=ASensor.TrueHeading ;
  FSpeedMs:=ASensor.SpeedMetersPerSec;
  FSpeedKmh:=ASensor.SpeedKilometersPerHour;
end;

{$IFNDEF UseAppForm}

function TGpsDataSource.AdjustedListOfGPSPlots: TList<RNavigateLongLat>;
Var
  LTravel, NewTravel: Double;
  i, iGpsR, ProgCount, ResetCount, NewListCount: integer;
  NxtReset: TLocalGpsStartRec;
  LastPos: RNavigateLongLat;
begin
  Result := FListOfAllProgress;
  if FListOfAllProgress = nil then
    exit;

  if FListOfGPSResets = nil then
    exit;

  if FBestListOfProgress = nil then
    FBestListOfProgress := TList<RNavigateLongLat>.Create;
  FBestListOfProgress.Clear;
  LTravel := TotalDistance;
  ResetCount := FListOfGPSResets.Count;
  ProgCount := FListOfAllProgress.Count;
  iGpsR := 0;
  i := 0;
  while i < ProgCount do
  begin
    if iGpsR < ResetCount then
      NxtReset := FListOfGPSResets[iGpsR]
    else
      NxtReset.SetData(ProgCount);
    while i < NxtReset.LastSampleNo do
    begin
      FBestListOfProgress.Add(FListOfAllProgress[i]);
      inc(i);
    end;
    inc(i); // skip first reading after start
  end;
  NewTravel := 0.0;
  i := 1;
  NewListCount := FBestListOfProgress.Count;
  if NewListCount > 1 then
  begin
    LastPos := FBestListOfProgress[0];
    while i < NewListCount do
    begin
      NewTravel := NewTravel + LastPos.MetresFrom(FBestListOfProgress[i]);
      LastPos := FBestListOfProgress[i];
      inc(i);
    end;
  end;
  if Abs(NewTravel - LTravel) > 1 then
    FTotalDistance := (NewTravel + LTravel) / 2;
end;
{$ENDIF}

constructor TGpsDataSource.Create;
begin
  if LGPSDataSource <> nil then
    raise Exception.Create('TGpsDataSource.Create >> LGPSDataSource<>nil');
  LGPSDataSource := self;
end;

destructor TGpsDataSource.Destroy;
begin
  if LGPSDataSource = self then
    LGPSDataSource := nil;
{$IFDEF UseAppForm}
  FAppForm := nil;
{$ENDIF}
  inherited;
end;

procedure TGpsDataSource.DoGpsStartStopProc(ASensor: TIsLocationSensor);
Var
  LocalGpsStartRec: TLocalGpsStartRec;
begin
  LocalGpsStartRec.SetData(ListOfAllProgress.Count);
  ListOfGPSResetTimes.Add(LocalGpsStartRec);
  if Assigned(FOnGpsStartStopProc) then
    FOnGpsStartStopProc(ASensor);
end;

function TGpsDataSource.GetAboutText: String;
{$IFNDEF UseAppForm}
//Var
//  Man: TIsSensorManager;
{$ENDIF}
begin
{$IFDEF UseAppForm}
  if FAppForm <> nil then
    FAboutText := FAppForm.MmoAbout.Text;
  Result := FAboutText;
{$ELSE}
//  if FAboutText = '' then
//  Begin          FAbount text updaed by sensor
//    Man := TIsSensorManager.CurrentIsSensorManager; //Singleton appears a problem
//    if Man <> nil then
//      FAboutText := Man.TextList;
//    FAboutText := FAboutText + #13#10 + LibCompilerString;
//  End;
  Result := FAboutText + #13#10 + ProgressText;
{$ENDIF}
end;

function TGpsDataSource.GetListOfAllProgress: TList<RNavigateLongLat>;
var
  Count: integer;
begin
  Count := 10;
{$IFDEF UseAppForm}
  Result := FAppForm.ListOfAllProgress;
  While (Result = nil) and (Count > 0) do
  Begin
    Result := FAppForm.ListOfAllProgress;
    Sleep(1000);
    Dec(Count);
  End;
{$ELSE}
  if FListOfAllProgress = nil then
    FListOfAllProgress := TList<RNavigateLongLat>.Create;
  Result := FListOfAllProgress;
{$ENDIF}
  if (Result = nil) then
    raise Exception.Create('Error (FListOfAllProgress=nil)');
end;

function TGpsDataSource.GetListOfSampleTimes: TList<TDateTime>;
Var
  Count: integer;
Begin
  Count := 10;
{$IFDEF UseAppForm}
  Result := FAppForm.ListOfSampleTimes;
  While (Result = nil) and (Count > 0) do
  Begin
    Result := FAppForm.ListOfSampleTimes;
    Sleep(1000);
    Dec(Count);
  End;
{$ELSE}
  if FListOfSampleTimes = nil then
    FListOfSampleTimes := TList<TDateTime>.Create;
  Result := FListOfSampleTimes;
{$ENDIF}
  if (Result = nil) then
    raise Exception.Create('Error (ListOfSampleTimes=nil)');

end;

function TGpsDataSource.GetSamplingDetails: String;
begin
{$IFDEF UseAppForm}
  if FAppForm <> nil then
    FSamplingDetails := FAppForm.MmoSamplingDetails.Text;
{$ELSE}
{$ENDIF}
  Result := FSamplingDetails;
end;

function TGpsDataSource.GetTotalDistance: Double;
Var
  LastValidLoc: RNavigateLongLat;
  i, last: integer;
  Traveled: Double;
const
  Delta: Double = 20; // meters
begin
{$IFDEF UseAppForm}
  FTotalDistance := FAppForm.TotalDistance;
{$ELSE}
  FTotalDistance := 0.0;
  last := ListOfAllProgress.Count;
  if last > 1 then
  Begin
    LastValidLoc := ListOfAllProgress[0];
    i := 1;
    while i < last do
    Begin
      Traveled := LastValidLoc.MetresFrom(ListOfAllProgress[i]);
      if Traveled > Delta then
      Begin
        LastValidLoc := ListOfAllProgress[i];
        FTotalDistance := FTotalDistance + Traveled;
      End;
      inc(i);
    End;
  End;
{$ENDIF}
  Result := FTotalDistance;
end;

function TGpsDataSource.ListOfGPSResetTimes: TList<TLocalGpsStartRec>;
begin
  if FListOfGPSResets = nil then
    FListOfGPSResets := TList<TLocalGpsStartRec>.Create;
  Result := FListOfGPSResets;
end;

procedure TGpsDataSource.LoadForm;
Var
  Count: integer;
begin
{$IFDEF UseAppForm}
  Count := 20;
  While (FAppForm = nil) and (Count > 0) do
  Begin
    FAppForm := TabbedwithNavigationForm;
    Sleep(1000);
    Dec(Count);
  End;
  if FAppForm = nil then
    raise Exception.Create('Error TabbedwithNavigationForm=nil');

  if Assigned(FAppForm.OnCloseQuery) then
  begin
    if not Assigned(FAppForm.OnClose) then
      FAppForm.OnClose := NavFormClose;
  end
  else
    FAppForm.OnCloseQuery := NavCloseQuery;
{$ELSE}
  Count := 20;
{$ENDIF}
end;

procedure TGpsDataSource.NavCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self);
  CanClose := true;
  FAppForm := nil;
  // Free;
end;

procedure TGpsDataSource.NavFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self);
  // Action := TCloseAction.caFree;
  FAppForm := nil;
  // Free;
end;

procedure TGpsDataSource.SetOnCloseQuery(const Value: TNotifyEvent);
begin
  FOnCloseQuery := Value;
end;

procedure TGpsDataSource.SetOnGpsStartStopProc(const Value
  : TISLocationChangedEvent);
begin
{$IFDEF UseAppForm}
  FAppForm.SetOnGpsStartStopProc(Value);
{$ENDIF}
  FOnGpsStartStopProc := Value;
end;

procedure TGpsDataSource.SetProgressText(const Value: String);
begin
  FProgressText := Value;
end;

{ TLocalGpsStartRec }

procedure TLocalGpsStartRec.SetData(ALastSample: integer);
begin
  LastSampleNo := ALastSample;
  ResetTime := Now;
end;

end.
