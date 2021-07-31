unit IsMobileSensors;
{ Android const    C:\Program Files (x86)\Embarcadero\Studio\19.0\source\rtl\android\Androidapi.Sensor
  ASENSOR_TYPE_ACCELEROMETER       = 1;
  ASENSOR_TYPE_MAGNETIC_FIELD      = 2;
  ASENSOR_TYPE_GYROSCOPE           = 4;
  ASENSOR_TYPE_LIGHT               = 5;
  ASENSOR_TYPE_PRESSURE            = 6;
  ASENSOR_TYPE_PROXIMITY           = 8;
  ASENSOR_TYPE_GRAVITY             = 9;
  ASENSOR_TYPE_LINEAR_ACCELERATION = 10;
  ASENSOR_TYPE_ROTATION_VECTOR     = 11;
  ASENSOR_TYPE_RELATIVE_HUMIDITY   = 12;
  ASENSOR_TYPE_AMBIENT_TEMPERATURE = 13;

  TAndroidSensorManager.Activate;
  Accelerator: TAndroidNativeAccelerometrSensor;
  Orientation: TAndroidNativeGyroscopeSensor;
  Light: TAndroidNativeLightSensor;
  Pressure: TAndroidNativePressureSensor;
  MagneticField: TAndroidNativeMagneticSensor;
  Proximity: TAndroidNativeProximitySensor;
  Rotation: TAndroidNativeRotationSensor;
  Temperature: TAndroidNativeTemperatureSensor;
  Humidity: TAndroidNativeHumiditySensor;
  Gravity: TAndroidNativeGravitySensor;
  LinearAcceleration: TAndroidNativeLinearAccelerometrSensor;
  Location: TUIAndroidLocationSensor;

}

interface
{$I InnovaMultiPlatLibDefs.inc}
Uses IsNavUtils, System.Math, System.Sysutils, System.classes,
  System.Generics.Collections,
  FMX.Types, System.Sensors;

Type

  TIsTimedEventSensor = Class;
  TIsTimedEventSensorClass = Class of TIsTimedEventSensor;

  TIsSensorManager = Class(TObject) // Just to see available sensors
  Private
    FTextList: String;
    FSensorList: TStringList;
    Function GetIsTimedEventSensor(ACat: TSensorCategory)
      : TIsTimedEventSensorClass;
    Function SensorRegistered(ASensor: TIsTimedEventSensor): Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function AddIsSensor(ASensor: TIsTimedEventSensor): Integer;
    Procedure DropIsSensor(ASensor: TIsTimedEventSensor);
    Procedure ActivateAllSensors(AReActivate: Boolean);
    Class Function CurrentIsSensorManager: TIsSensorManager;
    Class Function SensorCatText(ACat: TSensorCategory): String;
    property TextList: String read FTextList;
    // Function TextListAllSensors:String;
  End;

  RRunAveLocRec = Record
    NavLoc: RNavigateLongLat;
    Time: TDateTime;
    JitterStr: String;
    NumberInAverage: Integer;
  end;

{$IFDEF ANDROID}

  TAndroideTime = record
    Error: Double;
    Function DelphiDateTime(AStartOffsetTick: Int64; AOffsetStart: TDateTime)
      : TDateTime;
    case Integer of
      1:
        (DateTimeVal: TDateTime;);
      2:
        (ValInt64: Int64;);
      3:
        (a, b, c, d, e, f, g, h: byte;);
      4:
        (hw, lw, hw2, lw2: word;);
      5:
        (llw, hlw: longword;);
  end;
{$ENDIF}

  TIsLocationSensor = class;
  TIsMotionSensor = class;
  TIsOrientationSensor = class;

  TISLocationChangedEvent = Procedure(ASensor: TIsLocationSensor) of Object;
  TISMotionChangedEvent = Procedure(ASensor: TIsMotionSensor) of Object;
  TISOrientationChangedEvent = Procedure(ASensor: TIsOrientationSensor)
    of Object;

  RISSensorVector = record
    TwoDOnly: Boolean;
    x, y, z: Double;
    // Function Vector3D: TVector3D; // W always 1
    // https://www.songho.ca/math/homogeneous/homogeneous.html
    Procedure Create(Ax, Ay, Az: Double);
    Procedure SetZero;
    Function IsValid: Boolean;
    Function SameAs(ATst: RISSensorVector;
      AEpsilonVector: RISSensorVector): Boolean;
    // could specify epsilon but calculate
  end;

  TIsTimedEventSensor = Class(TObject)
  private
    FIsSensorManager: TIsSensorManager;
    FSensors: TSensorArray;
    FNoOfTimeSamples: Integer;
{$IFDEF ANDROID}
    FStartAndroidTimes: Array of Int64;
    FLastAndroidTimes: Array of Int64;
{$ENDIF}
    FStartTimes, FSensorStampTimes, FCurrentSampleTime: Array of TDateTime;
    FUpdateInterval: Double; // USeconds???
    FTimerInterval: cardinal; // msec;
    FEventTimer: TTimer;
    procedure SetUpdateInterval(const Value: Double);
    Procedure TimerEvent(Sender: TObject);
    Procedure SensorDataChange(Sender: TObject);
    function GetStarted: Boolean;
    procedure SetStarted(const Value: Boolean);
  protected
    FOnChange: TNotifyEvent;
    FNewSample: Boolean;
    FSampleSeconds: Double;
    FLastError: String;
    FRunningAverageSampleTime, FRunningSumOfSquaresSampleTime: Double;
    // seconds
    Procedure RefreshSensors;
    Procedure ConfirmSensors; Virtual;
    Procedure FilterProperties; Virtual;
    // Procedure DoCalculatedValues; Virtual; Abstract;
    function ThisCategory: TSensorCategory; virtual; abstract;
    function TextProperties(Index: Integer): string; virtual; abstract;
    procedure SetSensorInterval(ASensor: TCustomSensor);
    procedure GetValues(Index: Integer); virtual;
    Class Function DecodeType(ASensor: TCustomSensor): String; virtual;
    Function IndexOfSensor(ASensor: TObject): Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    // Function MagAcceleration: Single;
    Function TextSensorTypes: String;
    Class Function ClassTextDetails(ASensor: TCustomSensor;
      Const APreamble: string = ''): String;
    Function AllTextProperties: string;
    Function SampleTimes: string;
    Procedure StartSensorByIndex(Index: Integer);
    Procedure StartSensorByTextType(AStartString: String);
    Procedure StartAllSensors;
    Procedure StopSensorByIndex(Index: Integer);
    Procedure StopAllSensors;
    // Stop interogating a sensor after creation
    Procedure DropSensorByIndex(Index: Integer);
    Procedure DropSensorByTextType(ADrop: String);
    property UpdateInterval: Double read FUpdateInterval
      write SetUpdateInterval; // uSec
    Property Started: Boolean read GetStarted write SetStarted;
    Property NoOfTimeSamples: Integer Read FNoOfTimeSamples;
  End;

  TIsOrientationSensor = Class(TIsTimedEventSensor)
  private
    FCurrentValue, FLastValue: RISSensorVector;
    FSensorTypes: Array of TOrientationSensorType;
    FAvailableProperties: Array of TCustomOrientationSensor.TProperties;
    FCurrentTilt, FCurrentDistance, FCurrentHeading: RISSensorVector;
    FLastTilt, FLastDistance, FLastHeading: RISSensorVector;
    { FTiltX: Double;
      // Inclinometer y-axis angle in degrees
      FTiltY: Double;
      // Inclinometer z-axis angle in degrees
      FTiltZ: Double;
      // Distance x-axis in meters
      FDistanceX: Double;
      // Distance y-axis in meters
      FDistanceY: Double;
      // Distance z-axis in meters
      FDistanceZ: Double;
      // Compass heading x-axis in degrees
      FHeadingX: Double;
      // Compass heading y-axis in degrees
      FHeadingY: Double;
      // Compass heading z-axis in degrees
      FHeadingZ: Double;
      // Compass heading relative to magnetic north (uncompensated)
    }
    FMagHeading: Double;
    // Compass heading relative to true north (uncompensated)
    FTrueHeading: Double;
    // Compass heading relative to magnetic north (compensated)
    FCompMagHeading: Double;
    // Compass heading relative to true north (compensated)
    FCompTrueHeading: Double;
    // Determines how often motion data is updated
    Procedure SetOnChange(Value: TISOrientationChangedEvent);
    Function PropText(AProp: TCustomOrientationSensor.TProperty): String;
  protected
    // Procedure DoCalculatedValues; override;
    function ThisCategory: TSensorCategory; override;
    function TextProperties(Index: Integer): string; override;
    procedure GetValues(Index: Integer); override;
    Procedure ConfirmSensors; override;
    Class Function DecodeType(ASensor: TCustomSensor): String; override;
  public
    Constructor Create;
    // Destructor Destroy; override;
    Property CurrentValue: RISSensorVector read FCurrentValue
      write FCurrentValue;
    Property LastValue: RISSensorVector read FLastValue write FLastValue;
    Property OnMotionEvent: TISOrientationChangedEvent write SetOnChange;
  End;

{$IFDEF msWindows}

  TLocationPropArray = Array
    [0 .. Ord(TCustomLocationSensor.TProperty.CountryRegion)] of Double;
  { .Latitude ..TProperty.CountryRegion) }

  TDummyWindowsLocationSensor = Class(TCustomLocationSensor)
  private
    FThisSampleTime, FSpeed, FTrueHeading: Double;
    FDummyLocationTime: TDateTime;
    fDoubleProps: TLocationPropArray;
    FRandomTimer: TTimer;
    FLatitude, FLongitude: Double;
    FLastValue: TLocationCoord2D;
    Procedure LocationTimerEvent(Sender: TObject);
  protected
    function GetAccuracy: TLocationAccuracy;  override;
    function GetDistance: TLocationDistance;  override;
    function DoGetInterface(const IID: TGUID; out Obj): HResult;
{$IFDEF  ISD102T_DELPHI}
    override;
{$ENDIF}
    function GetPowerConsumption: TPowerConsumption;  override;
    procedure SetAccuracy(const Value: TLocationAccuracy);  override;
    procedure SetDistance(const Value: TLocationDistance); override;
    procedure DoLocationChangeType; override;
    procedure DoOptimize;  override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetLocationSensorType: TLocationSensorType; override;
    function GetDoubleProperty(Prop: TCustomLocationSensor.TProperty)
      : Double; override;
    function GetAvailableProperties: TCustomLocationSensor.TProperties;
      override;
    function GetAuthorized: TAuthorizationType; override;
    procedure SetDoubleProperty(Val: Double;
      Prop: TCustomLocationSensor.TProperty);
    constructor Create(AManager: TSensorManager); override;
  public
    destructor Destroy; override;
  End;

  TMotionPropArray = Array [0 .. Ord(TCustomMotionSensor.TProperty.Speed)
    ] of Double; { .AccelerationX ..TProperty.Speed) }

  TDummyWindowsMotionSensor = Class(TCustomMotionSensor)
  private
    FThisSampleTime: Double;
    FDummyMotionTime, FNextChange: TDateTime;
    fDoubleProps: TMotionPropArray;
    FRandomTimer: TTimer;
    FLx, FLy, FLz: Double;
    Procedure MotionTimerEvent(Sender: TObject);
  protected
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetState: TSensorState; override;
    function DoGetInterface(const IID: TGUID; out Obj): HResult;
{$ifdef ISD102T_DELPHI}
    override;
{$Endif}
    function GetTimeStamp: TDateTime; override;
    function GetMotionSensorType: TMotionSensorType; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override; // uSec
    function GetDoubleProperty(Prop: TCustomMotionSensor.TProperty)
      : Double; override;
    function GetAvailableProperties: TCustomMotionSensor.TProperties; override;
    procedure SetDoubleProperty(Val: Double;
      Prop: TCustomMotionSensor.TProperty);
    constructor Create(AManager: TSensorManager); override;
  public
    destructor Destroy; override;
  End;

{$ENDIF}

  TIsMotionSensor = Class(TIsTimedEventSensor)
  private
    FCurrentValueLinear, FLastValueLinear: RISSensorVector;
    FCurrentValueAngle, FLastValueAngle: RISSensorVector;
    FSensorTypes: Array of TMotionSensorType;
    FAvailableProperties: Array of TCustomMotionSensor.TProperties;
    // sensor data is updated in microseconds.
    // FUpdateInterval: Double;
    // X-axis Acceleration in G's (Gravity)
    { FAccelerationX: Double;
      // Y-axis Acceleration in G's (Gravity)
      FAccelerationY: Double;
      // Z-axis Acceleration in G's (Gravity)
      FAccelerationZ: Double;
      // X-axis Angular Acceleration in Degrees/Second^2
      FAngleAccelX: Double;
      // Y-axis Angular Acceleration in Degrees/Second^2
      FAngleAccelY: Double;
      // Z-axis Angular Acceleration in Degrees/Second^2
      FAngleAccelZ: Double;
    }
    { // Motion State
      FMotion: Double; Specifies whether the device is currently in motion (True) or not (False).
      This property is only supported on Windows. }
    // Speed in Meters/Second
    FSpeed: Double;
    FChangedAngular, FChangedLinear: Boolean;
    // Determines how often motion data is updated

    Function PropText(i: Integer): String;
    procedure SetOnChange(const Value: TISMotionChangedEvent);
  protected
    // Procedure DoCalculatedValues; override;
    function ThisCategory: TSensorCategory; override;
    function TextProperties(Index: Integer): string; override;
    procedure GetValues(Index: Integer); override;
    Procedure ConfirmSensors; override;
    Class Function DecodeType(ASensor: TCustomSensor): String; override;
  public
    Constructor Create;
    // Destructor Destroy; override;
    // Function MagAcceleration: Single;
    Property CurrentValueLinear: RISSensorVector read FCurrentValueLinear
      write FCurrentValueLinear;
    Property LastValueLinear: RISSensorVector read FLastValueLinear
      write FLastValueLinear;
    Property CurrentValueAngle: RISSensorVector read FCurrentValueAngle
      write FCurrentValueAngle;
    Property LastValueAngle: RISSensorVector read FLastValueAngle
      write FLastValueAngle;
    Property OnMotionEvent: TISMotionChangedEvent write SetOnChange;
    Property ChangedAngular: Boolean Read FChangedAngular;
    Property ChangedLinear: Boolean Read FChangedLinear;
  End;

  TIsLocationSensor = Class(TIsTimedEventSensor)
  Private
    FSensorTypes: Array of TLocationSensorType;
    FAvailableProperties: Array of TCustomLocationSensor.TProperties;
    FNavSensors: Array of Boolean;
    FNoOfLocValSamples, FNoOfGenSamples { AllowsReset } ,
      FNoOfConsistentAltSamples,//Altitude
      FNoOfConsistentLocSamples,FAverageResetLimit: Integer;
    // FPrevLocationValue,
    FLastLocValue, FStartLocValue, FRunningLocationSumOfSquares,
      FRunningLocationValue: TLocationCoord2D;
    FRunningAltitudeValue, FRunningAltitudeSumOfSquares, FRunningHeadingValue,
      FRunningHeadingSumOfSquares, FRunningSpeedValue,
      FRunningSpeedSumOfSquares, FLastAlt, FLastHeading, FLastSpeed: Double;
    FAllowNaN: Boolean;
    // FCurrentErrors: String;
    FCalNavSensor: Integer; // sensor used to Calculate Navigation
    FLatitude, FLongitude, FErrorRadius, FAltitude, FSpeed, FTrueHeading,
      FMagneticHeading: Double;
    FAddress1, FAddress2, FCity, FStateProvince, FPostalCode,
      FCountryRegion: string;
    FDoRunningAveLoc: Boolean;
    FOnBeforeAverageReset: TISLocationChangedEvent;
    FMetersToTriggerLocationChange: Integer;
    FMotionSensor: TIsMotionSensor;
    FMotionChange: TISMotionChangedEvent;
    FGpsResetTimer: TTimer;
    FGpsTimeToReset: Integer; { by 10 seconds }
    Procedure OnGpsResetTimer(Sender: TObject);
    Procedure StartGpsResetTimer;
    Procedure RestartGps;
    // if Calculating averages and Gps give no value for 3000 Seconds
    Procedure CalNewCalNewLocationAverageAndSumOfSquares
      (ANewSample: TLocationCoord2D; Var ARunningLocAveage,
      ARunningLocAveageSumOfSqrs: TLocationCoord2D; ANoOfSamples: Integer);
    Procedure OnSensorLocChange(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
    Procedure OnMotionSensorChange(Sender: TIsMotionSensor);
    // Procedure OnSensorHeadingChange(Sender: TObject; const AHeading: THeading);
    Procedure SetOnChange(Value: TISLocationChangedEvent);
    Function PropText(AProp: TCustomLocationSensor.TProperty): String;
    procedure SetCalNavSensor(const Value: Integer);
    procedure SetDoRunningAveLoc(const Value: Boolean);
    procedure SetOnAverageReset(const Value: TISLocationChangedEvent);
    procedure SetMetersToTriggerLocationChange(const Value: Integer);
    procedure SetMotionSensor(const Value: TIsMotionSensor);
    procedure SetOnMotionChange(const Value: TISMotionChangedEvent);
  Protected
    // Procedure DoCalculatedValues; override;
    function ThisCategory: TSensorCategory; override;
    function TextProperties(Index: Integer): string; override;
    procedure GetValues(Index: Integer); override;
    Procedure ConfirmSensors; override;
    Class Function DecodeType(ASensor: TCustomSensor): String; override;
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Function ValidLocation(ALocation: TLocationCoord2D): Boolean;
    Class Function SameLocation(ALocA, ALocB: TLocationCoord2D;
      Epsilon: Double = 1 / 60 / 60): Boolean; Overload;
    Class Function SameLocation(ALocA, ALocB: RNavigateLongLat;
      Epsilon: Double = 1 / 60 / 60): Boolean;  Overload;
    Procedure NullLocation(Var ALocation: TLocationCoord2D);
    Function IsNullLocation(ALocation: TLocationCoord2D): Boolean;
    Function CurrentLocation: RNavigateLongLat;
    // Function LastLocation: RNavigateLongLat;
    Function StartLocation: RNavigateLongLat;
    Function RunningLocationValue: RNavigateLongLat;
    Function RunningLocaltionSample: RRunAveLocRec;
    Function TrueHeading: Double;
    Function AltitudeMeters: Double;
    Function SpeedMetersPerSec: Double;
    Function AverageLocation(APrecision: Integer = 1): string;
    Function HasAveLoc: Boolean;
    Function AverageLocationDev(AAsMeters: Boolean): string;
    Function AverageSpeed: string;
    Function AverageAltitude: string;
    Function HasAveAlt: Boolean;
    Function AverageHeadinge: string;
    Function SpeedKilometersPerHour: Double;
    Function LastNavQuery: TDateTime;
    procedure ResetLocationAverages;
    Procedure ResetAverages;
    Procedure Restart;
    Procedure StartNav;
    Property CalNavSensor: Integer read FCalNavSensor write SetCalNavSensor;
    Property AllowNaN: Boolean read FAllowNaN write FAllowNaN;
    Property OnLocChange: TISLocationChangedEvent write SetOnChange;
    Property OnMotionChange: TISMotionChangedEvent Write SetOnMotionChange;
    Property OnBeforeAverageReset: TISLocationChangedEvent
      write SetOnAverageReset;
    Property NoOfLocSamples: Integer Read FNoOfLocValSamples;
    Property NoOfGenSamples: Integer Read FNoOfGenSamples;
    Property NoOfConsistentAltSamples: Integer Read FNoOfConsistentAltSamples;
    Property NoOfConsistenLocSamples: Integer Read FNoOfConsistentLocSamples;
    Property DoRunningAveLoc: Boolean Read FDoRunningAveLoc
      Write SetDoRunningAveLoc;
    Property EndAverageMotionSensor: TIsMotionSensor Read FMotionSensor
      Write SetMotionSensor;
    Property MetersToTriggerLocationChange: Integer
      read FMetersToTriggerLocationChange
      write SetMetersToTriggerLocationChange;
    Property AverageResetLimit: Integer Read    FAverageResetLimit;
    Property LastError: String Read FLastError;
  End;

Const
  TestStaticLocation: Boolean = True; // False;
  GPSAccuracy = 40; // meters
  CGpsResetCount = 30; // By 10 Seconds

implementation

var
  LocalCurrentSensorManager: TSensorManager = nil;
  // Motion Sensors  https://developer.android.com/guide/topics/sensors/sensors_motion#sensors-motion-grav
  // http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Sensors.TMotionSensorType
  { Offers information about the acceleration, angle, state, and speed of the device motion.

    AccelerationX, AccelerationY and AccelerationZ return the acceleration in gals (g) for the X,Y, and Z axis.
    https://en.wikipedia.org/wiki/Gal_(unit)

    AngleAccelX, AngleAccelY and AngleAccelZ return the angular acceleration in degrees per second squared (°/s²) for the X,Y, and Z axis.

    Speed determines the speed of the device in meters per second (m/s).

    Motion determines whether the device is currently in motion or not.
    Platform Support

    This class supports the following platforms:
    Platform 	Details

    Android


    Android provides different types of motion sensors. Each type of sensor supports a different set of properties:

    The Accelerometer3D sensor provides the AccelerationX, AccelerationY and AccelerationZ properties.
    The Gyrometer3D sensor provides the AngleAccelX, AngleAccelY and AngleAccelZ properties.
    The GravityAccelerometer3D sensor provides the AccelerationX, AccelerationY and AccelerationZ properties.
    The LinearAccelerometer3D sensor provides the AccelerationX, AccelerationY and AccelerationZ properties.

    iOS


    iOS provides different types of motion sensors. Each type of sensor supports a different set of properties:

    The Accelerometer3D sensor provides the AccelerationX, AccelerationY and AccelerationZ properties.
    The MotionDetector sensor provides the AccelerationX, AccelerationY, AccelerationZ, AngleAccelX, AngleAccelY and AngleAccelZ properties.

    Windows


    Complete support. \ }

  { Location Sensor
    The following table provides platform support details for properties that provide data measured by a location sensor:
    Item 	    Android 	  iOS 	      OS X 	      Windows
    Address1 				      Supported
    Address2 				      Supported
    Altitude 	Supported 	Supported 	Supported 	Supported
    City 				                                  Supported
    CountryRegion 				                        Supported
    ErrorRadius 				                          Supported
    Latitude 	Supported 	Supported 	Supported 	Supported
    Longitude Supported 	Supported 	Supported 	Supported
    MagneticHeading 		  Supported 		          Supported
    PostalCode 				                            Supported
    Speed 	  Supported 	Supported 	Supported 	Supported
    TrueHeadingSupported 	Supported 		          Supported


    Sensor Properties

    The following table provides platform support details for properties that you can use to configure and manage a location sensor:
    Item 	      Android 	    iOS 	        OS X 	      Windows 	Notes
    Accuracy 		              Supported 	Supported
    Distance 		              Supported 	Supported
    On iOS, you must set LocationChange to TLocationChangeType.lctSmall in order to use Distance.
    On Windows, use the less precise LocationChange instead.

    LocationChange 		        Supported 		          Supported
    On iOS, if you select a small location change (TLocationChangeType.lctSmall), you can additionally use Distance to specify a more precise value.
    On OS X, use the more precise Distance instead.

    Optimize 				                                  Supported
    On iOS and OS X, the type of the location sensor that you choose, such as GPS or Lookup, determines the level of optimization.
    Regions 		                                      Supported
  }
  // http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Sensors.TCustomLocationSensor
  // http://docwiki.embarcadero.com/Libraries/Rio/en/System.Sensors.TCustomLocationSensor

  { From IsArrayLib }
Type
  TArrayofObjects = array of TObject;

function IndexInArray(AArray: TArrayofObjects; ATest: TObject)
  : Integer; overload;
var
  i: Integer;
begin
  Result := -1;
  for i := low(AArray) to high(AArray) do
    if AArray[i] = ATest then
    begin
      Result := i;
      break;
    end;
end;

{ TIsLocationSensor }

function TIsLocationSensor.AltitudeMeters: Double;
begin
  if Length(FSensors) < 0 then
  begin
    if FAllowNaN then
      Result := NaN
    else
      Result := 0.0;
  end
  else
  begin
    // UpdateSensorValues;
    Result := FLastAlt;
    if not FAllowNaN then
      if isNaN(Result) then
        Result := 0.0;
  end;
end;

function TIsLocationSensor.AverageAltitude: string;
begin
  Result := FormatFloat('0.000', FRunningAltitudeValue) + 'Mtrs (Dev ' +
    FormatFloat('0.0', CalDoubleStdDevFromSumOfSquares
    (FRunningAltitudeSumOfSquares, FNoOfLocValSamples) / FRunningAltitudeValue *
    100) + '%)';
end;

function TIsLocationSensor.AverageHeadinge: string;
begin
  Result := TextDegreeMinuteSecondFrmDec(FRunningHeadingValue, 5) + '  (' +
    FormatFloat('0.00', CalDoubleStdDevFromSumOfSquares
    (FRunningHeadingSumOfSquares, FNoOfGenSamples)) + AngleDegreeChar + ')';
end;

function TIsLocationSensor.AverageLocation(APrecision: Integer = 1): string;
Var
  Pos: RNavigateLongLat;
begin
  Pos.CreateDec(FRunningLocationValue.Longitude,
    FRunningLocationValue.Latitude);
  Result := Pos.LocatationText(APrecision);
end;

function TIsLocationSensor.AverageLocationDev(AAsMeters: Boolean): string;
Var
  LongDev, LatDev: Double;
begin
  LongDev := CalDoubleStdDevFromSumOfSquares
    (FRunningLocationSumOfSquares.Longitude, FNoOfConsistentLocSamples);
  LatDev := CalDoubleStdDevFromSumOfSquares
    (FRunningLocationSumOfSquares.Latitude, FNoOfConsistentLocSamples);
  if AAsMeters then
    Result := '(Jitter ' + FormatFloat('0.00',
      LongDev * MtrsPerDegreeLongAdjustForLatitude(FLastLocValue.Latitude)) +
      'm E/W ' + FormatFloat('0.00', LatDev * MtrsPerDegree) + 'm N/S)'
  Else
    Result := '(Jitter ' + FormatFloat('0.0', LongDev * 60 * 60) + '" Long ' +
      FormatFloat('0.0', LatDev * 60 * 60) + '" Lat)';
end;

function TIsLocationSensor.AverageSpeed: string;
begin
  Result := FormatFloat('0.000', FRunningSpeedValue / 1000 * 60 * 60) +
    'kmph (Dev ' + FormatFloat('0.0',
    CalDoubleStdDevFromSumOfSquares(FRunningSpeedSumOfSquares,
    FNoOfLocValSamples) / FRunningSpeedValue * 100) + '%)';
end;

procedure TIsLocationSensor.CalNewCalNewLocationAverageAndSumOfSquares
  (ANewSample: TLocationCoord2D; var ARunningLocAveage,
  ARunningLocAveageSumOfSqrs: TLocationCoord2D; ANoOfSamples: Integer);
begin
  CalNewDoubleAverageAndSumOfSquares(ANewSample.Latitude,
    ARunningLocAveage.Latitude, ARunningLocAveageSumOfSqrs.Latitude,
    ANoOfSamples);
  // Problem across international dateline
  CalNewRationalAngleDegreesAverageAndSumOfSquares(ANewSample.Longitude,
    ARunningLocAveage.Longitude, ARunningLocAveageSumOfSqrs.Longitude,
    ANoOfSamples, false);
end;

procedure TIsLocationSensor.ConfirmSensors;
Var
  i: Integer;
begin
  inherited;
  if Length(FNavSensors) < 1 then
    Exit; // inherited create
  for i := 0 to High(FSensors) do
    if FSensorTypes[i] <> TCustomLocationSensor(FSensors[i]).SensorType then
      raise Exception.Create
        ('TIsLocationSensor.ConfirmSensors >> Order was not preserved')
    Else if TCustomLocationSensor.TProperty.Latitude in FAvailableProperties[i]
    then
    Begin
      FNavSensors[i] := True;
      TCustomLocationSensor(FSensors[i]).OnLocationChanged := OnSensorLocChange;
    End;

end;

constructor TIsLocationSensor.Create;
Var
  i: Integer;
begin
  FCalNavSensor := -20;
  inherited;
  SetLength(FAvailableProperties, Length(FSensors));
  SetLength(FSensorTypes, Length(FSensors));
  SetLength(FNavSensors, Length(FSensors));
  for i := 0 to High(FSensors) do
  begin
    FSensorTypes[i] := TCustomLocationSensor(FSensors[i]).SensorType;
    FAvailableProperties[i] := TCustomLocationSensor(FSensors[i])
      .AvailableProperties;
  end;
  FAverageResetLimit:=10;
  FMetersToTriggerLocationChange := 5;
  FLastLocValue.Create(0.0, 0.0);
  FStartLocValue.Create(0.0, 0.0);
  FRunningSumOfSquaresSampleTime := 0;
  FRunningAverageSampleTime := 0;
  ResetAverages;
  ConfirmSensors;
end;

function TIsLocationSensor.CurrentLocation: RNavigateLongLat;
begin
  if Length(FSensors) < 0 then
{$IFDEF MsWindows}
    Result.CreateDec(FLastLocValue.Longitude, FLastLocValue.Latitude)
{$ELSE}
    begin
  if Not FAllowNaN then
    Result.CreateDec(0.0, 0.0);
end
{$ENDIF}
else
begin
  // UpdateSensorValues;
  Result.CreateDec(FLastLocValue.Longitude, FLastLocValue.Latitude);
end;
end;

Class function TIsLocationSensor.DecodeType(ASensor: TCustomSensor): String;
Var
  Stype: TLocationSensorType;
begin
  Stype := TCustomLocationSensor(ASensor).SensorType;
  case Stype of
    TLocationSensorType.Gps:
      Result := 'GPS';
    TLocationSensorType.Static:
      Result := 'Static';
    TLocationSensorType.Lookup:
      Result := 'Lookup';
    TLocationSensorType.Triangulation:
      Result := 'Triangulation';
    TLocationSensorType.Broadcast:
      Result := 'Broadcast';
    TLocationSensorType.DeadReckoning:
      Result := 'DeadReckoning';
    TLocationSensorType.Other:
      Result := 'Other';
  else
    Result := 'Error';
  end;
end;

destructor TIsLocationSensor.Destroy;
begin
  FGpsResetTimer.Free;
  inherited;
end;

procedure TIsLocationSensor.GetValues(Index: Integer);
Var
  Sensor: TCustomLocationSensor;
  AvailableProperties: TCustomLocationSensor.TProperties;
  NxtProp: TCustomLocationSensor.TProperty;
  FCalNav: Integer;
  Dist: Double;
  ActSamples: Integer;
  NxtLocation: TLocationCoord2D;
  LastNav, ThisNav: RNavigateLongLat;
begin
  // https://developer.android.com/reference/android/location/Location
  // getTime()
  // Return the UTC time of this fix, in milliseconds since January 1, 1970.
  // Sensor have different reference time us the Start Time to get delphi Time In Inherited

  FNewSample := false;
  FCalNav := 0;
  Inherited;
  Sensor := FSensors[index] as TCustomLocationSensor;
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomLocationSensor.TProperty(0)
    to TCustomLocationSensor.TProperty.CountryRegion do
    if NxtProp in AvailableProperties then
      case NxtProp of
        // Latitude:Double
        TCustomLocationSensor.TProperty.Latitude:
          Begin
            FLatitude := Sensor.Latitude;
            Inc(FCalNav);
          end;
        TCustomLocationSensor.TProperty.Longitude:
          Begin
            FLongitude := Sensor.Longitude;
            Inc(FCalNav);
          end;
        TCustomLocationSensor.TProperty.ErrorRadius:
          FErrorRadius := Sensor.ErrorRadius;
        TCustomLocationSensor.TProperty.Altitude:
          FAltitude := Sensor.Altitude;
        TCustomLocationSensor.TProperty.Speed:
          FSpeed := Sensor.Speed;
        TCustomLocationSensor.TProperty.TrueHeading:
          FTrueHeading := Sensor.TrueHeading;
        TCustomLocationSensor.TProperty.MagneticHeading:
          FMagneticHeading := Sensor.MagneticHeading;
        TCustomLocationSensor.TProperty.Address1:
          FAddress1 := Sensor.Address1;
        TCustomLocationSensor.TProperty.Address2:
          FAddress2 := Sensor.Address2;
        TCustomLocationSensor.TProperty.City:
          FCity := Sensor.City;
        TCustomLocationSensor.TProperty.StateProvince:
          FStateProvince := Sensor.StateProvince;
        TCustomLocationSensor.TProperty.PostalCode:
          FPostalCode := Sensor.PostalCode;
        TCustomLocationSensor.TProperty.CountryRegion:
          FCountryRegion := Sensor.CountryRegion;
      end;

  if FCalNav > 1 then
  Begin
    if FCalNavSensor < 0 then
      FCalNavSensor := Index;
    if (FCalNavSensor = Index) and FNewSample then
    begin
      NxtLocation.Create(FLatitude, FLongitude);
      Inc(FNoOfLocValSamples);
      if FDoRunningAveLoc then
      Begin
        If SameLocation(NxtLocation, FLastLocValue,
          MetersToTriggerLocationChange * OneMeterAsDegrees) then
        begin
          if FNoOfConsistentLocSamples = 0 then
            FRunningLocationValue.Create(FLastLocValue.Latitude,
              FLastLocValue.Longitude);
          Inc(FNoOfConsistentLocSamples);
        end
        Else if FNoOfConsistentLocSamples > 0 then
          ResetLocationAverages;

        if FNoOfConsistentLocSamples > FAverageResetLimit then
          // Restart Average after many samples the same
          ResetLocationAverages;

        if SameValue(FAltitude, FLastAlt, 1.0) Then
        begin
          if FNoOfConsistentAltSamples = 0 then
            FRunningAltitudeValue := FLastAlt;
          Inc(FNoOfConsistentAltSamples);
        end
        Else if FNoOfConsistentAltSamples > 0 then
          ResetLocationAverages;
      end;

      if FSampleSeconds > 0.0 then
      Begin
        LastNav.CreateDec(FLastLocValue.Longitude, FLastLocValue.Latitude);
        ThisNav.CreateDec(FLongitude, FLatitude);
        Dist := ThisNav.MetresFrom(LastNav);
        FLastSpeed := Dist / FSampleSeconds;
        if FSpeed <> 0.0 then
          if not SameValue(FLastSpeed, FSpeed, FLastSpeed / 100) then
            FLastError := FLastError + 'Speed Dif ' +
              FormatFloat('0.00', FSpeed) + '<>' + FormatFloat('0.00',
              FLastSpeed) + #13#10;
        FLastHeading := ThisNav.HeadingFrom(LastNav);
        if not SameValue(FLastHeading, FTrueHeading, 1) then
          FLastError := FLastError + 'Heading Dif ' +
            FormatFloat('0.00', FTrueHeading) + '<>' +
            FormatFloat('0.00', FLastHeading) + #13#10;
      End;

      // FPrevLocationValue:=FLastLocValue;
      FLastLocValue.Create(FLatitude, FLongitude);
      FLastAlt := FAltitude;
      FLastHeading := FTrueHeading;
      FLastSpeed := FSpeed;

      if FDoRunningAveLoc then
      Begin
        ActSamples := FNoOfConsistentLocSamples + 1;
        CalNewCalNewLocationAverageAndSumOfSquares(FLastLocValue,
          FRunningLocationValue, FRunningLocationSumOfSquares, ActSamples);
        CalNewDoubleAverageAndSumOfSquares(FLastAlt, FRunningAltitudeValue,
          FRunningAltitudeSumOfSquares, ActSamples);
      end;

      // ReStart on Reset
      Inc(FNoOfGenSamples);;
      CalNewRationalAngleDegreesAverageAndSumOfSquares(FLastHeading,
        FRunningHeadingValue, FRunningHeadingSumOfSquares,
        FNoOfGenSamples, True);

      CalNewDoubleAverageAndSumOfSquares(FLastSpeed, FRunningSpeedValue,
        FRunningSpeedSumOfSquares, FNoOfGenSamples);
    end;
  end;
End;

function TIsLocationSensor.HasAveAlt: Boolean;
begin
  Result := FRunningAltitudeValue <> 0.0;
end;

function TIsLocationSensor.HasAveLoc: Boolean;
begin
  Result := (FRunningLocationValue.Latitude <> 0.0) or
    (FRunningLocationValue.Longitude <> 0.0);
end;

function TIsLocationSensor.IsNullLocation(ALocation: TLocationCoord2D): Boolean;
begin
  Result := (ALocation.Latitude = 0.0) and (ALocation.Longitude = 0.0);
end;

// function TIsLocationSensor.LastLocation: RNavigateLongLat;
// begin
// Result.CreateDec(FPrevLocationValue.Longitude, FPrevLocationValue.Latitude);
// end;

function TIsLocationSensor.LastNavQuery: TDateTime;
begin
  Result := 0.0;
  if FCalNavSensor >= 0 then
    Result := FSensorStampTimes[FCalNavSensor];
end;

procedure TIsLocationSensor.NullLocation(var ALocation: TLocationCoord2D);
begin
  ALocation.Longitude := 0.0;
  ALocation.Latitude := 0.0;
end;

// procedure TIsLocationSensor.OnSensorHeadingChange(Sender: TObject;
// const AHeading: THeading);
// begin
// try
// if Assigned(FOnChange) then
// FOnChange(self);
// Except
// on e: Exception do
// FLastError := 'OnSensorHeadingChange Exception: ' + e.message;
// end;
// end;

procedure TIsLocationSensor.OnGpsResetTimer(Sender: TObject);
begin
  FGpsResetTimer.Enabled := false;
  try
    Dec(FGpsTimeToReset);
    if FGpsTimeToReset < 0 then
    begin
      FGpsTimeToReset := CGpsResetCount;
      if FDoRunningAveLoc then
        RestartGps;
    end;
  finally
    FGpsResetTimer.Enabled := True;
  end;
end;

procedure TIsLocationSensor.OnMotionSensorChange(Sender: TIsMotionSensor);
begin
  If FMetersToTriggerLocationChange < 100 then
    FMetersToTriggerLocationChange := 100;

  If Sender.ChangedAngular or Sender.ChangedLinear then
    If DoRunningAveLoc then
      if FNoOfLocValSamples > 1 then
        DoRunningAveLoc := false;

  if Assigned(FMotionChange) then
    FMotionChange(Sender);
end;

procedure TIsLocationSensor.OnSensorLocChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
Var
  SensorIndex: Integer;
begin
  FLastError := '';
  FGpsTimeToReset := CGpsResetCount;
  SensorIndex := IndexOfSensor(Sender);
  if SensorIndex <> FCalNavSensor then
    if (FCalNavSensor < 0) and ValidLocation(NewLocation) then
      FCalNavSensor := SensorIndex;
  if SensorIndex < 0 then
    Raise Exception.Create('No Sensor Data in OnSensorLocChange');

  if SensorIndex = FCalNavSensor then
    try
      if IsNullLocation(FStartLocValue) then
        FStartLocValue := NewLocation;

      // FPrevQueryLocValue := OldLocation;
      // must come first else old location changes
      GetValues(SensorIndex);
      // FLastLocValue := NewLocation;      d
      If Not FNewSample then
        FLastError := FLastError + 'Differing Time Stamps in OnSensorLocChange '
          + FormatDateTime('ddd hh:nn:ss.nnn', FSensorStampTimes[SensorIndex]) +
          ' and ' + FormatDateTime('ddd hh:nn:ss.nnn',
          FSensorStampTimes[SensorIndex]) + #13#10;
      if Assigned(FOnChange) then
        FOnChange(self);
    Except
      on e: Exception do
        FLastError := FLastError + 'OnSensorLocChange Exception: ' +
          e.message + #13#10;
    end;
end;

function TIsLocationSensor.PropText
  (AProp: TCustomLocationSensor.TProperty): String;
begin
  case AProp of
    TCustomLocationSensor.TProperty.Latitude:
      Result := 'Latitude';
    TCustomLocationSensor.TProperty.Longitude:
      Result := 'Longitude';
    TCustomLocationSensor.TProperty.ErrorRadius:
      Result := 'ErrorRadius';
    TCustomLocationSensor.TProperty.Altitude:
      Result := 'Altitude';
    TCustomLocationSensor.TProperty.Speed:
      Result := 'Speed';
    TCustomLocationSensor.TProperty.TrueHeading:
      Result := 'TrueHeading';
    TCustomLocationSensor.TProperty.MagneticHeading:
      Result := 'MagneticHeading';
    TCustomLocationSensor.TProperty.Address1:
      Result := 'Address1';
    TCustomLocationSensor.TProperty.Address2:
      Result := 'Address2';
    TCustomLocationSensor.TProperty.City:
      Result := 'City';
    TCustomLocationSensor.TProperty.StateProvince:
      Result := 'StateProvince';
    TCustomLocationSensor.TProperty.PostalCode:
      Result := 'PostalCode';
    TCustomLocationSensor.TProperty.CountryRegion:
      Result := 'CountryRegion';
  else
    Result := 'Error';
  end;
end;

procedure TIsLocationSensor.ResetAverages;
begin
  ResetLocationAverages;
  FStartLocValue.Create(0.0, 0.0);
  FRunningSpeedValue := 0.0;
  FRunningSpeedSumOfSquares := 0.0;
  FRunningHeadingValue := 0.0;
  FRunningHeadingSumOfSquares := 0.0;
  FNoOfGenSamples := 0;
  FNoOfLocValSamples := 0;
end;

function TIsLocationSensor.RunningLocaltionSample: RRunAveLocRec;
begin
  Result.NavLoc := RunningLocationValue;
  Result.Time := Now;
  Result.JitterStr := AverageLocationDev(True);
  Result.NumberInAverage := FNoOfConsistentLocSamples;
end;

function TIsLocationSensor.RunningLocationValue: RNavigateLongLat;
begin
  Result.CreateDec(FRunningLocationValue.Longitude,
    FRunningLocationValue.Latitude);
end;

class function TIsLocationSensor.SameLocation(ALocA, ALocB: TLocationCoord2D;
  Epsilon: Double): Boolean;
begin
  Try
    Result := SameValue(ALocA.Longitude, ALocB.Longitude, Epsilon);
    if Result then
      Result := SameValue(ALocA.Latitude, ALocB.Latitude, Epsilon);
  Except
    Result := false;
  End;
end;

class function TIsLocationSensor.SameLocation(ALocA, ALocB: RNavigateLongLat;
  Epsilon: Double): Boolean;
begin
  Try
    Result := SameValue(ALocA.Longitude, ALocB.Longitude, Epsilon);
    if Result then
      Result := SameValue(ALocA.Latitude, ALocB.Latitude, Epsilon);
  Except
    Result := false;
  End;
end;

procedure TIsLocationSensor.SetCalNavSensor(const Value: Integer);
begin
  if Value < 0 then
    FCalNavSensor := Value // set to Auto
  else if Value <= High(FSensors) then
    if FSensors[Value] is TCustomLocationSensor then
      if TCustomLocationSensor.TProperty.Latitude
        in TCustomLocationSensor(FSensors[Value]).AvailableProperties then
        FCalNavSensor := Value; // Manual set
end;

procedure TIsLocationSensor.SetDoRunningAveLoc(const Value: Boolean);
begin
  if not Value And FDoRunningAveLoc then
    ResetLocationAverages;
  FDoRunningAveLoc := Value;
  if FDoRunningAveLoc then
    StartGpsResetTimer;
end;

procedure TIsLocationSensor.SetMetersToTriggerLocationChange
  (const Value: Integer);
begin
  FMetersToTriggerLocationChange := Value;
end;

procedure TIsLocationSensor.SetMotionSensor(const Value: TIsMotionSensor);
begin
  FMotionSensor := Value;
  if FMotionSensor <> nil then
  Begin
    if Assigned(FMotionSensor.FOnChange) then
      SetOnMotionChange(TISMotionChangedEvent(FMotionSensor.FOnChange));
    FMotionSensor.OnMotionEvent := OnMotionSensorChange;
  End;
end;

procedure TIsLocationSensor.SetOnAverageReset(const Value
  : TISLocationChangedEvent);
begin
  FOnBeforeAverageReset := Value;
end;

procedure TIsLocationSensor.SetOnChange(Value: TISLocationChangedEvent);
begin
  FOnChange := TNotifyEvent(Value);
end;

procedure TIsLocationSensor.SetOnMotionChange(const Value
  : TISMotionChangedEvent);
begin
  FMotionChange := Value;
end;

function TIsLocationSensor.SpeedKilometersPerHour: Double;
begin
  if FAllowNaN then
  begin
    Result := SpeedMetersPerSec;
    if not isNaN(Result) then
      Result := Result / 1000 * 3600;
  end
  else
    Result := SpeedMetersPerSec / 1000 * 3600;
end;

function TIsLocationSensor.SpeedMetersPerSec: Double;
begin
  if Length(FSensors) < 0 then
  begin
    if FAllowNaN then
      Result := NaN
    else
      Result := 0.0;
  end
  else
  begin
    // UpdateSensorValues;
    Result := FLastSpeed;
    if not FAllowNaN then
      if isNaN(Result) then
        Result := 0.0;
  end;
end;

procedure TIsLocationSensor.StartGpsResetTimer;
begin
  if FGpsResetTimer = nil then
  Begin
    FGpsResetTimer := TTimer.Create(nil);
    FGpsResetTimer.Enabled := false;
    FGpsResetTimer.OnTimer := OnGpsResetTimer;
  End;
  FGpsResetTimer.Enabled := True;
  FGpsTimeToReset := CGpsResetCount; { by 10 seconds }
end;

function TIsLocationSensor.StartLocation: RNavigateLongLat;
begin
  Result.CreateDec(FStartLocValue.Longitude, FStartLocValue.Latitude);
end;

procedure TIsLocationSensor.StartNav;
Var
  i: Integer;
begin
  if CalNavSensor >= 0 then
    StartSensorByIndex(CalNavSensor)
  else if Length(FSensors) = 1 then
    StartSensorByIndex(0)
  Else
    for i := 0 to High(FSensors) do
      if FNavSensors[i] then
        StartSensorByIndex(i);
  if Assigned(EndAverageMotionSensor) then
    EndAverageMotionSensor.StartAllSensors;
end;

procedure TIsLocationSensor.ResetLocationAverages;
begin
  if Assigned(FOnBeforeAverageReset) then
    FOnBeforeAverageReset(self);

  FRunningLocationSumOfSquares.Create(0.0, 0.0);
  FRunningLocationValue.Create(0.0, 0.0);
  FRunningAltitudeValue := 0.0;
  FRunningAltitudeSumOfSquares := 0.0;
  // FNoOfLocValSamples:=0;            in ResetAverages
  FNoOfConsistentLocSamples := 0;
  FNoOfConsistentAltSamples := 0;
end;

procedure TIsLocationSensor.Restart;
Var
  // i: integer;
  ThisLocation: RNavigateLongLat;
begin
  ResetAverages;
  ThisLocation := CurrentLocation;
  if ThisLocation.IsValid then
  begin
    FStartLocValue.Create(ThisLocation.Latitude, ThisLocation.Longitude);
    FLastLocValue.Create(ThisLocation.Latitude, ThisLocation.Longitude);
  end
  else
  begin
    NullLocation(FStartLocValue);
    NullLocation(FLastLocValue);
  end;
end;

procedure TIsLocationSensor.RestartGps;
// Procedure RestartGps; //if Calculating averages and Gps give no value for 3000 Seconds
Var
  GPSSensor: TCustomLocationSensor;
begin
  // FGpsTimeToReset:=CGpsResetCount;
  if FCalNavSensor < 0 then
    Exit;
  if FDoRunningAveLoc then
  Begin
    GPSSensor := FSensors[FCalNavSensor] as TCustomLocationSensor;
    GPSSensor.Stop;
    Sleep(500);
    GPSSensor.Start;
  End;
end;

function TIsLocationSensor.TextProperties(Index: Integer): string;
Var
  // Sensor: TCustomLocationSensor;
  AvailableProperties: TCustomLocationSensor.TProperties;
  NxtProp: TCustomLocationSensor.TProperty;
begin
  // Sensor := FSensors[index] as TCustomLocationSensor;
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomLocationSensor.TProperty(0)
    to TCustomLocationSensor.TProperty.CountryRegion do
    if NxtProp in AvailableProperties then
      Result := Result + PropText(NxtProp) + '; ';
end;

function TIsLocationSensor.ThisCategory: TSensorCategory;
begin
  Result := TSensorCategory.Location;
end;

function TIsLocationSensor.TrueHeading: Double;
begin
  if Length(FSensors) < 0 then
  begin
    if FAllowNaN then
      Result := NaN
    else
      Result := 0.0;
  end
  else
  begin
    // UpdateSensorValues;
    Result := FLastHeading;
    if not FAllowNaN then
      if isNaN(Result) then
        Result := 0.0;
  end;
end;

class function TIsLocationSensor.ValidLocation
  (ALocation: TLocationCoord2D): Boolean;
Var
  Nav: RNavigateLongLat;
begin
  Try
    Nav.CreateDec(ALocation.Longitude, ALocation.Latitude);
    Result := Nav.IsValid;
  Except
    Result := false;
  End;
end;

{$IFDEF msWindows}
{ TDummyWindowsMotionSensor }

constructor TDummyWindowsMotionSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FRandomTimer := TTimer.Create(nil);
  FRandomTimer.Enabled := false;
  FRandomTimer.Interval := 10000000000;
  FRandomTimer.OnTimer := MotionTimerEvent;
  FLx := 0.01;
  FLy := 0.01;
  FLz := 0.99;
end;

destructor TDummyWindowsMotionSensor.Destroy;
begin
  FRandomTimer.Free;
  inherited;
end;

function TDummyWindowsMotionSensor.DoGetInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result:=  0;
end;

function TDummyWindowsMotionSensor.DoStart: Boolean;
begin
  Inherited;
  Result := Started;
  FRandomTimer.Enabled := True;
end;

procedure TDummyWindowsMotionSensor.DoStop;
begin
  inherited;
  FRandomTimer.Enabled := false;
end;

function TDummyWindowsMotionSensor.GetAvailableProperties
  : TCustomMotionSensor.TProperties;
begin
  Result := [TCustomMotionSensor.TProperty.AccelerationX,
    TCustomMotionSensor.TProperty.AccelerationY,
    TCustomMotionSensor.TProperty.AccelerationZ { ,
    TCustomMotionSensor.TProperty.Speed,
    TCustomMotionSensor.TProperty.AngleAccelX,
    TCustomMotionSensor.TProperty.AngleAccelY,
    TCustomMotionSensor.TProperty.AngleAccelZ } ];
end;

function TDummyWindowsMotionSensor.GetDoubleProperty
  (Prop: TCustomMotionSensor.TProperty): Double;
begin
  Result := fDoubleProps[Ord(Prop)];
end;

function TDummyWindowsMotionSensor.GetMotionSensorType: TMotionSensorType;
begin
  Result := TMotionSensorType.Accelerometer3D;
end;

function TDummyWindowsMotionSensor.GetState: TSensorState;
begin
  Result := TSensorState.Ready;
end;

function TDummyWindowsMotionSensor.GetTimeStamp: TDateTime;
begin
  Result := FDummyMotionTime;
end;

function TDummyWindowsMotionSensor.GetUpdateInterval: Double;
begin
  Result := FRandomTimer.Interval * 1000;
end;

procedure TDummyWindowsMotionSensor.MotionTimerEvent(Sender: TObject);
// Var
// NewValue: TLocationCoord2D;
// Inc: Double;
// LastLoc, ThisLoc: RNavigateLongLat;
// DeltaX, DeltaY, Meters: Double;

begin
  FRandomTimer.Enabled := false;
  try
    if FDummyMotionTime < 5 then
      FDummyMotionTime := Now - 1 / 24 / 60 / 60;
    if FNextChange < 5 then
      FNextChange := Now + 1 / 24 / 60 / 2;
    FThisSampleTime := FDummyMotionTime;
    FDummyMotionTime := Now;
    FThisSampleTime := (FDummyMotionTime - FThisSampleTime) * 24 * 60 * 60;
    // Seconds
    if FThisSampleTime > 20 then
      FThisSampleTime := 1;

    // LastLoc.CreateDec(FLongitude, FLatitude);
    if TestStaticLocation then
    Begin
      FThisSampleTime := 10; // test
      if FNextChange < FDummyMotionTime then
      begin
        FNextChange := Now + 1 / 24 / 60 / 2; // 30 seconds
        FLx := FLx * 1.1;
        FLy := FLy * 1.1;
        FLz := FLz * 0.9;
      end;
    end
    Else
    Begin
      FLx := FLx * 1.1;
      FLy := FLy * 1.1;
      FLz := FLz * 0.9;
    end;
    SetDoubleProperty(FLx, TCustomMotionSensor.TProperty.AccelerationX);
    SetDoubleProperty(FLy, TCustomMotionSensor.TProperty.AccelerationY);
    SetDoubleProperty(FLz, TCustomMotionSensor.TProperty.AccelerationZ);
    // if Assigned(OnDataChanged) then
    // OnDataChanged(Self);
  finally
    FRandomTimer.Interval := 990 + random(20);
    FRandomTimer.Enabled := True;
  end;
end;

procedure TDummyWindowsMotionSensor.SetDoubleProperty(Val: Double;
  Prop: TCustomMotionSensor.TProperty);
begin
  fDoubleProps[Ord(Prop)] := Val;
end;

procedure TDummyWindowsMotionSensor.SetUpdateInterval(AInterval: Double);
begin
  inherited;
  if AInterval < 1000 then // uSecs
    FRandomTimer.Interval := 100000000
  Else
    FRandomTimer.Interval := Round(AInterval / 1000);
end;

{ TDummyWindowsLocationSensor }
constructor TDummyWindowsLocationSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FRandomTimer := TTimer.Create(nil);
  FRandomTimer.Enabled := false;
  FRandomTimer.Interval := 1000;
  FRandomTimer.OnTimer := LocationTimerEvent;
  FLatitude := -37 - 58.6 / 60; // 37 58.6s
  FLongitude := 145 + 5.4 / 60; // 145 05.4  morrabin ndb
  FTrueHeading := 32; // degrees;
  FSpeed := 1;
end;

destructor TDummyWindowsLocationSensor.Destroy;
begin
  FRandomTimer.Free;
  inherited;
end;

function TDummyWindowsLocationSensor.DoGetInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result:=0;
end;

procedure TDummyWindowsLocationSensor.DoLocationChangeType;
begin
  inherited;
end;

procedure TDummyWindowsLocationSensor.DoOptimize;
begin
  inherited;

end;

function TDummyWindowsLocationSensor.DoStart: Boolean;
begin
  Inherited;
  Result := Started;
  FRandomTimer.Enabled := True;
end;

procedure TDummyWindowsLocationSensor.DoStop;
begin
  inherited;
  FRandomTimer.Enabled := false;
end;

function TDummyWindowsLocationSensor.GetAccuracy: TLocationAccuracy;
begin
   Result:=0.3;
end;

function TDummyWindowsLocationSensor.GetAuthorized: TAuthorizationType;
begin
  // dummy
  Result := TAuthorizationType.atAuthorized;
end;

function TDummyWindowsLocationSensor.GetAvailableProperties
  : TCustomLocationSensor.TProperties;
begin
  Result := [TCustomLocationSensor.TProperty.Latitude,
    TCustomLocationSensor.TProperty.Longitude,
    TCustomLocationSensor.TProperty.Altitude,
    TCustomLocationSensor.TProperty.Speed,
    TCustomLocationSensor.TProperty.TrueHeading];
end;

function TDummyWindowsLocationSensor.GetDistance: TLocationDistance;
begin
    Result:=3;
end;

function TDummyWindowsLocationSensor.GetDoubleProperty
  (Prop: TCustomLocationSensor.TProperty): Double;
begin
  Result := fDoubleProps[Ord(Prop)];
end;

function TDummyWindowsLocationSensor.GetLocationSensorType: TLocationSensorType;
begin
  Result := TLocationSensorType.Gps;
end;

function TDummyWindowsLocationSensor.GetPowerConsumption: TPowerConsumption;
begin
  Result:=TPowerConsumption.pcMedium;
end;

function TDummyWindowsLocationSensor.GetState: TSensorState;
begin
  Result := TSensorState.Ready;
end;

function TDummyWindowsLocationSensor.GetTimeStamp: TDateTime;
begin
  Result := FDummyLocationTime;
end;

procedure TDummyWindowsLocationSensor.LocationTimerEvent(Sender: TObject);
Var
  NewValue: TLocationCoord2D;
  Inc: Double;
  LastLoc, ThisLoc: RNavigateLongLat;
  Meters, LDist: Double;

begin
  FRandomTimer.Enabled := false;
  try
    if FDummyLocationTime < 5 then
      FDummyLocationTime := Now - 1 / 24 / 60 / 60;
    FThisSampleTime := FDummyLocationTime;
    FDummyLocationTime := Now;
    FThisSampleTime := (FDummyLocationTime - FThisSampleTime) * 24 * 60 * 60;
    // Seconds
    // if FThisSampleTime > 20 then
    // FThisSampleTime := 1;

    LastLoc.CreateDec(FLongitude, FLatitude);
    if TestStaticLocation then
    Begin
      // FThisSampleTime := 10; // test
      FLatitude := -37 - 58.592 / 60; // 37 58.6s
      FLongitude := 145 + 5.4 / 60; // 145 05.4  morrabin ndb
      FLatitude := FLatitude + OneMeterAsDegrees - random * 2 *
        OneMeterAsDegrees;
      FLongitude := FLongitude + OneMeterAsDegrees - random * 2 *
        OneMeterAsDegrees;
      ThisLoc.CreateDec(FLongitude, FLatitude);
      Meters := ThisLoc.MetresFrom(LastLoc);
      if Meters > 2 then
        // Meters:=1;
    end
    else if FSpeed > 0.001 then
    Begin
      // FTrueHeading := FTrueHeading + 2;
      Meters := FSpeed * FThisSampleTime;
      ThisLoc := LastLoc.LocationAt(Meters, FTrueHeading);
      FLatitude := ThisLoc.Latitude;
      FLongitude := ThisLoc.Longitude;
    End
    else
    Begin
      FSpeed := 4 / FThisSampleTime;
      ThisLoc := LastLoc.LocationAt(40, 30);
      FLatitude := ThisLoc.Latitude;
      FLongitude := ThisLoc.Longitude;
    end;

    ThisLoc.CreateDec(FLongitude, FLatitude);
    LDist := ThisLoc.MetresFrom(LastLoc);
    FSpeed := LDist / FThisSampleTime;
    SetDoubleProperty(FSpeed, TCustomLocationSensor.TProperty.Speed);
    FTrueHeading := ThisLoc.HeadingFrom(LastLoc);
    SetDoubleProperty(FTrueHeading,
      TCustomLocationSensor.TProperty.TrueHeading);

    NewValue := TLocationCoord2D.Create(FLatitude, FLongitude);
    SetDoubleProperty(NewValue.Latitude,
      TCustomLocationSensor.TProperty.Latitude);
    SetDoubleProperty(NewValue.Longitude,
      TCustomLocationSensor.TProperty.Longitude);

    Inc := 0.5 - random;
    SetDoubleProperty(5.25 + Inc, TCustomLocationSensor.TProperty.Altitude);
    SetDoubleProperty(TrueHeading, TCustomLocationSensor.TProperty.TrueHeading);
    if Assigned(OnLocationChanged) then
      OnLocationChanged(self, FLastValue, NewValue);
    if Assigned(OnHeadingChanged) then
      OnHeadingChanged(self,
        THeading(GetDoubleProperty(TCustomLocationSensor.TProperty.
        TrueHeading)));
    FLastValue := NewValue;
  finally
    Sleep(random(2000));
    FRandomTimer.Interval := 1000;
    // Sample time delivered by Dummy Time  //990 + random(20);
    FRandomTimer.Enabled := True;
  end;
end;
(*

  {$IFDEF msWindows}

  begin
  Last.CreateDec(FPrevQueryLocValue.Longitude, FPrevQueryLocValue.Latitude);
  Cur.CreateDec(FLastLocValue.Longitude, FLastLocValue.Latitude);
  DMtrs := Cur.MetresFrom(Last);
  Result := DMtrs * 1000 / FTimer.Interval;
  end
  {$ELSE}
  {$ENDIF}
  {$IFDEF msWindows}

  begin
  Last.CreateDec(FPrevQueryLocValue.Longitude, FPrevQueryLocValue.Latitude);
  Cur.CreateDec(FLastLocValue.Longitude, FLastLocValue.Latitude);
  DMtrs := Cur.MetresFrom(Last);
  Result := DMtrs * 1000 / FTimer.Interval;
  end
  {$ELSE}
  {$ENDIF}
  {$IFDEF msWindows }
  if Length(FSensors) < 1 then
  Begin
  FAccelerationX := 0.0; // (g)
  FAccelerationY := 0.0;
  FAccelerationZ := 1.0;
  FAngleAccelX := 0.0;
  FAngleAccelY := 0.0;
  FAngleAccelZ := 0.0;
  FSpeed := 0.0;
  End;
  {$ENDIF }

  {$IFDEF msWindows }
  if Length(FSensors) < 1 then
  Begin
  FTiltX := 0.0;
  FTiltY := 1.0;
  FTiltZ := 0.0;
  FDistanceX := 0.0;
  FDistanceY := 0.0;
  FDistanceZ := 0.0;
  FHeadingX := 0.0;
  FHeadingY := 0.0;
  FHeadingZ := 0.0;
  FMagHeading := 0.0;
  FCompMagHeading := 0.0;
  FCompTrueHeading := 0.0;
  End;
  {$ENDIF }


  end;
*)

procedure TDummyWindowsLocationSensor.SetAccuracy(
  const Value: TLocationAccuracy);
begin
  inherited;

end;

procedure TDummyWindowsLocationSensor.SetDistance(
  const Value: TLocationDistance);
begin
  inherited;

end;

procedure TDummyWindowsLocationSensor.SetDoubleProperty(Val: Double;
  Prop: TCustomLocationSensor.TProperty);
begin
  fDoubleProps[Ord(Prop)] := Val;
end;
{$ENDIF}
{ TIsMotionSensor }

procedure TIsMotionSensor.ConfirmSensors;
Var
  i: Integer;
begin
  inherited;
  if Length(FAvailableProperties) < 1 then
    Exit; // inherited create
  for i := 0 to High(FSensors) do
    if FSensorTypes[i] <> TCustomMotionSensor(FSensors[i]).SensorType then
      raise Exception.Create
        ('TIsMotionSensor.ConfirmSensors >> Order was not preserved');
  for i := 0 to High(FSensors) do
    SetSensorInterval(FSensors[i]);
end;

constructor TIsMotionSensor.Create;
Var
  i: Integer;
begin
  inherited;
  SetLength(FAvailableProperties, Length(FSensors));
  SetLength(FSensorTypes, Length(FSensors));
  for i := 0 to High(FSensors) do
  begin
    FSensorTypes[i] := TCustomMotionSensor(FSensors[i]).SensorType;
    FAvailableProperties[i] := TCustomMotionSensor(FSensors[i])
      .AvailableProperties;
  end;
end;

Class function TIsMotionSensor.DecodeType(ASensor: TCustomSensor): String;
begin
  if ASensor is TCustomMotionSensor then
    case Ord(TCustomMotionSensor(ASensor).SensorType) of
      0:
        Result := 'Accelerometer1D';
      1:
        Result := 'Accelerometer2D';
      2:
        Result := 'Accelerometer3D';
      3:
        Result := 'MotionDetector';
      4:
        Result := 'Gyrometer1D';
      5:
        Result := 'Gyrometer2D';
      6:
        Result := 'Gyrometer3D';
      7:
        Result := 'Speedometer';
      8:
        Result := 'LinearAccelerometer3D';
      9:
        Result := 'GravityAccelerometer3D';
    else
      Result := 'Error';
    end
  else
    Result := 'Error';
end;

procedure TIsMotionSensor.GetValues(Index: Integer);
Var
  Sensor: TCustomMotionSensor;
  AvailableProperties: TCustomMotionSensor.TProperties;
  NxtProp: TCustomMotionSensor.TProperty;
  NewValueAcc, NewValueAngle, CompareEps: RISSensorVector;
begin
  Inherited;
  Sensor := FSensors[index] as TCustomMotionSensor;
  NewValueAcc.SetZero;
  NewValueAngle.SetZero;
  CompareEps.Create(0.1, 0.1, 0.1); // Allows slight vibration
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomMotionSensor.TProperty(0)
    to TCustomMotionSensor.TProperty.Speed do
    if NxtProp in AvailableProperties then
      case Ord(NxtProp) of
        0:
          NewValueAcc.x := Sensor.AccelerationX; // 'AccelerationX';
        1:
          NewValueAcc.y := Sensor.AccelerationY; // 'AccelerationY';
        2:
          NewValueAcc.z := Sensor.AccelerationZ; // 'AccelerationZ';
        3:
          NewValueAngle.x := Sensor.AngleAccelX; // 'AngleAccelX';
        4:
          NewValueAngle.y := Sensor.AngleAccelY; // 'AngleAccelY';
        5:
          NewValueAngle.z := Sensor.AngleAccelX; // 'AngleAccelZ';
        // 6:FMotion:= Sensor.Motion;       //'Motion';
        7:
          FSpeed := Sensor.Speed; // 'Speed';
      end;
  FChangedAngular := false;
  FChangedLinear := false;
  if NewValueAcc.IsValid then
    if NewValueAcc.SameAs(FCurrentValueLinear, CompareEps) then
      FCurrentValueLinear := NewValueAcc
    Else
    Begin
      FChangedLinear := FLastValueLinear.IsValid;
      FLastValueLinear := FCurrentValueLinear;
      FCurrentValueLinear := NewValueAcc;

    End;
  if NewValueAngle.IsValid then
    if NewValueAngle.SameAs(FCurrentValueAngle, CompareEps) then
      FCurrentValueLinear := NewValueAngle
    Else
    Begin
      FChangedAngular := FLastValueAngle.IsValid;
      FLastValueAngle := FCurrentValueAngle;
      FCurrentValueAngle := NewValueAngle;
    End;

  if Assigned(FOnChange) then
    FOnChange(self);
  // GetValues called by main sensor change
end;

function TIsMotionSensor.PropText(i: Integer): String;
begin
  case i of
    0:
      Result := 'AccelerationX'; // AccelerationZ, AngleAccelX, Motion, Speed
    1:
      Result := 'AccelerationY';
    2:
      Result := 'AccelerationZ';
    3:
      Result := 'AngleAccelX';
    4:
      Result := 'AngleAccelY';
    5:
      Result := 'AngleAccelZ';
    6:
      Result := 'Motion';
    7:
      Result := 'Speed';
  end;
end;

procedure TIsMotionSensor.SetOnChange(const Value: TISMotionChangedEvent);
begin
  FOnChange := TNotifyEvent(Value);
end;

function TIsMotionSensor.TextProperties(Index: Integer): string;
Var
  // Sensor: TCustomMotionSensor;
  AvailableProperties: TCustomMotionSensor.TProperties;
  NxtProp: TCustomMotionSensor.TProperty;
begin
  // Sensor := FSensors[index] as TCustomMotionSensor;
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomMotionSensor.TProperty(0)
    to TCustomMotionSensor.TProperty.Speed do
    if NxtProp in AvailableProperties then
      Result := Result + PropText(Ord(NxtProp)) + '; ';
end;

function TIsMotionSensor.ThisCategory: TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

{ TIsSensorManager }
Var
  GlobalManager: TIsSensorManager = nil;

procedure TIsSensorManager.ActivateAllSensors(AReActivate: Boolean);
Var
  SensorManager: TSensorManager;
  Sensor: TCustomSensor;
  ThisISSensorClass: TIsTimedEventSensorClass;
  i: Integer;
begin
  if GlobalManager <> self then
    Exit;

  FTextList := '';
  SensorManager := TSensorManager.Current;
  if SensorManager = nil then
    Exit;

  if AReActivate and SensorManager.Active then
  Begin
    SensorManager.Deactivate;
    Sleep(2000);
  End;

  if not SensorManager.Active then
    SensorManager.Activate;

  FTextList := '';
  SensorManager := TSensorManager.Current;
  if SensorManager = nil then
    Exit;

  if not SensorManager.Active then
    SensorManager.Activate;
  for i := 0 to SensorManager.count - 1 do
  begin
    Sensor := SensorManager.Sensors[i];
    if Sensor <> nil then
    begin
      FTextList := FTextList + SensorCatText(Sensor.Category) + '::' +
        Sensor.Description + #13#10;
      ThisISSensorClass := GetIsTimedEventSensor(Sensor.Category);
      if ThisISSensorClass <> nil then
        FTextList := FTextList + ThisISSensorClass.ClassTextDetails(Sensor,
          '    ') + #13#10;
    end;
  end;
  if FTextList = '' then
    FTextList := 'NoSensors';

  if (FSensorList <> nil) then
    for i := 0 to FSensorList.count - 1 do
      if FSensorList.Objects[i] is TIsTimedEventSensor then
        Try
          TIsTimedEventSensor(FSensorList.Objects[i]).RefreshSensors;
        Except
          On e: Exception do
            FTextList := FTextList + IntToStr(i) + '/Exception::' +
              e.message + #13#10;
        End;
end;

function TIsSensorManager.AddIsSensor(ASensor: TIsTimedEventSensor): Integer;
begin
  Result := SensorRegistered(ASensor);
  if ASensor = nil then
    Exit;
  If Result < 0 then
    FSensorList.AddObject(ASensor.TextSensorTypes, ASensor);
  Result := SensorRegistered(ASensor);
end;

constructor TIsSensorManager.Create;
begin
  Try
    inherited;
    if GlobalManager <> nil then
      if GlobalManager <> self then
        FreeAndNil(GlobalManager);

    GlobalManager := self;
    ActivateAllSensors(false);
  Except
    GlobalManager := nil;
  End;
end;

class function TIsSensorManager.CurrentIsSensorManager: TIsSensorManager;
begin
  if GlobalManager = nil then
    TIsSensorManager.Create;
  if GlobalManager = nil then
    raise Exception.Create('Error CurrentIsSensorManager No GlobalManager');
  Result := GlobalManager;
end;

destructor TIsSensorManager.Destroy;
begin
  if Self = GlobalManager then
    GlobalManager := nil;
  FSensorList.Free;
  inherited;
end;

procedure TIsSensorManager.DropIsSensor(ASensor: TIsTimedEventSensor);
Var
  SensorManager: TSensorManager;
  i: Integer;
begin
  if ASensor = nil then
    Exit;
  i := SensorRegistered(ASensor);
  If i >= 0 then
    FSensorList.Delete(i);
  if FSensorList.count < 1 then
  Begin
    SensorManager := TSensorManager.Current;
    if SensorManager <> nil then
      if SensorManager.Active then
        SensorManager.Active := false;
  End;

end;

function TIsSensorManager.GetIsTimedEventSensor(ACat: TSensorCategory)
  : TIsTimedEventSensorClass;
begin
  Result := nil;
  case ACat of
    TSensorCategory.Location:
      Result := TIsLocationSensor;
    TSensorCategory.Environmental:
      Result := nil;
    TSensorCategory.Motion:
      Result := TIsMotionSensor;
    TSensorCategory.Orientation:
      Result := TIsOrientationSensor;
    TSensorCategory.Mechanical:
      Result := nil;
    TSensorCategory.Electrical:
      Result := nil;
    TSensorCategory.Biometric:
      Result := nil;
    TSensorCategory.Light:
      Result := nil;
    TSensorCategory.Scanner:
      Result := nil;
  end;
end;

class function TIsSensorManager.SensorCatText(ACat: TSensorCategory): String;
begin
  case ACat of
    TSensorCategory.Location:
      Result := 'Location';
    TSensorCategory.Environmental:
      Result := 'Environmental';
    TSensorCategory.Motion:
      Result := 'Motion';
    TSensorCategory.Orientation:
      Result := 'Orientation';
    TSensorCategory.Mechanical:
      Result := 'Mechanical';
    TSensorCategory.Electrical:
      Result := 'Electrical';
    TSensorCategory.Biometric:
      Result := 'Biometric';
    TSensorCategory.Light:
      Result := 'Light';
    TSensorCategory.Scanner:
      Result := 'Scanner';
  else
    Result := 'Error';
  end
end;

function TIsSensorManager.SensorRegistered
  (ASensor: TIsTimedEventSensor): Integer;
Var
  i,SensorCnt: Integer;
begin
  Result := -1;
  if ASensor = nil then
    Exit;
  if FSensorList = nil then
    FSensorList := TStringList.Create;
  SensorCnt:= FSensorList.count;
  i := 0;
  while (Result < 0) and (i<SensorCnt) do
  begin
    if FSensorList.Objects[i] = ASensor then
      Result := i
    else
      Inc(i);
  end;
end;

{ TIsTimedEventSensor }

function TIsTimedEventSensor.AllTextProperties: string;
Var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FSensors) do
    Result := Result + TextProperties(i) + #13#10;
end;

constructor TIsTimedEventSensor.Create;
begin
  inherited;
  FEventTimer := TTimer.Create(nil);
  FEventTimer.Enabled := false;
  FEventTimer.OnTimer := TimerEvent;
  RefreshSensors;
end;

class function TIsTimedEventSensor.DecodeType(ASensor: TCustomSensor): String;
begin
  Result := 'Error Base Class TIsTimedEventSensor';
end;

destructor TIsTimedEventSensor.Destroy;
Var
  i: Integer;
begin
  FEventTimer.Free;
  For i := 0 to high(FSensors) do
    FSensors[i].Free;
  if FIsSensorManager <> nil then
    FIsSensorManager.DropIsSensor(self);
  inherited;
end;

procedure TIsTimedEventSensor.DropSensorByIndex(Index: Integer);
begin
  If Index < 0 then
    Exit;
  if Index <= High(FSensors) then
  begin
    if FStartTimes[Index] > 1 then
      StopSensorByIndex(Index);
    FSensors[Index] := nil;
  end;
end;

procedure TIsTimedEventSensor.DropSensorByTextType(ADrop: String);
Var
  i: Integer;
begin
  for i := 0 to High(FSensors) do
    if DecodeType(FSensors[i]) = ADrop then
      DropSensorByIndex(i);
end;

procedure TIsTimedEventSensor.FilterProperties;
begin
  // Null  Populate to choose which properties to populate with which sensor
end;

function TIsTimedEventSensor.GetStarted: Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to High(FSensors) do
    if FStartTimes[i] > 5 then
      Result := True;
end;

procedure TIsTimedEventSensor.GetValues(Index: Integer);
Var
  LocalPrevSensorTimes, LSensorTime, LSampleTime: TDateTime;
  // s: String;
{$IFDEF ANDROID}
  // LAndriodStart,
  LAndriodNowDelta: Int64;
  Val: TAndroideTime;
{$ENDIF}
begin
  if Index < 0 then
    Exit;
  if Index <= High(FSensors) then
    if FSensors[Index] <> nil then
      if FStartTimes[Index] > 1 then
      Begin
        LocalPrevSensorTimes := FSensorStampTimes[Index];
{$IFDEF ANDROID}
{$IFDEF VER320}
        // Value of tickcount for Tokyo
{$ENDIF}
{$IFDEF VER330}
        // need to check Value of tickcount
        // for Rio
{$ENDIF}
{$IFDEF VER340}
        // need to check Value of tickcount
        // for Sydney
{$ENDIF}
        Val.DateTimeVal := FSensors[Index].TimeStamp;
        if FStartAndroidTimes[index] < 1 then
        // is first data from this sensor
        begin
          FSampleSeconds := 0;
          FLastAndroidTimes[Index] := Val.ValInt64;
          FStartAndroidTimes[index] := Val.ValInt64;
          LSensorTime := Now;
          FStartTimes[index] := LSensorTime;
          FSensorStampTimes[Index] := LSensorTime;
        end
        Else
        Begin
          LAndriodNowDelta := Val.ValInt64 - FLastAndroidTimes[Index];
          FLastAndroidTimes[Index] := Val.ValInt64;
          FSampleSeconds := (LAndriodNowDelta / 1000000);
          // LStartTime := FStartTimes[index];
          // LAndriodStart := FStartAndroidTimes[Index];
          // LAndriodNowDelta:= Val.ValInt64 - LAndriodStart;
          LSensorTime := Val.DelphiDateTime(FStartAndroidTimes[index],
            FStartTimes[index]);
          FSensorStampTimes[Index] := LSensorTime;
        End;
{$ELSE}
        LSensorTime := FSensors[Index].TimeStamp;
        FSensorStampTimes[Index] := LSensorTime;
        if LocalPrevSensorTimes > 5 then
          FSampleSeconds := ((LSensorTime - LocalPrevSensorTimes) * 24
            * 60 * 60)
        Else
        Begin
          FSampleSeconds := 0;
        End;
{$ENDIF}
        FNewSample := Not SameValue(LSensorTime, LocalPrevSensorTimes,
          1 / 24 / 60 / 60 / 1000);
        // s := FormatDateTime('yy/mm/dd hh:nn:ss.zzz', LSensorTime);
        if FNewSample then
        Begin
          // s := FormatDateTime('yy/mm/dd hh:nn:ss.zzz', LocalPrevSensorTimes);
          if LocalPrevSensorTimes > 1 then
          Begin
            LSampleTime := LocalPrevSensorTimes;
            LSampleTime := FSensorStampTimes[Index] - LSampleTime;

            FCurrentSampleTime[Index] := LSampleTime;
          End;
        End;
        // not affected by reset
        if FSampleSeconds > 0.0 then
        begin
          Inc(FNoOfTimeSamples);
          CalNewDoubleAverageAndSumOfSquares(FSampleSeconds,
            FRunningAverageSampleTime, FRunningSumOfSquaresSampleTime,
            FNoOfTimeSamples);
        end;
      End;

end;

function TIsTimedEventSensor.IndexOfSensor(ASensor: TObject): Integer;
begin
  Result := -1;
  if ASensor is TCustomSensor then
    Result := IndexInArray(TArrayofObjects(FSensors), ASensor);
{$IFDEF msWindows}
  if Result < 0 then
    Result := IndexInArray(TArrayofObjects(FSensors), ASensor);
{$ENDIF}
end;

procedure TIsTimedEventSensor.RefreshSensors;
var
  SensorManager: TSensorManager;
  i: Integer;
  // Assumes order remains unchanged

begin
  SensorManager := TSensorManager.Current;
  if SensorManager = nil then
    Exit;

  if not SensorManager.Active then
   try
    SensorManager.Activate;
   Except
    On E:exception do
      Begin
        FLastError:=FLastError+#13#10 + E.Message;
      End;
   end;
  FIsSensorManager := TIsSensorManager.CurrentIsSensorManager;
  if FIsSensorManager=GlobalManager then
      FIsSensorManager.AddIsSensor(self);
  FSensors := SensorManager.GetSensorsByCategory(ThisCategory);
{$IFDEF msWindows}
  if Length(FSensors) < 1 then
    case ThisCategory of
      TSensorCategory.Location:
        Begin
          SetLength(FSensors, 1);
          FSensors[0] := TDummyWindowsLocationSensor.Create(nil);
        End;
      TSensorCategory.Motion:
        Begin
          SetLength(FSensors, 1);
          FSensors[0] := TDummyWindowsMotionSensor.Create(nil);
        End;
    end;
{$ENDIF}
  for i := 0 to High(FSensors) do
    FSensors[i].OnDataChanged := SensorDataChange;
  SetLength(FStartTimes, Length(FSensors));
  SetLength(FSensorStampTimes, Length(FSensors));
  SetLength(FCurrentSampleTime, Length(FSensors));
{$IFDEF ANDROID}
  SetLength(FStartAndroidTimes, Length(FSensors));
  SetLength(FLastAndroidTimes, Length(FSensors));
{$ENDIF}
  ConfirmSensors;
end;

function TIsTimedEventSensor.SampleTimes: string;
begin
  if FRunningAverageSampleTime < 100 then
  Begin
    Result := 'Sampling ' + FormatFloat('0.000', FRunningAverageSampleTime)
      + ' Secs';
    If FRunningSumOfSquaresSampleTime > 0.0 then
      Result := Result + '  Jitter: ' + FormatFloat('0.000',
        CalDoubleStdDevFromSumOfSquares(FRunningSumOfSquaresSampleTime,
        FNoOfTimeSamples));
  End
  Else
  Begin
    Result := 'Sampling ' + FormatFloat('0.000', FRunningAverageSampleTime / 60)
      + ' Mins';
    If FRunningSumOfSquaresSampleTime > 0.0 then
      Result := Result + '  Jitter: ' + FormatFloat('0.000',
        CalDoubleStdDevFromSumOfSquares(FRunningSumOfSquaresSampleTime,
        FNoOfTimeSamples) / 60);
  End;
end;

procedure TIsTimedEventSensor.SensorDataChange(Sender: TObject);
Var
  SensorIndex: Integer;
begin
  SensorIndex := IndexOfSensor(Sender);
  GetValues(SensorIndex);
end;

procedure TIsTimedEventSensor.SetSensorInterval(ASensor: TCustomSensor);
begin
  if ThisCategory in [TSensorCategory.Motion, TSensorCategory.Orientation] then
  Begin
    if ASensor.Started then
      ASensor.Stop;
    if ThisCategory = TSensorCategory.Motion then
      TCustomMotionSensor(ASensor).UpdateInterval := FUpdateInterval
    else if ThisCategory = TSensorCategory.Orientation then
      TCustomOrientationSensor(ASensor).UpdateInterval := FUpdateInterval;
    { Mechanical, Electrical, Biometric, Light, Scanner }
    ASensor.Start;
  End;
end;

procedure TIsTimedEventSensor.SetStarted(const Value: Boolean);
var
  i: Integer;
begin
  if Value then
  Begin
    If not GetStarted then
      StartSensorByIndex(0)
  End
  else
    for i := 0 to High(FSensors) do
      StopSensorByIndex(i);
end;

procedure TIsTimedEventSensor.SetUpdateInterval(const Value: Double);
var
  i: Integer;
begin
  if ThisCategory in [ { Location used internally
    ,Environmental, } TSensorCategory.Motion,
    TSensorCategory.
    Orientation { Mechanical, Electrical, Biometric, Light, Scanner } ] then
  Begin
    if FEventTimer = nil then
      FEventTimer := TTimer.Create(nil);

    FEventTimer.Enabled := false;
    if Value <> FUpdateInterval then
      Try
        FUpdateInterval := Value;
        FTimerInterval := Round(FUpdateInterval / 1000); // Seconds
        FEventTimer.Enabled := false;
        if FTimerInterval > 1000 then
          FEventTimer.Interval := FTimerInterval div 10
        else
          FEventTimer.Interval := 100; // ms delayed start up
        for i := 0 to High(FSensors) do
          SetSensorInterval(FSensors[i]);
      Finally
        FEventTimer.Enabled := True;
      End;
  End
  Else
    raise Exception.Create('Error Message:UpdateInterval not supported');
end;

procedure TIsTimedEventSensor.StartAllSensors;
Var
  i: Integer;
begin
  for i := 0 to High(FSensors) do
    StartSensorByIndex(i);
end;

procedure TIsTimedEventSensor.StartSensorByIndex(Index: Integer);
Var
  ThisSensor: TCustomSensor;
begin
  Try
    if Index < 0 then
      Exit;
    if Index <= High(FSensors) then
      if FSensors[Index] <> nil then
      Begin
        ThisSensor := FSensors[Index];
        if FStartTimes[Index] < 1 then
          FStartTimes[Index] := Now;
        if Not Assigned(ThisSensor.OnDataChanged) then
          ThisSensor.OnDataChanged := SensorDataChange;
        if Not ThisSensor.Started then
          ThisSensor.Start;
      End;
  Except
    On e: Exception do
      FLastError := FLastError + 'TIsTimedEventSensor.StartSensorByIndex' +
        e.message + #10#13;
  End;
end;

procedure TIsTimedEventSensor.StartSensorByTextType(AStartString: String);
Var
  i: Integer;
begin
  for i := 0 to High(FSensors) do
    if DecodeType(FSensors[i]) = AStartString then
      StartSensorByIndex(i);
end;

procedure TIsTimedEventSensor.StopAllSensors;
Var
  i: Integer;
begin
  for i := 0 to High(FSensors) do
    StopSensorByIndex(i);
end;

procedure TIsTimedEventSensor.StopSensorByIndex(Index: Integer);
begin
  If Index < 0 then
    Exit;
  if Index <= High(FSensors) then
  begin
    if FStartTimes[Index] > 1 then
      FStartTimes[Index] := 0.0;
    if FSensors[Index] <> nil then
      FSensors[Index].Stop;
  end;
end;

class function TIsTimedEventSensor.ClassTextDetails(ASensor: TCustomSensor;
  const APreamble: string): String;
begin
  Result := APreamble + DecodeType(ASensor);
end;

procedure TIsTimedEventSensor.ConfirmSensors;
// Var
// i:integer;
begin
  // if ThisCategory in [TSensorCategory.Motion, TSensorCategory.Orientation] then
  // for i := 0 to High(FSensors) do
  // SetSensorInterval(FSensors[i]);
end;

function TIsTimedEventSensor.TextSensorTypes: String;
Var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FSensors) do
    Result := Result + DecodeType(FSensors[i]) + #13#10;
end;

procedure TIsTimedEventSensor.TimerEvent(Sender: TObject);
var
  i: Integer;
begin
  FEventTimer.Enabled := false;
  try
    if FTimerInterval <> FEventTimer.Interval then
      FEventTimer.Interval := FTimerInterval;
    for i := 0 to High(FSensors) do
      if FSensors[i] <> nil then // we can remove sensors
        GetValues(i);
    // DoCalculatedValues;
  finally
    FEventTimer.Enabled := True;
  end;
end;

{ TIsOrientationSensor }

procedure TIsOrientationSensor.ConfirmSensors;
Var
  i: Integer;
begin
  inherited;
  if Length(FAvailableProperties) < 1 then
    Exit; // inherited create
  for i := 0 to High(FSensors) do
    if FSensorTypes[i] <> TCustomOrientationSensor(FSensors[i]).SensorType then
      raise Exception.Create
        ('TIsOrientationSensor.ConfirmSensors >> Order was not preserved');
  for i := 0 to High(FSensors) do
    SetSensorInterval(FSensors[i]);
end;

constructor TIsOrientationSensor.Create;
Var
  i: Integer;
begin
  inherited;
  SetLength(FAvailableProperties, Length(FSensors));
  SetLength(FSensorTypes, Length(FSensors));
  for i := 0 to High(FSensors) do
  begin
    FSensorTypes[i] := TCustomOrientationSensor(FSensors[i]).SensorType;
    FAvailableProperties[i] := TCustomOrientationSensor(FSensors[i])
      .AvailableProperties;
  end;
end;

Class function TIsOrientationSensor.DecodeType(ASensor: TCustomSensor): String;
Var
  Stype: TOrientationSensorType;

begin
  Stype := TCustomOrientationSensor(ASensor).SensorType;
  case Stype of
    TOrientationSensorType.Compass1D:
      Result := 'Compass1D';
    TOrientationSensorType.Compass2D:
      Result := 'Compass2D';
    TOrientationSensorType.Compass3D:
      Result := 'Compass3D';
    TOrientationSensorType.Inclinometer1D:
      Result := 'Inclinometer1D';
    TOrientationSensorType.Inclinometer2D:
      Result := 'Inclinometer2D';
    TOrientationSensorType.Inclinometer3D:
      Result := 'Inclinometer3D';
    TOrientationSensorType.Distance1D:
      Result := 'Distance1D';
    TOrientationSensorType.Distance2D:
      Result := 'Distance2D';
    TOrientationSensorType.Distance3D:
      Result := 'Distance3D';
  else
    Result := 'Error';
  end;
end;

procedure TIsOrientationSensor.GetValues(Index: Integer);
Var
  Sensor: TCustomOrientationSensor;
  AvailableProperties: TCustomOrientationSensor.TProperties;
  NxtProp: TCustomOrientationSensor.TProperty;
  NewTilt, NewDistance, NewHeading: RISSensorVector;
begin
  Inherited;
  Sensor := FSensors[index] as TCustomOrientationSensor;
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomOrientationSensor.TProperty(0)
    to TCustomOrientationSensor.TProperty.CompTrueHeading do
    if NxtProp in AvailableProperties then
      case NxtProp of
        TCustomOrientationSensor.TProperty.TiltX:
          NewTilt.x := Sensor.TiltX;
        TCustomOrientationSensor.TProperty.TiltY:
          NewTilt.y := Sensor.TiltY;
        TCustomOrientationSensor.TProperty.TiltZ:
          NewTilt.z := Sensor.TiltZ;
        TCustomOrientationSensor.TProperty.DistanceX:
          NewDistance.x := Sensor.DistanceX;
        TCustomOrientationSensor.TProperty.DistanceY:
          NewDistance.y := Sensor.DistanceY;
        TCustomOrientationSensor.TProperty.DistanceZ:
          NewDistance.z := Sensor.DistanceZ;
        TCustomOrientationSensor.TProperty.HeadingX:
          NewHeading.x := Sensor.HeadingX;
        TCustomOrientationSensor.TProperty.HeadingY:
          NewHeading.y := Sensor.HeadingY;
        TCustomOrientationSensor.TProperty.HeadingZ:
          NewHeading.z := Sensor.HeadingZ;
        TCustomOrientationSensor.TProperty.MagHeading:
          FMagHeading := Sensor.MagHeading;
        TCustomOrientationSensor.TProperty.TrueHeading:
          FTrueHeading := Sensor.TrueHeading;
        TCustomOrientationSensor.TProperty.CompMagHeading:
          FCompMagHeading := Sensor.CompMagHeading;
        TCustomOrientationSensor.TProperty.CompTrueHeading:
          FCompTrueHeading := Sensor.CompTrueHeading;
      end;

  FLastTilt := FCurrentTilt;
  FLastDistance := FCurrentDistance;
  FCurrentHeading := FLastHeading;

  FCurrentTilt := NewTilt;
  FCurrentDistance := NewDistance;
  FCurrentHeading := NewHeading;

  { FChangedAngular:=false;
    FChangedLinear:=false;
    if NewValueAcc.IsValid then
    if NewValueAcc.SameAs(FCurrentValueLinear) then
    FCurrentValueLinear := NewValueAcc
    Else
    Begin
    FLastValueLinear := FCurrentValueLinear;
    FCurrentValueLinear := NewValueAcc;

    End;
    if NewValueAngle.IsValid then
    if NewValueAngle.SameAs(FCurrentValueAngle) then
    FCurrentValueLinear := NewValueAngle
    Else
    Begin
    FLastValueAngle := FCurrentValueAngle;
    FCurrentValueAngle := NewValueAngle;
    End;

  }

  if Assigned(FOnChange) then
    FOnChange(self);
  // GetValues called by main sensor change
end;

function TIsOrientationSensor.PropText
  (AProp: TCustomOrientationSensor.TProperty): String;
begin
  case AProp of
    TCustomOrientationSensor.TProperty.TiltX:
      Result := 'TiltX';
    TCustomOrientationSensor.TProperty.TiltY:
      Result := 'TiltY';
    TCustomOrientationSensor.TProperty.TiltZ:
      Result := 'TiltZ';
    TCustomOrientationSensor.TProperty.DistanceX:
      Result := 'DistanceX';
    TCustomOrientationSensor.TProperty.DistanceY:
      Result := 'DistanceY';
    TCustomOrientationSensor.TProperty.DistanceZ:
      Result := 'DistanceZ';
    TCustomOrientationSensor.TProperty.HeadingX:
      Result := 'HeadingX';
    TCustomOrientationSensor.TProperty.HeadingY:
      Result := 'HeadingY';
    TCustomOrientationSensor.TProperty.HeadingZ:
      Result := 'HeadingZ';
    TCustomOrientationSensor.TProperty.MagHeading:
      Result := 'MagHeading';
    TCustomOrientationSensor.TProperty.TrueHeading:
      Result := 'TrueHeading';
    TCustomOrientationSensor.TProperty.CompMagHeading:
      Result := 'CompMagHeading';
    TCustomOrientationSensor.TProperty.CompTrueHeading:
      Result := 'CompTrueHeading';
  end;
end;

procedure TIsOrientationSensor.SetOnChange(Value: TISOrientationChangedEvent);
begin
  FOnChange := TNotifyEvent(Value);
end;

function TIsOrientationSensor.TextProperties(Index: Integer): string;
Var
  // Sensor: TCustomOrientationSensor;
  AvailableProperties: TCustomOrientationSensor.TProperties;
  NxtProp: TCustomOrientationSensor.TProperty;
begin
  // Sensor := FSensors[index] as TCustomOrientationSensor;
  AvailableProperties := FAvailableProperties[Index];
  for NxtProp := TCustomOrientationSensor.TProperty(0)
    to TCustomOrientationSensor.TProperty.CompTrueHeading do
    if NxtProp in AvailableProperties then
      Result := Result + PropText(NxtProp) + '; ';
end;

function TIsOrientationSensor.ThisCategory: TSensorCategory;
begin
  Result := TSensorCategory.Orientation;
end;

{ RISSensorVector }

procedure RISSensorVector.Create(Ax, Ay, Az: Double);
begin
  Try
    if isNaN(Az) then
      TwoDOnly := True
    else
     begin
      TwoDOnly := False;
      z := Az;
     end;
    if isNaN(Ax) then
      raise Exception.Create('X Value is Nan');
    x := Ax;
    if isNaN(Ay) then
      raise Exception.Create('Y Value is Nan');
    y := Ay;
  Except
    On e: Exception Do
      raise Exception.Create('RISSensorVector.Create Error::' + e.message);
  End;
end;

function RISSensorVector.IsValid: Boolean;
begin
  // Result := (x <> NaN) And (y <> NaN);
  Result := Not isNaN(x);
  if Result then
    Result := Not isNaN(y);
  if Result then
    TwoDOnly := isNaN(z);
end;

function RISSensorVector.SameAs(ATst: RISSensorVector;
  AEpsilonVector: RISSensorVector): Boolean;

begin
  Result := IsValid and ATst.IsValid;
  if Result then
    Result := SameValue(x, ATst.x, AEpsilonVector.x);
  if Result then
    Result := SameValue(y, ATst.y, AEpsilonVector.y);
  if Result then
    if TwoDOnly then
      Result := ATst.TwoDOnly
    else
      Result := SameValue(z, ATst.z, AEpsilonVector.z);
end;

procedure RISSensorVector.SetZero;
begin
  TwoDOnly := True;
  x := NaN;
  y := NaN;
  z := NaN;
end;

{ TAndroideTime }

{$IFDEF ANDROID}

function TAndroideTime.DelphiDateTime(AStartOffsetTick: Int64;
  AOffsetStart: TDateTime): TDateTime;
Var
  Seconds: Double;
  Inc: Int64;
begin
  Inc := ValInt64 - AStartOffsetTick;
  Seconds := Inc / 1000000;
  Result := AOffsetStart + Seconds / 24 / 60 / 60;
end;

{$ENDIF}

end.
