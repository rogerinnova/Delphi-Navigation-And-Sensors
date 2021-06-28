unit IsNavUtils;
// Ref https://www.movable-type.co.uk/scripts/latlong.html
{ On ARM system include iOS and Android, however, the System.Extended type is an alias for System.Double,
  which is only 8 bytes. This difference can adversely affect numeric precision in floating-point operations. }

interface

uses Math, System.SysUtils, System.Types, System.UITypes, System.Classes,
{$IFDEF MSWINDOWS}
  WinApi.Windows, WinApi.ShellAPI, ansistrings,
{$ENDIF}
  System.Variants;

Type
  RNavigateLongLat = record
  Private
    LongitudeRad, LatitudeRad: Double;
    LongitudeDec, LatitudeDec: Double;
    FGoogleScale: Integer;
    procedure SetLongitude(const Value: Double);
    procedure SetLatitude(const Value: Double);
    Procedure Normalize;
    function GetGoogleScale: Integer;
  Public
{$IFDEF msWindows}
    Procedure GoGoogle;
    Procedure GoGoogleDirectionsTo(ADestination:RNavigateLongLat;
    AMapLoc: Integer = 0 { 0 Start 1 End 3 Center });
{$ENDIF}
    Procedure SetToNull;
    Procedure CreateDec(ADecLong, ADecLatt: Double);
    Procedure Create(ALongRad, ALatRad: Double);
    Procedure GoogleScaleByMeters(AMeters:Double);
    Function LocatationText(ASecDecPlaces: Integer): String;
    Function LocatationCsv(ADecPlaces: Integer): String;
    Function LongitudeText(ASecDecPlaces: Integer): String;
    Function LatitudeText(ASecDecPlaces: Integer): String;
    Function MetresFrom(AStart: RNavigateLongLat): Double;
    Function MetresFromPrecision(AStart: RNavigateLongLat;
      Out ARadiansToStart, ARadiansFromStart: Double{Radians}; AAllowPolarRoute: Boolean = false): Double;
    Function LocationAtMtrsRad(AMeters, ATrueHeadingRadians: Double)
      : RNavigateLongLat;
    Function LocationAt(AMeters, ATrueHeadingDegrees: Double): RNavigateLongLat;
    Function HeadingFrom(AStart: RNavigateLongLat): Double;
    // 0..360
    Function HeadingInRadFrom(AStart: RNavigateLongLat): Double;
    // 0..2Pi
    Function GoogleLink: String;
    Function GoogleLinkDirectionsTo(ADestination: RNavigateLongLat;
      AMapLoc: Integer = 0 { 0 Start 1 End 3 Center } ): String;
    Function IsValid: Boolean;
    Function NotNull: Boolean;
    Function HeadingFromAsText(AStart: RNavigateLongLat;
      ADecPlace: Integer): String;
    Property Longitude: Double read LongitudeDec write SetLongitude;
    Property Latitude: Double read LatitudeDec write SetLatitude;
    Property LongAsRad: Double read LongitudeRad;
    Property LatAsRad: Double read LatitudeRad;
    Property GoogleScale: Integer Read GetGoogleScale Write FGoogleScale;

  end;

Function TextMeters(AMeters: Single; DecPlace, MaxLen, PosPoint: Byte): String;
// Meters Kilometres mm

function AngleDecFrmDegreeMinuteSecond(ADegrees, AMinutes: Integer;
  ASeconds: Double): Double; Overload;

function TextDegreeMinuteSecond(ARadians: Double; AAccuracy: Integer)
  : String; Overload;
{ AAccuracy 0 Degrees Only <3 Minutes >3 Second decimal=a-3 }
function TextDegreeMinuteSecondFrmDec(Value: Double;
  AAccuracy: Integer): String;

Procedure CalNewDoubleAverageAndSumOfSquares(ANewSample: Double;
  Var ARunningAveage, ARunningAveageSumOfSqrs: Double; ANoOfSamples: Integer);

Procedure CalNewRationalAngleDegreesAverageAndSumOfSquares(ANewSample: Double;
  Var ARunningAngleAve, RunningAngleSumOfSquares: Double; ANoOfSamples: Integer;
  AReturn0to360: Boolean = false);

Procedure CalNewRationalAngleRadiansAverageAndSumOfSquares(ANewSample: Double;
  Var RunningAngleSumOfSquares, ARunningAngleAve: Double;
  ANoOfSamples: Integer);

Function CalDoubleStdDevFromSumOfSquares(ARunningAveageSumOfSqrs: Double;
  ANoOfSamples: Integer): Double;

Function MtrsPerDegreeLongAdjustForLatitude(ALatitudeDegree: Double): Double;

Function RealFrmDegreeText(AText:String):double;

Const
  // Radius of Earth =~~ 6,371Km  6,357km Polar 6,378Km Equatorial
  EarthRad: Double = 6371008.8;
  MtrsPerDegree: Double = Pi * 6371008.8 / 180;
  // Radius of earth in Meters >> Meters Circumference per radian
  OneMeterAsDegrees: Double = 180 / Pi / 6371008.8;
  // 1/MtrsPerDegree       Mtres as Degres

  AngleDegreeChar = '°'; // 'Degree';
  AngleMins = '''';
  AngleSecs = '"';
  EffectiveZero = 0.0000000001; { Testing for meters }

implementation

function AngleDecFrmDegreeMinuteSecond(ADegrees, AMinutes: Integer;
  ASeconds: Double): Double; Overload;
Begin
  if ADegrees < 0 then
    Result := -(-ADegrees + AMinutes / 60 + ASeconds / 3600)
  else
    Result := ADegrees + AMinutes / 60 + ASeconds / 3600;
End;

Function MtrsPerDegreeLongAdjustForLatitude(ALatitudeDegree: Double): Double;
Var
  LatitudeFactor: Double;
Begin
  LatitudeFactor := Cos(ALatitudeDegree / 2 / 180 * Pi);
  Result := MtrsPerDegree * LatitudeFactor;
End;

Function TextMeters(AMeters: Single; DecPlace, MaxLen, PosPoint: Byte): String;
// Meters Kilometres mm
var
  PartStr: String;
  Len: Integer;
  AbsVal, KMeters: Single;
begin
  if DecPlace < 1 then
    Len := PosPoint + DecPlace
  else
    Len := PosPoint + 1 + DecPlace;
  AbsVal := Abs(AMeters);
  if (AbsVal > 1000) then
  Begin
    KMeters := AMeters / 1000;
    Str(KMeters: Len: DecPlace, PartStr);
    PartStr := PartStr + ' km'; // + MetricL;
  End
  Else if (AbsVal >= 0.1) or (AbsVal < EffectiveZero) then
  begin
    Str(AMeters: Len: DecPlace, PartStr);
    PartStr := PartStr + ' m'; // + MetricL;
  end
  else
  begin
    AMeters := AMeters * 1000;
    Str(AMeters: Len + 1 - DecPlace: 1, PartStr);
    PartStr := PartStr + ' mm'; // ' + MetricmmL;
  end;

  if MaxLen <> 0 then
  begin
    PartStr := PartStr + '                         '; // PadOut;
    PartStr := Copy(PartStr, 0, MaxLen);
  end;
  Result := PartStr;
end;

function TextDegreeMinuteSecond(ARadians: Double; AAccuracy: Integer): String;
{ Value in Radians } // See Also  ISRcStdLib
{ AAccuracy 0 Degrees Only <3 Minutes >3 Second decimal=a-3 }

Begin
  if IsNan(ARadians) then
    Result := 'NAN ' + AngleDegreeChar
  else
    Result := TextDegreeMinuteSecondFrmDec(ARadians * 180 / Pi, AAccuracy);
End;

{Derived rom ISArrayLib}
type
TArrayOfUnicodeStrings = Array of String;
TArrayOfAnsiStrings =Array of Ansistring;

function GetNumericArrayFromAlphaNumericString(AData: String;
  ASepControls: Boolean; ASigns: Boolean; ADollars: Boolean)
  : TArrayOfUnicodeStrings;
{ Extracts Array ['7777','88'] from
  junk7777more junk88fg }
{ ASepControls splits on spaces lf etc if Asigns then signs are numeric ditto Adollars
  {Splits array into numerics and ignors non numerics - Removes nulls }

{ sub } function GetFlag(AChar: Char): Integer;
  begin
    case AChar of
      'A' .. 'Z', 'a' .. 'z':
        Result := 1;
      '0' .. '9':
        Result := 2;
      '.':
        Result:=3; // Numeric if 66.66 or .88 but not stops. Here
      '+', '-':
        if ASigns then
          Result := 2
        else
          Result := 0;
      '$':
        if ADollars then
          Result := 2
        else
          Result := 0;
    else
      Result := 0; // Non Alpha numeric
    end;
  end;
{ sub } function UnMatchedFlags(var AFlgA, AFlgB: Integer): Boolean;
  begin
    if AFlgB=3 then
     if AFlgA=0 then
       Result:=true
     else
     begin
        Result:=Not (AFlgA=2);
        if Not Result then
         AFlgB:=2;
     end
    else
    Result := AFlgA <> AFlgB;
    if Result and not ASepControls then
      if (AFlgA < 2) and (AFlgB < 2) then
        Result := false;
    if Result then  // Numeric if 66.66 or .88 but not stops. Here
      if AflgA=3 then
        Result:= not (AFlgB=2);
    AFlgA := AFlgB;
  end;

var
  Rcrds, cursz: Integer;
  NextChar: PChar;
  AlphaFlg, NxtAlphaFlag, TypeFlag,TstLen, i: Integer;
  S: String;
begin
  SetLength(Result, 0);
  if AData = '' then
    exit;

  Rcrds := 1;
  cursz := 0;
  NextChar := PChar(AData);
  AlphaFlg := GetFlag(Char(NextChar[0]));
  TstLen:=length(Adata);
  while Pointer(NextChar) <> nil do
  begin
    S := '';
    i := 0;
    if Rcrds >= cursz then
    begin
      inc(cursz, 5);
      SetLength(Result, cursz);
    end;
    while (S = '') and (NextChar[0] <> Char(0)) and (NextChar <> nil) do
    begin
      NxtAlphaFlag := GetFlag(Char(NextChar[i + 1]));
      TypeFlag := AlphaFlg;
      if UnMatchedFlags(AlphaFlg, NxtAlphaFlag) then
      begin
        S := Copy(NextChar, 0, i + 1);
        S := Trim(S);
        if (S <> '') and (TypeFlag = 2) then
        begin
          Result[Rcrds - 1] := S;
          inc(Rcrds, 1);
        end;
        inc(NextChar, i + 1);
        TstLen:=Length(NextChar);
        AlphaFlg := GetFlag(Char(NextChar[0]));
        i := 0;
      end
      else
      inc(i);
       if i>TstLen then
         NextChar:='';
    end;
    if Length(S) = 0 then
    begin
      S := NextChar;
      S := Trim(S);
      if Length(S) > 0 then
        Result[Rcrds - 1] := S
      else
        Dec(Rcrds);
      NextChar := nil;
    end;
  end;
  SetLength(Result, Rcrds);
end;

Function RealFrmDegreeText(AText:String):double;
var
  ValueArray:TArrayOfUnicodeStrings;
  Deg,min,sec:double;
  tstneg,lastno:integer;
  Negative:Boolean;
  LText:string;
Begin
  try
   Result:=0.0;
   Trim(AText);
   if AText<>'' then
      Result:=StrToFloat(AText);
  Except
    min:=0.0;
    sec:=0.0;
    ValueArray:=GetNumericArrayFromAlphaNumericString(AText,false,true,false);
    if Length(ValueArray)>0 then
        Deg:=StrToFloat(ValueArray[0]);
    Negative:=Deg<0;
    if Negative then
       Deg:=-Deg
     else
      begin
       LastNo:=pos(ValueArray[high(ValueArray)],AText);
       if lastno>0 then
         begin
          LText:=Lowercase(AText);
          Negative:=(Pos('s',LText)>lastno)Or(Pos('w',LText)>lastno);
         end;
      end;
    Result:=Deg;
    if Length(ValueArray)>1 then
       begin
        Min:=StrToFloat(ValueArray[1]);
        Result:=Deg+Min/60;
       end;
    if Length(ValueArray)>2 then
       Begin
        Sec:=StrToFloat(ValueArray[2]);
        Result:=Result+Sec/60/60;
       End;
    if Negative then
      Result:=-Result;
  end;
End;



function TextDegreeMinuteSecondFrmDec(Value: Double;
  AAccuracy: Integer): String;
{ AAccuracy 0 Degrees Only <3 Minutes >3 Second decimal=a-3 }

var
  ConvDgree, ConvMinutes: Integer;
  Remainder: real;
  PartStr, partstr1, partstr2, Str60Test: String;

begin
  // Result:= TextDegreeMinuteSecond(value/180*pi,AAccuracy, 0);
  { Value in Radians }
  // PartStr:=Result;
  if IsNan(Value) then
    Result := 'NAN ' + AngleDegreeChar
  else If Value < 0 Then
    Result := '-' + TextDegreeMinuteSecondFrmDec(-Value, AAccuracy)
    // Do Negative
  Else
  Begin
    partstr2 := '';
    partstr1 := '';
    Remainder := Value; // was * 180 / pi;
    if AAccuracy = 0 then
      ConvDgree := Round(Remainder)
    else
    begin
      ConvDgree := Trunc(Remainder);
      Remainder := Remainder - ConvDgree;
      Remainder := Remainder * 60;
      if AAccuracy < 3 then
        ConvMinutes := Round(Remainder)
      else
      begin
        ConvMinutes := Trunc(Remainder);
        Remainder := Remainder - ConvMinutes;
        Remainder := Remainder * 60;
        if AAccuracy > 3 then
          AAccuracy := AAccuracy - 3
        else
          AAccuracy := 0;
        Str(Remainder: 1: AAccuracy, partstr2);
        Str(60.0: 3: AAccuracy, Str60Test);
        if partstr2 = Str60Test then
        begin
          Str(0.0: 3: AAccuracy, partstr2);
          ConvMinutes := ConvMinutes + 1;
        end;
        partstr2 := partstr2 + AngleSecs;
      end;
      if ConvMinutes = 60 then
      begin
        ConvMinutes := 0;
        ConvDgree := ConvDgree + 1;
      end;
      Str(ConvMinutes: 3, partstr1);
      partstr1 := partstr1 + AngleMins;
    end;
    Str(ConvDgree: 3, PartStr);
    PartStr := PartStr + AngleDegreeChar + ' ' + partstr1 + ' ' + partstr2;
    Result := PartStr;
  End;
end;

{ RNavigateLongLat }
procedure RNavigateLongLat.Create(ALongRad, ALatRad: Double);
begin
  LongitudeRad := ALongRad;
  LatitudeRad := ALatRad;
  LongitudeDec := LongitudeRad * 180 / Pi;
  LatitudeDec := LatitudeRad * 180 / Pi;
  Normalize;
end;

procedure RNavigateLongLat.CreateDec(ADecLong, ADecLatt: Double);
begin
  // LongitudeRad := 0.0;
  LatitudeRad := 0.0;
  Longitude := ADecLong;
  Latitude := ADecLatt;
end;


function RNavigateLongLat.GetGoogleScale: Integer;
begin
  if (FGoogleScale < 1) or (FGoogleScale > 21) then
    FGoogleScale := 17;
  Result := FGoogleScale;
end;

{$IFDEF msWindows}

Procedure RNavigateLongLat.GoGoogle;
var
  Command: String;
  lpParameters, lpDirectory, lpOperation: PChar;
  LocalAction: String;
  Return: DWord;
begin
  LocalAction := 'open';
  lpOperation := PChar(LocalAction);
  Command := GoogleLink;
  // Parameters:=  GoogleLink;
  // lpParameters := @Parameters[1];
  lpParameters := nil;
  lpDirectory := nil;
  Return := ShellExecuteW(0, // handle to parent window
    lpOperation, // pointer to string that specifies operation to perform
    @Command[1], // pointer to filename or folder name string
    lpParameters,
    // pointer to string that specifies executable-file parameters
    lpDirectory, // pointer to string that specifies default directory
    // whether file is shown when opened
    SW_RESTORE);
  Inc(Return);
end;


procedure RNavigateLongLat.GoGoogleDirectionsTo(ADestination: RNavigateLongLat;AMapLoc: Integer);
var
  Command{, Parameters}: String;
//  Visiblity: DWord;
  lpParameters, lpDirectory, lpOperation: PChar;
  LocalAction: String;
//  Return: DWord;
begin
  LocalAction := 'open';
  lpOperation := PChar(LocalAction);
  Command := GoogleLinkDirectionsTo(ADestination,AMapLoc);
  // Parameters:=  GoogleLink;
  // lpParameters := @Parameters[1];
  lpParameters := nil;
  lpDirectory := nil;
  {Return := }ShellExecuteW(0, // handle to parent window
    lpOperation, // pointer to string that specifies operation to perform
    @Command[1], // pointer to filename or folder name string
    lpParameters,
    // pointer to string that specifies executable-file parameters
    lpDirectory, // pointer to string that specifies default directory
    // whether file is shown when opened
    SW_RESTORE);

end;

{$ENDIF}

function RNavigateLongLat.GoogleLink: String;
const
  // LGoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s';//+',21z';
  LGoogleMapsURL: String = 'https://www.google.com/maps/place/%s/@%s,%s,%s';
var
  ENUSLat, ENUSLong, ENULocal, ENUScale: String; // holders for URL strings
begin
  // ENUSLat := Latitude.ToString(ffGeneral, 5, 2,
  // TFormatSettings.Create('en-US'));
  ENUSLat := FormatFloat('000.00000', Latitude);
  ENUSLong := FormatFloat('000.00000', Longitude);
  ENULocal := LocatationText(3);
  ENUScale := IntToStr(GoogleScale) + 'z';
  // ENUSLong := Longitude.ToString(ffGeneral, 5, 2,
  // TFormatSettings.Create('en-US'));
  { convert the location to latitude and longitude }
  { and track the location via Google Maps }
  // Result := Format(LGoogleMapsURL, [ENUSLat, ENUSLong]);
  // https://maps.google.com/maps?q= -38.55,145.55
  // https://www.google.com/maps/dir/-38.0497722,145.1384916/-38.0498333,145.1385556/@-38.04983,145.1384232,21z
  Result := Format(LGoogleMapsURL, [ENULocal, ENUSLat, ENUSLong, ENUScale]);
  // https://www.google.com/maps/place/38%C2%B002'59.4%22S+145%C2%B008'18.8%22E/@-38.0498333,145.1363669,17z/data=!3m1!4b1!4m5!3m4!1s0x0:0x0!8m2!3d-38.04983!4d145.13856
end;

function RNavigateLongLat.GoogleLinkDirectionsTo(ADestination: RNavigateLongLat;
  AMapLoc: Integer = 0): String;
const
  LGoogleMapsURL
    : String = 'https://www.google.com/maps/dir/%s,%s/%s,%s/@%s,%s,%s';
var
  ENUSLat, ENUSLong, ENUSLatTo, ENUSLongTo, ENUSLatCenter, ENUSLongCenter,
    ENUScale: String;
  // holders for URL strings
  // ENULocal,
  // 0 Start 1 End 3 Center
   Center: RNavigateLongLat;
   LBearingTo,LBearingFrm,LMeters:Double;
begin
  case AMapLoc of
    0:
      Center := Self;
    1:
      Center := ADestination;
    2:
      Begin
        Lmeters:=MetresFromPrecision(ADestination,LBearingTo,LBearingFrm);
        Center:=LocationAtMtrsRad(LMeters/2,LBearingFrm);
        GoogleScaleByMeters(LMeters);
      End
  Else
    Center := Self;
  end;
  if (FGoogleScale<1)or (FGoogleScale>21) then
      Begin
        Lmeters:=MetresFromPrecision(ADestination,LBearingTo,LBearingFrm);
        GoogleScaleByMeters(LMeters);
      End;
  // ENUSLat := Latitude.ToString(ffGeneral, 5, 2,
  // TFormatSettings.Create('en-US'));
  ENUSLat := FormatFloat('000.00000', Latitude);
  ENUSLong := FormatFloat('000.00000', Longitude);
  ENUSLatTo := FormatFloat('000.00000', ADestination.Latitude);
  ENUSLongTo := FormatFloat('000.00000', ADestination.Longitude);
  ENUSLatCenter := FormatFloat('000.00000', Center.Latitude);
  ENUSLongCenter := FormatFloat('000.00000', Center.Longitude);
  // ENULocal := LocatationText(3);
  ENUScale := IntToStr(GoogleScale) + 'z';
  // From                        To                       Center
  // https://www.google.com/maps/dir/-38.0497722,145.1384916/-38.0498333,145.1385556/@-38.04983,145.1384232,21z
  Result := Format(LGoogleMapsURL, [ENUSLat, ENUSLong, ENUSLatTo, ENUSLongTo,
    ENUSLatCenter, ENUSLongCenter, ENUScale]);
//  Center.GoogleScale:=GoogleScale;
//  Center.GoGoogle;
end;

procedure RNavigateLongLat.GoogleScaleByMeters(AMeters: Double);
Const
   LGScale=20;

begin
  if AMeters<LGScale then
     FGoogleScale:=21
     Else
  if AMeters<LGScale*2 then
     FGoogleScale:=20
     Else
  if AMeters<LGScale*4 then
     FGoogleScale:=19
     Else
  if AMeters<LGScale*6 then
     FGoogleScale:=18
     Else
  if AMeters<LGScale*16 then
     FGoogleScale:=17
     Else
  if AMeters<LGScale*32 then
     FGoogleScale:=16
     Else
  if AMeters<LGScale*64 then
     FGoogleScale:=15
     Else
  if AMeters<LGScale*128 then
     FGoogleScale:=14
     Else
  if AMeters<LGScale*512 then
     FGoogleScale:=13
     Else
  if AMeters<LGScale*1024 then
     FGoogleScale:=12
     Else
  if AMeters<LGScale*2048 then
     FGoogleScale:=11
     Else
  if AMeters<LGScale*4096 then
     FGoogleScale:=10
     Else
       FGoogleScale:=9;
end;

function RNavigateLongLat.HeadingFrom(AStart: RNavigateLongLat): Double;
begin
  Result := HeadingInRadFrom(AStart) * 180 / Pi;
end;

function RNavigateLongLat.HeadingFromAsText(AStart: RNavigateLongLat;
  ADecPlace: Integer): String;
// Var
// Hdg:Double;
begin
  // Hdg:=;
  Result := TextDegreeMinuteSecond(HeadingFrom(AStart), ADecPlace)
end;

function RNavigateLongLat.HeadingInRadFrom(AStart: RNavigateLongLat): Double;
{ Formula: 	θ = atan2( sin Δλ ⋅ cos φ2 , cos φ1 ⋅ sin φ2 − sin φ1 ⋅ cos φ2 ⋅ cos Δλ )
  where 	φ1,λ1 is the start point, φ2,λ2 the end point (Δλ is the difference in longitude)
  JavaScript:
  (all angles
  in radians)
  φ2 LatitudeRad  φ1 LatitudeRad

  var y = Math.sin(λ2-λ1) * Math.cos(φ2);
  var x = Math.cos(φ1)*Math.sin(φ2) -
  Math.sin(φ1)*Math.cos(φ2)*Math.cos(λ2-λ1);
  var brng = Math.atan2(y, x).toDegrees(); }
Var
  y, x: Double;

begin
  if not(IsValid and AStart.IsValid) then
  begin
    Result := 0.0;
    Exit
  end;

  y := sin(LongitudeRad - AStart.LongitudeRad) * Cos(LatitudeRad);
  x := Cos(AStart.LatitudeRad) * sin(LatitudeRad) - sin(AStart.LatitudeRad) *
    Cos(LatitudeRad) * Cos(LongitudeRad - AStart.LongitudeRad);
  Result := ArcTan2(y, x);
  while (Result > 2 * Pi) do
    Result := Result - 2 * Pi;
  while (Result < 0) do
    Result := Result + 2 * Pi;
end;

function RNavigateLongLat.IsValid: Boolean;
begin
  Result := not(IsNan(LongitudeRad) or IsNan(LatitudeRad));
end;

function RNavigateLongLat.LatitudeText(ASecDecPlaces: Integer): String;
begin
  if LatitudeDec > 0.0 then
    Result := TextDegreeMinuteSecondFrmDec(LatitudeDec, ASecDecPlaces + 3) + 'N'
  Else if LatitudeDec < 0.0 then
    Result := TextDegreeMinuteSecondFrmDec(-LatitudeDec,
      ASecDecPlaces + 3) + 'S'
  Else
    Result := TextDegreeMinuteSecondFrmDec(0.0, ASecDecPlaces + 3) + ' ';
end;

function RNavigateLongLat.LocatationCsv(ADecPlaces: Integer): String;
Var
  SFloat: String;
  i: Integer;
begin
  SFloat := '000.';
  for i := 1 to ADecPlaces do
    SFloat := SFloat + '0';
  Result := ',Long=,' + FormatFloat(SFloat, LongitudeDec) + ',Lat=,' +
    FormatFloat(SFloat, LatitudeDec);
end;

function RNavigateLongLat.LocatationText(ASecDecPlaces: Integer): String;
begin
  Result := LongitudeText(ASecDecPlaces) + '  ' + LatitudeText(ASecDecPlaces);
end;

function RNavigateLongLat.LocationAt(AMeters, ATrueHeadingDegrees: Double)
  : RNavigateLongLat;
Var
  Rad: Double;
begin
  Rad := ATrueHeadingDegrees / 180 * Pi;
  Result := LocationAtMtrsRad(AMeters, Rad);
end;

function RNavigateLongLat.LocationAtMtrsRad(AMeters, ATrueHeadingRadians
  : Double): RNavigateLongLat;
// https://www.movable-type.co.uk/scripts/latlong.html
{
  Formula:
  φ2 = asin( sin φ1 * cos δ + cos φ1 * sin δ * cos θ )
  λ2 = λ1 + atan2( sin θ * sin δ * cos φ1, cos δ − sin φ1 * sin φ2 )
  where 	φ is latitude, λ is longitude, θ is the bearing (clockwise from north),
  δ is the angular distance d/R;
  d being the distance travelled,
  R the earth’s radius

  For final bearing, simply take the initial bearing from the end point to the start point and reverse it with (brng+180)%360.
}
Var
  Long, Lat, Delta: Double;
begin
  Try
    Delta := AMeters / EarthRad;
    Lat := ArcSin(sin(LatitudeRad) * Cos(Delta) + Cos(LatitudeRad) * sin(Delta)
      * Cos(ATrueHeadingRadians));
    Long := LongitudeRad + ArcTan2(sin(ATrueHeadingRadians) * sin(Delta) *
      Cos(LatitudeRad), Cos(Delta) - sin(LatitudeRad) * sin(Lat));
    Result.Create(Long, Lat);
  Except
    On E: Exception do
      raise Exception.Create('Error LocationAt :: ' + E.Message);
  End;
end;

function RNavigateLongLat.LongitudeText(ASecDecPlaces: Integer): String;
begin
  if LongitudeDec > 0.0 then
    Result := TextDegreeMinuteSecondFrmDec(LongitudeDec,
      ASecDecPlaces + 3) + 'E'
  Else if LongitudeDec < 0.0 then
    Result := TextDegreeMinuteSecondFrmDec(-LongitudeDec,
      ASecDecPlaces + 3) + 'W'
  Else
    Result := TextDegreeMinuteSecondFrmDec(0.0, ASecDecPlaces + 3) + ' ';

end;

function RNavigateLongLat.MetresFrom(AStart: RNavigateLongLat): Double;
{ https://www.movable-type.co.uk/scripts/latlong.html
  Haversine
  formula: 	a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
  c = 2 ⋅ atan2( √a, √(1−a) )
  d = R ⋅ c
  where 	φ is latitude, λ is longitude, R is earth’s radius (mean radius = 6,371km);
  note that angles need to be in radians to pass to trig functions!
  JavaScript:

  var R = 6371e3; // metres
  var φ1 = lat1.toRadians();
  var φ2 = lat2.toRadians();
  var Δφ = (lat2-lat1).toRadians();
  var Δλ = (lon2-lon1).toRadians();

  var a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
  Math.cos(φ1) * Math.cos(φ2) *
  Math.sin(Δλ/2) * Math.sin(Δλ/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));

  var d = R * c;

  Also http://www.reeve.com/Documents/Articles%20Papers/Reeve_PosDistBrngCalcs.pdf
}
Var
  DeltaLong, DeltaLat, a, b, c: Double;
begin
  if not(IsValid and AStart.IsValid) then
  begin
    Result := 0.0;
    Exit
  end;

  DeltaLong := LongitudeRad - AStart.LongitudeRad; // Δλ
  DeltaLat := LatitudeRad - AStart.LatitudeRad; // Δφ
  a := sin(DeltaLat / 2);
  b := sin(DeltaLong / 2);
  a := a * a + Cos(AStart.LatitudeRad) * Cos(LatitudeRad) * b * b;
  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));
  Result := EarthRad * c;
end;

function RNavigateLongLat.MetresFromPrecision(AStart: RNavigateLongLat;
  out ARadiansToStart, ARadiansFromStart: Double; AAllowPolarRoute: Boolean): Double;
// https://www.movable-type.co.uk/scripts/latlong-vincenty.html
// Semi-major axis	a	= 6378137.0	metres
// Semi-minor axis	b	≈ 6356752.314245	metres
// Inverse flattening	1/f	= 298.257223563
Const
  MjrAxis = 6378137.0; // Metres
  MnrAxis = 6356752.314245;
  PolarElipseSemiGa = 20003931.458;
  // From www.ga.gov.au/geodesy/datums/vincenty_inverse.jsp
var
  FinalBearing:Double;
  Flat, TanLat1Adj, CosLat1Adj, SinLat1Adj: Extended;
  SpecialCaseAdj: RNavigateLongLat;
  TanLat2Adj, CosLat2Adj, SinLat2Adj, SinLatAdjMult, CosLatAdjMult: Extended;
  DeltaLong, Lamda, LamdaDash, Sigma: Extended;
  SinLamda, CosLamda, SinSqSigma, SinSigma, CosSigma, PolarElipseSemi, EqaSemi,
    SqVal: Extended;
  SinAlpha, CosSqAlpha, Cos2SigmaM, Cos2SigmaMSq, DeltaSigma, c, uSq, AVal,
    BVal, TstComplete, Result2: Extended;
  InterateLim: Integer;
  AZEqFwd, AzEqRev, SpecialCaseLongitude: Double;
  Done: Boolean;
begin
  Flat := (MjrAxis - MnrAxis) / MjrAxis;
  DeltaLong := LongitudeRad - AStart.LongitudeRad;
  if SameValue(LatitudeRad, -AStart.LatitudeRad, 0.25) then
    // 80, -5, 260, 4.78, Failed to Converge
    if AAllowPolarRoute then
    Begin
      if SameValue(Abs(DeltaLong), Pi,
        0.0105331658 { 23' 47.3"  36 / 60 * pi / 180 } ) then
        // 90+47/60, 0, 270,0,  Failed to Converge
        if ((LatitudeRad < 40 / 180 * Pi) and (LatitudeRad > -40 / 180 * Pi))
        then
        // 80, -40, 260, 40, Converged
        begin
          SpecialCaseAdj.Create(LongitudeRad, -AStart.LatitudeRad);
          // PolarElipseSemi := pi * Sqrt((MjrAxis * MjrAxis + MnrAxis * MnrAxis) / 2);
          // EqaSemi := pi * MjrAxis;
          // Special Case  over the pole
          Result := MetresFromPrecision(SpecialCaseAdj, ARadiansToStart, ARadiansFromStart);
          Result := PolarElipseSemiGa - Result;
          ARadiansToStart := Pi;
          ARadiansFromStart := Pi;
          if LatitudeRad < -AStart.LongitudeRad then
          Begin // end closer to equator
            if LatitudeRad < 0 then
              ARadiansFromStart := 0
            Else
              ARadiansToStart := 0;
          End
          Else // Start closer to equator
            if LatitudeRad > 0 then
              ARadiansFromStart := 0
            Else
              ARadiansToStart := 0;
          Exit;
        end;
    End
    else if SameValue(Abs(DeltaLong), Pi, 0.01 * Pi) then
      // Solution over a wider range as uses mid point on equater
      if ((LatitudeRad < 40 / 180 * Pi) and (LatitudeRad > -40 / 180 * Pi)) then
      // 80, -40, 260, 40, Converged
      begin
        // Split into two arround equiltorial mid piont
        SpecialCaseLongitude := (LongitudeRad + AStart.LongitudeRad) / 2;
        SpecialCaseAdj.Create(SpecialCaseLongitude, 0);
        Result := MetresFromPrecision(SpecialCaseAdj, AZEqFwd, ARadiansFromStart, false);
        Result2 := SpecialCaseAdj.MetresFromPrecision(AStart, ARadiansToStart,
          AzEqRev, false);
        Result := Result + Result2;
        Exit;
      end;

  TanLat1Adj := (1 - Flat) * Tan(AStart.LatitudeRad);
  CosLat1Adj := 1 / Sqrt(1 + TanLat1Adj * TanLat1Adj);
  SinLat1Adj := TanLat1Adj * CosLat1Adj;

  TanLat2Adj := (1 - Flat) * Tan(LatitudeRad);
  CosLat2Adj := 1 / Sqrt(1 + TanLat2Adj * TanLat2Adj);
  SinLat2Adj := TanLat2Adj * CosLat2Adj;

  SinLatAdjMult := SinLat1Adj * SinLat2Adj;
  CosLatAdjMult := CosLat1Adj * CosLat2Adj;
  // Only for warnings
  Cos2SigmaM := 0.0;
  SinLamda := 0.0;
  SinSigma := 0.0;
  CosSqAlpha := 0.0;
  CosLamda := 0.0;
  CosSigma := 0.0;
  SinSqSigma := 0.0;
  Sigma := 0.0;
  // Only for warnings

  Lamda := DeltaLong;
  TstComplete := Lamda * 0.000001;
  Done := false;
  InterateLim := 100;
  while Not Done and (InterateLim > 0) do
  begin
    SinLamda := sin(Lamda);
    CosLamda := Cos(Lamda);
    SqVal := (CosLat1Adj * SinLat2Adj - SinLat1Adj * CosLat2Adj * CosLamda);
    SqVal := SqVal * SqVal;
    SinSqSigma := CosLat2Adj * SinLamda * CosLat2Adj * SinLamda + SqVal;
    SinSigma := Sqrt(SinSqSigma);
    Done := SameValue(SinSigma, 0.0, 0);
    if Done then
    begin // Coincident points
      Result := 0.0;
      ARadiansToStart := 0.0;
      ARadiansFromStart := 0.0;
      Exit;
    end
    Else
    begin
      CosSigma := SinLatAdjMult + CosLatAdjMult * CosLamda;
      Sigma := ArcTan2(SinSigma, CosSigma);
      SinAlpha := CosLatAdjMult * SinLamda / SinSigma;
      CosSqAlpha := 1 - SinAlpha * SinAlpha;
      if SameValue(CosSqAlpha, EqualsValue, 0) then
        Cos2SigmaM := 0.0
      else
        Cos2SigmaM := CosSigma - 2 * SinLatAdjMult / CosSqAlpha;
      if IsNan(Cos2SigmaM) then
        Cos2SigmaM := 0.0; // Equitorial Line
      c := Flat / 16 * CosSqAlpha * (4 + Flat + +(4 - 3 * CosSqAlpha));
      LamdaDash := Lamda;
      Lamda := DeltaLong + (1 - c) * Flat * SinAlpha *
        (Sigma + c * SinSigma * (Cos2SigmaM + c * CosSigma *
        (-1 + Cos2SigmaM * Cos2SigmaM)));
      Done := SameValue(Lamda, LamdaDash, 0.0);
    end;
    Dec(InterateLim);
  end;
  if Not Done then
    raise Exception.Create('MetresFromPrecision Failed to Converge');

  uSq := CosSqAlpha * (MjrAxis * MjrAxis - MnrAxis * MnrAxis) /
    (MnrAxis * MnrAxis);
  AVal := 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)));
  BVal := uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)));

  Cos2SigmaMSq := Cos2SigmaM * Cos2SigmaM;
  DeltaSigma := BVal * SinSigma *
    (Cos2SigmaM + BVal / 4 * (CosSigma * (-1 + 2 * Cos2SigmaMSq) - BVal / 6 *
    Cos2SigmaM * (-3 + 4 * SinSqSigma) * (-3 + 4 * Cos2SigmaMSq)));

  Result := MnrAxis * AVal * (Sigma - DeltaSigma);
  ARadiansToStart := ArcTan2(CosLat2Adj * SinLamda,
    (CosLat1Adj * SinLat2Adj - SinLat1Adj * CosLat2Adj * CosLamda));
  if ARadiansToStart < 0 then
    ARadiansToStart := ARadiansToStart + 2 * Pi;
  FinalBearing := ArcTan2(CosLat1Adj * SinLamda,
    (-SinLat1Adj * CosLat2Adj + CosLat1Adj * SinLat2Adj * CosLamda));
  ARadiansFromStart:=FinalBearing-Pi;
  While ARadiansFromStart < 0 Do
    ARadiansFromStart := ARadiansFromStart + 2 * Pi;
end;

procedure RNavigateLongLat.Normalize;
begin
  if (LongitudeRad > Pi) or (LongitudeRad < -Pi) then
  begin
    while (LongitudeRad > Pi) do
      LongitudeRad := LongitudeRad - 2 * Pi;
    while (LongitudeRad < -Pi) do
      LongitudeRad := LongitudeRad + 2 * Pi;
    LongitudeDec := LongitudeRad / Pi * 180;
  end;
  if (LatitudeRad > Pi / 2) then
  Begin
    LatitudeRad := Pi / 2;
    LatitudeDec := 90.0;
    raise Exception.Create('Illegal Latitude');
  end;
  if (LatitudeRad < -Pi / 2) then
  begin
    LatitudeRad := -Pi / 2;
    LatitudeDec := -90.0;
    raise Exception.Create('Illegal Latitude');
  end;
end;

function RNavigateLongLat.NotNull: Boolean;
begin
  Result := LongitudeRad <> 0.0;
  if not Result then
    Result := LatitudeRad <> 0.0;
end;

procedure RNavigateLongLat.SetLatitude(const Value: Double);
begin
  LatitudeDec := Value;
  LatitudeRad := LatitudeDec / 180 * Pi;
  if (LongitudeRad > 5 * Pi) or (LongitudeRad < -5 * Pi) then
    // Values Empty
    LongitudeRad := 0.0;
  Normalize;
end;

procedure RNavigateLongLat.SetLongitude(const Value: Double);
begin
  LongitudeDec := Value;
  LongitudeRad := LongitudeDec / 180 * Pi;
  if (LatitudeRad > Pi / 2) then
    // Values Empty
    LatitudeRad := 0.0;
  Normalize;
end;

procedure RNavigateLongLat.SetToNull;
begin
  LongitudeRad := 0.0;
  LatitudeRad := 0.0;
  LongitudeDec := 0.0;
  LatitudeDec := 0.0;
end;

Procedure CalNewDoubleAverageAndSumOfSquares(ANewSample: Double;
  Var ARunningAveage, ARunningAveageSumOfSqrs: Double; ANoOfSamples: Integer);

begin
  (*
    https://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods
  *)
  if ANoOfSamples < 2 then
  Begin
    ARunningAveageSumOfSqrs := 0.0;
    ARunningAveage := ANewSample;
  end
  else
  Begin
    ARunningAveageSumOfSqrs := ARunningAveageSumOfSqrs + (ANoOfSamples - 1) /
      ANoOfSamples * (Sqr(ANewSample - ARunningAveage));
    ARunningAveage := ARunningAveage + (ANewSample - ARunningAveage) /
      ANoOfSamples;
  end;
end;

Function CalDoubleStdDevFromSumOfSquares(ARunningAveageSumOfSqrs: Double;
  ANoOfSamples: Integer): Double;
begin
  if (ANoOfSamples < 2) or IsNan(ARunningAveageSumOfSqrs) then
    Result := 0.0
  else
    // Result:=  Sqrt(ARunningAveageSumOfSqrs)/(ANoOfSamples-1);
    Result := Sqrt(ARunningAveageSumOfSqrs / (ANoOfSamples - 1));
end;

Procedure CalNewRationalAngleDegreesAverageAndSumOfSquares(ANewSample: Double;
  Var ARunningAngleAve, RunningAngleSumOfSquares: Double; ANoOfSamples: Integer;
  AReturn0to360: Boolean = false);
// Handles 5 degrees and 355 degrees;
begin
  if Abs(ANewSample - ARunningAngleAve) > 180 then
    if (ANewSample < 0) or (ARunningAngleAve < 0) then
    Begin // go to 0 - 360
      while (ANewSample < 0) do
        ANewSample := ANewSample + 360;
      while (ARunningAngleAve < 0) do
        ARunningAngleAve := ARunningAngleAve + 360;
    End
    Else // go to -180 - +180
    Begin
      while (ANewSample > 180) do
        ANewSample := ANewSample - 360;
      while (ARunningAngleAve > 180) do
        ARunningAngleAve := ARunningAngleAve - 360;
    end;
  CalNewDoubleAverageAndSumOfSquares(ANewSample, ARunningAngleAve,
    RunningAngleSumOfSquares, ANoOfSamples);
  if AReturn0to360 then
  Begin // go to 0 - 360
    while (ANewSample < 0) do
      ANewSample := ANewSample + 360;
    while (ARunningAngleAve < 0) do
      ARunningAngleAve := ARunningAngleAve + 360;
  End
  Else // go to -180 - +180
  Begin
    while (ANewSample > 180) do
      ANewSample := ANewSample - 360;
    while (ARunningAngleAve > 180) do
      ARunningAngleAve := ARunningAngleAve - 360;
  end;
end;

Procedure CalNewRationalAngleRadiansAverageAndSumOfSquares(ANewSample: Double;
  Var RunningAngleSumOfSquares, ARunningAngleAve: Double;
  ANoOfSamples: Integer);
Begin
  if Abs(ANewSample - ARunningAngleAve) > Pi then
    if (ANewSample < 0) or (ARunningAngleAve < 0) then
    Begin // go to 0 - 2pi
      while (ANewSample < 0) do
        ANewSample := ANewSample + 2 * Pi;
      while (ARunningAngleAve < 0) do
        ARunningAngleAve := ARunningAngleAve + 2 * Pi;
    End
    Else // go to -pi - +pi
    Begin
      while (ANewSample > Pi) do
        ANewSample := ANewSample - 2 * Pi;
      while (ARunningAngleAve > Pi) do
        ARunningAngleAve := ARunningAngleAve - 2 * Pi;
    end;
  CalNewDoubleAverageAndSumOfSquares(ANewSample, ARunningAngleAve,
    RunningAngleSumOfSquares, ANoOfSamples);
  // go to -180 - +180
  while (ANewSample > Pi) do
    ANewSample := ANewSample - 2 * Pi;
  while (ARunningAngleAve > Pi) do
    ARunningAngleAve := ARunningAngleAve - 2 * Pi;

end;

end.
