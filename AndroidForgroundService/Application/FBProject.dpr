program FBProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  ApplicationUnit in 'ApplicationUnit.pas' {LocationTrackingForm},
  IsMobileSensors in '..\..\LibraryCode\IsMobileSensors.pas',
  IsPermissions in '..\..\LibraryCode\IsPermissions.pas',
  {$IfDef AccessOnlineDb}
  GpsDbBusObjects in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\MobileDeviceSensors\GpsDbSaverFiles\GpsDbBusObjects.pas',
  GpsDbVersionInfo in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\MobileDeviceSensors\GpsDbSaverFiles\GpsDbVersionInfo.pas',
  ISPermObjFileStm in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISPermObjFileStm.pas',
  ISMultiUserPermObjFileStm in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserPermObjFileStm.pas',
  IsWindowsPickUp in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\Lib\IsWindowsPickUp.pas',
  ISMultiUserRemoteDb in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserRemoteDb.pas',
  IsUnicodeStrUtl in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsUnicodeStrUtl.pas',
  ISObjectCounter in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISObjectCounter.pas',
  ISMultiUserRemoteDBIndyTCPObjs in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserRemoteDBIndyTCPObjs.pas',
  ISIndyUtils in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISIndyUtils.pas',
  IsRemoteDbLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsRemoteDbLib.pas',
  ISBase64AndEncryption in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISBase64AndEncryption.pas',
  XE3LibPickup in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\XE3LibPickup.pas',
  IsGeneralLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsGeneralLib.pas',
  ISDelphi2009Adjust in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISDelphi2009Adjust.pas',
  ISMathsGraphicsLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMathsGraphicsLib.pas',
  IsArrayLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsArrayLib.pas',
  IsrcStdLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsrcStdLib.pas',
  IsProcCl in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsProcCl.pas',
  ISStrUtl in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISStrUtl.pas',
  {$Endif }
  GpsUserDataAccess in '..\DataAccess\GpsUserDataAccess.pas',
  IsFmxGraphics in '..\..\LibraryCode\IsFmxGraphics.pas',
  ServiceUnit in '..\Service\ServiceUnit.pas' {FBServiceModule: TAndroidService},
  IsNavUtils in '..\..\LibraryCode\IsNavUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLocationTrackingForm, LocationTrackingForm);
  Application.Run;
end.
