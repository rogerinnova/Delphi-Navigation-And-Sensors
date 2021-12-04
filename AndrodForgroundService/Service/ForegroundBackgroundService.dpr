program ForegroundBackgroundService;

uses
  System.Android.ServiceApplication,
  ServiceUnit in 'ServiceUnit.pas' {FBServiceModule: TAndroidService},
  IsMobileSensors in 'Z:\Repositories\GitRepository\PublicGit\Roger\Delphi-Navigation-And-Sensors\LibraryCode\IsMobileSensors.pas',
  IsNavUtils in 'Z:\Repositories\GitRepository\PublicGit\Roger\Delphi-Navigation-And-Sensors\LibraryCode\IsNavUtils.pas',
  GpsDbBusObjects in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\MobileDeviceSensors\GpsDbSaverFiles\GpsDbBusObjects.pas',
  GpsDbVersionInfo in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\MobileDeviceSensors\GpsDbSaverFiles\GpsDbVersionInfo.pas',
  GpsUserDataAccess in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\MobileDeviceSensors\GpsDbSaverFiles\GpsUserDataAccess.pas',
  IsWindowsPickUp in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\MultiPlatDev\Lib\IsWindowsPickUp.pas',
  ISPermObjFileStm in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISPermObjFileStm.pas',
  ISMultiUserRemoteDb in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserRemoteDb.pas',
  ISMultiUserPermObjFileStm in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserPermObjFileStm.pas',
  IsUnicodeStrUtl in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsUnicodeStrUtl.pas',
  ISObjectCounter in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISObjectCounter.pas',
  ISMultiUserRemoteDBIndyTCPObjs in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMultiUserRemoteDBIndyTCPObjs.pas',
  ISIndyUtils in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISIndyUtils.pas',
  IsRemoteDbLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsRemoteDbLib.pas',
  ISBase64AndEncryption in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISBase64AndEncryption.pas',
//  XE3LibPickup in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\XE3LibPickup.pas',
  //IsGeneralLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsGeneralLib.pas',
  //IsArrayLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\IsArrayLib.pas',
  ISMathsGraphicsLib in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISMathsGraphicsLib.pas',
  ISDelphi2009Adjust in 'Z:\RogerHome\RepositoryHg\InnovaSolHomeOnSalmon\Delphi Projects\Delphi 3_5 Source Code\LibraryV3\ISDelphi2009Adjust.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFBServiceModule, FBServiceModule);
  Application.Run;
end.
