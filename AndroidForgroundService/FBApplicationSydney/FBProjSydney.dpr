program FBProjSydney;

uses
  System.StartUpCopy,
  FMX.Forms,
  ApplicationUnit in '..\Application\ApplicationUnit.pas' {LocationTrackingForm},
  ServiceUnit in '..\Service\ServiceUnit.pas' {FBServiceModule: TAndroidService},
  GpsUserDataAccess in '..\DataAccess\GpsUserDataAccess.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLocationTrackingForm, LocationTrackingForm);
  Application.Run;
end.
