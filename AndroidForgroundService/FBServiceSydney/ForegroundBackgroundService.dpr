program ForegroundBackgroundService;

uses
  System.Android.ServiceApplication,
  ServiceUnit in '..\Service\ServiceUnit.pas' {FBServiceModule: TAndroidService},
  GpsUserDataAccess in '..\DataAccess\GpsUserDataAccess.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFBServiceModule, FBServiceModule);
  Application.Run;
end.
