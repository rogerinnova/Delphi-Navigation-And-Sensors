program FBSampleProjectSydney;

uses
  System.StartUpCopy,
  FMX.Forms,
  ApplicationUnit in '..\Application\ApplicationUnit.pas' {LocationTrackingForm},
  ServiceUnit in '..\Service\ServiceUnit.pas' {FBServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLocationTrackingForm, LocationTrackingForm);
  Application.CreateForm(TFBServiceModule, FBServiceModule);
  Application.Run;
end.
