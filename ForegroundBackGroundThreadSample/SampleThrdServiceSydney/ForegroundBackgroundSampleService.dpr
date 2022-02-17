program ForegroundBackgroundSampleService;

uses
  System.Android.ServiceApplication,
  ServiceUnit in '..\Service\ServiceUnit.pas' {FBServiceModule: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFBServiceModule, FBServiceModule);
  Application.Run;
end.
