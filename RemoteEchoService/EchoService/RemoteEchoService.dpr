program RemoteEchoService;

uses
  System.Android.ServiceApplication,
  SvcEchoDM in 'SvcEchoDM.pas' {SvcDMExho: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSvcDMExho, SvcDMExho);
  Application.Run;
end.
