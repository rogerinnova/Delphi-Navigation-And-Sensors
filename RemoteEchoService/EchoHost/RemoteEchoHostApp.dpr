program RemoteEchoHostApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormEchoHostApp in 'FormEchoHostApp.pas' {FormEchoApp},
  SvcEchoDM in '..\EchoService\SvcEchoDM.pas' {SvcDMExho: TAndroidService},
  RemoteServiceUnit in '..\..\..\..\..\..\DelphiOldDemos\Sample Services\RemoteServiceDemo\RemoteService\RemoteServiceUnit.pas' {RemoteServiceDM: TAndroidService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEchoApp, FormEchoApp);
  Application.Run;
end.
