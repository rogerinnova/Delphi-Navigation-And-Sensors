program AndroidExamineSensors;

uses
  System.StartUpCopy,
  FMX.Forms,
  SensorTabbedFormwithNavigation in '..\MobileSensorApp\SensorTabbedFormwithNavigation.pas' {TabbedwithNavigationForm},
  IsFmxGraphics in '..\LibraryCode\IsFmxGraphics.pas',
  IsMobileSensors in '..\LibraryCode\IsMobileSensors.pas',
  IsNavUtils in '..\LibraryCode\IsNavUtils.pas',
  IsPermissions in '..\LibraryCode\IsPermissions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedwithNavigationForm, TabbedwithNavigationForm);
  Application.Run;
end.
