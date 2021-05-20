program PlayWithNavigationAndGoogle;

uses
  System.StartUpCopy,
  FMX.Forms,
  PlayGoogleTabbedForm in 'Play GPs Code\PlayGoogleTabbedForm.pas' {TabbedwithNavigationForm},
  IsNavUtils in 'LibraryCode\IsNavUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedwithNavigationForm, TabbedwithNavigationForm);
  Application.Run;
end.
