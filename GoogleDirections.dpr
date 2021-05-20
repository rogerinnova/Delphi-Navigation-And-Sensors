program GoogleDirections;

uses
  System.StartUpCopy,
  FMX.Forms,
  GoogleDirForm in 'GoogleDirections\GoogleDirForm.pas' {Directions},
  IsNavUtils in 'LibraryCode\IsNavUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDirections, Directions);
  Application.Run;
end.
