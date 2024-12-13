program AutoNameGen;

uses
  Forms,
  AutoName in 'AutoName.pas' {AutoNameDialog},
  AutoNameGenerator in 'AutoNameGenerator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAutoNameDialog, AutoNameDialog);
  Application.Run;
end.
