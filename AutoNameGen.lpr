program AutoNameGen;

{$MODE Delphi}

uses
  Forms, Interfaces,
  AutoName in 'AutoName.pas' {AutoNameDialog},
  AutoNameGenerator in 'AutoNameGenerator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAutoNameDialog, AutoNameDialog);
  Application.Run;
end.
