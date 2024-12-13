unit NewGroup;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAddSymbolGroup = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    NewGroupName: TEdit;
    procedure NewGroupNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddSymbolGroup: TAddSymbolGroup;

implementation

{$R *.lfm}

const BadCharsInFilename='<>:"/\|';

procedure TAddSymbolGroup.NewGroupNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (pos(key,BadCharsInFilename)<>0) then Key := #0;
end;

procedure TAddSymbolGroup.FormShow(Sender: TObject);
begin
  NewGroupName.Text := '';
  NewGroupName.SetFocus;
end;

end.
