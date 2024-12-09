unit PersistentForm;

interface

uses
  Forms, IniFiles, Classes;

type
  TPersistentForm = class(TForm)
  private
    FIniFileName: string;
    procedure SaveState;
    procedure LoadState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IniFileName: string read FIniFileName write FIniFileName;
  end;

implementation

constructor TPersistentForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniFileName := 'formstate.ini'; // Default filename
  LoadState;  // Load form state when creating
end;

destructor TPersistentForm.Destroy;
begin
  SaveState;  // Save form state before destroying
  inherited Destroy;
end;

procedure TPersistentForm.SaveState;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FIniFileName);
  try
    Ini.WriteInteger('Form', 'Left', Left);
    Ini.WriteInteger('Form', 'Top', Top);
    Ini.WriteInteger('Form', 'Width', Width);
    Ini.WriteInteger('Form', 'Height', Height);
  finally
    Ini.Free;
  end;
end;

procedure TPersistentForm.LoadState;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FIniFileName);
  try
    Left := Ini.ReadInteger('Form', 'Left', 100);  // default values
    Top := Ini.ReadInteger('Form', 'Top', 100);
    Width := Ini.ReadInteger('Form', 'Width', 800);
    Height := Ini.ReadInteger('Form', 'Height', 600);
  finally
    Ini.Free;
  end;
end;

end.

