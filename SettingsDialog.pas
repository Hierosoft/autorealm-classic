{+------------------------------------------------------------------------+
 | AutoREALM.   Copyright (c) 2000, Andrew J. Gryc.                       |
 |                                                                        |
 | This program is free software; you can redistribute it and/or modify   |
 | it under the terms of the GNU General Public License as published by   |
 | the Free Software Foundation; either version 2 of the License, or (at  |
 | your option) any later version.                                        |
 |                                                                        |
 | This program is distributed in the hope that it will be useful, but    |
 | WITHOUT ANY WARRANTY; without even the implied warranty of             |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      |
 | General Public License for more details.                               |
 |                                                                        |
 | For a copy of the GNU General Public License, write to the Free        |
 | Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA     |
 | 02111-1307, USA.                                                       |
 +------------------------------------------------------------------------+}
unit SettingsDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, PersistentForm, ComCtrls, PoliteSpinEdit;

type
  TSettings = class(TPersistentForm)
    OK: TButton;
    Button2: TButton;
    Drawing: TPageControl;
    General: TTabSheet;
    Label1: TLabel;
    UndoLevels: TPoliteSpinEdit;
    VisualOverlays: TCheckBox;
    MakeBackups: TCheckBox;
    DraftPanning: TCheckBox;
    ColorCoded: TRadioButton;
    OverlayIcons: TRadioButton;
    DecomposePrint: TCheckBox;
    NoPrintedHyperlinks: TCheckBox;
    Selection: TTabSheet;
    Label7: TLabel;
    SelectLast: TCheckBox;
    AskForShapeClosure: TCheckBox;
    FreezeProperties: TCheckBox;
    RedundantGrouping: TCheckBox;
    CrosshairOn: TCheckBox;
    ZoomFactor: TEdit;
    AutoOrder: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    TopmostObject: TRadioGroup;
    TabSheet1: TTabSheet;
    RelativeTextSize: TCheckBox;
    RelativeIconSize: TCheckBox;
    FreehandQuality: TRadioGroup;
    cbAntiAliasSymbols: TCheckBox;
    cbFixedIconSize: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RelativeIconSizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Settings: TSettings;

implementation

uses LocalizedStrings;

{$R *.DFM}


procedure TSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var z:double;
begin
  try
    z:=StrToFloat(ZoomFactor.Text)
  except
    z:=0.0;
  end;

  if (z <= 1.0) then begin
    ShowMessage(res_settings_badzoom);
    CanClose := false;
    end
  else
    CanClose := true;
end;

procedure TSettings.RelativeIconSizeClick(Sender: TObject);
begin
  cbFixedIconSize.Enabled := Not RelativeIconSize.Checked;
  If RelativeIconSize.Checked Then cbFixedIconSize.Checked := False;
end;

procedure TSettings.FormCreate(Sender: TObject);
var
    s : String;
    j: integer;
begin
  // because of internationalization the default value '2.0' is not valid in
  // for example Holland where DecimalSeparator yields a comma :-(
  s := ZoomFactor.Text;
  for j := 1 to Length(s) do begin
     if pos(s[j],'1234567890-+ ') = 0 then s[j] := DecimalSeparator;
  end;
  ZoomFactor.Text := s;
end;

end.
