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
unit movesel;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;
// Mask

type
  TMoveSelection = class(TForm)
    Cartesian: TRadioButton;
    Polar: TRadioButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    OffsetX: TEdit;
    Label1: TLabel;
    OffsetY: TEdit;
    Label2: TLabel;
    Angle: TEdit;
    Label3: TLabel;
    Dist: TEdit;
    Label4: TLabel;
    UnitCombo: TComboBox;
    Label5: TLabel;
    procedure OffsetXChange(Sender: TObject);
    procedure AngleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MoveSelection: TMoveSelection;

implementation

uses MapObject;

{$R *.dfm}

procedure TMoveSelection.OffsetXChange(Sender: TObject);
begin
  Cartesian.Checked:=true;
end;

procedure TMoveSelection.AngleChange(Sender: TObject);
begin
  Polar.Checked:=true;
end;

procedure TMoveSelection.FormShow(Sender: TObject);
begin
  Map.CurrentView.Grid.FillComboList(UnitCombo);
  UnitCombo.ItemIndex:=Map.CurrentView.Grid.CurrentGraphUnits;
end;

end.
