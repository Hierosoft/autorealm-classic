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
unit ChartGrid;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TChartGridDialog = class(TForm)
    OK: TButton;
    Cancel: TButton;
    PerimeterGroup: TGroupBox;
    Perimeter8: TRadioButton;
    Perimeter16: TRadioButton;
    Perimeter32: TRadioButton;
    PerimeterOther: TRadioButton;
    Other: TSpinEdit;
    GroupBox1: TGroupBox;
    ExtendToCrop: TCheckBox;
    SkipLabel: TLabel;
    SkipPoints: TSpinEdit;
    CropRosette: TCheckBox;
    procedure Perimeter8Click(Sender: TObject);
    procedure Perimeter16Click(Sender: TObject);
    procedure Perimeter32Click(Sender: TObject);
    procedure OtherChange(Sender: TObject);
    procedure CropRosetteClick(Sender: TObject);
    procedure PerimeterOtherClick(Sender: TObject);
    procedure OtherKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetPerimeterPoints(n:integer);
  end;

var
  ChartGridDialog: TChartGridDialog;

implementation

{$R *.lfm}

procedure TChartGridDialog.SetPerimeterPoints(n:integer);
begin
  n:= (n div 2) - 1;

  if (n=0) then begin
    SkipLabel.Enabled:=false;
    SkipPoints.Enabled:=false;
    end
  else begin
    SkipLabel.Enabled:=true;
    SkipPoints.Enabled:=true;
    SkipPoints.MaxValue := n;
    end;
end;

procedure TChartGridDialog.Perimeter8Click(Sender: TObject);
begin
  SetPerimeterPoints(8);
end;

procedure TChartGridDialog.Perimeter16Click(Sender: TObject);
begin
  SetPerimeterPoints(16);
end;

procedure TChartGridDialog.Perimeter32Click(Sender: TObject);
begin
  SetPerimeterPoints(32);
end;

procedure TChartGridDialog.PerimeterOtherClick(Sender: TObject);
begin
  if (Other.Text<>'') then SetPerimeterPoints(Other.Value);
end;

procedure TChartGridDialog.OtherKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Other.Text<>'') then SetPerimeterPoints(Other.Value);
end;

procedure TChartGridDialog.OtherChange(Sender: TObject);
begin
  PerimeterOther.Checked:=true;
  if (Other.Text<>'') then SetPerimeterPoints(Other.Value);
end;

procedure TChartGridDialog.CropRosetteClick(Sender: TObject);
begin
  ExtendToCrop.Enabled := CropRosette.Checked;
end;

end.
