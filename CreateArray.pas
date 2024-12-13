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
unit CreateArray;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MapObject, StdCtrls, Spin;

type
  TArrayForm = class(TForm)
    GroupBox1: TGroupBox;
    HorzCount: TSpinEdit;
    HorzSpace: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    HorzUnitCombo: TComboBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    VertCount: TSpinEdit;
    VertSpace: TEdit;
    VertUnitCombo: TComboBox;
    Button1: TButton;
    Button2: TButton;
    HorzEvery: TRadioButton;
    HorzBetween: TRadioButton;
    VertEvery: TRadioButton;
    VertBetween: TRadioButton;
    grpEllipse: TGroupBox;
    cbEllipse: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    cbEllipseHUnits: TComboBox;
    cbEllipseVUnits: TComboBox;
    edtHRadius: TEdit;
    edtVRadius: TEdit;
    cbRotate: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure cbEllipseClick(Sender: TObject);
  private
    { Private declarations }
    Procedure EnableGUI;
  public
    { Public declarations }
  end;

var
  ArrayForm: TArrayForm;

implementation

{$R *.lfm}

procedure TArrayForm.FormShow(Sender: TObject);
begin
  Map.CurrentView.Grid.FillComboList(HorzUnitCombo);
  HorzUnitCombo.ItemIndex:=Map.CurrentView.Grid.CurrentGraphUnits;

  Map.CurrentView.Grid.FillComboList(VertUnitCombo);
  VertUnitCombo.ItemIndex:=Map.CurrentView.Grid.CurrentGraphUnits;

  // JD 7-30-02

  Map.CurrentView.Grid.FillComboList(cbEllipseHUnits);
  cbEllipseHUnits.ItemIndex:=Map.CurrentView.Grid.CurrentGraphUnits;

  Map.CurrentView.Grid.FillComboList(cbEllipseVUnits);
  cbEllipseVUnits.ItemIndex:=Map.CurrentView.Grid.CurrentGraphUnits;

  EnableGUI;
end;

procedure TArrayForm.cbEllipseClick(Sender: TObject);
begin
  EnableGUI;
end;

Procedure TArrayForm.EnableGUI;
Var B: Boolean;
Begin
  B                       := cbEllipse.Checked;
  grpEllipse.Enabled      := B;
  cbRotate.Enabled        := B;
  edtHRadius.Enabled      := B;
  edtVRadius.Enabled      := B;
  cbEllipseHUnits.Enabled := B;
  cbEllipseVUnits.Enabled := B;
  HorzBetween.Enabled     := Not B;
  HorzEvery.Enabled       := Not B;
  HorzSpace.Enabled       := Not B;
  HorzUnitCombo.Enabled   := Not B;
End; // EnableGUI

end.
