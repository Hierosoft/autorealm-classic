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
unit Scale;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, PoliteSpinEdit;

type
  TScaleForm = class(TForm)
    UniformRadioBtn: TRadioButton;
    UniformScaling: TPoliteSpinEdit;
    AsymmetricRadioBtn: TRadioButton;
    HorizontalScaling: TPoliteSpinEdit;
    VerticalScaling: TPoliteSpinEdit;
    HorizontalLabel: TLabel;
    VerticalLabel: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    UPercent: TLabel;
    HPercent: TLabel;
    VPercent: TLabel;
    procedure UniformRadioBtnClick(Sender: TObject);
    procedure AsymmetricRadioBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScaleForm: TScaleForm;

implementation

{$R *.dfm}


procedure TScaleForm.UniformRadioBtnClick(Sender: TObject);
begin
  UniformScaling.Enabled:=true;
  UPercent.Enabled:=true;
  HorizontalScaling.Enabled:=false;
  HorizontalLabel.Enabled:=false;
  HPercent.Enabled:=false;
  VerticalScaling.Enabled:=false;
  VerticalLabel.Enabled:=false;
  VPercent.Enabled:=false;
end;

procedure TScaleForm.AsymmetricRadioBtnClick(Sender: TObject);
begin
  UniformScaling.Enabled:=false;
  UPercent.Enabled:=false;
  HorizontalScaling.Enabled:=true;
  HorizontalLabel.Enabled:=true;
  HPercent.Enabled:=true;
  VerticalScaling.Enabled:=true;
  VerticalLabel.Enabled:=true;
  VPercent.Enabled:=true;
end;

end.
