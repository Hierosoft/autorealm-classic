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
unit Skew;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, PoliteSpinEdit;

type
  TSkewForm = class(TForm)
    HorizontalLabel: TLabel;
    VerticalLabel: TLabel;
    HorizontalSkew: TPoliteSpinEdit;
    VerticalSkew: TPoliteSpinEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    HPercent: TLabel;
    VPercent: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkewForm: TSkewForm;

implementation

{$R *.dfm}


end.
