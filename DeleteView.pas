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
unit DeleteView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MapObject;

type
  TDeleteViewForm = class(TForm)
    OK: TButton;
    Cancel: TButton;
    ViewListBox: TListBox;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DeleteViewForm: TDeleteViewForm;

implementation

{$R *.DFM}

procedure TDeleteViewForm.FormShow(Sender: TObject);
var s:string;
    index:integer;
begin
  if Map.CurrentView.Name='' then
    s:=LastViewName
  else
    s:=Map.CurrentView.Name;

  { This silly exercise is to force the stupid ListBox to
    get the correct positition.  For some reason, ItemIndex
    defaults to 0, and it cannot select the first item
    (since the ItemIndex property code bails out if setting
    to the same value). }
  index:=ViewListBox.Items.IndexOf(s);
  SendMessage(ViewListBox.Handle, LB_SETCARETINDEX, index, 0);
  SendMessage(ViewListBox.Handle, LB_SETCURSEL,     index, 0);

  ActiveControl:=ViewListBox;
end;

end.
