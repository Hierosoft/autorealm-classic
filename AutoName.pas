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
unit AutoName;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Buttons, ExtCtrls;

type
  TAutoNameDialog = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GenerateButton: TSpeedButton;
    CheckAllButton: TSpeedButton;
    CopyButton: TSpeedButton;
    Panel3: TPanel;
    RuleFile: TComboBox;
    GeneratedList: TCheckListBox;
    PasteButton: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure RuleFileChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure CheckAllButtonClick(Sender: TObject);
    procedure GeneratedListClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RemoveUnchecked;
  end;

var
  AutoNameDialog: TAutoNameDialog;

implementation

{$R *.lfm}

uses AutoNameGenerator,SelectFont,Clipbrd,LocalizedStrings;

var RULDirectory:string;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.FormShow
//
// Purpose: Finds all RUL files in the program directory and adds them to
//          the list box.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.FormShow(Sender: TObject);
var search:TSearchRec;
    path:string;
    err:integer;
    s:string;
begin
  RemoveUnchecked;
  RuleFile.Items.Clear;
  path:=RULDirectory + '*.rul';
  err:=FindFirst(path,0,search);
  while (err=0) do begin
    s:=copy(search.Name,1,length(search.Name)-4);
    RuleFile.Items.Add(s);
    err:=FindNext(search);
    end;
  FindClose(search);
  RuleFile.ItemIndex:=0;
  RuleFileChange(Sender);
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.RuleFileChange
//
// Purpose: Switches to the rul file indicated when clicked, provided it
//          is a valid one that can be opened.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.RuleFileChange(Sender: TObject);
var s:string;
begin
  s:=RuleFile.Text;
  if not OpenRuleFile(RULDirectory, s) then begin
    ShowMessage(Format(res_autoname_nopen,[s]));
    exit;
    end;

  GenerateButton.Click;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.FormHide
//
// Purpose: Hides the form and closes the open RUL file.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.FormHide(Sender: TObject);
begin
  CloseRuleFile;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.RemoveUnchecked
//
// Purpose: Called when the user clicks the "Generate" button, keeps only
//          the names in the list that are checked.  All unchecked items
//          are deleted.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.RemoveUnchecked;
var i:integer;
begin
  i:=0;
  while (i<GeneratedList.Items.Count) do begin
    if GeneratedList.Checked[i] then
       inc(i)
    else
       GeneratedList.Items.Delete(i);
    end;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.GenerateButtonClick
//
// Purpose: Generates a bunch of new names using the existing rul set.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.GenerateButtonClick(Sender: TObject);
var i:integer;
    s:string;
begin
  RemoveUnchecked;
  for i:=1 to 50 do begin
    s:=GenerateRule;
    if (s<>'') then GeneratedList.Items.Add(s);
    end;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.CheckAllButtonClick
//
// Purpose: Checks (or unchecks) all the names, depending on whether
//          all of them are already checked or not.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.CheckAllButtonClick(Sender: TObject);
var i:integer;
    state:boolean;
begin
  state:=false;
  // Figure out if any are not checked
  for i:=0 to GeneratedList.Items.Count-1 do
      if not GeneratedList.Checked[i] then state:=true;

  for i:=0 to GeneratedList.Items.Count-1 do
      GeneratedList.Checked[i]:=state;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.GeneratedListClick
//
// Purpose: Copies the item directly into the Text toolbar for placement
//          into the map.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.GeneratedListClick(Sender: TObject);
begin
  ChooseFont.TextContent.Text:=GeneratedList.Items[GeneratedList.ItemIndex];
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.CopyButtonClick
//
// Purpose: Copies all names (or only checked ones) into the clipboard.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.CopyButtonClick(Sender: TObject);
var i:integer;
    s:string;
begin
  s:='';
  for i:=0 to GeneratedList.Items.Count-1 do begin
    if GeneratedList.Checked[i] then s:=s+GeneratedList.Items[i]+#13#10;
    end;

  // If none are checked, then add them all.
  if s='' then begin
    for i:=0 to GeneratedList.Items.Count-1 do begin
      s:=s+GeneratedList.Items[i]+#13#10;
      end;
    end;

  Clipboard.AsText:=s;
end;

//--------------------------------------------------------------------------
// Name: TAutoNameDialog.PasteButtonClick
//
// Purpose: Pastes names from the clipboard into the AutoName window.
//
// Notes: I found this ability useful to do the following: copying some
//        auto generated names I almost liked, pasting into Notepad, editing
//        to my satisfaction, and pasting back.  Then, you can directly
//        place them onto the map.
//--------------------------------------------------------------------------
procedure TAutoNameDialog.PasteButtonClick(Sender: TObject);
var s,t:string;
    p:integer;
begin
  RemoveUnchecked;
  s:=Clipboard.AsText;

  // Keep going until we've exhausted the string
  while (s<>'') do begin
    // Find the CR/LF at the end of the string.
    p:=pos(#10,s);

    if (p=0) then begin
      // No LF?  Must be the last choice
      t:=s; s:='';
      end
    else begin
      // Found an LF: delete it and the preceeding
      // CR (note that we assume it is in CR/LF format)
      t:=copy(s,1,p-1);
      delete(s,1,p);
      end;

    // Add to the list box
    t:=Trim(t);
    if (t<>'') then GeneratedList.Items.Add(t);
    end;
end;

begin
  RULDirectory := ExtractFilePath(ParamStr(0))+'AutoNAME\';
end.
