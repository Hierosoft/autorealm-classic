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
unit SymbolLib;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, SymbolFile, TextTool;

type
  TSymbolLibraryForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    btnDefine: TButton;
    btnRevert: TButton;
    btnAddGroup: TButton;
    btnDelGroup: TButton;
    btnSaveAll: TButton;
    btnInsert: TButton;
    btnCancel: TButton;
    Panel4: TPanel;
    btnEdit: TButton;
    btnDelete: TButton;
    btnEditPic: TButton;
    chkFavorites: TCheckBox;
    Label3: TLabel;
    Label2: TLabel;
    GroupNameList: TComboBox;
    Label1: TLabel;
    SymbolNameText: TEdit;
    SymbolTree: TTreeView;
    LargeSymbol: TPaintBox;
    SymbolCommentsText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnRevertClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnSaveAllClick(Sender: TObject);
    procedure btnDelGroupClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDefineClick(Sender: TObject);
    procedure LargeSymbolPaint(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnEditPicClick(Sender: TObject);
    procedure SymbolTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function IsEditing:boolean;
    procedure SetEditing(b:boolean);
  public
    { Public declarations }
    procedure RefreshTree;
    function GetSymbol:Symbol;
    function GetGroup:SymbolGroup;
    property EditingSymbol:boolean read IsEditing write SetEditing;
    procedure LoadGroupList;
    procedure SelectGroup(grp:SymbolGroup);
  end;

var
  SymbolLibraryForm: TSymbolLibraryForm;
  CurrentLibrary: SymbolGroupList;

implementation

{$R *.DFM}

uses Main,MapObject,Primitives,NewGroup,DefineNewSymbol,SettingsDialog,Buttons,LocalizedStrings;

var SymbolDir:string;
    fUpdateTree:boolean;

function TSymbolLibraryForm.GetSymbol:Symbol;
var  node:TTreeNode;
begin
  GetSymbol:=nil;
  node:=SymbolTree.Selected;

  // Nothing selected or group selected
  if (node=nil) or (node.Parent=nil) then exit;

  GetSymbol := Symbol(node.Data);
end;

function TSymbolLibraryForm.GetGroup:SymbolGroup;
var  node:TTreeNode;
begin
  GetGroup:=nil;

  node:=SymbolTree.Selected;
  if node<>nil then begin
    if node.Parent<>nil then begin
      node:=node.Parent;
      end;
    GetGroup:=SymbolGroup(node.Data);
  end;
end;


procedure TSymbolLibraryForm.RefreshTree;
var grp:SymbolGroup;
    sym:Symbol;
    parentnode,node:TTreeNode;
    index:integer;
begin
  Screen.Cursor := crHourglass;
  SymbolTree.Items.BeginUpdate;          // Supress updates
  SymbolTree.Items.Clear;
  GroupNameList.Items.Clear;

  grp := CurrentLibrary.FirstGroup;

  // For every group in the symbol library
  while (grp<>nil) do begin
    index:=GroupNameList.Items.Add(grp.GroupName);
    GroupNameList.Items.Objects[index]:=grp;

    parentnode := SymbolTree.Items.Add(nil,grp.GroupName);
    parentnode.Data := grp;

    // For every symbol in the group
    sym:=grp.FirstSymbol;
    while (sym<>nil) do begin
      node:=SymbolTree.Items.AddChild(parentnode, sym.Name);
      node.Data := sym;
      sym := grp.NextSymbol(sym);
    end;

    grp:=CurrentLibrary.NextGroup(grp);
  end;

  // Force a Resort
  SymbolTree.SortType := stNone;
  SymbolTree.SortType := stText;
  SymbolTree.Items.EndUpdate;
  Screen.Cursor := crDefault;

  fUpdateTree := false;
end;

procedure TSymbolLibraryForm.SelectGroup(grp:SymbolGroup);
var IconList:TListView;
    sym:Symbol;

   procedure CreateSymbolImage(sym:Symbol);
   var w,h:integer;
   begin
     w := MainForm.FavoritesImageList.Width;
     h := MainForm.FavoritesImageList.Height;

     // Create image bitmap, store into the imagelist linked with the icon view,
     // and save the index into the symbol for later.
     sym.ImageIndex := MainForm.FavoritesImageList.Add(
                  sym.CreateBitmap(w,h,Settings.cbAntiAliasSymbols.Checked),nil);
   end;

   procedure CreateSymbolListEntry(sym:Symbol);
   var ListItem:TListItem;
   begin
     ListItem := IconList.Items.Add;
     ListItem.Data := sym;

     // Add space to front of caption to sort "Favorites" first.
     // Previously Favorite status was the only way to show the icons in the toolbar.
     // Now, favorites status just percolates the icons to the front of the list.
     if (sym.Favorite) then
       ListItem.Caption := ' ' + sym.Name
     else
       ListItem.Caption := sym.Name;

     ListItem.ImageIndex := sym.ImageIndex;
   end;

begin
  If Grp = Nil Then Exit;
   
  Screen.Cursor := crHourglass;

  IconList  := MainForm.SymbolIconList;

  // Create a cached icon for any symbols that haven't been cached yet.
  // Note that this also includes anytime the group hasn't been loaded:
  // if this is the first time the group has been touched, we load
  // the group implicitly when we start walking the group list.
  // load all the icons, and make images for them.
  sym:=grp.FirstSymbol;
  while (sym<>nil) do begin
    if (sym.ImageIndex=-1) then CreateSymbolImage(sym);

    sym := grp.NextSymbol(sym);
    end;

  // Reload the view list with the icons appropriate for this group.
  IconList.Items.Clear;

  // Add all the symbols
  sym:=grp.FirstSymbol;
  while (sym<>nil) do begin
    CreateSymbolListEntry(sym);
    sym := grp.NextSymbol(sym);
    end;

  IconList.Arrange(arAlignLeft);

  Screen.Cursor := crDefault;
end;

procedure TSymbolLibraryForm.LoadGroupList;
var GroupList:TListBox;
    grp:SymbolGroup;

begin
  Screen.Cursor := crHourglass;

  GroupList := MainForm.SymbolGroupList;
  GroupList.Clear;

  grp := CurrentLibrary.FirstGroup;

  // For every group in the symbol library, create an entry in the listbox
  while (grp<>nil) do begin
    GroupList.Items.AddObject(grp.GroupName, grp);

    grp:=CurrentLibrary.NextGroup(grp);
  end;

  // Set our group index to 0 (select the first group),
  // and pre-load that page.
  GroupList.ItemIndex := 0;
  SelectGroup(CurrentLibrary.FirstGroup);

  Screen.Cursor := crDefault;
end;


procedure TSymbolLibraryForm.FormCreate(Sender: TObject);
begin
  SymbolDir := ExtractFilePath(ParamStr(0)) + 'Library\';
  CurrentLibrary := SymbolGroupList.Create;
  CurrentLibrary.Clear;
  CurrentLibrary.Load(SymbolDir);
  LoadGroupList;
  fUpdateTree := true;
end;


procedure TSymbolLibraryForm.btnRevertClick(Sender: TObject);
begin
  if Application.MessageBox(pchar(res_symbollib_lose),
                         pchar(res_symbollib_lose_title),
                         MB_YESNO) = IDYES then begin
    MainForm.FavoritesImageList.Clear;
    CurrentLibrary.Load(SymbolDir);
    LoadGroupList;
    RefreshTree;
  end;
end;

procedure TSymbolLibraryForm.FormDestroy(Sender: TObject);
begin
  CurrentLibrary.Save(SymbolDir);
end;

procedure TSymbolLibraryForm.btnAddGroupClick(Sender: TObject);
begin
  Application.CreateForm(TAddSymbolGroup, AddSymbolGroup);

  if AddSymbolGroup.ShowModal = mrOK then begin
    CurrentLibrary.NewGroup(AddSymbolGroup.NewGroupName.Text);
    LoadGroupList;
    RefreshTree;
  end;

  AddSymbolGroup.Free;
end;

procedure TSymbolLibraryForm.btnSaveAllClick(Sender: TObject);
begin
  CurrentLibrary.Save(SymbolDir);
end;

procedure TSymbolLibraryForm.btnDelGroupClick(Sender: TObject);
var selgroup:SymbolGroup;
    msg:string;
begin
  selgroup:=GetGroup;

  if (selgroup<>nil) then begin
    msg := Format (res_symbollib_del,[selGroup.GroupName]);
    if Application.MessageBox(PChar(msg),PChar(res_symbollib_del_title),MB_YESNO) = IDYES then begin
      CurrentLibrary.DeleteGroup(selgroup);
      LoadGroupList;
      RefreshTree;
      end;
    end;
end;

procedure TSymbolLibraryForm.btnInsertClick(Sender: TObject);
var sym:Symbol;
begin
  sym:=GetSymbol;

  if (sym<>nil) then begin
    TextTool.InsertSymbol := sym;
    MainForm.aPlaceOneExecute(self);
    SymbolLibraryForm.Hide;
  end;
end;

procedure TSymbolLibraryForm.btnDefineClick(Sender: TObject);
begin
  Application.CreateForm(TNewSymbol, NewSymbol);
  NewSymbol.ShowModal;
  NewSymbol.Free;
end;

procedure TSymbolLibraryForm.LargeSymbolPaint(Sender: TObject);
var  selsym:Symbol;
begin
  selsym := GetSymbol;

  if (selsym<>nil) then begin
    selsym.DrawImage(LargeSymbol.Canvas,LargeSymbol.Width,LargeSymbol.Height, 0.0);
  end;
end;

procedure TSymbolLibraryForm.btnCancelClick(Sender: TObject);
begin
  SymbolLibraryForm.Hide;
end;

procedure TSymbolLibraryForm.btnDeleteClick(Sender: TObject);
var grp:SymbolGroup;
    sym:Symbol;
    msg:string;
begin
  // This button does double duty: if not editing, then it
  // deletes the symbol.  If editing, then it cancels the
  // edit.

  if EditingSymbol then begin
    // Turn off editing mode
    EditingSymbol:=false;
    // Re-click in the tree view to restore old properties
    SymbolTreeChange(self,nil);
    end
  else begin
    // This is a delete comment: remove the symbol.
    grp:=GetGroup;
    sym:=GetSymbol;

    if (grp<>nil) and (sym<>nil) then begin
      msg := Format(res_symbollib_del_symbol ,[sym.Name,grp.GroupName]);
      if Application.MessageBox(PChar(msg),pchar(res_symbollib_del_symbol_title),MB_YESNO) = IDYES then begin
        // They've deleted the symbol: clear the cached bitmap from the image list.
        CurrentLibrary.RemoveCachedImage(sym.ImageIndex);

        // Remove the symbol
        grp.DeleteSymbol(sym);

        // Refresh the symbol library's tree
        RefreshTree;

        // Refresh the favorites page
        MainForm.SymbolGroupListClick(self);
      end;
    end;
  end;
end;

function TSymbolLibraryForm.IsEditing:boolean;
begin
  IsEditing := SymbolNameText.Enabled;
end;

procedure TSymbolLibraryForm.SetEditing(b:boolean);
begin
  if b then begin
    // Change buttons to editing mode
    // and enable entry into boxes.
    SymbolNameText.Enabled := true;
    GroupNameList.Enabled := true;
    SymbolCommentsText.Enabled := true;
    chkFavorites.Enabled := true;

    // Don't allow moving in the list while editing.
    SymbolTree.Enabled := false;

    // Replace the button captions
    btnEdit.Caption := res_symbollib_caption_done;
    btnDelete.Caption := res_symbollib_caption_cancel;
    end
  else begin
    // Restore old edit/button state
    // and prevent entry.
    SymbolNameText.Enabled := false;
    GroupNameList.Enabled := false;
    SymbolCommentsText.Enabled := false;
    chkFavorites.Enabled := false;

    SymbolTree.Enabled := true;
    btnEdit.Caption := res_symbollib_caption_edit;
    btnDelete.Caption := res_symbollib_caption_del;
  end;
end;


procedure TSymbolLibraryForm.btnEditClick(Sender: TObject);
var sym:Symbol;
    oldgrp,newgrp:SymbolGroup;
    refresh:boolean;
begin
  if not EditingSymbol then begin
    EditingSymbol := true;
    end
  else begin
    EditingSymbol := false;

    // Perform the symbol edits
    sym := GetSymbol;

    if (sym<>nil) then begin
      refresh := false;

      // Need to update the treeview if the name changed.
      if (sym.Name<>SymbolNameText.Text) then refresh:=true;

      sym.Name := SymbolNameText.Text;
      sym.Comments:=SymbolCommentsText.Text;
      sym.Favorite:=chkFavorites.Checked;

      // We allow them to change the group to
      // move the symbol: delete and re-add the
      // symbol to the (possibly) new group.
      newgrp:=SymbolGroup(GroupNameList.Items.Objects[GroupNameList.ItemIndex]);
      oldgrp:=GetGroup;

      // If the group name changed, move the symbol between groups
      // by deleting it from the old group and adding to the new one.
      if (oldgrp<>nil) and (oldgrp<>newgrp) then begin
        // Use 'false' to Delete without ownership: we still retain the symbol
        oldgrp.DeleteSymbol(sym,false);
        newgrp.AddSymbol(sym);

        // The group we moved the symbol from was modified; make sure
        // we save it.
        oldgrp.Modified := true;

        // Reconstruct the tree view
        refresh:=true;
        end;

      // Set the group as having been modified so we'll save it.
      newgrp.Modified := true;

      // If we need to refresh the tree, do it now
      if refresh then RefreshTree;
    end;
  end;
end;

procedure TSymbolLibraryForm.btnEditPicClick(Sender: TObject);
var sym:Symbol;
    grp:SymbolGroup;
begin
  // Perform the symbol edits by entering into a special edit mode
  // and hiding the existing map.
  sym := GetSymbol;
  grp := GetGroup;

  if (sym<>nil) and (grp<>nil) then begin
    SymbolLibraryForm.Hide;         // Get out of symbol library mode
    grp.Modified := true;           // Mark the group as changed (even if they back out, we'll save the old one again)

    MainForm.InvokeSymbolEditor(grp.GroupName + ':' + sym.Name, sym);
    end;
end;

procedure TSymbolLibraryForm.SymbolTreeChange(Sender: TObject; Node: TTreeNode);
var selsym:Symbol;
    grp:SymbolGroup;
begin
  selsym := GetSymbol;

  if selsym<>nil then begin
    SymbolNameText.Text:=selsym.Name;
    grp:=GetGroup;

    if (grp<>nil) then
       GroupNameList.ItemIndex := GroupNameList.Items.IndexOf(grp.GroupName)
    else
      GroupNameList.ItemIndex:=-1;

    SymbolCommentsText.Text:=selsym.Comments;
    chkFavorites.Checked := selsym.Favorite;
    LargeSymbol.Repaint;
    end
  else begin
    SymbolNameText.Text:='';
    GroupNameList.Text:='';
    SymbolCommentsText.Text:='';
    chkFavorites.Checked := false;
    LargeSymbol.Repaint;
  end;
end;

procedure TSymbolLibraryForm.FormShow(Sender: TObject);
begin
  if fUpdateTree then RefreshTree;
end;

end.
