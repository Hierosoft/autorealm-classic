unit DefineNewSymbol;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNewSymbol = class(TForm)            
    SymbolNameText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    GroupNameList: TComboBox;
    Label3: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    chkFavorites: TCheckBox;
    SymbolCommentsText: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewSymbol: TNewSymbol;

implementation

uses SymbolLib,SymbolFile,MapObject, Main, LocalizedStrings;

{$R *.DFM}

var lastGroup:integer;
    lastComments:string;

procedure TNewSymbol.FormShow(Sender: TObject);
begin
  GroupNameList.Items := MainForm.SymbolGroupList.Items;

  if (GroupNameList.Items.Count=0) then begin
    ShowMessage(res_defsymbol_group);
    Hide;
    exit;
    end;

  if (lastGroup>GroupNameList.Items.Count) then lastgroup:=0;

  GroupNameList.ItemIndex := lastGroup;
  SymbolNameText.Text:='';
  SymbolCommentsText.Text := LastComments;
  SymbolNameText.SetFocus;
end;

procedure TNewSymbol.btnOKClick(Sender: TObject);
var grp:SymbolGroup;
    sym:Symbol;
begin
  if (GroupNameList.ItemIndex<>-1) then begin
    lastgroup:=GroupNameList.ItemIndex;
    LastComments:=NewSymbol.SymbolCommentsText.Text;
    grp := SymbolGroup(GroupNameList.Items.Objects[lastgroup]);

    sym := Symbol.Create;
    sym.Name := NewSymbol.SymbolNameText.Text;
    sym.Objects := Map.GetSelectedObjects(MainForm);
    sym.Comments := NewSymbol.SymbolCommentsText.Text;
    sym.Favorite := NewSymbol.chkFavorites.Checked;
    grp.AddSymbol(sym);

    // Refresh the current group's cached icons so we recreate the new symbol.
    MainForm.SymbolGroupListClick(self);
    // Make sure it appears in the symbol library tree
    SymbolLibraryForm.RefreshTree;
    Hide;
    end
  else begin
    ShowMessage(res_defsymbol_group_sel);
  end;
end;

procedure TNewSymbol.btnCancelClick(Sender: TObject);
begin
  Hide;
end;

begin
  lastGroup:=0;
end.
