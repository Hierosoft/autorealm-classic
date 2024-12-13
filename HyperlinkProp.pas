unit HyperlinkProp;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Primitives, Menus;

type
  THyperlinkProperties = class(TForm)
    Panel1: TPanel;
    Text: TMemo;
    RadioHyperlink: TRadioButton;
    RadioPopup: TRadioButton;
    TextEditPopupMenu: TPopupMenu;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    CancelMenuItem: TMenuItem;
    CheckHidden: TCheckBox;
    procedure TextChange(Sender: TObject);
    procedure RadioClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure CancelMenuItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ApplyChanges(const lbl:string; compress:boolean; const attrib:TextAttrib);
  end;

var
  HyperlinkProperties: THyperlinkProperties;

procedure AssignHyperlinkAttribs(attrib:TextAttrib);

implementation

{$R *.lfm}

uses MapObject, LocalizedStrings;

procedure AssignHyperlinkAttribs(attrib:TextAttrib);
var SaveChangeProc:TNotifyEvent;
begin
  with HyperlinkProperties do begin
    if tatHyperlinkText in attrib.Valid then begin
      SaveChangeProc:=Text.OnChange;
      Text.OnChange:=nil;
      Text.Text := Attrib.HyperlinkText;
      Text.OnChange:=SaveChangeProc;
      end;
    if tatHyperlinkFlags in attrib.Valid then begin
      SaveChangeProc:=RadioHyperlink.OnClick;
      RadioHyperlink.OnClick:=nil;
      RadioPopup.OnClick:=nil;
      CheckHidden.OnClick:=nil;

      if (hyperExecute in attrib.HyperlinkFlags) then
        RadioHyperlink.Checked := true
      else
        RadioPopup.Checked := true;

      CheckHidden.Checked := (hyperHidden in attrib.HyperlinkFlags);

      RadioHyperlink.OnClick:=SaveChangeProc;
      RadioPopup.OnClick:=SaveChangeProc;
      CheckHidden.OnClick:=SaveChangeProc;
      end;
   end;
end;

procedure THyperlinkProperties.ApplyChanges(const lbl:string; compress:boolean; const attrib:TextAttrib);
var currattrib:TextAttrib;
begin
  currattrib:=Map.GetTextAttrib;

  // if any hyperlinks are selected, change them.
  if currattrib.Valid <> [] then begin
    Map.SetUndoPoint(lbl,compress);
    Map.SetTextAttrib(Map.CurrentView, attrib);
    end;

  HyperlinkProperties.ActiveControl:=Text;
end;

procedure THyperlinkProperties.CutMenuItemClick(Sender: TObject);
begin
  Text.CutToClipboard;
end;

procedure THyperlinkProperties.CopyMenuItemClick(Sender: TObject);
begin
  Text.CopyToClipboard;
end;

procedure THyperlinkProperties.PasteMenuItemClick(Sender: TObject);
begin
  Text.PasteFromClipboard;
end;

procedure THyperlinkProperties.DeleteMenuItemClick(Sender: TObject);
begin
  SendMessage(Text.Handle, WM_KEYDOWN, VK_DELETE, 0);
  SendMessage(Text.Handle, WM_KEYUP, VK_DELETE, 0);
end;

procedure THyperlinkProperties.CancelMenuItemClick(Sender: TObject);
begin
  Visible:=false;
end;

procedure THyperlinkProperties.TextChange(Sender: TObject);
var attrib:TextAttrib;
begin
  attrib.Valid:=[tatHyperlinkText];
  attrib.HyperlinkText:=Text.Text;
  ApplyChanges(res_hyperlink_change,true,attrib);
end;

procedure THyperlinkProperties.RadioClick(Sender: TObject);
var attrib:TextAttrib;
begin
  attrib.Valid:=[tatHyperlinkFlags];

  if (RadioHyperlink.Checked) then
    attrib.HyperlinkFlags:=[hyperExecute]
  else
    attrib.HyperlinkFlags:=[];

  if (CheckHidden.Checked) then
    attrib.HyperlinkFlags := attrib.HyperlinkFlags + [hyperHidden];

  ApplyChanges(res_hyperlink_change,true,attrib);
end;


end.
