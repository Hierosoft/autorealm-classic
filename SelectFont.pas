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
unit SelectFont;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, Primitives;

type
  TChooseFont = class(TForm)
    Panel1: TPanel;
    FontName: TComboBox;
    FontSize: TComboBox;
    BoldButton: TSpeedButton;
    ItalicButton: TSpeedButton;
    UnderlineButton: TSpeedButton;
    AlignLeftBtn: TSpeedButton;
    AlignCenterBtn: TSpeedButton;
    AlignRightBtn: TSpeedButton;
    TextEditPopupMenu: TPopupMenu;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    CancelMenuItem: TMenuItem;
    TextContent: TMemo;
    procedure ApplyChanges(const lbl:string; compress:boolean; const attrib:TextAttrib);
    procedure FormCreate(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure AlignLeftBtnClick(Sender: TObject);
    procedure AlignCenterBtnClick(Sender: TObject);
    procedure AlignRightBtnClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure CancelMenuItemClick(Sender: TObject);
    procedure TextContentEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TextContentChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChooseFont: TChooseFont;

procedure AssignTextAttribs(attrib:TextAttrib);

implementation

uses Geometry, MAIN, MapObject, SettingsDialog, LocalizedStrings;

const FontSizeList:array[1..24] of integer=
(4,5,6,7,8,9,10,12,14,16,18,20,24,28,30,32,36,40,48,56,64,72,80,96);

{$R *.lfm}

procedure AssignTextAttribs(attrib:TextAttrib);
var SaveChangeProc:TNotifyEvent;
    size:integer;
begin
  with ChooseFont do begin
    if tatText in attrib.Valid then begin
      SaveChangeProc:=TextContent.OnChange;
      TextContent.OnChange:=nil;
      TextContent.Text := Attrib.Text;
      TextContent.OnChange:=SaveChangeProc;
      end;
    if tatFontName in attrib.Valid then begin
      SaveChangeProc:=FontName.OnChange;
      FontName.OnChange:=nil;
      FontName.Text := Attrib.FontName;
      TextContent.Font.Name:=Attrib.FontName;
      FontName.OnChange:=SaveChangeProc;
      end;

    if tatFontSize in attrib.Valid then begin
      size := Attrib.FontSize;
      SaveChangeProc:=FontSize.OnChange;
      FontSize.OnChange:=nil;
      if (size<>0) then begin
        FontSize.ItemIndex := FontSize.Items.IndexOf(IntToStr(size));
        TextContent.Font.Size:=size;
        end;
      FontSize.OnChange:=SaveChangeProc;
      end;
    if tatFontBold in attrib.Valid then begin
      SaveChangeProc:=BoldButton.OnClick;
      BoldButton.OnClick:=nil;
      BoldButton.Down:=attrib.FontBold;

      TextContent.Font.Style := TextContent.Font.Style - [fsBold];
      if attrib.FontBold then
         TextContent.Font.Style := TextContent.Font.Style + [fsBold];

      BoldButton.OnClick:=SaveChangeProc;
      end;
    if tatFontItalic in attrib.Valid then begin
      SaveChangeProc:=ItalicButton.OnClick;
      ItalicButton.OnClick:=nil;
      ItalicButton.Down:=attrib.FontItalic;

      TextContent.Font.Style := TextContent.Font.Style - [fsItalic];
      if attrib.FontItalic then
         TextContent.Font.Style := TextContent.Font.Style + [fsItalic];

      ItalicButton.OnClick:=SaveChangeProc;
      end;
    if tatFontUnderline in attrib.Valid then begin
      SaveChangeProc:=UnderlineButton.OnClick;
      UnderlineButton.OnClick:=nil;
      UnderlineButton.Down:=attrib.FontUnderline;
      UnderlineButton.OnClick:=SaveChangeProc;
      TextContent.Font.Style := TextContent.Font.Style - [fsUnderline];
      if attrib.FontUnderline then
         TextContent.Font.Style := TextContent.Font.Style + [fsUnderline];
      end;
    if tatAlignment in attrib.Valid then begin
      AlignLeftBtn.OnClick:=nil;
      AlignCenterBtn.OnClick:=nil;
      AlignRightBtn.OnClick:=nil;
      case attrib.Alignment of
        DT_LEFT:   begin
                     AlignLeftBtn.Down:=true;
                     TextContent.Alignment:=taLeftJustify;
                   end;
        DT_CENTER: begin
                     AlignCenterBtn.Down:=true;
                     TextContent.Alignment:=taCenter;
                   end;
        DT_RIGHT:  begin
                     AlignRightBtn.Down:=true;
                     TextContent.Alignment:=taRightJustify;
                   end;
        end;
      AlignLeftBtn.OnClick:=AlignLeftBtnClick;
      AlignCenterBtn.OnClick:=AlignCenterBtnClick;
      AlignRightBtn.OnClick:=AlignRightBtnClick;
      end;
    end;
end;



function FontEnumCallback(logfont:PLogFont; newtextmetric:PNewTextMetric; FontType:DWord; dwUser:DWord):integer; stdcall;
begin
  if (FontType and TRUETYPE_FONTTYPE)<>0 then begin
    ChooseFont.FontName.Items.Add(logfont^.lfFaceName);
    end;

  Result:=1;   { non-zero continues enumeration }
end;

procedure TChooseFont.FormCreate(Sender: TObject);
var i:integer;
begin
  Top:=0;
  Left:=0;

  EnumFontFamilies(Canvas.Handle, nil, @FontEnumCallback, 0);

  for i:=Low(FontSizeList) to High(FontSizeList) do begin
    FontSize.Items.Add(IntToStr(FontSizeList[i]));
    end;

  FontName.ItemIndex := FontName.Items.IndexOf('Arial');
  FontSize.ItemIndex := FontSize.Items.IndexOf('12');
end;


procedure TChooseFont.ApplyChanges(const lbl:string; compress:boolean; const attrib:TextAttrib);
var currattrib:TextAttrib;
begin
  currattrib:=Map.GetTextAttrib;
  // What we're really asking is if any text items are selected.
  // Don't repaint if unnecessary.
  if currattrib.Valid <> [] then begin
    Map.SetUndoPoint(lbl,compress);
    if Map.SetTextAttrib(Map.CurrentView, attrib) then MainForm.RefreshExtent;
    end;
  ChooseFont.ActiveControl:=TextContent;
end;

procedure TChooseFont.FontNameChange(Sender: TObject);
var attrib:TextAttrib;
begin
  TextContent.Font.Name := FontName.Text;

  attrib.Valid:=[tatFontName];
  attrib.FontName:=FontName.Text;
  ApplyChanges(res_selectfont_font_name, false, attrib);
end;

procedure TChooseFont.FontSizeChange(Sender: TObject);
var attrib:TextAttrib;
begin
  TextContent.Font.Size := StrToIntDef(FontSize.Text,10);

  attrib.Valid:=[tatFontSize];
  attrib.FontSize:=TextContent.Font.Size;
  ApplyChanges(res_selectfont_font_size, false, attrib);
end;

procedure TChooseFont.BoldButtonClick(Sender: TObject);
var attrib:TextAttrib;
begin
  TextContent.Font.Style := TextContent.Font.Style - [fsBold];
  if BoldButton.Down then
    TextContent.Font.Style := TextContent.Font.Style + [fsBold];

  attrib.Valid:=[tatFontBold];
  attrib.FontBold:=BoldButton.Down;
  ApplyChanges(res_selectfont_font_bold, false, attrib);
end;

procedure TChooseFont.ItalicButtonClick(Sender: TObject);
var attrib:TextAttrib;
begin
  TextContent.Font.Style := TextContent.Font.Style - [fsItalic];
  if ItalicButton.Down then
    TextContent.Font.Style := TextContent.Font.Style + [fsItalic];

  attrib.Valid:=[tatFontItalic];
  attrib.FontItalic:=ItalicButton.Down;
  ApplyChanges(res_selectfont_font_italic,false, attrib);
end;

procedure TChooseFont.UnderlineButtonClick(Sender: TObject);
var attrib:TextAttrib;
begin
  TextContent.Font.Style := TextContent.Font.Style - [fsUnderline];
  if UnderlineButton.Down then
    TextContent.Font.Style := TextContent.Font.Style + [fsUnderline];

  attrib.Valid:=[tatFontUnderline];
  attrib.FontUnderline:=UnderlineButton.Down;
  ApplyChanges(res_selectfont_font_underline,false, attrib);
end;

procedure TChooseFont.AlignLeftBtnClick(Sender: TObject);
var {align:TParaAttributes;}
    attrib:TextAttrib;
begin
{  align:=TParaAttributes.Create(TextContent);
  align.Alignment := taLeftJustify;
  TextContent.Paragraph.Assign(align);
  align.Free;}
  TextContent.Alignment:=taLeftJustify;

  attrib.Valid:=[tatAlignment];
  attrib.Alignment:=DT_LEFT;
  ApplyChanges(res_selectfont_align_left,false, attrib);
end;

procedure TChooseFont.AlignCenterBtnClick(Sender: TObject);
var {align:TParaAttributes; }
    attrib:TextAttrib;
begin
{  align:=TParaAttributes.Create(TextContent);
  align.Alignment := taCenter;
  TextContent.Paragraph.Assign(align);
  align.Free;}
  TextContent.Alignment:=taCenter;

  attrib.Valid:=[tatAlignment];
  attrib.Alignment:=DT_CENTER;
  ApplyChanges(res_selectfont_align_center,false, attrib);
end;

procedure TChooseFont.AlignRightBtnClick(Sender: TObject);
var {align:TParaAttributes; }
    attrib:TextAttrib;
begin
  {align:=TParaAttributes.Create(TextContent);
  align.Alignment := taRightJustify;
  TextContent.Paragraph.Assign(align);
  align.Free;}
  TextContent.Alignment:=taRightJustify;

  attrib.Valid:=[tatAlignment];
  attrib.Alignment:=DT_RIGHT;
  ApplyChanges(res_selectfont_align_right,false, attrib);
end;

procedure TChooseFont.TextContentChange(Sender: TObject);
var attrib:TextAttrib;
begin
  attrib.Valid:=[tatText];
  attrib.Text:=TextContent.Text;
  ApplyChanges(res_selectfont_text_change,true,attrib);

  // Select the text tool if they type and it isn't already selected.
  with MainForm do begin
    if (not aTextOut.Checked) and (not aCurvedText.Checked) then begin
      aTextOutExecute(self);
    end;
  end;
end;

procedure TChooseFont.CutMenuItemClick(Sender: TObject);
begin
  TextContent.CutToClipboard;
end;

procedure TChooseFont.CopyMenuItemClick(Sender: TObject);
begin
  TextContent.CopyToClipboard;
end;

procedure TChooseFont.PasteMenuItemClick(Sender: TObject);
begin
  TextContent.PasteFromClipboard;
end;

procedure TChooseFont.DeleteMenuItemClick(Sender: TObject);
begin
  SendMessage(TextContent.Handle, WM_KEYDOWN, VK_DELETE, 0);
  SendMessage(TextContent.Handle, WM_KEYUP, VK_DELETE, 0);
end;

procedure TChooseFont.CancelMenuItemClick(Sender: TObject);
begin
  Visible:=false;
end;

procedure TChooseFont.TextContentEnter(Sender: TObject);
begin
  TextContent.SelStart:=0;
  TextContent.SelLength:=length(TextContent.Text);
end;

procedure TChooseFont.FormActivate(Sender: TObject);
begin
//  MainForm.TextOutBtn.Click;
  TextContent.SelStart:=0;
  TextContent.SelLength:=length(TextContent.Text);
end;

procedure TChooseFont.FormShow(Sender: TObject);
var p:TPoint;
begin
  if (Top=0) and (Left=0) then begin
    p:=ScreenToClient(MainForm.ClientToScreen(Point(MainForm.Width,0)));
    Top:=P.Y;
    Left:=P.X - Width - GetSystemMetrics(SM_CXSIZEFRAME);
    end;
end;

end.
