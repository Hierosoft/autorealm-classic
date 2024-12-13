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
unit MapSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PoliteEdit, ExtCtrls, PoliteComboBox, ComCtrls, Spin,
  Math, StrUtils;

type
  TMapSettingsDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    SettingsPageControl: TPageControl;
    CommentsSheet: TTabSheet;
    MeasurementSheet: TTabSheet;
    UnitsLabel: TLabel;
    UnitComboBox: TPoliteComboBox;
    Shape1: TShape;
    Label1: TLabel;
    ALabel: TLabel;
    AEdit: TPoliteEdit;
    BLabel: TLabel;
    BEdit: TPoliteEdit;
    CLabel: TLabel;
    CEdit: TPoliteEdit;
    TriangleImage: TImage;
    HexImage: TImage;
    SquareImage: TImage;
    GeneralPage: TTabSheet;
    Comments: TMemo;
    GroupBox2: TGroupBox;
    DisplayGrid: TCheckBox;
    SnapToGrid: TCheckBox;
    Label2: TLabel;
    DesignGridUnits: TSpinEdit;
    SnapToPoint: TCheckBox;
    RHexImage: TImage;
    DiamondImage: TImage;
    HalfDiamondImage: TImage;
    PolarImage: TImage;
    cbSnapTo: TCheckBox;
    cbRotateSnap: TCheckBox;
    GroupBox1: TGroupBox;
    GridOnTop: TCheckBox;
    lblGridLevel: TLabel;
    txtCurrGridPos: TEdit;
    procedure SetGraphScale(which:integer; text:string; multiplier:double);
    procedure UnitComboBoxChange(Sender: TObject);
    procedure AEditChange(Sender: TObject);
    procedure BEditChange(Sender: TObject);
    procedure CEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridOnTopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CommentsChange(Sender: TObject);
    function  StrToCardinal(s: String): Cardinal;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapSettingsDialog: TMapSettingsDialog;

implementation

uses Main,GraphGrid,MapObject,Primitives,LocalizedStrings;

{$R *.dfm}

procedure TMapSettingsDialog.SetGraphScale(which:integer; text:string; multiplier:double);
var f:double;
    i:integer;
    view:ViewPoint;
begin
  try
    if (text<>'') then begin
      { If they change the map scale, all the views to reflect the
        new sizing } 
      for i:=0 to Map.GetViewPoints-1 do begin
        View:=Map.GetViewPoint(i);
        View.Grid.SetGraphUnits(-1,0);
        View.Grid.GraphScale := StrToFloat(text)/multiplier;
        View.Grid.SetGraphUnits(UnitComboBox.ItemIndex,MainForm.GridSizeBar.Position*UnitsPerGridTick);
        end;
      Map.CurrentView.Grid.SetGraphUnits(-1,0);
      Map.CurrentView.Grid.GraphScale := StrToFloat(text)/multiplier;
      Map.CurrentView.Grid.SetGraphUnits(UnitComboBox.ItemIndex,MainForm.GridSizeBar.Position*UnitsPerGridTick);
      end;

    f:=Map.CurrentView.Grid.GetUnitLength;

    if Assigned(Self) and Visible then begin
      AEdit.OnChange:=nil;
      BEdit.OnChange:=nil;
      CEdit.OnChange:=nil;
      case Map.CurrentView.Grid.GridType of
        gtTriangle: begin
            if (which<>0) then AEdit.Text:=FloatToStrF(f,              ffGeneral, 8, 2);
            if (which<>1) then BEdit.Text:=FloatToStrF(f*(sqrt(3)/2.0),ffGeneral, 8, 2);
          end;
        gtDiamond, gtSquare: begin
            if (which<>0) then AEdit.Text:=FloatToStrF(f,              ffGeneral, 8, 2);
            if (which<>1) then BEdit.Text:=FloatToStrF(f*(sqrt(2)),    ffGeneral, 8, 2);
          end;
        gtHex, gtRotatedHex: begin
            if (which<>0) then AEdit.Text:=FloatToStrF(f,              ffGeneral, 8, 2);
            if (which<>1) then BEdit.Text:=FloatToStrF(f*(sqrt(3)),    ffGeneral, 8, 2);
            if (which<>2) then CEdit.Text:=FloatToStrF(f*(2),          ffGeneral, 8, 2);
          end;
        gtHalfDiamond: begin
            if (which<>0) then AEdit.Text:=FloatToStrF(f*0.8,          ffGeneral, 8, 2);
            if (which<>1) then BEdit.Text:=FloatToStrF(f*(sqrt(2)),    ffGeneral, 8, 2);
            if (which<>2) then CEdit.Text:=FloatToStrF(f*(sqrt(2)/2.0),ffGeneral, 8, 2);
          end;
        gtPolar: begin
            if (which<>0) then AEdit.Text:=FloatToStrF(f,              ffGeneral, 8, 2);
          end;
        end;

      AEdit.OnChange:=AEditChange;
      BEdit.OnChange:=BEditChange;
      CEdit.OnChange:=CEditChange;
      end;

  except
    on EConvertError do begin end;
    on EDivByZero do begin end;
  end;
end;

procedure TMapSettingsDialog.AEditChange(Sender: TObject);
begin
  if (AEdit.Text<>'') then begin
    case Map.CurrentView.Grid.GridType of
      gtTriangle:  SetGraphScale(0,AEdit.Text, 1.0);

      gtDiamond,
      gtSquare:    SetGraphScale(0,AEdit.Text, 1.0);

      gtRotatedHex,
      gtHex:       SetGraphScale(0,AEdit.Text, 1.0);

      gtPolar:     SetGraphScale(0,AEdit.Text, 1.0);

      gtHalfDiamond:SetGraphScale(0,AEdit.Text, 0.8);
      end;
    Map.SetModified(modUnitScale);
    end;
end;

procedure TMapSettingsDialog.BEditChange(Sender: TObject);
begin
  if (BEdit.Text<>'') then begin
    case Map.CurrentView.Grid.GridType of
      gtTriangle:  SetGraphScale(1,BEdit.Text, sqrt(3)/2.0);

      gtDiamond,
      gtHalfDiamond,
      gtSquare:    SetGraphScale(1,BEdit.Text, sqrt(2));

      gtHex,
      gtRotatedHex: SetGraphScale(1,BEdit.Text, sqrt(3));
      end;
    Map.SetModified(modUnitScale);
    end;
end;

procedure TMapSettingsDialog.CEditChange(Sender: TObject);
begin
  if (CEdit.Text<>'') then begin
    case Map.CurrentView.Grid.GridType of
       gtHex,
       gtRotatedHex:  SetGraphScale(2,CEdit.Text, 2);

       gtHalfDiamond: SetGraphScale(2,CEdit.Text, sqrt(2)/2.0);
       end;
    Map.SetModified(modUnitScale);
    end;
end;

procedure TMapSettingsDialog.UnitComboBoxChange(Sender: TObject);
begin
  Map.CurrentView.Grid.SetMeasurementUnits(UnitComboBox.ItemIndex);
  { Refresh all the text boxes based on the new unit }
  SetGraphScale(-1, '', 1);
end;


procedure TMapSettingsDialog.FormCreate(Sender: TObject);
begin
  UnitComboBox.ItemIndex:=DefaultUnit;
  Map.CurrentView.Grid.FillComboList(TComboBox(UnitComboBox));
  Map.CurrentView.Grid.SetGraphUnits(DefaultUnit,MainForm.GridSizeBar.Position*UnitsPerGridTick);
end;

procedure TMapSettingsDialog.FormShow(Sender: TObject);
begin
  HexImage.Visible:= false;
  RHexImage.Visible:= false;
  TriangleImage.Visible:=false;
  SquareImage.Visible:=false;
  DiamondImage.Visible:=false;
  HalfDiamondImage.Visible:=false;
  PolarImage.Visible:=false;

  case Map.CurrentView.Grid.GridType of
    gtNone: begin
       ALabel.Visible:=false;
       AEdit.Visible:=false;
       BLabel.Visible:=false;
       BEdit.Visible:=false;
       CLabel.Visible:=false;
       CEdit.Visible:=false;
     end;
    gtDiamond, gtSquare: begin
       if (Map.CurrentView.Grid.GridType=gtSquare) then
          SquareImage.Visible := true
       else
          DiamondImage.Visible := true;

       ALabel.Visible:=true;
       AEdit.Visible:=true;
       BLabel.Visible:=true;
       BEdit.Visible:=true;
       CLabel.Visible:=false;
       CEdit.Visible:=false;
     end;
    gtHex, gtRotatedHex: begin
       if (Map.CurrentView.Grid.GridType=gtHex) then
          HexImage.Visible := true
       else
          rHexImage.Visible := true;

       ALabel.Visible:=true;
       AEdit.Visible:=true;
       BLabel.Visible:=true;
       BEdit.Visible:=true;
       CLabel.Visible:=true;
       CEdit.Visible:=true;
     end;
    gtTriangle: begin
       TriangleImage.Visible:=true;
       ALabel.Visible:=true;
       AEdit.Visible:=true;
       BLabel.Visible:=true;
       BEdit.Visible:=true;
       CLabel.Visible:=false;
       CEdit.Visible:=false;
     end;
     gtHalfDiamond: begin
       HalfDiamondImage.Visible:=true;
       ALabel.Visible:=true;
       AEdit.Visible:=true;
       BLabel.Visible:=true;
       BEdit.Visible:=true;
       CLabel.Visible:=true;
       CEdit.Visible:=true;
     end;
     gtPolar: begin
       PolarImage.Visible:=true;
       ALabel.Visible:=true;
       AEdit.Visible:=true;
       BLabel.Visible:=false;
       BEdit.Visible:=false;
       CLabel.Visible:=false;
       CEdit.Visible:=false;
     end;
   end;

  UnitComboBox.ItemIndex := Map.CurrentView.Grid.CurrentGraphUnits;
  UnitComboBoxChange(Sender);

  Comments.Text:=Map.Comments;

  if ( Map.CurrentView.Grid.GridFlags and gfGridOnTop ) = gfGridOnTop then
    GridOnTop.Checked := True;
  txtCurrGridPos.Text := IntToStr(Map.CurrentView.Grid.getGridPosition());
end;


procedure TMapSettingsDialog.GridOnTopClick(Sender: TObject);
begin
  if GridOnTop.Checked then
    Map.CurrentView.Grid.GridFlags := Map.CurrentView.Grid.GridFlags or gfGridOnTop
  else
    Map.CurrentView.Grid.GridFlags := Map.CurrentView.Grid.GridFlags and (not gfGridOnTop);
  Map.SetModified(modSettings);
end;

procedure TMapSettingsDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  new_grid_pos: Cardinal;
begin
  if (Comments.Text<>Map.Comments) then begin
    Map.SetUndoPoint(res_mapset_undo_comment);
    Map.Comments := Comments.Text;
    end;

  // 2003-05-29 - J.Friant
  // Save the grid position if the user changed it
  //
  new_grid_pos := StrToCardinal(txtCurrGridPos.Text);
  if Map.CurrentView.Grid.GridPosition <> Cardinal(new_grid_pos) then
    Map.CurrentView.Grid.GridPosition := new_grid_pos;
end;

procedure TMapSettingsDialog.CommentsChange(Sender: TObject);
begin
  Map.SetModified(modComments);
end;

function TMapSettingsDialog.StrToCardinal(s: string): Cardinal;
var
 card_out: Cardinal;
 str_pos:  Integer; // position in the string
 num_pos:  Integer; // position in the number (to determine power)
begin
 card_out := 0;
 if s = '' then
   card_out := 0
 else begin
   str_pos := length(s);
   num_pos := 0;
   while str_pos > 0 do begin
      card_out := card_out + (StrToInt(MidStr(s, str_pos, 1)) * Round(Power(10,num_pos)));
      str_pos := str_pos - 1;
      num_pos := num_pos + 1;
   end;
 end;
 Result := card_out;
end;

end.
