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
unit SelectionTool;

interface

uses SysUtils, Messages, Classes, Graphics, Controls, Forms, ToolObject,
     DrawLines, MapObject, Primitives, GraphGrid, Geometry, Math, CustomHint;

Const SelBorder = 3;

type SelectMode=(smMoveTop,smMoveBottom,smMoveLeft,smMoveRight,smMovePoint,smSelect,smRotate,smPopup,
                 smStretchTL,smStretchTR,smStretchBL,smStretchBR);

     TSelectionToolHandler = class(TToolHandler)
     private
        ShiftState: TShiftState;
        mode:SelectMode;
        PointMode:HandleMode;
        aspect:double;
        OriginalExtent:CoordRect;
        hint:TCustomHint;
        LastX,LastY: Coord;
        OrigX,OrigY: Coord;
        SOrigX,SOrigY: Coord;
        LastA: Coord;
     public
        Extent: CoordRect;
        procedure Draw(erase:boolean); override;
        procedure Paint; override;
        procedure Done; override;
        procedure Refresh; override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; _X, _Y: Integer);   override;
        procedure MouseMove(Shift: TShiftState; _X,_Y: Integer); override;
        function  MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        Procedure Adjust(Var X,Y: Coord); // JD 10-13-02
        Procedure DoSnap(Var X,Y: Coord); // JD 10-13-02
     end;

     TRulerToolHandler = class(TToolHandler)
     protected
        hint:TCustomHint;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        function  MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure SetDistance(d:double; showing:boolean);
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
     end;

     TFreehandRulerToolHandler = class(TRulerToolHandler)
     private
        linelist:TList;
        total:double;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        procedure Paint; override;
        function  MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure ClearList;
     end;

     TZoomToolHandler = class(TToolHandler)
     public
        function DoingZoomRect(X,Y:integer):boolean;
        procedure Draw(erase:boolean); override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   override;
        procedure Zoom(zoomin:boolean; x,y:integer);
     end;

     TPanToolHandler = class(TToolHandler)
        holdx,holdy:integer;
        holdtime:integer;
     public
        procedure Done; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   override;
     end;

     TGlueHandler = class(TToolHandler)
     private
        StartObject,EndObject:DrawPrimitive;
        StartPoint,EndPoint:CoordPoint;
     public
        procedure Cancel; override;
        procedure Done; override;
        procedure Refresh; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   override;
        procedure ResetCursor; Virtual;
     end;

     TScalpelHandler = class(TToolHandler)
     private
        StartObject:DrawPrimitive;
        node_index:integer;
        IX,IY : Coord;
     public
        procedure Cancel; override;
        procedure Done; override;
        procedure Refresh; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        function  MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
        procedure Draw(erase:boolean); override;
     end;

implementation

uses Main, SelectFont, Snap, SettingsDialog, MatrixMath, Dialogs, LocalizedStrings, HyperlinkProp, MapSettings,
     TB2ExtItems;

var DontAskAboutFrozen:boolean;

function PtOnRectBorder(r:TRect; p:TPoint):boolean;
begin
  Result:=PtInRect(Rect(r.left-selborder,r.top-selborder,r.right+selborder,r.top+selborder),p) OR
          PtInRect(Rect(r.left-selborder,r.top-selborder,r.left+selborder,r.bottom+selborder),p) OR
          PtInRect(Rect(r.right-selborder,r.top-selborder,r.right+selborder,r.bottom+selborder),p) OR
          PtInRect(Rect(r.left-selborder,r.bottom-selborder,r.right+selborder,r.bottom+selborder),p);
end;

function  TestPoint(p1x,p1y,p2x,p2y: Coord):boolean;
begin
  Result:=(p2x >= p1x-selborder) and (p2x<=p1x+selborder) and
          (p2y >= p1y-selborder) and (p2y<=p1y+selborder);
end;

procedure ExplainFrozenMessage(FrozenSelectOverlay:integer);
var ret:Word;
begin
  if DontAskAboutFrozen then exit;
  ret := MessageDlg( Format(res_seltool_freez,[MainForm.ActiveOverlay.Items[FrozenSelectOverlay]]),
      mtInformation,
      [mbOK,mbIgnore],0);
  DontAskAboutFrozen := (ret=mrIgnore);
end;

{--------------------------------------------------------------------------------}
{--------------------------------------------------------------------------------}

constructor TSelectionToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  hint := TCustomHint.Create(cv);
  mode := smSelect;
end;

destructor TSelectionToolHandler.Destroy;
begin
  hint.Destroy;
  inherited Destroy;
end;

// --------------------------------------------------------
//  TSelectionToolHandler.Draw
//  Draw a red marquis while the user is clicking and dragging
//  a new select extent.  Note that we can actually have two
//  marquis on the screen--this one (the temporary one being
//  created by the user), and the old one (the one with the
//  sizing handles that shows the current extent).
// --------------------------------------------------------
procedure TSelectionToolHandler.Draw(erase:boolean);
var oc:TColor;
    OldMode:TPenMode;
begin
  hint.Visible := false;
  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmNotXor;   // We can erase this one--make it xored
  oc:=Canvas.Pen.Color;
  Canvas.Pen.Color := clRed;
  Marquis(Canvas, Round(StartX), Round(StartY), Round(EndX), Round(EndY));
  Canvas.Pen.Color := oc;
  Canvas.Pen.Mode := OldMode;
end;

procedure TSelectionToolHandler.Done;
var
  rough,seed          : integer;
  overlay             : integer;
  textattribs         : TextAttrib;
  style               : StyleAttrib;
  clr                 : TColor;
  selectRect          : CoordRect;
  FrozenSelectOverlay : integer;
  SavedNotifyProc     : TNotifyEvent;
  CR                  : CoordRect;
  SavedAcceptProc     : TTBAcceptTextEvent;

begin
  { Remove the hint }
  hint.Visible := false;

  { Remove the marquis }
  Draw(false);

  { Do the selection }
  if mode = smSelect then
  begin
    CR.Left   := StartX;
    CR.Top    := StartY;
    CR.Right  := EndX;
    CR.Bottom := EndY;
    SelectRect := Map.CurrentView.ScreenToCoordRect(CR);

    if ssAlt in ShiftState then
    begin
      if (abs(StartX - EndX) <= 5) and (abs(StartY - EndY) <= 5) then
      begin
        if not (ssCtrl in ShiftState) then Map.ClearSelection;
        // Alt+click selects closest handle
        Map.SelectClosestObject(SelectRect.Right,SelectRect.Bottom);
      end
      else
      begin
        // Alt+drag selects iteration over a region
        Map.IterateSelect(SelectRect);
      end;
    end
    else
    begin
      if not (ssCtrl in ShiftState) then Map.ClearSelection;

      if (abs(StartX - EndX) <= 5) and (abs(StartY - EndY) <= 5) then
      begin
        // A single click selects one object
        FrozenSelectOverlay := Map.SelectClickedObject(SelectRect.Right,SelectRect.Bottom);
      end
      else
      begin
        // A "drag" selects a region
        FrozenSelectOverlay := Map.Select(SelectRect);
      end;

      if (FrozenSelectOverlay <> -1) then ExplainFrozenMessage(FrozenSelectOverlay);
    end;

    Extent := Map.ExtentFloat(Map.CurrentView,false);

    with MainForm do
    begin
      overlay := Map.GetOverlay;
      if overlay <> -1 then
      begin
        SavedNotifyProc         := ActiveOverlay.OnChange;
        ActiveOverlay.OnChange  := nil;
        ActiveOverlay.ItemIndex := overlay;
        ActiveOverlay.OnChange  := SavedNotifyProc;
      end;

      style := Map.GetStyle;
      if style.Fill <> $FF then
      begin
        SavedNotifyProc      := FillPattern.OnChange;
        FillPattern.OnChange := nil;
        FillPattern.Pattern  := style.Fill;
        FillPattern.OnChange := SavedNotifyProc;
      end;
      if style.Line <> $FF then
      begin
        SavedNotifyProc             := LineStyleComboBox.OnChange;
        LineStyleComboBox.OnChange  := nil;
        LineStyleComboBox.ItemIndex := style.Line;
        LineStyleComboBox.OnChange  := SavedNotifyProc;
      end;
      if style.First <> $FF then
      begin
        SavedNotifyProc          := BeginLineStyle.OnChange;
        BeginLineStyle.OnChange  := nil;
        BeginLineStyle.ItemIndex := style.First;
        BeginLineStyle.OnChange  := SavedNotifyProc;
      end;
      if style.Last <> $FF then
      begin
        SavedNotifyProc        := EndLineStyle.OnChange;
        EndLineStyle.OnChange  := nil;
        EndLineStyle.ItemIndex := style.Last;
        EndLineStyle.OnChange  := SavedNotifyProc;
      end;
      If Style.FullStyle.Thickness >= 0 Then
      Begin
        SavedAcceptProc               := edtLineThickness.OnAcceptText;
        edtLineThickness.OnAcceptText := Nil;
        edtLineThickness.Text         := FloatToStr(Style.FullStyle.Thickness);
        edtLineThickness.OnAcceptText := SavedAcceptProc;
      End;

      // Force a repaint of line ends if main style has changed
      
      if style.Line <> $FF then
      begin
        BeginLineStyle.Repaint;
        EndLineStyle.Repaint;
      end;

      rough := Map.GetRoughness;
      if rough <> -1 then
      begin
        SavedNotifyProc            := RoughnessTrackBar.OnChange;
        RoughnessTrackBar.OnChange := nil;
        RoughnessTrackBar.Position := rough;
        RoughnessTrackBar.OnChange := SavedNotifyProc;
      end;

      seed := Map.GetSeed;
      if seed <> -1 then
      begin
        SavedNotifyProc   := SeedSpin.OnChange;
        SeedSpin.OnChange := nil;
        SeedSpin.Value    := seed;
        SeedSpin.OnChange := SavedNotifyProc;
      end;

      clr := Map.GetColor;
      if clr <> clNone then
      begin
        SavedNotifyProc             := MainColor.OnChange;
        MainColor.OnChange          := nil;
        MainColor.Color             := clr;
        // Not only update the button color, but update the
        // global currentcolor and the fill pattern color too.
        CurrentColor                := clr;
        FillPattern.ForegroundColor := clr;
        MainColor.OnChange          := SavedNotifyProc;
      end;

      clr := Map.GetFillColor;
      if clr <> clNone then
      begin
        SavedNotifyProc             := FillColor.OnChange;
        FillColor.OnChange          := nil;
        FillColor.Color             := clr;
        // Not only update the button color, but update the
        // global fillcurrentcolor and the fill pattern color too.
        CurrentFillColor            := clr;
        FillPattern.BackgroundColor := clr;
        FillColor.OnChange          := SavedNotifyProc;
      end;

      textattribs := Map.GetTextAttrib;
      AssignTextAttribs(textattribs);
      AssignHyperlinkAttribs(textattribs);

      if tatOutlineColor in textattribs.Valid then
      begin
        SavedNotifyProc       := OutlineColor.OnChange;
        OutlineColor.OnChange := nil;
        OutlineColor.Color    := textattribs.FontOutlineColor;
        OutlineColor.OnChange := SavedNotifyProc;
      end;

      if tatIconSize in textattribs.Valid then
      begin
        SavedNotifyProc      := IconSizeBar.OnChange;
        IconSizeBar.OnChange := nil;
        IconSizeBar.Position := textattribs.IconSize;
        IconSizeBar.OnChange := SavedNotifyProc;
      end;
    end;
  end;

  // Set to select mode when finished
  mode := smSelect;

  inherited Done;
end;

procedure TSelectionToolHandler.Refresh;
begin
  Extent := Map.ExtentFloat(Map.CurrentView,false);
end;

// --------------------------------------------------------
//  TSelectionToolHandler.Paint;
//  Draw the red marquis, the corner sizing handles, and
//  the rotate handle.  Note that we draw right over the
//  underlying surface, so the only way to remove these
//  select markings is to repaint the underlying objects.
// --------------------------------------------------------
procedure TSelectionToolHandler.Paint;
var oc:TColor;
    OldMode:TPenMode;
    OldBrushColor:TColor;
  procedure DrawHandle(x,y:integer);
  begin
    Canvas.Rectangle(x-selborder,y-selborder,x+selborder,y+selborder);
  end;
begin
  OldBrushColor:=Canvas.Brush.Color;
  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmCopy; //pmNotXor;
  oc:=Canvas.Pen.Color;
  Canvas.Pen.Color := clRed;

  if (extent.Left<>extent.Right) and (extent.Top<>extent.Bottom) then begin
    // Marquis border is for moving
    Marquis(Canvas,Round(Extent.Left),Round(Extent.Top),Round(Extent.Right),Round(Extent.Bottom));

    // Red and white Handles on each corner are for stretching
    DrawHandle(Round(Extent.Left),Round(Extent.Top));
    DrawHandle(Round(Extent.Left),Round(Extent.Bottom));
    DrawHandle(Round(Extent.Right),Round(Extent.Top));
    DrawHandle(Round(Extent.Right),Round(Extent.Bottom));

    // Red and Blue Handles in middle top is for free-rotate
    // Note that we don't draw the handle once they've started the rotate
    // operation because the handle is always drawn at the middle top
    // of the selection box, and that doesn't usually match the actual
    // cursor point.
    if (mode<>smRotate) then begin
      Canvas.Brush.Color:=clBlue;
      DrawHandle(Round((Extent.Right + Extent.Left) / 2), Round(Extent.Top));
      end;
    end;

  Canvas.Pen.Color := oc;
  Canvas.Pen.Mode:=OldMode;
  Canvas.Brush.Color:=OldBrushColor;
end;

function TSelectionToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var cx,cy:Coord;
    hypertext:string;
    hyperflags:THyperlinkFlags;
    error:string;
    P : TPoint;
    R : TRect;
    
begin
  OrigX := X;
  OrigY := Y;
  LastX := X;
  LastY := Y;
  LastA := 0;
  R.Left   := Round(Extent.Left);
  R.Top    := Round(Extent.Top);
  R.Right  := Round(Extent.Right);
  R.Bottom := Round(Extent.Bottom);
  Result:=inherited MouseDown(Button,Shift,X,Y);

  if Result then begin
    Map.CurrentView.ScreenToCoord(x,y,cx,cy);

    if Map.FindHandle(cx,cy)<>nil then begin
      mode:=smMovePoint;
      Map.SetUndoPoint(res_seltool_undo_handle);
      if ssAlt in Shift then
        PointMode:=hmOne
      else
        PointMode:=hmAll;
      end
    else if (Map.FindHyperlink(cx,cy,hypertext,hyperflags)) and
            not (ssAlt in Shift) then begin
      mode:=smSelect;
      inherited Done;  // Finished; hyperlink execution resets the cursor

      error := MainForm.ExecuteHyperlink(hypertext, hyperflags);
      if error<>'' then begin
        Mainform.Cursor:=crCross;
        Application.MessageBox(PChar(error),PChar(res_fail_hyperlink), MB_OK);
        end;
      end
    else if TestPoint(extent.Left,extent.Top,x,y) then begin
      StartX:=extent.Right;
      StartY:=extent.Bottom;
      mode:=smStretchTL;
      AdjustForSelBorder(X,Y,ApplyScreenSnapsDX1,ApplyScreenSnapsDY1,ApplyScreenSnapsDX2,ApplyScreenSnapsDY2);
      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_strech);
      end
    else if TestPoint(extent.Right,extent.Bottom,x,y) then begin
      StartX:=extent.Left;
      StartY:=extent.Top;
      mode:=smStretchBR;
      AdjustForSelBorder(X,Y,ApplyScreenSnapsDX1,ApplyScreenSnapsDY1,ApplyScreenSnapsDX2,ApplyScreenSnapsDY2);
      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_strech);
      end
    else if TestPoint(extent.Left,extent.Bottom,x,y) then begin
      StartX:=extent.Right;
      StartY:=extent.Top;
      mode:=smStretchBL;
      AdjustForSelBorder(X,Y,ApplyScreenSnapsDX1,ApplyScreenSnapsDY1,ApplyScreenSnapsDX2,ApplyScreenSnapsDY2);
      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_strech);
      end
    else if TestPoint(extent.Right,extent.Top,x,y) then begin
      StartX:=extent.Left;
      StartY:=extent.Bottom;
      mode:=smStretchTR;
      AdjustForSelBorder(X,Y,ApplyScreenSnapsDX1,ApplyScreenSnapsDY1,ApplyScreenSnapsDX2,ApplyScreenSnapsDY2);
      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_strech);
      end
    else if TestPoint(Round((Extent.Right + Extent.Left) / 2), Round(Extent.Top), x,y) then begin
      StartX := (Extent.Right + Extent.Left) / 2;
      StartY := Extent.Top;
      mode:=smRotate;
      ClearAdjust;
      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_rotate);
      end
    else if PtOnRectBorder(R,Point(x,y)) then begin
      P := Point(X,Y);
           If PtInRect(Rect(Round(extent.left-selborder), Round(extent.top-selborder),Round(extent.right+selborder),Round(extent.top+selborder)),p)    Then Mode := smMoveTop
      Else If PtInRect(Rect(Round(extent.left-selborder), Round(extent.top-selborder),Round(extent.left+selborder), Round(extent.bottom+selborder)),p) Then Mode := smMoveLeft
      Else If PtInRect(Rect(Round(extent.right-selborder),Round(extent.top-selborder),Round(extent.right+selborder),Round(extent.bottom+selborder)),p) Then Mode := smMoveRight
      Else Mode := smMoveBottom;

      AdjustForSelBorder(X,Y,ApplyScreenSnapsDX1,ApplyScreenSnapsDY1,ApplyScreenSnapsDX2,ApplyScreenSnapsDY2);

      OriginalExtent:=Map.CurrentView.ScreenToCoordRect(extent);
      Map.SetUndoPoint(res_seltool_undo_move);
      end
    else
      mode:=smSelect;
      EndX:=StartX;
      EndY:=StartY;

      Draw(false);

      if (extent.Bottom=extent.Top) or (extent.Right=extent.Left) then
        aspect:=1.0
      else
        aspect := (extent.Right-extent.Left)/(extent.Bottom-extent.Top);

    end;
  SOrigX := Extent.Left;
  SOrigY := Extent.Top;
end;

Procedure TSelectionToolHandler.Adjust(Var X,Y: Coord);

  Procedure Add(Var X,Y: Coord; DX1,DY1,DX2,DY2: Integer);
  Begin
    X := X - DX1 * ApplyScreenSnapsEX1 - DX2 * ApplyScreenSnapsEX2;
    Y := Y - DY1 * ApplyScreenSnapsEY1 - DY2 * ApplyScreenSnapsEY2;
  End; // Add

  Procedure AddFixed(Var X,Y: Coord; DX1,DY1,DX2,DY2: Integer);
  Begin
    X := X - DX1 * SelBorder + DX2 * SelBorder;
    Y := Y - DY1 * SelBorder + DY2 * SelBorder;
  End; // AddFixed

Begin
  Case Mode Of
      smMoveTop: Add(X,Y,0,1,0,0);
   smMoveBottom: Add(X,Y,0,0,0,1);
     smMoveLeft: Add(X,Y,1,0,0,0);
    smMoveRight: Add(X,Y,0,0,1,0);
    smStretchTL: AddFixed(X,Y,1,1,0,0);
    smStretchTR: AddFixed(X,Y,0,1,1,0);
    smStretchBL: AddFixed(X,Y,1,0,0,1);
    smStretchBR: AddFixed(X,Y,0,0,1,1);
  End; // Case
End; // TSelectionToolHandler.Adjust

Procedure TSelectionToolHandler.DoSnap(Var X,Y: Coord);
Var
  X1,Y1,X2,Y2,X3,Y3,X4,Y4,D1,D2,D3,I,N,N1 : Coord;
  N3                                      : Coord;

  Procedure Swap12;
  Begin
    I  := X1;
    X1 := X2;
    X2 := I;
    I  := Y1;
    Y1 := Y2;
    Y2 := I;
    I  := D1;
    D1 := D2;
    D2 := I;
  End; // Swap12

  Procedure Swap23;
  Begin
    I  := X3;
    X3 := X2;
    X2 := I;
    I  := Y3;
    Y3 := Y2;
    Y2 := I;
    I  := D3;
    D3 := D2;
    D2 := I;
  End; // Swap23

  Procedure XMove;
  Begin
    X1 := X;
    Y1 := SOrigY + SelBorder + (Y - OrigY);
    X2 := X;
    Y2 := Y1 + (Extent.Bottom - Extent.Top) - 2 * SelBorder;
    X3 := X;
    Y3 := (Y1 + Y2) / 2;
    If Mode = smMoveLeft
     Then X4 := X + ((Extent.Right - Extent.Left - 2 * SelBorder) / 2)
     Else X4 := X - ((Extent.Right - Extent.Left - 2 * SelBorder) / 2);
    Y4 := Y1;
    ApplyScreenSnapsFloat(True,X1,Y1,1,False);
    ApplyScreenSnapsFloat(True,X2,Y2,1,False);
    ApplyScreenSnapsFloat(True,X3,Y3,1,False);
    ApplyScreenSnapsFloat(True,X4,Y4,1,False);

    If Mode = smMoveLeft
     Then X4 := X4 - ((Extent.Right - Extent.Left - 2 * SelBorder) / 2)
     Else X4 := X4 + ((Extent.Right - Extent.Left - 2 * SelBorder) / 2);
    D1 := X1 - X;
    D2 := X4 - X;

    If Abs(D2) < Abs(D1) Then
    Begin
      D1 := D2;
      X1 := X4;
    End;
    LastX := X;
    X     := X1;

    N3 := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    Map.CurrentView.CoordToScreen(N3,N3,N,N1);

    Y1 := Y1 + (OrigY - SOrigY) - SelBorder;
    Y2 := Y2 - (Extent.Bottom - Extent.Top) + (OrigY - SOrigY) + SelBorder;
    Y3 := Y3 - (Extent.Bottom - Extent.Top - 2 * SelBorder) / 2 + (OrigY - SOrigY) - SelBorder;
    D1 := Y1 - Y;
    D2 := Y2 - Y;
    D3 := Y3 - Y;
    If Y < LastY Then
    Begin
      If Abs(D3) < Abs(D2) Then Swap23;
      If Abs(D2) < Abs(D1) Then Swap12;
      If D1 > 0 Then Y1 := StartY;
    End
    Else If Y > LastY Then
    Begin
      If Abs(D3) < Abs(D2) Then Swap23;
      If Abs(D2) < Abs(D1) Then Swap12;
      If D1 < 0 Then Y1 := StartY;
    End
    Else Y1 := StartY;

    LastY := Y;
    Y     := Y1;
  End; // XMove

  Procedure YMove;
  Begin
    Y1 := Y;
    X1 := SOrigX + SelBorder + (X - OrigX);
    Y2 := Y;
    X2 := X1 + (Extent.Right - Extent.Left) - 2 * SelBorder;
    Y3 := Y;
    X3 := (X1 + X2) / 2;
    If Mode = smMoveTop
     Then Y4 := Y + ((Extent.Bottom - Extent.Top - 2 * SelBorder) / 2)
     Else Y4 := Y - ((Extent.Bottom - Extent.Top - 2 * SelBorder) / 2);
    X4 := X1;
    ApplyScreenSnapsFloat(True,X1,Y1,1,False);
    ApplyScreenSnapsFloat(True,X2,Y2,1,False);
    ApplyScreenSnapsFloat(True,X3,Y3,1,False);
    ApplyScreenSnapsFloat(True,X4,Y4,1,False);

    If Mode = smMoveTop
     Then Y4 := Y4 - ((Extent.Bottom - Extent.Top - 2 * SelBorder) / 2)
     Else Y4 := Y4 + ((Extent.Bottom - Extent.Top - 2 * SelBorder) / 2);
    D1 := Y1 - Y;
    D2 := Y4 - Y;

    If Abs(D2) < Abs(D1) Then
    Begin
      D1 := D2;
      Y1 := Y4;
    End;
    LastY := Y;
    Y     := Y1;

    N3 := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    Map.CurrentView.CoordToScreen(N3,N3,N,N1);

    X1 := X1 + (OrigX - SOrigX) - SelBorder;
    X2 := X2 - (Extent.Right - Extent.Left) + (OrigX - SOrigX) + SelBorder;
    X3 := X3 - (Extent.Right - Extent.Left - 2 * SelBorder) / 2 + (OrigX - SOrigX) - SelBorder;
    D1 := X1 - X;
    D2 := X2 - X;
    D3 := X3 - X;
    If X < LastX Then
    Begin
      If Abs(D3) < Abs(D2) Then Swap23;
      If Abs(D2) < Abs(D1) Then Swap12;
      If D1 > 0 Then X1 := StartX;
    End
    Else If X > LastX Then
    Begin
      If Abs(D3) < Abs(D2) Then Swap23;
      If Abs(D2) < Abs(D1) Then Swap12;
      If D1 < 0 Then X1 := StartX;
    End
    Else X1 := StartX;

    LastX := X;
    X     := X1;
  End; // YMove

  Procedure Rotate(Angle: Coord);
  Var
    Mat     : Matrix;
    CenterX : Coord;
    CenterY : Coord;

  Begin
    CenterX := X;//(OriginalExtent.right+OriginalExtent.left)/2;
    CenterY := Y;//(OriginalExtent.bottom+OriginalExtent.top)/2;

    Mat := OffsetMatrix(-CenterX,-CenterY);
    MatrixMultiplyBy(Mat,RotationMatrix((Angle - LastA) * 180 / Pi));
    MatrixMultiplyBy(Mat,OffsetMatrix(CenterX,CenterY));
    Map.ApplyMatrix(Mat);
    Refresh;
    LastA  := Angle;
  End; // Rotate

Begin
  If MapSettingsDialog.SnapToGrid.Checked Then
  Begin
    If Mode <> smSelect Then
    Begin
      Case Mode Of
        smMoveLeft,
        smMoveRight: XMove;
        smMoveTop,
        smMoveBottom: YMove;
      Else
        ApplyScreenSnapsFloat(True,X,Y,1,False);
      End; // Case
      Adjust(X,Y);
    End;
  End
  Else
  Begin
    ApplyScreenSnapsFloat(True,X,Y,-1,False);
    Adjust(X,Y);
    If (LastSnapKind = 3) And (MapSettingsDialog.cbRotateSnap.Checked) Then
    Begin
      Case Mode Of
        smMoveLeft: Rotate(Pi / 2 - LastSnapAngle);
       smMoveRight: Rotate(Pi / 2 - LastSnapAngle);
         smMoveTop: Rotate(Pi - LastSnapAngle);
      smMoveBottom: Rotate(-LastSnapAngle);
      End; // Case
    End;
  End;
End; // TSelectionToolHandler.DoSnap

procedure TSelectionToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; _X, _Y: Integer);
Var X,Y: Coord;
begin
  X := _X;
  Y := _Y;
  if state = tsStarted then
  begin
    DoSnap(X,Y);
    EndX := Round(X);
    EndY := Round(Y);
    if mode <> smSelect then Draw(false);
    ShiftState := Shift;
    Done;
  end;
end;

procedure TSelectionToolHandler.MouseMove(Shift: TShiftState; _X,_Y: Integer);
var cx,cy,cdx,cdy:Coord;
    rx,ry:Coord;
    r,ER:TRect;
    hypertext:string;
    hyperflags:THyperlinkFlags;
    X,Y : Coord;

  procedure StretchRect(oldscreenrect,newscreenrect:TRect);
  var mat:Matrix;
      xfactor,yfactor:double;
      oldrect,newrect:CoordRect;

      procedure ShowStretchHint;
      var xf,yf:double;
      begin
        // Compute scaling, and show it.
        if (OriginalExtent.right-OriginalExtent.Left = 0) then
           xf := 1.0
        else
           xf := (newrect.Right-newRect.left) / (OriginalExtent.right-OriginalExtent.Left);

        if (OriginalExtent.bottom-OriginalExtent.top = 0) then
           yf := 1.0
        else
           yf := (newrect.Bottom-newRect.top) / (OriginalExtent.bottom-OriginalExtent.top);

        HideCrosshair;
        hint.Visible := false;
        hint.StartX := Round(StartX);
        hint.StartY := Round(StartY);
        hint.X := Round(X);
        hint.Y := Round(Y);
        hint.OffsetX := 20;
        hint.OffsetY := 20;
        hint.Text := Format('%4.2fX %4.2fY', [xf, yf]);
        hint.Visible := true;
      end;

  begin
    oldrect:=Map.CurrentView.ScreenToCoordRect(oldscreenrect);
    newrect:=Map.CurrentView.ScreenToCoordRect(newscreenrect);

    if (oldrect.right>oldrect.left) and (newrect.right>newrect.left) then
      xfactor:=(newrect.right-newrect.left)/(oldrect.right-oldrect.left)
    else
      xfactor:=1.0;

    if (oldrect.bottom>oldrect.top) and (newrect.bottom>newrect.top) then
      yfactor:=(newrect.bottom-newrect.top)/(oldrect.bottom-oldrect.top)
    else
      yfactor:=1.0;

    if (xfactor=1.0) and (yfactor=1.0) then exit;

    mat:=OffsetMatrix(-oldrect.left,-oldrect.top);
    MatrixMultiplyBy(mat,ScaleMatrix(xfactor,yfactor));
    MatrixMultiplyBy(mat,OffsetMatrix(newrect.left,newrect.top));
    Map.ApplyMatrix(mat);
    Refresh;

    ShowStretchHint;
  end;

  procedure Rotate(X,Y: Coord);
  var theta,phi,angle:double;
      op,np:CoordPoint;
      mat:Matrix;
      centerx,centery:Coord;

    procedure ShowRotateHint;
    var angle:double;
        sx,sy:integer;
    begin
      Map.CurrentView.CoordToScreen(centerx,centery,sx,sy);
      angle := RadToDeg(arctan2(y-sy,x-sx)) + 90;

      if (angle > 180) then angle := angle - 360;

      HideCrosshair;
      hint.Visible := false;
      hint.X := Round((Extent.Left + Extent.Right) / 2);
      hint.Y := Round(Extent.Top - 50);
      hint.OffsetX := 0;
      hint.OffsetY := 0;
      hint.StartX := -32767;
      hint.StartY := -32767;
      hint.Text := Format('%4.2f°', [angle]);
      hint.Visible := true;
    end;

  begin
    centerx:=(OriginalExtent.right+OriginalExtent.left)/2;
    centery:=(OriginalExtent.bottom+OriginalExtent.top)/2;

    Map.CurrentView.ScreenToCoord(StartX, StartY, op.x, op.y);
    Map.CurrentView.ScreenToCoord(x, y, np.x, np.y);
    theta := arctan2(op.y-centery, op.X-centerx);
    phi   := arctan2(np.Y-centery, np.X-centerx);
    angle := RadToDeg(theta-phi);

    mat:=OffsetMatrix(-centerx,-centery);
    MatrixMultiplyBy(mat,RotationMatrix(angle));
    MatrixMultiplyBy(mat,OffsetMatrix(centerx,centery));
    Map.ApplyMatrix(mat);
    Refresh;
    ShowRotateHint;
    StartX:=X;
    Starty:=Y;
  end;

begin
  X := _X;
  Y := _Y;
  ER.Left   := Round(Extent.Left);
  ER.Top    := Round(Extent.Top);
  ER.Right  := Round(Extent.Right);
  ER.Bottom := Round(Extent.Bottom);
  HideCrosshair;

  if (state<>tsStarted) then begin
    Map.CurrentView.ScreenToCoord(x,y,cx,cy);

    // Only if the cursor isn't already involved in something else do we allow popups to work.
    if ((mode=smPopup) or (mode=smSelect)) and
        Map.FindHyperlink(cx,cy,hypertext,hyperflags) and
        not (ssAlt in Shift) then begin
      MainForm.Cursor := crHandPoint;
      hint.Visible := false;
      hint.X := _X;
      hint.Y := _Y;
      hint.OffsetX := 10;
      hint.OffsetY := 10;
      hint.Text := hypertext;
      hint.Visible := true;
      mode := smPopup;
      end
    else if Map.FindHandle(cx,cy)<>nil then
      MainForm.Cursor := crArrow
    else if TestPoint(extent.Left,extent.Top,x,y) or TestPoint(extent.Right,extent.Bottom,x,y) then
      MainForm.Cursor := crSizeNWSE
    else if TestPoint(extent.Left,extent.Bottom,x,y) or TestPoint(extent.Right,extent.Top,x,y) then
      MainForm.Cursor := crSizeNESW
    else if TestPoint(Round((Extent.Left + Extent.Right) / 2),Round(Extent.Top),x,y) then
      MainForm.Cursor := crSizeWE
    else if PtOnRectBorder(ER,Point(Round(x),Round(y))) then
      MainForm.Cursor := crSize
    else begin
      // If they wander away from a popup, then turn it off.
      if (mode=smPopup) then begin
        hint.Visible := false;
        mode := smSelect;
        end;
      MainForm.Cursor := crCross;
    end;
    end
  else begin
    DoSnap(X,Y);
    case mode of
      smMovePoint: begin
                hint.visible:=false;
                MainForm.Cursor := crArrow;
                Map.CurrentView.ScreenToCoord(StartX,StartY,cx,cy);
                Map.CurrentView.DeltaScreenToCoord(X - StartX,Y - StartY,cdx,cdy);
                Map.InvalidateSelect(true);
                Map.MoveHandle(PointMode,cx,cy,cdx,cdy);
                Extent := Map.ExtentFloat(Map.CurrentView,false);
                Map.InvalidateSelect(true);
                StartX := X;
                StartY := Y;
              end;
      smMoveTop,smMoveBottom,smMoveLeft,smMoveRight: begin
                hint.visible    := false;
                MainForm.Cursor := crSize;
                Map.CurrentView.DeltaScreenToCoord(X - StartX,Y - StartY,cdx,cdy);
                Map.MoveSelection(cdx,cdy);
                Extent.Left   := Extent.Left   + (X - StartX);
                Extent.Right  := Extent.Right  + (X - StartX);
                Extent.Top    := Extent.Top    + (Y - StartY);
                Extent.Bottom := Extent.Bottom + (Y - StartY);
                StartX := X;
                StartY := Y;
              end;
      smSelect: begin
                MainForm.Cursor := crCross;
                inherited MouseMove(Shift,Round(X),Round(Y));
                exit;
              end;
      smRotate: begin
                hint.visible    := false;
                MainForm.Cursor := crSize;
                Rotate(X,Y);
               end;
      smStretchTL,smStretchTR,smStretchBL,smStretchBR: begin
                hint.visible := false;

                if (mode=smStretchTL) or (mode=smStretchBR) then
                  MainForm.Cursor := crSizeNWSE
                else
                  MainForm.Cursor := crSizeNESW;

                rx := X;
                ry := Y;
                if GetKeyState(VK_SHIFT)<0 then
                  ApplyProportionalSnap(aspect,StartX,StartY,rx,ry);

                case mode of
                  smStretchTL: begin
                       r.Right  := Round(StartX);
                       r.Bottom := Round(StartY);
                       r.Left   := trunc(rx);
                       r.Top    := trunc(ry);
                     end;
                  smStretchTR: begin
                       r.Left   := Round(StartX);
                       r.Bottom := Round(StartY);
                       r.Right  := trunc(rx);
                       r.Top    := trunc(ry);
                     end;
                  smStretchBL: begin
                       r.Right  := Round(StartX);
                       r.Top    := Round(StartY);
                       r.Left   := trunc(rx);
                       r.Bottom := trunc(ry);
                     end;
                  smStretchBR: begin
                       r.Left   := Round(StartX);
                       r.Top    := Round(StartY);
                       r.Right  := trunc(rx);
                       r.Bottom := trunc(ry);
                     end;
                  end;
                ER.Left   := Round(Extent.Left);
                ER.Top    := Round(Extent.Top);
                ER.Right  := Round(Extent.Right);
                ER.Bottom := Round(Extent.Bottom);
                StretchRect(ER,r);
              end;
      end;
    end;
  ShowCrossHair(Round(X),Round(Y));
end;

{--------------------------------------------------------------------------------}

procedure TGlueHandler.Done;
const ObjectType='LlPpCcKk';
      ObjectXlate:array[1..8,1..8] of char=
      {  L   l   P   p   C   c   K   k   }
      { --- --- --- --- --- --- --- ---  }
      (('P','P','P','P','K','P','K','P'),  {L=Line}
       ('P','p','P','p','P','P','P','P'),  {l=Fractal Line}
       ('P','P','P','P','K','P','K','P'),  {P=Polyline}
       ('P','p','P','p','P','P','P','P'),  {p=Fractal Polyline}
       ('K','P','K','P','K','P','K','P'),  {C=Curve}
       ('P','P','P','P','P','k','P','k'),  {c=Fractal Curve}
       ('K','P','K','P','K','P','K','P'),  {K=Polycurve}
       ('P','P','P','P','P','k','P','k')); {k=Fractal Polycurve}

var  First,Last,NewObj:DrawPrimitive;
     OType1,OType2:integer;
     CommonDenominator:char;
     mode:HandleMode;

begin
  ResetCursor;

  if (StartObject = nil) or (EndObject = nil) then
  begin
    Cancel;
    exit;
  end;

  Map.SetModified(modChanged);
  Map.SetUndoPoint(res_seltool_undo_glue);

  // The cleanest way to do this is first to make both objects totally
  // "unique".  MapCollection.AddObject will figure out for us whether the
  // results need to be bases or aliases.

  StartObject.SplitOff;
  EndObject.SplitOff;

  Map.SelectFromPoint(0,0,[]);
  { Move the start line to match up with the end line }
  StartObject.Select(Map.CurrentView,true);
  Map.MoveSelection(EndPoint.X - StartPoint.X,EndPoint.Y - StartPoint.Y);
  { Move the handle to the exact endpoint in case of any question
    so we can use an exact match to see which endpoint is the one to
    be joined }
  mode := hmAll;
  Map.MoveHandle(mode, EndPoint.X, EndPoint.Y, 0, 0);

  { Select both objects, since we're going to delete them both }
  EndObject.Select(Map.CurrentView, true);

  { Figure out what we have to convert to... }
  OType1 := pos(StartObject.GetId,ObjectType);
  OType2 := pos(EndObject.GetId,ObjectType);

  if (OType1 = 0) or (OType2 = 0) then exit;

  CommonDenominator := ObjectXlate[OType1,OType2];

  { Convert them both }
  First := ConvertObject(EndObject,   CommonDenominator);
  Last  := ConvertObject(StartObject, CommonDenominator);

  { Remove the objects in the map that we converted from }
  Map.DeleteSelected;
  { Combine the new objects into one, and remove the new objects }
  NewObj := CombineObjects(First,Last);
  First.Free;
  Last.Free;
  Map.AddObject(NewObj);
  NewObj.Select(Map.CurrentView,false);

  StartObject := nil;
end;

procedure TGlueHandler.ResetCursor;
begin
  CustomCursor := crGlue;
  MainForm.Cursor:=crGlue;
end;

procedure TGlueHandler.Cancel;
begin
  ResetCursor;
  StartObject:=nil;
  EndObject:=nil;
end;

procedure TGlueHandler.Refresh;
begin
  Map.ClearSelection;
  StartObject:=nil;
  EndObject:=nil;
end;

procedure TGlueHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var cx,cy:Coord;
begin
  Map.CurrentView.ScreenToCoord(x,y,cx,cy);

  Map.SelectFromPoint(cx,cy, ['L','l','P','p','C','c','K','k']);

  if Map.FindEndPoint(cx,cy,StartObject,EndObject) then begin
    if StartObject<>nil then begin
      EndPoint.X:=cx;
      EndPoint.Y:=cy;
      CustomCursor:=crTGlueHnd;
      end
    else
      CustomCursor:=crGlueHnd;
    end
  else begin
    if StartObject<>nil then CustomCursor:=crTGlue else CustomCursor:=crGlue;
    end;

  if CustomCursor<>MainForm.Cursor then MainForm.Cursor:=CustomCursor;
end;

function TGlueHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var cx,cy:Coord;
begin
  if Button=mbLeft then begin
    Map.CurrentView.ScreenToCoord(x,y,cx,cy);

    if Map.FindEndPoint(cx,cy,nil,StartObject) then begin
      StartPoint.X:=cx;
      StartPoint.Y:=cy;
      MainForm.Cursor:=crTGlue;
      EndObject:=nil;
      { Turn off mouse capture; VCL's implementation of MouseDown
        events causes the mouse to be captured, which in turn
        interferes with the updating of the mouse cursor (since it's
        presumed to be a drag/drop operation) }
      Mouse.Capture:=0;
      end;

    Result:=true;
    end
  else
    Result:=inherited MouseDown(Button,Shift,X,Y);
end;

procedure TGlueHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then begin
    if StartObject<>nil then Done else Cancel;
    end;
end;

{--------------------------------------------------------------------------------}

procedure TScalpelHandler.Draw(erase:boolean);
var x1,y1,x2,y2:Coord;
begin
  Map.CurrentView.ScreenToCoord(StartX,StartY,x1,y1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,x2,y2);

  Canvas.Pen.Color:=clRed;
  Canvas.MoveTo(Round(StartX),Round(StartY));
  Canvas.LineTo(Round(EndX),Round(EndY));
end;

procedure TScalpelHandler.Done;
var x1,y1,x2,y2:Coord;
begin
  Map.SetUndoPoint(res_seltool_undo_scalpel4);
  Map.CurrentView.ScreenToCoord(StartX,StartY,x1,y1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,x2,y2);
  Cancel;
  Map.SliceAlong(MakeCoordPoint(x1,y1),MakeCoordPoint(x2,y2));
end;

procedure TScalpelHandler.Cancel;
begin
  inherited Cancel;
  StartObject:=nil;
end;

procedure TScalpelHandler.Refresh;
begin
  Map.ClearSelection(true);
  StartObject:=nil;
end;

procedure TScalpelHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var cx,cy:Coord;
P : CoordPoint;
D : Coord;
x1,y1: Integer;
begin
  Map.CurrentView.ScreenToCoord(x,y,cx,cy);

  Map.SelectFromPoint(cx,cy, ['L','l','P','p','C','c','K','k']);

  if state = tsOff then
  begin
    { If we're over a polyline or polycurve (fractal or normal),
      and it isn't the endpoint, allow them to manipulate the point }
    if Map.FindScalpelPoint(cx,cy,StartObject,node_index) then
    begin
      if GetKeyState(VK_CONTROL) < 0
       then CustomCursor := crTScalpelSeparate
       else CustomCursor := crTScalpelDelete;
    end
    else CustomCursor := crTScalpelAdd;
  end;

  If GetKeyState(VK_SHIFT) < 0 Then
  Begin
    If Map.FindClosestIntersection(cx,cy,P) Then
    Begin
      d := distance(cx,cy,p.x,p.y);
      Map.CurrentView.DeltaCoordToScreen(d,d,x1,y1);
      if (x1 < 10) and (y1 < 10) then
      begin
        CustomCursor := crTScalpelAddIntersection;
//        Map.CurrentView.CoordToScreen(P.X,P.Y,StartX,StartY);
//        EndX := StartX;
//        EndY := StartY;
        IX   := P.X;
        IY   := P.Y;
//        StartX       := P.X;
//        StartY       := P.Y;
{        cx     := p.x;
        cy     := p.y;
        ApplyScreenSnapsEX1 := ApplyScreenSnapsDX1;
        ApplyScreenSnapsEY1 := ApplyScreenSnapsDY1;
        ApplyScreenSnapsEX2 := ApplyScreenSnapsDX2;
        ApplyScreenSnapsEY2 := ApplyScreenSnapsDY2;
        ApplyScreenSnapsFloat(False,CX,CY,-1,False);
        StartX := X;
        StartY := Y;
}      end;
    End;
  End;


  inherited MouseMove(Shift,X,Y);
end;

procedure TScalpelHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if state = tsStarted Then
  Begin
    // If the shift key is not being pressed, then we'll end the click normally
    // (i.e. by performing scalpel operations).
    If HiWord(GetKeyState(VK_SHIFT)) = 0 then
    begin
      EndX:=X;
      EndY:=Y;
      Done;
    end
    Else
    begin
     // otherwise we'll keep on should keep on drawing cut lines
     // and attempt to use them later, but that ability is not working (see
     // MouseDown below) so we just cancel the operation all together
     Cancel;
    end;
  End;
end;

function TScalpelHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var NewObject:DrawPrimitive;
//I : Integer;
begin
  if Button=mbLeft then begin
    Map.SetModified(modChanged);

    case CustomCursor of
      crTScalpelDelete: begin    { Remove the point under the scalpel }
        Map.SetUndoPoint(res_seltool_undo_scalpel1);
        Map.SelectFromPoint(0,0, []);
        StartObject.Invalidate(Mainform.Handle,Map.CurrentView);
        StartObject.DeleteNode(Map.CurrentView,node_index);
        StartObject.Invalidate(Mainform.Handle,Map.CurrentView);
        StartObject:=nil;
        end;
      crTScalpelSeparate: begin  { Separate the point under the scalpel }
        Map.SetUndoPoint(res_seltool_undo_scalpel2);
        Map.SelectFromPoint(0,0, []);
        if StartObject.SeparateNode(Map.CurrentView,node_index, NewObject) then begin
          StartObject.Next.Invalidate(Mainform.Handle,Map.CurrentView);
          end;
        StartObject.Invalidate(Mainform.Handle,Map.CurrentView);
        end;
      crTScalpelAdd: begin
        inherited MouseDown(Button,Shift,X,Y);
        end;
      crTScalpelAddIntersection:     { JD 10-16-02: Add a point at this intersection }
      Begin
        Map.SetUndoPoint(res_seltool_undo_scalpel3);
//        For I := 0 To IntersectionList.Count - 1 Do
//        Begin
//          NewObject := IntersectionList.Objects[I] As DrawPrimitive;
          Map.SliceAtPoint(MakeCoordPoint(IX,IY));
//        End; // For I
//        Map.SelectFromPoint(0,0, []);
//        StartObject.Invalidate(Mainform.Handle,Map.CurrentView);
//        StartObject.DeleteNode(Map.CurrentView,node_index);
//        StartObject.Invalidate(Mainform.Handle,Map.CurrentView);
        StartObject:=nil;
      end;
    end;

    Result:=true;
    end
  else
    Result:=inherited MouseDown(Button,Shift,X,Y);
end;

{--------------------------------------------------------------------------------}

constructor TRulerToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  hint := TCustomHint.Create(cv);
  CustomCursor:=crRuler;
end;

destructor TRulerToolHandler.Destroy;
begin
  hint.Destroy;
  inherited Destroy;
end;

procedure TRulerToolHandler.Draw(erase:boolean);
var x1,y1,x2,y2:Coord;
begin
  Map.CurrentView.ScreenToCoord(StartX,StartY,x1,y1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,x2,y2);

  SetDistance(Distance(x1,y1,x2,y2), true);

  Canvas.Pen.Color:=clRed;
  Canvas.MoveTo(Round(StartX),Round(StartY));
  Canvas.LineTo(Round(EndX),Round(EndY));
end;

procedure TRulerToolHandler.SetDistance(d:double; showing:boolean);
begin
  HideCrosshair;
  hint.Visible := false;
  hint.X       := Round(EndX);
  hint.Y       := Round(EndY);
  hint.Text    := Format('%f %s', [(Map.CurrentView.Grid.GraphUnitConvert*
                         Map.CurrentView.Grid.GraphScale)*d,
                         Map.CurrentView.Grid.GraphUnits]);
  hint.Visible := showing;
end;

procedure TRulerToolHandler.Done;
var OldMode : TPenMode;
begin
  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmNotXor;
  Canvas.Pen.Color:=clRed;
  Canvas.MoveTo(Round(StartX),Round(StartY));
  Canvas.LineTo(Round(EndX),Round(EndY));
  Canvas.Pen.Mode := OldMode;

  inherited Done;
  SetDistance(0, False);
end;

procedure TRulerToolHandler.Cancel;
begin
  inherited Cancel;
  SetDistance(0, False);
end;

function TRulerToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
begin
  if Button<>mbRight then begin
    hint.StartX := X;
    hint.StartY := Y;
    Result := inherited MouseDown(Button,Shift,X,Y);
    end
  else
    Result:=false;
end;

{--------------------------------------------------------------------------------}

constructor TFreehandRulerToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  linelist:=TList.Create;
  CustomCursor:=crRuler;
end;

destructor TFreehandRulerToolHandler.Destroy;
begin
  inherited Destroy;
  ClearList;
  linelist.Destroy;
end;

procedure TFreehandRulerToolHandler.ClearList;
var i:integer;
begin
  for i:=0 to linelist.Count-1 do Dispose(linelist.Items[i]);
  linelist.Clear;
end;

function TFreehandRulerToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PCoordPoint;
    cx,cy:Coord;
    OldMode:TPenMode;
begin
  if (Button=mbLeft) then begin
    case state of
      tsOff: begin
          OldMode := Canvas.Pen.Mode;
          Canvas.Pen.Mode := pmNotXor;
          Paint;
          Canvas.Pen.Mode := OldMode;

          ClearList;
          state  := tsStarted;
        end;
      tsStarted: begin
        end;
      end;

    New(p);
    Map.CurrentView.ScreenToCoord(X,Y,cx,cy);
    p^.X := cx;
    p^.Y := cy;
    linelist.Add(p);

    Map.CurrentView.CoordToScreen(cx,cy,X,Y);
    StartX := X;
    StartY := Y;
    EndX   := X;
    EndY   := Y;
    Result:=true;
    total:=0.0;
    hint.StartX := X;
    hint.StartY := Y;
    SetDistance(total,True);
    end
  else
    Result:=false;
end;

procedure TFreehandRulerToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var p,plast:PCoordPoint;
    cx,cy:Coord;
    OldMode:TPenMode;
begin
  HideCrosshair;
  if state = tsStarted then begin
    Map.CurrentView.ScreenToCoord(X,Y,cx,cy);
    Map.CurrentView.CoordToScreen(cx,cy,X,Y);

    StartX := EndX;
    StartY := EndY;
    EndX   := X;
    EndY   := Y;

    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(false);
    Canvas.Pen.Mode := OldMode;

    plast:=PCoordPoint(linelist.Items[linelist.Count-1]);
    total:=total + Distance(cx,cy, plast^.X, plast^.Y);

    New(p);
    p^.X := cx;
    p^.Y := cy;
    linelist.Add(p);

    SetDistance(total,True);
    end;
  RefreshCursor;
  ShowCrosshair(X,Y);
end;

procedure TFreehandRulerToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (state=tsStarted) then begin
    Cancel;
    end;
end;

procedure TFreehandRulerToolHandler.Cancel;
var OldMode:TPenMode;
begin
  { Prevent Draw from re-xoring the last segment of the line }
  StartX := -1;
  inherited Cancel;

  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmNotXor;
  Paint;
  Canvas.Pen.Mode := OldMode;
  ClearList;
end;

procedure TFreehandRulerToolHandler.Draw(erase:boolean);
var x1,y1,x2,y2:Coord;
begin
  if not erase then begin
    Map.CurrentView.ScreenToCoord(StartX,StartY,x1,y1);
    Map.CurrentView.ScreenToCoord(EndX,EndY,x2,y2);
    SetDistance(Distance(x1,y1,x2,y2), true);
    end;

  if (StartX<>-1) then begin
    Canvas.Pen.Color:=clRed;
    Canvas.MoveTo(Round(StartX),Round(StartY));
    Canvas.LineTo(Round(EndX),Round(EndY));
    end;
end;

procedure TFreehandRulerToolHandler.Done;
begin
  inherited Done;
end;

procedure TFreehandRulerToolHandler.Paint;
var i:integer;
    sx,sy:integer;
    start:boolean;
    p:PCoordPoint;
begin
  Canvas.Pen.Color:=clRed;

  start:=true;
  for i:=0 to linelist.Count-1 do begin
    p:=PCoordPoint(linelist.Items[i]);
    Map.CurrentView.CoordToScreen(p^.X, p^.Y,sx,sy);
    if start then begin
      Canvas.MoveTo(sx,sy);
      start:=false;
      end
    else
      Canvas.LineTo(sx,sy);
    end;
end;


{--------------------------------------------------------------------------------}

procedure TZoomToolHandler.Zoom(zoomin:boolean; x,y:integer);
var cx,cy:Coord;
    px,py:single;
    ClientWidth,ClientHeight:integer;
    factor:single;
begin
  Map.CurrentView.ScreenToCoord(x,y,cx,cy);
  Map.CurrentView.GetCoordinateSize(ClientWidth,ClientHeight);
  px := (x/ClientWidth);
  py := (y/ClientHeight);

  try
    factor := StrToFloat(Settings.ZoomFactor.Text);
  except
    factor := 0.0;
  end;
  if (factor <= 1.0) then factor := 2.0;

  if zoomin then factor := 1.0/factor;

  Map.CurrentView.Zoom(MakeCoordPoint(cx,cy),factor,px,py);

  Map.SetModified(modViewport);
  MainForm.DoInvalidate;
end;

procedure TZoomToolHandler.Draw(erase:boolean);
var oc:TColor;
begin
  if DoingZoomRect(Round(EndX),Round(EndY)) then begin
    oc:=Canvas.Pen.Color;
    Canvas.Pen.Color := clRed;

    Marquis(Canvas,Round(StartX),Round(StartY),Round(EndX),Round(EndY));

    Canvas.Pen.Color := oc;
    end;
end;

function TZoomToolHandler.DoingZoomRect(X,Y:integer):boolean;
begin
  Result := (abs(StartX-X)>=10) or (abs(StartY-Y)>=10);
end;

procedure TZoomToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var r: CoordRect;
begin
  if state=tsStarted then begin
    if DoingZoomRect(X,Y) then begin
      R.Left   := StartX;
      R.Top    := StartY;
      R.Right  := EndX;
      R.Bottom := EndY;
      CorrectCoordRect(r);
      Map.CurrentView.SetCoordinateRect(Map.CurrentView.ScreenToCoordRect(r));
      Map.SetModified(modViewport);
      MainForm.DoInvalidate;
      end
    else begin
      if (Button = mbLeft) then begin
        Zoom(true,x,y);
        end
      else if (Button = mbRight) then begin
        Zoom(false,x,y);
        end;
      end;
    end;  

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TZoomToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  MainForm.Cursor := crZoom;
  inherited MouseMove(Shift, X, Y);
end;

function TZoomToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
begin
  StartX := X;
  StartY := Y;
  state  := tsStarted;
  Result := true;
end;

{--------------------------------------------------------------------------------}

procedure TPanToolHandler.Done;
begin
  inherited Done;
  Map.CurrentView.QuickDraw:=QuickDraw_None;
  InvalidateRect(MainForm.Handle, nil, true);
end;

function TPanToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
begin
  if (Button = mbLeft) then begin
    MainForm.PanTimer.Enabled := true;
    if Settings.DraftPanning.Checked then
      Map.CurrentView.QuickDraw:=QuickDraw_All
    else
      Map.CurrentView.QuickDraw:=QuickDraw_None;

    holdx:=X;
    holdy:=Y;
    holdtime:=0;
    end;
  Result:=inherited MouseDown(Button,Shift,X,Y);
end;

procedure TPanToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then begin
    MainForm.PanTimer.Enabled := false;
    Map.CurrentView.QuickDraw:=QuickDraw_None;
    end;
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TPanToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var dx,dy:integer;
begin
  MainForm.Cursor := crPan;

  if (state=tsStarted) then begin
    if (holdx <> X) or (holdy <> y) then begin
      if holdtime <> 0 then begin
        StartX := holdx;
        StartY := holdy;
        end;

      holdx:=X;
      holdy:=Y;
      holdtime := 0;
      end
    else begin
      inc(holdtime);
      end;

    dx := Round(X - StartX);
    dy := Round(Y - StartY);

    if (dx<0) then dx:=-trunc(sqrt(-dx)) else dx:=trunc(sqrt(dx));
    if (dy<0) then dy:=-trunc(sqrt(-dy)) else dy:=trunc(sqrt(dy));

    dx:=dx*2;
    dy:=dy*2;

    Map.Pan(dx,dy);
    end;
end;

initialization
  DontAskAboutFrozen:=false;

finalization

end.
