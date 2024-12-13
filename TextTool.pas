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
unit TextTool;

{$MODE Delphi}

interface

uses SysUtils, LCLIntf, LCLType, LMessages, Messages, Classes, Graphics, Controls, Forms, ToolObject,
     DrawLines, MapObject, SelectFont, Geometry, TextSpecialties, SymbolFile,
     MatrixMath, LocalizedStrings;

type TIconToolHandler = class(TToolHandler)
        size:integer;
     public
        constructor Create(cv:TCanvas);
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure ShowCrosshair(X,Y:integer); override;
        procedure HideCrosshair; override;
     end;

     TPatternIconToolHandler = class(TToolHandler)
     private
        fSquare:boolean;
        size:integer;
        procedure SetSquare(const Value: boolean);
     public
        iconlist:TList;

        property Square:boolean read FSquare write SetSquare;
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure ShowCrosshair(X,Y:integer); override;
        procedure HideCrosshair; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure ClearList;
        procedure Move(dx,dy:integer); override;
     end;

     TSprayIconToolHandler = class(TToolHandler)
        size:integer;
     public
        iconlist:TList;

        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        procedure ShowCrosshair(X,Y:integer); override;
        procedure HideCrosshair; override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure ClearList;
        procedure Move(dx,dy:integer); override;
     end;

     TTextToolHandler = class(TToolHandler)
     private
       format:integer;
       Font:TFont;
     public
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure UpdateFont;
        procedure Done; override;
        procedure Draw(erase:boolean); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
     end;

     TTextCurveToolHandler = class(TTextToolHandler)
        p1,p2,p3,p4:CoordPoint;
        which:integer;
        underneath:TBitmap;
     public
        constructor Create(cv:TCanvas);
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure Add(sp1,sp2,sp3,sp4:CoordPoint); virtual;
        procedure DrawCurve(sp1,sp2,sp3,sp4:CoordPoint); virtual;
        procedure Move(dx,dy:integer); override;
     end;

     THyperlinkToolHandler = class(TToolHandler)
     public
        constructor Create(cv:TCanvas);
//        procedure Draw(erase:boolean); override;
        procedure Done; override;
//        procedure Cancel; override;
        procedure Refresh; override;
     end;

var InsertSymbol:Symbol;

implementation

uses MAIN,Primitives,SettingsDialog,Snap,HyperlinkProp;

var iconlastx, iconlasty, iconlastsize, iconlastpos:integer;


function ComputeSymbolSize(relative:boolean; size:integer):integer;
var
  cx,cy : Coord;
  e     : CoordRect;
  C     : Coord;
  I     : Integer;

begin
  Result := size;

  if not relative then
  begin
    If Not Settings.cbFixedIconSize.Checked Then
    Begin
      Map.CurrentView.DeltaScreenToCoord(1,1,cx,cy);
      if cx <> 0 then Result := trunc(Result / cx);
    End
    Else
    Begin
      E := InsertSymbol.Objects.CoordExtent(true);
      C := Max(E.Right - E.Left,E.Bottom - E.Top);
      Map.CurrentView.DeltaCoordToScreen(C,C,Size,I);
      Result := Size;
    End;


  end;
end;

function ComputeTextSize(relative:boolean; size:integer):integer;
var cx,cy:Coord;
begin
  Result:=size;

  if not relative then begin
    Map.CurrentView.DeltaScreenToCoord(1,1,cx,cy);
    if (cx<>0) then Result:=trunc(Result/cx);
    end;
end;

function ComputeDensity(size:integer):integer;
begin
  Result:=MainForm.DensityBar.Max - MainForm.DensityBar.Position + MainForm.DensityBar.Min;
  Result:=(Result*size) div 24;
end;

procedure DrawIcon(Canvas:TCanvas; x,y:integer; size:integer);
var width,height:integer;
    r:TRect;
    OldMode:TPenMode;
begin
  if InsertSymbol=nil then exit;

  r:=InsertSymbol.Objects.Extent(Map.CurrentView,true);

  width := r.right-r.Left;
  height := r.bottom-r.top;

  if (width > height) then begin
     if (width=0) then width:=1;
     height:=size*height div width;
     width :=size;
  end else begin
     if (height=0) then height:=1;
     width:=size*width div height;
     height :=size;
  end;

  r.Left := x;
  r.Top  := y;
  r.Right := r.Left + width;
  r.Bottom := r.Top + height;

  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmNotXor;
  Canvas.Pen.Color := clRed;
  canvas.Brush.Style := bsClear;

  Canvas.Rectangle(r.left,r.top,r.right,r.bottom);

  Canvas.Pen.Mode := OldMode;
end;

function CopyOfSymbol(sym:Symbol; cx,cy:Coord; size:integer):DrawPrimitive;
var r:CoordRect;
    factor:Coord;
    width,height:Coord;
    sx,sy:Coord;
    mat:matrix;
    I : Integer;
begin
  // Make a copy and completely decouple it from the symbol's MapCollection

  I := Sym.Objects.BasePrimitives.Count;
  Result:=sym.Objects.CopyContents(True); // Make a FULL copy
  While Sym.Objects.BasePrimitives.Count > I Do Sym.Objects.BasePrimitives.Delete(Sym.Objects.BasePrimitives.Count - 1);

  // If empty symbol, don't do anything else with it.
  if (Result=nil) then exit;

  r:=Result.ChainExtent(true);

  // Figure out how big our on screen box is in coordinate system
  Map.CurrentView.DeltaScreenToCoord(size,size,sx,sy);

  // Set width and height to the aspect ratio of that box
  width := r.right-r.Left;
  height := r.bottom-r.top;

  if (width > height) then begin
     width :=sx;
  end else begin
     width :=sx*width/height;
  end;

  // This is the scaling factor and position
  // we need to apply to the symbol to locate
  // to the user's location
  factor:=width/(r.right-r.Left);

  mat:=OffsetMatrix(-r.left,-r.top);
  MatrixMultiplyBy(mat, ScaleMatrix(factor,factor));
  MatrixMultiplyBy(mat, OffsetMatrix(cx,cy));
  Result.ChainApplyMatrix(mat, true);
end;

procedure ShowIconCrosshair(Canvas:TCanvas; X,Y:integer);
begin
  PaintBox.Update;
  iconlastx:=x;
  iconlasty:=y;
  if (iconlastpos <> MainForm.IconSizeBar.Position) then begin
     iconlastsize:=ComputeSymbolSize(Settings.RelativeIconSize.Checked,MainForm.IconSizeBar.Position);
  end;
  DrawIcon(Canvas,iconlastx,iconlasty,iconlastsize);
end;

procedure HideIconCrosshair(Canvas:TCanvas);
begin
  if (iconlastx<>-1) then begin;
    DrawIcon(Canvas, iconlastx, iconlasty, iconlastsize);
    iconlastx:=-1;
    iconlastpos:=-1;
  end;
end;

{--------------------------------------------------------------------------------}

constructor TIconToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crSngIcon;
end;

procedure TIconToolhandler.Cancel;
begin
  HideCrosshair;
  inherited Cancel;
end;

procedure TIconToolHandler.Draw(erase:boolean);
begin
end;

procedure TIconToolHandler.Done;
var cx,cy:Coord;
begin
  if (InsertSymbol<>nil) then begin
    Map.CurrentView.ScreenToCoord(Endx,EndY,cx,cy);

    ApplySnaps(false,cx,cy);
    Map.StartAdding(res_texttool_symbol_add);
    try
      Map.AddObject(GroupPrimitive.Create(CopyOfSymbol(InsertSymbol,cx,cy,size)));
    except
        // Attempt to add null group
    end;
    Map.EndAdding;
    end;

  inherited Done;
end;

function TIconToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var
  oldpen : TPenMode;

begin
  if (Button=mbLeft) then begin
    size:=ComputeSymbolSize(Settings.RelativeIconSize.Checked,MainForm.IconSizeBar.Position);
    oldpen := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    EndX := x;
    EndY := y;
    Canvas.Pen.Mode := oldpen;
    end;

  Result:=inherited MouseDown(Button,Shift, X, Y);
end;

procedure TIconToolhandler.ShowCrosshair(X,Y:integer);
begin
  inherited ShowCrosshair(X,y);
  ShowIconCrosshair(Canvas,X,Y);
end;

procedure TIconToolhandler.HideCrosshair;
begin
  inherited HideCrosshair;
  HideIconCrosshair(Canvas);
end;


{--------------------------------------------------------------------------------}

procedure TPatternIconToolHandler.ClearList;
var i:integer;
begin
  for i:=0 to iconlist.Count-1 do Dispose(iconList.Items[i]);
  iconlist.Clear;
end;

procedure TPatternIconToolHandler.Cancel;
begin
  HideCrosshair;
  inherited Cancel;
  ClearList;
end;

procedure TPatternIconToolHandler.Draw(erase:boolean);
var i:integer;
    p:PTPoint;
begin
  for i:=0 to iconlist.Count-1 do begin
    p:=PTPoint(iconlist.Items[i]);
    DrawIcon(Canvas, p^.X,p^.Y,size);
    end;
end;

procedure TPatternIconToolHandler.Done;
var cx,cy:Coord;
    i:integer;
    p:PTPoint;
begin
  if (InsertSymbol=nil) then exit;

  Map.StartAdding(res_texttool_symbols_adds);

  // remove the placement rectangles
  Draw(true);

  for i:=0 to iconList.Count-1 do begin
    p:=PTPoint(iconList.Items[i]);
    Map.CurrentView.ScreenToCoord(p^.x,p^.y,cx,cy);
    try
      Map.AddObject(GroupPrimitive.Create(CopyOfSymbol(InsertSymbol,cx,cy,size)));
    except
        // Attempt to add null group
    end;
  end;

  // Reorder items if necessary
  Map.OrderSelection(OrderType(Settings.TopmostObject.ItemIndex));

  // Group them if necessary
  if (iconList.Count>1) and Settings.RedundantGrouping.Checked then Map.GroupSelected;

  Map.EndAdding;
  ClearList;
  inherited Done;
end;


function TPatternIconToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PTPoint;
begin
  Result:=inherited MouseDown(Button, Shift, X,Y);
  if (state=tsStarted) then begin
    ApplyScreenSnapsFloat(true,StartX,StartY);
    size:=ComputeSymbolSize(Settings.RelativeIconSize.Checked,MainForm.IconSizeBar.Position);
    ClearList;
    New(p);
    ApplyScreenSnaps(false,x,y);
    p^.X := X;
    p^.Y := Y;
    iconlist.Add(p);
    DrawIcon(Canvas, X,Y,size);
    end;
end;

procedure TPatternIconToolHandler.SetSquare(const Value: boolean);
begin
  FSquare := Value;
  if Square then
    CustomCursor:=crSqrIcon
  else
    CustomCursor:=crDiaIcon;
end;

procedure TPatternIconToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
label Done;
var p:PTPoint;
    i:integer;
    density:integer;
    HasPanned:boolean;
    oldx,oldy:integer;
begin
  HideCrosshair;
  oldx := x;
  oldy := y;

  if (state=tsStarted) then begin
    HasPanned:=Pan(x,y);

    density := ComputeDensity(size);

    if Square then begin
      X := Round(StartX + (Trunc((X - StartX) / density) * density));
      Y := Round(StartY + (Trunc((Y - StartY) / density) * density));
      end
    else begin
      X := Trunc((X - StartX) / density);
      Y := Trunc((Y - StartY) / density);
      if odd(y)
       then X := Round(StartX + X * density) + density div 2
       else X := Round(StartX + X * density);

      Y := Round(StartY + Y * density);
      end;

    // Is it already in the list?
    for i:=0 to iconList.Count-1 do begin
      p:=PTPoint(iconList.Items[i]);
      if (p^.X=X) and (p^.Y=Y) then goto Done;
      end;

    // Nope; add it
    New(p);
    p^.X := X;
    p^.Y := Y;
    iconlist.Add(p);

    if HasPanned then
      Draw(true)
    else
      DrawIcon(Canvas, X,Y,size);
    end;

Done:
  RefreshCursor;
  ShowCrossHair(oldx,oldy);
end;

constructor TPatternIconToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  iconlist:=TList.Create;
  AutoPan:=true;
end;

destructor TPatternIconToolHandler.Destroy;
begin
  inherited Destroy;
  ClearList;
  iconlist.Destroy;
end;

procedure TPatternIconToolHandler.Move(dx,dy:integer);
var i:integer;
    p:PTPoint;
begin
  inherited Move(dx,dy);
  for i:=0 to iconlist.Count-1 do begin
    p:=PTPoint(iconlist.Items[i]);
    p^.X := p^.X + dx;
    p^.Y := p^.Y + dy;
    end;
end;

procedure TPatternIconToolhandler.ShowCrosshair(X,Y:integer);
begin
  inherited ShowCrosshair(X,y);
  ShowIconCrosshair(Canvas,X,Y);
end;

procedure TPatternIconToolhandler.HideCrosshair;
begin
  inherited HideCrosshair;
  HideIconCrosshair(Canvas);
end;


{--------------------------------------------------------------------------------}

procedure TSprayIconToolHandler.ClearList;
var i:integer;
begin
  for i:=0 to iconlist.Count-1 do Dispose(iconList.Items[i]);
  iconlist.Clear;
end;

procedure TSprayIconToolHandler.Cancel;
begin
  HideCrosshair;
  inherited Cancel;
  ClearList;
end;

procedure TSprayIconToolHandler.Draw(erase:boolean);
var i:integer;
    p:PTPoint;
begin
  for i:=0 to iconlist.Count-1 do begin
    p:=PTPoint(iconlist.Items[i]);
    DrawIcon(Canvas, p^.X,p^.Y,size);
    end;
end;

procedure TSprayIconToolHandler.Done;
var cx,cy:Coord;
    i:integer;
    p:PTPoint;
begin
  if (InsertSymbol=nil) then exit;

  Map.StartAdding(res_texttool_symbols_adds);

  // erase placement rectangles
  Draw(true);

  for i:=0 to iconList.Count-1 do begin
    p:=PTPoint(iconList.Items[i]);
    Map.CurrentView.ScreenToCoord(p^.x,p^.y,cx,cy);
    try
      Map.AddObject(GroupPrimitive.Create(CopyOfSymbol(InsertSymbol,cx,cy,size)));
    except
        // Attempt to add null group
    end;
  end;
  // Reorder items if necessary
  Map.OrderSelection(OrderType(Settings.TopmostObject.ItemIndex));

  // Group them if necessary
  if (iconList.Count>1) and Settings.RedundantGrouping.Checked then Map.GroupSelected;

  Map.EndAdding;
  ClearList;
  inherited Done;
end;

function TSprayIconToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PTPoint;
begin
  Result:=inherited MouseDown(Button, Shift, X,Y);
  if (state=tsStarted) then begin
    ApplyScreenSnapsFloat(true,StartX,StartY);
    size:=ComputeSymbolSize(Settings.RelativeIconSize.Checked,MainForm.IconSizeBar.Position);
    ClearList;
    New(p);
    ApplyScreenSnaps(false,x,y);
    p^.X := X;
    p^.Y := Y;
    iconlist.Add(p);
    DrawIcon(Canvas, X,Y,size);
    end;
end;

procedure TSprayIconToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
label Done;
var p:PTPoint;
    density:integer;
    i:integer;
    HasPanned:boolean;
    oldx,oldy:integer;
begin
  HideCrosshair;
  oldx:=x;
  oldy:=y;

  if (state=tsStarted) then begin
    HasPanned:=Pan(x,y);
    density := ComputeDensity(size);

    X := X + random(2*density) - density;
    Y := Y + random(2*density) - density;

    // Is something close to our random spot already in the list?
    for i:=0 to iconList.Count-1 do begin
      p:=PTPoint(iconList.Items[i]);
      if (abs(p^.X - X)<(density/2)) and (abs(p^.Y-Y)<(density/2)) then goto Done;
      end;

    New(p);
    p^.X := X;
    p^.Y := Y;
    iconlist.Add(p);

    if HasPanned then
      Draw(true)
    else
      DrawIcon(Canvas, X, Y,size);
    end;

Done:
  RefreshCursor;
  ShowCrossHair(oldx,oldy);
end;

procedure TSprayIconToolhandler.ShowCrosshair(X,Y:integer);
begin
  inherited ShowCrosshair(X,y);
  ShowIconCrosshair(Canvas,X,Y);
end;

procedure TSprayIconToolhandler.HideCrosshair;
begin
  inherited HideCrosshair;
  HideIconCrosshair(Canvas);
end;


constructor TSprayIconToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  iconlist:=TList.Create;
  AutoPan:=true;
  CustomCursor:=crRndIcon;
end;

destructor TSprayIconToolHandler.Destroy;
begin
  inherited Destroy;
  ClearList;
  iconlist.Destroy;
end;

procedure TSprayIconToolHandler.Move(dx,dy:integer);
var i:integer;
    p:PTPoint;
begin
  inherited Move(dx,dy);
  for i:=0 to iconlist.Count-1 do begin
    p:=PTPoint(iconlist.Items[i]);
    p^.X := p^.X + dx;
    p^.Y := p^.Y + dy;
    end;
end;

{--------------------------------------------------------------------------------}

procedure TTextToolHandler.Done;
var cx,cy:Coord;
begin
  if (ChooseFont.TextContent.Text<>'') then begin
    Draw(true);
    Map.CurrentView.ScreenToCoord(Endx,EndY,cx,cy);
    ApplySnaps(false,cx,cy);
    Map.StartAdding(res_texttool_text_add);
    Map.AddObject(TextPrimitive.Create(Map.CurrentView, cx,cy,ChooseFont.TextContent.Text,Font,format,MainForm.OutlineColor.Color));
    Map.EndAdding;

    // Clear the selection so that changes to the font dialog don't hose
    // up our just placed text.  This is a convience so the user can drop
    // a bunch of text by just editing and clicking, instead of having to
    // unselect the text after each placement.
    Map.ClearSelection;

    ChooseFont.Show;
    end;
  inherited Done;
end;

procedure TTextToolHandler.UpdateFont;
begin
  Font.Name := ChooseFont.FontName.Text;
  Font.Size := ComputeTextSize(Settings.RelativeTextSize.Checked,StrToIntDef(ChooseFont.FontSize.Text,12));
  Font.Color:= CurrentColor;
  Font.Style:=[];
  if (ChooseFont.BoldButton.Down) then Font.Style:=Font.Style+[fsBold];
  if (ChooseFont.ItalicButton.Down) then Font.Style:=Font.Style+[fsItalic];
  if (ChooseFont.UnderlineButton.Down) then Font.Style:=Font.Style+[fsUnderline];

  Canvas.Font.Assign(Font);

  format := DT_NOPREFIX or DT_NOCLIP;
  if (ChooseFont.AlignCenterBtn.Down) then
    format:=format or DT_CENTER
  else if (ChooseFont.AlignRightBtn.Down) then
    format:=format or DT_RIGHT
  else if (ChooseFont.AlignLeftBtn.Down) then
    format:=format or DT_LEFT;
end;

procedure TTextToolHandler.Draw(erase:boolean);
begin
  UpdateFont;
  if not erase then ApplyScreenSnapsFloat(true,EndX,EndY);
  XorDrawText(Canvas,Round(EndX),Round(EndY),ChooseFont.TextContent.Text,format,MainForm.Color);
end;

function TTextToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var oldpen:TPenMode;
begin
  if (Button=mbLeft) then begin
    oldpen := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    ApplyScreenSnaps(true,x,y);
    EndX := x;
    EndY := y;
    Draw(false);
    Canvas.Pen.Mode := oldpen;
    end;

  Result:=inherited MouseDown(Button,Shift, X, Y);
end;

procedure TTextToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseMove(Shift,X,Y);
end;

constructor TTextToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  Font:=TFont.Create;
  AutoPan:=true;
  CustomCursor:=crText;
end;

destructor TTextToolHandler.Destroy;
begin
  inherited Destroy;
  Font.Free;
end;


{--------------------------------------------------------------------------------}

constructor TTextCurveToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  CustomCursor:=crCurve1;
end;

procedure TTextCurveToolHandler.Add(sp1,sp2,sp3,sp4:CoordPoint);
begin
  Map.StartAdding(res_texttool_text_c_add);
  Map.AddObject(TextCurvePrimitive.Create(Map.CurrentView, sp1,sp2,sp3,sp4,ChooseFont.TextContent.Text,Font,MainForm.OutlineColor.Color));
  Map.EndAdding;
end;

procedure TTextCurveToolHandler.Done;
var sp1,sp2,sp3,sp4:CoordPoint;
begin
  if which = 3 then
  begin
    Map.CurrentView.ScreenToCoord(trunc(p1.x),trunc(p1.y),sp1.x,sp1.y);
    Map.CurrentView.ScreenToCoord(trunc(p2.x),trunc(p2.y),sp2.x,sp2.y);
    Map.CurrentView.ScreenToCoord(trunc(p3.x),trunc(p3.y),sp3.x,sp3.y);
    Map.CurrentView.ScreenToCoord(trunc(p4.x),trunc(p4.y),sp4.x,sp4.y);
    ApplySnaps(false,sp1.x,sp1.y);
    ApplySnaps(false,sp2.x,sp2.y);
    ApplySnaps(false,sp3.x,sp3.y);
    ApplySnaps(false,sp4.x,sp4.y);
    Add(sp1,sp2,sp3,sp4);
  end;

  state := tsOff;
  EndX  := -1;
  underneath.Free;
  underneath:=nil;
end;

procedure TTextCurveToolHandler.Cancel;
begin
  inherited Cancel;
  which:=0;
  CustomCursor:=crCurve1;
  underneath.Free;
  underneath:=nil;
  MainForm.DoInvalidate;
end;

procedure TTextCurveToolHandler.DrawCurve(sp1,sp2,sp3,sp4:CoordPoint);
var OldColor:TColor;
    continue:TLineContinue;
    st:StyleAttrib;
begin
  UpdateFont;
  OldColor:=Canvas.Pen.Color;
  Canvas.Pen.Color:=clRed;
  st.Bits:=0;
  st.Line:=1;
  continue:=GetLineStyleStart(st);
  DrawBezier(Canvas,sp1,sp2,sp3,sp4,continue);
  Canvas.Pen.Color:=OldColor;
  DrawBezierText(Canvas,sp1,sp2,sp3,sp4,ChooseFont.TextContent.Text);
end;

procedure TTextCurveToolHandler.Draw(erase:boolean);
var r:TRect;
begin
  if (underneath<>nil) then begin
    r:=Rect(0,0,underneath.Width,underneath.Height);
    Main.PaintBox.Canvas.CopyRect(r,underneath.canvas,r); (*!!*)
    end;

  if not erase then ApplyScreenSnapsFloat(true,EndX,EndY);
  case (which) of
    1: DrawCurve(p1,p1,MakeCoordPoint(EndX,EndY),MakeCoordPoint(EndX,EndY));
    2: DrawCurve(p1,MakeCoordPoint(EndX,EndY),MakeCoordPoint(EndX,EndY),p4);
    3: DrawCurve(p1,p2,MakeCoordPoint(EndX,EndY),p4);
    end;
end;

function TTextCurveToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var r:TRect;
begin
  if (Button=mbRight) and (state<>tsOff) then begin
    Cancel;
    Result:=true;
    exit;
    end;

  if (Button=mbLeft) then begin
    if (which=0) then begin
      if (underneath=nil) then begin
        r:=Rect(0,0,MainForm.Width,MainForm.Height);
        underneath:=TBitmap.Create;
        underneath.Width := MainForm.Width;
        underneath.Height:= MainForm.Height;
        underneath.Canvas.CopyRect(r,MainForm.(*PaintBox.*)Canvas,r);
        end;
      end;

    state  := tsStarted;

    ApplyScreenSnaps(true,x,y);
    case (which) of
      0: begin
           p1.X := X;
           p1.Y := y;
           CustomCursor:=crCurve2;
         end;
      1: begin
           p4.X := X;
           p4.Y := y;
           CustomCursor:=crCurve3;
         end;
      2: begin
           p2.X := X;
           p2.Y := y;
           CustomCursor:=crCurve4;
         end;
      3: begin
           p3.X := X;
           p3.Y := y;
           Done;
           CustomCursor:=crCurve1;
         end;
      end;

    EndX:=X;
    EndY:=Y;

    which := (which+1) and 3;
    Draw(false);
    Result:=true;
    end
  else
    Result:=false;
end;

procedure TTextCurveToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Don't let the default handler do it; we don't want to leave the started state.
end;

procedure TTextCurveToolHandler.Move(dx,dy:integer);
var r:TRect;
begin
  inherited Move(dx,dy);
  p1.x:=p1.x+dx;  p1.y:=p1.y+dy;
  p2.x:=p2.x+dx;  p2.y:=p2.y+dy;
  p3.x:=p3.x+dx;  p3.y:=p3.y+dy;
  p4.x:=p4.x+dx;  p4.y:=p4.y+dy;

  MainForm.Repaint;

  r:=Rect(0,0,MainForm.Width,MainForm.Height);
  underneath.Canvas.CopyRect(r,MainForm.(*PaintBox.*)Canvas,r);
end;

{--------------------------------------------------------------------------------}

constructor THyperlinkToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  CustomCursor:=crHyperlink;
end;

//procedure THyperlinkToolHandler.Draw(erase:boolean);
//begin
//end;

procedure THyperlinkToolHandler.Done;
var cx,cy:Coord;
    flags:THyperlinkFlags;
begin
  Map.CurrentView.ScreenToCoord(Endx,EndY,cx,cy);

  if HyperlinkProperties.RadioHyperlink.Checked then
    flags := [hyperExecute]
  else
    flags := [];

  if HyperlinkProperties.CheckHidden.Checked then
    flags := flags + [hyperHidden];

  ApplySnaps(false,cx,cy);
  Map.StartAdding(res_texttool_hyperlink_add);
  Map.AddObject(HyperlinkPrimitive.Create(cx,cy,HyperlinkProperties.Text.Text, flags));
  Map.EndAdding;

  // Clear the selection so that changes to the hyperlink dialog don't hose
  // up our just placed link.  This is a convience so the user can drop
  // a bunch of links by just editing and clicking, instead of having to
  // unselect the links after each placement.
  Map.ClearSelection;

  inherited Done;
end;

procedure THyperlinkToolHandler.Refresh;
begin
  HyperlinkProperties.Show;
end;

//procedure THyperlinkToolHandler.Cancel;
//begin
//end;

{--------------------------------------------------------------------------------}

begin
  InsertSymbol:=nil;

  iconlastx:=-1;
  iconlasty:=-1;
  iconlastsize:=-1;
  iconlastpos:=-1;
end.
