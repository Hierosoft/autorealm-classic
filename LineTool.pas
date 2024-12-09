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
unit LineTool;

interface

uses SysUtils, Messages, Classes, Graphics, Controls, Forms, ToolObject,
     DrawLines, MapObject, Dialogs, Math, PolygonSides, Geometry, Primitives;

type TNormalLineToolHandler = class(TToolHandler)
     public
        constructor Create(cv:TCanvas);
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Add(cx1,cy1,cx2,cy2:Coord); virtual;
     end;

     { --- }

     type TFractalLineToolHandler = class(TNormalLineToolHandler)
     protected
        seed:integer;
        roughness:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Add(cx1,cy1,cx2,cy2:Coord); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
     end;

     { --- }

     TNormalPolylineToolHandler = class(TToolHandler)
        linelist:TList;
        continue:TLineContinue;
     protected
        SnapWhenDone:boolean;

     public
        procedure Done; override;
        procedure Draw(erase:boolean); override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure Start; virtual;
        procedure Add(list:PCoordArray; count:integer); virtual;
        procedure DrawXorPortion; virtual;
        procedure SetSeed; virtual;
        procedure ClearList;
        procedure CreateClosedFigure; override;
        procedure CreateOpenFigure; override;
        procedure Move(dx,dy:integer); override;
        procedure Backspace; override;
     end;

     { --- }

     TFractalPolylineToolHandler = class(TNormalPolylineToolHandler)
        seed,roughness:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Start; override;
        procedure Add(list:PCoordArray; count:integer); override;
        procedure DrawXorPortion; override;
        procedure SetSeed; override;
     end;

     { --- }

     TNormalCurveToolHandler = class(TToolHandler)
        p1,p2,p3,p4:CoordPoint;
        which:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Cancel; override;
        procedure Backspace; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure Add(sp1,sp2,sp3,sp4:CoordPoint); virtual;
        procedure DrawCurve(sp1,sp2,sp3,sp4:CoordPoint); virtual;
        constructor Create(cv:TCanvas);
        procedure Move(dx,dy:integer); override;
     end;

     { --- }

     TFractalCurveToolHandler = class(TNormalCurveToolHandler)
     public
        procedure Add(sp1,sp2,sp3,sp4:CoordPoint); override;
        procedure DrawCurve(sp1,sp2,sp3,sp4:CoordPoint); override;
     end;

     { --- }

     TNormalArcToolHandler = class(TToolHandler)
        p1,p2,p3 : CoordPoint;
        Which    : Integer;
     public
        procedure   Draw(Erase: Boolean); override;
        procedure   Done; override;
        procedure   Cancel; override;
        procedure   Backspace; override;
        function    MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
        procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure   Add(sp1,sp2,sp3: CoordPoint); virtual;
        procedure   DrawCurve(sp1,sp2,sp3: CoordPoint); virtual;
        constructor Create(cv: TCanvas);
        procedure   Move(dx,dy: integer); override;
     end;

     { --- }

     TPolyCurveToolHandler = class(TNormalCurveToolHandler)
        linelist:TList;
        continue:TLineContinue;
     public
        procedure Done; override;
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        procedure AddPoint(pt:CoordPoint);
        procedure AddCurve(list:PCoordArray; count:integer); virtual;
        procedure AddCurveSection;
        procedure Start; virtual;
        procedure ClearList;
        procedure CreateClosedFigure; override;
        procedure CreateOpenFigure; override;
        procedure Move(dx,dy:integer); override;
        procedure Backspace; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        Procedure ForceContinuity(Shift: TShiftState; Var X,Y: Integer); Virtual;
     end;

     { --- }

     TFractalPolyCurveToolHandler = class(TPolyCurveToolHandler)
     protected
        seed:integer;
        roughness:integer;
     public
        procedure Start; override;
        procedure AddCurve(list:PCoordArray; count:integer); override;
        procedure DrawCurve(sp1,sp2,sp3,sp4:CoordPoint); override;
     end;


     { --- }

     TNormalCircleToolHandler = class(TToolHandler)
     private
        lastx1,lasty1,lastx2,lasty2:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Add(cx1,cy1,cx2,cy2:Coord); virtual;
        constructor Create(cv:TCanvas);
     end;

     { --- }
     TRectangleToolHandler = class(TToolHandler)
     private
        lastx1,lasty1,lastx2,lasty2:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        constructor Create(cv:TCanvas);
     end;

     { --- }

     TPolygonToolHandler = class(TToolHandler)
     private
        sides:integer;
        initialangle:double;
        lastx1,lasty1,lastx2,lasty2:integer;
     public
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        procedure Add(cx1,cy1,cx2,cy2:Coord); virtual;
        function AskSides:boolean;
        constructor Create(cv:TCanvas);
     end;

     { --- }

     TFreehandLineToolHandler = class(TNormalPolylineToolHandler)
     public
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        constructor Create(cv:TCanvas);
        procedure CreateClosedFigure; override;
        procedure CreateOpenFigure; override;
        procedure Done; override;
        procedure Backspace; override;
     end;

     { --- }

     TFreehandFractalToolHandler = class(TFractalPolylineToolHandler)
     public
        procedure Cancel; override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        constructor Create(cv:TCanvas);
        procedure CreateClosedFigure; override;
        procedure CreateOpenFigure; override;
        procedure Done; override;
        procedure Backspace; override;
     end;

     { --- }

     TChartGridToolHandler = class(TToolHandler)
     private
        center:CoordPoint;
        radius:double;
        croprect:CoordRect;
        iscropping:boolean;
        FinishedCircle:boolean;
        allocatedlines,linecount:integer;
        linelist:PCoordArray;
     public
        function AskGrid:boolean;
        procedure Draw(erase:boolean); override;
        procedure Done; override;
        constructor Create(cv:TCanvas);
        procedure FreshenLineList;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; override;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
     end;

     function GetCurrentlySetStyles:StyleAttrib;

implementation

uses Main,ChartGrid,Snap,SettingsDialog,ColorButton,LocalizedStrings;

const fqSmooth = 0;
      fqStandard = 1;
      fqSelective = 2;

function GetCurrentlySetStyles:StyleAttrib;
Var
  Thickness : Coord;
  I         : Integer;
  T1,T2     : Coord;

begin
  Result.Line := MainForm.LineStyleComboBox.ItemIndex;
  Result.Fill := MainForm.FillPattern.Pattern;
  Result.First:= MainForm.BeginLineStyle.ItemIndex;
  Result.Last := MainForm.EndLineStyle.ItemIndex;
  Val(MainForm.edtLineThickness.Text,Thickness,I);
  If I = 0 Then
  Begin
    Result.FullStyle.Thickness := Thickness;
    T1                         := Thickness;
    T2                         := Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    If T2 <> 0 Then T1 := T1 / T2;
    Map.CurrentView.DeltaCoordToScreen(T1,0,T1,T2);
    Result.FullStyle.SThickness := T1;
  End
  Else
  Begin
    Result.FullStyle.Thickness  := 0;
    Result.FullStyle.SThickness := 0;
  End;
end;


constructor TNormalLineToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crLine;
end;

procedure TNormalLineToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    ex,ey:Coord;
begin
  oldcolor:=Canvas.Pen.Color;
  Canvas.Pen.Color:=CurrentColor;
  if not erase then begin
    ApplyScreenSnapsFloat(false,StartX,StartY);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyDirectionalSnap(StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;
    ApplyScreenSnapsFloat(true,EndX,EndY);
    end;  
  DrawLineStyle(Canvas, Round(StartX),Round(StartY),Round(EndX),Round(EndY), GetCurrentlySetStyles);
  Canvas.Pen.Color:=oldcolor;
end;

procedure TNormalLineToolHandler.Add(cx1,cy1,cx2,cy2:Coord);
begin
  Map.StartAdding(res_linetool_line_add);
  Map.AddObject(LinePrimitive.Create(cx1,cy1,cx2,cy2,GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TNormalLineToolHandler.Done;
var cx1,cx2,cy1,cy2:Coord;
begin
  if (StartX=EndX) and (StartY=EndY) then begin
    Map.ClearSelection;
    Cancel;
    exit;
    end;

  Map.CurrentView.ScreenToCoord(StartX,StartY,cx1,cy1);
  ApplySnaps(false,cx1,cy1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,cx2,cy2);
  if GetKeyState(VK_SHIFT)<0 then ApplyDirectionalSnap(cx1,cy1,cx2,cy2);
  ApplySnaps(false,cx2,cy2);
  Add(cx1,cy1,cx2,cy2);
  inherited Done;
end;

{------------------------------------------------------------------------------------}

procedure TFractalLineToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    continue:TLineContinue;
    ex,ey:Coord;
begin
  oldcolor:=Canvas.Pen.Color;
{  Canvas.Pen.Color:=clRed;
  Canvas.MoveTo(StartX,StartY);
  Canvas.LineTo(EndX,EndY); }
  Canvas.Pen.Color:=CurrentColor;
  if not erase then begin
    ApplyScreenSnapsFloat(false,StartX,StartY);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyDirectionalSnap(StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;
    ApplyScreenSnapsFloat(true,EndX,EndY);
    end;
  continue:=GetLineStyleStart(GetCurrentlySetStyles);

  // Reset the fractal seed; Screen snap logic uses a new
  // seed when it traverses to find fractal snapping points.
  FractalSetSeed(seed);
  FractalLine(Canvas,StartX,StartY,EndX,EndY,(distance(startx,starty,endx,endy)*roughness)/1000, continue);
  Canvas.Pen.Color:=oldColor;
end;

procedure TFractalLineToolHandler.Add(cx1,cy1,cx2,cy2:Coord);
begin
  Map.StartAdding(res_linetool_linef_add);
  MainForm.SeedSpin.Value:=(MainForm.SeedSpin.Value+1) and 65535;
  Map.AddObject(LinePrimitive.Create(cx1,cy1,cx2,cy2,seed,roughness,GetCurrentlySetStyles));
  Map.EndAdding;
end;

function TFractalLineToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
begin
  seed := MainForm.SeedSpin.Value;
  roughness := MainForm.RoughnessTrackBar.Position;
  Result:=inherited MouseDown(Button,Shift,X,Y);
end;

{------------------------------------------------------------------------------------}
procedure TNormalPolylineToolHandler.Backspace;
var p:PCoordPoint;
begin
  if (linelist.Count=1) then
    Cancel
  else if (linelist.Count>1) then begin
    DrawXorPortion;
    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    EndX:=Trunc(p.x);
    EndY:=Trunc(p.y);
    p:=PCoordPoint(linelist.Items[linelist.Count-2]);
    StartX:=Trunc(p.x);
    StartY:=Trunc(p.y);
    linelist.Delete(linelist.Count-1);
    MouseMove([],LastMouseX,LastMouseY);
    end;
end;

procedure TNormalPolylineToolHandler.ClearList;
var i:integer;
begin
  for i:=0 to linelist.Count-1 do Dispose(linelist.Items[i]);
  linelist.Clear;
end;

procedure TNormalPolylineToolHandler.Cancel;
begin
  DrawXorPortion;
  inherited Cancel;
  Done;
end;

procedure TNormalPolylineToolHandler.Add(list:PCoordArray; count:integer);
var lbl:string;
begin
  if SnapWhenDone then lbl:= res_linetool_linepoly_add else lbl:=res_linetool_linefree_add;
  Map.StartAdding(lbl);
  Map.AddObject(PolylinePrimitive.Create(list, count, GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TNormalPolylineToolHandler.Done;
var cx,cy,lastcx,lastcy:Coord;
    i,n,j:integer;
    p:PCoordPoint;
    list:PCoordArray;
begin
  if (linelist.Count=0) then exit;

  if (linelist.Count=1) then begin
    Map.ClearSelection;
    ClearList;
    state:=tsOff;
    CustomCursor:=crLine1;
    exit;
    end;

  { Remove duplicate points on top of each other (freehand) }
  n:=linelist.Count;
  lastcx:=0; lastcy:=0;
  GetMem(list,sizeof(CoordPoint)*n);
  j:=0;
  for i:=0 to n-1 do begin
    p:=PCoordPoint(linelist.Items[i]);
    Map.CurrentView.ScreenToCoord(p^.x,p^.y,cx,cy);
    if SnapWhenDone then ApplySnaps(false,cx,cy);
    if (i=0) or ((lastcx<>cx) or (lastcy<>cy)) then begin
      list^[j].X := cx;
      list^[j].Y := cy;
      lastcx:=cx;
      lastcy:=cy;
      inc(j);
      end;
    end;

  Add(list, j);
  ClearList;
  inherited Done;
  CustomCursor:=crLine1;
end;

constructor TNormalPolylineToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  linelist:=TList.Create;
  SnapWhenDone:=true;
  AutoPan:=true;
  CustomCursor:=crLine1;
end;

destructor TNormalPolylineToolHandler.Destroy;
begin
  inherited Destroy;
  ClearList;
  linelist.Destroy;
end;

procedure TNormalPolylineToolHandler.Start;
begin
  continue := GetLineStyleStart(GetCurrentlySetStyles);
  CustomCursor:=crLine1;
end;

procedure TNormalPolylineToolHandler.Move(dx,dy:integer);
var i:integer;
    p:PCoordPoint;
begin
  inherited Move(dx,dy);
  for i:=0 to linelist.Count-1 do begin
    p:=PCoordPoint(linelist.Items[i]);
    p^.X := p^.X + dx;
    p^.Y := p^.Y + dy;
    end;
end;

procedure TNormalPolylineToolHandler.CreateClosedFigure;
var p:PCoordPoint;
    x,y:Coord;
begin
  if (state=tsStarted) then begin
    MainForm.DoInvalidate;     // Make sure the ending line is undrawn

    p:=PCoordPoint(linelist.Items[0]);
    X := p^.X;
    Y := p^.Y;
    New(p);
    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);

    Cancel;
    end;
end;

procedure TNormalPolylineToolHandler.CreateOpenFigure;
begin
  if (state=tsStarted) then begin
    Cancel;
    end;
end;

function TNormalPolylineToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PCoordPoint;
    ex,ey:Coord;
begin
  if (Button=mbRight) then begin
    if (state=tsStarted) then begin
      if not Settings.AskForShapeClosure.Checked then begin
        if (ssShift in Shift) then CreateClosedFigure
                              else CreateOpenFigure;
        end
      else
        MainForm.ShapePopupMenu.Popup(X,Y);

      Result:=true;
      end
    else begin
      Result:=inherited MouseDown(Button, Shift, X, Y);
      end;

    exit;
    end;

  if (Button=mbLeft) then begin
    case state of
      tsOff: begin
          Start;
          ClearList;
          state  := tsStarted;
          CustomCursor:=crLine1;
        end;
      tsStarted: begin
          CustomCursor:=crLine2;
        end;
      end;

    New(p);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=X;
      ey:=Y;
      ApplyDirectionalSnap(StartX,StartY,ex,ey);
      X:=trunc(ex);
      Y:=trunc(ey);
      end;
    ApplyScreenSnaps(true,X,Y);

    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);

    StartX := X;
    StartY := Y;
    Result:=true;
    end
  else
    Result:=false;
end;

procedure TNormalPolylineToolHandler.DrawXorPortion;
var oldcolor:TColor;
    OldMode:TPenMode;
begin
  if (EndX<>-1) then begin
    OldMode := Canvas.Pen.Mode;
    oldcolor:=Canvas.Pen.Color;

    Canvas.Pen.Mode := pmNotXor;
    Canvas.Pen.Color:=CurrentColor;

    continue := GetLineStyleStart(GetCurrentlySetStyles);

    // Don't let the begin arrow tip get draw anew for each line segment.
    if (linelist.Count>1) then continue.Style.First:=0;

    DrawLineContinue(Canvas, StartX,StartY,EndX,EndY, continue);

    Canvas.Pen.Color:=oldcolor;
    Canvas.Pen.Mode:=OldMode;
    end;
end;

procedure TNormalPolylineToolHandler.SetSeed;
begin
end;

procedure TNormalPolylineToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var oldcolor:TColor;
    OldMode:TPenMode;
    ex,ey:Coord;
begin
  HideCrosshair;
  if (state=tsStarted) then begin
    SetSeed;
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    oldcolor:=Canvas.Pen.Color;
    Canvas.Pen.Color:=CurrentColor;
    DrawXorPortion;

    if Pan(X,Y) then begin
      Canvas.Pen.Mode := OldMode;
      Draw(false);
      Canvas.Pen.Mode := pmNotXor;
      SetSeed;
      end;

    EndX := X;
    EndY := Y;
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyDirectionalSnap(StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;
    ApplyScreenSnapsFloat(true,EndX,EndY);

    // Reset the fractal seed; Screen snap logic uses a new
    // seed when it traverses to find fractal snapping points.
    SetSeed;

    DrawXorPortion;

    Canvas.Pen.Color:=oldcolor;
    Canvas.Pen.Mode:=OldMode;
    end;
  RefreshCursor;
  ShowCrosshair(X,Y);
end;

procedure TNormalPolylineToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Don't let the default handler do it; we don't want to leave the started state.
end;

procedure TNormalPolylineToolHandler.Draw(erase:boolean);
var i:integer;
    x,y:Coord;
    p:PCoordPoint;
    oldcolor:TColor;
begin
   oldcolor:=Canvas.Pen.Color;
   Canvas.Pen.Color:=CurrentColor;

   if (linelist.Count>0) then begin
     continue := GetLineStyleStart(GetCurrentlySetStyles);
     p:=PCoordPoint(linelist.Items[0]);
     X:=p^.X;
     Y:=p^.y;

     for i:=1 to linelist.Count-1 do begin
       p:=PCoordPoint(linelist.Items[i]);
       DrawLineContinue(Canvas, X,Y,p^.X,p^.Y, continue);
       X:=p^.X;
       Y:=p^.y;
       end;
     end;

   Canvas.Pen.Color:=oldcolor;
end;

{------------------------------------------------------------------------------------}

procedure TFractalPolylineToolHandler.Add(list:PCoordArray; count:integer);
var lbl:string;
begin
  if SnapWhenDone then lbl:=res_linetool_linefpoly_add else lbl:=res_linetool_lineffree_add;
  Map.StartAdding(lbl);
  MainForm.SeedSpin.Value:=(MainForm.SeedSpin.Value+count-1) and 65535;
  Map.AddObject(PolylinePrimitive.Create(list, count, seed, roughness, GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TFractalPolylineToolHandler.Start;
begin
  inherited Start;
  seed := MainForm.SeedSpin.Value;
  roughness := MainForm.RoughnessTrackBar.Position;
end;

procedure TFractalPolylineToolHandler.DrawXorPortion;
var oldcolor:TColor;
    OldMode:TPenMode;
    continue:TLineContinue;
begin
  if (EndX<>-1) then begin
    OldMode := Canvas.Pen.Mode;
    oldcolor:=Canvas.Pen.Color;

    Canvas.Pen.Mode := pmNotXor;
    Canvas.Pen.Color:=CurrentColor;

    continue:=GetLineStyleStart(GetCurrentlySetStyles);
    FractalLine(Canvas,StartX,StartY,EndX,EndY,(distance(StartX,StartY,EndX,EndY)*roughness)/1000, continue);

    Canvas.Pen.Color:=oldcolor;
    Canvas.Pen.Mode:=OldMode;
    end;
end;

procedure TFractalPolylineToolHandler.SetSeed;
begin
  FractalSetSeed(seed + linelist.Count - 1);
end;

procedure TFractalPolylineToolHandler.Draw(erase:boolean);
var i:integer;
    x,y:Coord;
    p:PCoordPoint;
    oldcolor:TColor;
begin
   oldcolor:=Canvas.Pen.Color;
   Canvas.Pen.Color:=CurrentColor;

   if (linelist.Count>0) then begin
     continue := GetLineStyleStart(GetCurrentlySetStyles);
     p:=PCoordPoint(linelist.Items[0]);
     X:=p^.X;
     Y:=p^.y;

     for i:=1 to linelist.Count-1 do begin
       p:=PCoordPoint(linelist.Items[i]);
       FractalSetSeed(seed + i - 1);
       FractalLine(Canvas,X,Y,p^.X,p^.Y,(distance(X,Y,p^.X,p^.Y)*roughness)/1000, continue);
       X:=p^.X;
       Y:=p^.y;
       end;
     end;

   Canvas.Pen.Color:=oldcolor;
end;

{------------------------------------------------------------------------------------}

procedure TNormalCurveToolHandler.Backspace;
  procedure XorDraw;
  var OldMode:TPenMode;
  begin
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(true);
    Canvas.Pen.Mode := OldMode;
  end;
begin
  if (which>1) then begin
    XorDraw;
    dec(which);
    XorDraw;
    end
  else
    Cancel;

  case (which) of
    0: CustomCursor:=crCurve1;
    1: CustomCursor:=crCurve2;
    2: CustomCursor:=crCurve3;
    3: CustomCursor:=crCurve4;
    end;
end;

constructor TNormalCurveToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crCurve1;
end;

procedure TNormalCurveToolHandler.Add(sp1,sp2,sp3,sp4:CoordPoint);
begin
  Map.StartAdding(res_linetool_curve_add);
  Map.AddObject(CurvePrimitive.Create(sp1,sp2,sp3,sp4,GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TNormalCurveToolHandler.Done;
var sp1,sp2,sp3,sp4:CoordPoint;
begin
  if (which=3) then begin
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

  inherited Done;
end;

procedure TNormalCurveToolHandler.Move(dx,dy:integer);
begin
  inherited Move(dx,dy);
  p1.x:=p1.x+dx;  p1.y:=p1.y+dy;
  p2.x:=p2.x+dx;  p2.y:=p2.y+dy;
  p3.x:=p3.x+dx;  p3.y:=p3.y+dy;
  p4.x:=p4.x+dx;  p4.y:=p4.y+dy;
end;

procedure TNormalCurveToolHandler.Cancel;
begin
  inherited Cancel;
  which:=0;
  CustomCursor:=crCurve1;
end;

procedure TNormalCurveToolHandler.DrawCurve(sp1,sp2,sp3,sp4:CoordPoint);
var continue:TLineContinue;
begin
  continue:=GetLineStyleStart(GetCurrentlySetStyles);
  DrawBezier(Canvas,sp1,sp2,sp3,sp4,continue);
end;

procedure TNormalCurveToolHandler.Draw(erase:boolean);
begin
  if not erase then ApplyScreenSnapsFloat(true,EndX,EndY);
  case (which) of
    1: DrawCurve(p1,p1,MakeCoordPoint(EndX,EndY),MakeCoordPoint(EndX,EndY));
    2: DrawCurve(p1,MakeCoordPoint(EndX,EndY),MakeCoordPoint(EndX,EndY),p4);
    3: DrawCurve(p1,p2,MakeCoordPoint(EndX,EndY),p4);
    end;
end;

function TNormalCurveToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var OldMode:TPenMode;
begin
  if (Button=mbRight) and (state<>tsOff) then begin
    Cancel;
    Result:=true;
    exit;
    end;

  if (Button=mbLeft) then begin
    state  := tsStarted;
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(true);

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
    Canvas.Pen.Mode := OldMode;
    Result:=true;
    end
  else
    Result:=false;
end;

procedure TNormalCurveToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Don't let the default handler do it; we don't want to leave the started state.
end;

{------------------------------------------------------------------------------------}


procedure TFractalCurveToolHandler.DrawCurve(sp1,sp2,sp3,sp4:CoordPoint);
var d:double;
    continue:TLineContinue;
begin
  d := distance(sp1.x,sp1.y,sp2.x,sp2.y) + distance(sp4.x,sp4.y,sp3.x,sp3.y);

  FractalSetSeed(MainForm.SeedSpin.Value);
  continue:=GetLineStyleStart(GetCurrentlySetStyles);
  DrawFractalBezier(Canvas,sp1,sp2,sp3,sp4,(d * MainForm.RoughnessTrackBar.Position)/1000, continue);
end;

procedure TFractalCurveToolHandler.Add;
var seed:integer;
begin
  seed:=MainForm.SeedSpin.Value;
  Map.StartAdding(res_linetool_curvef_add);
  MainForm.SeedSpin.Value:=(seed+1) and 65535;
  Map.AddObject(CurvePrimitive.Create(sp1,sp2,sp3,sp4,
                    seed,MainForm.RoughnessTrackBar.Position, GetCurrentlySetStyles));
  Map.EndAdding;
end;

{------------------------------------------------------------------------------------}

Procedure TNormalArcToolHandler.Backspace;

  Procedure XorDraw;
  Var OldMode: TPenMode;
  Begin
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(True);
    Canvas.Pen.Mode := OldMode;
  End; // XorDraw

Begin
  If Which > 1 Then
  Begin
    XorDraw;
    Dec(Which);
    XorDraw;
  End
  Else Cancel;

  Case Which Of
    0: CustomCursor := crLine;
    1: CustomCursor := crLine;
    2: CustomCursor := crCurve3;
  End; // Case
end; // TNormalArcToolHandler.Backspace

Constructor TNormalArcToolHandler.Create(cv: TCanvas);
Begin
  Inherited Create(cv);
  AutoPan := True;
  CustomCursor := crLine;
End; // TNormalArcToolHandler.Create

Procedure TNormalArcToolHandler.Add(sp1,sp2,sp3: CoordPoint);
Var CX: CoordPoint;
Begin
  Map.StartAdding(res_linetool_arc_add);
  GetArcCenter(sp1,sp2,sp3,CX.X,CX.Y);
  Map.AddObject(PolyCurvePrimitive.Arc(CX,sp2,sp3,GetCurrentlySetStyles));
  Map.EndAdding;
End; // TNormalArcToolHandler.Add

Procedure TNormalArcToolHandler.Done;
Var sp1,sp2,sp3: CoordPoint;
Begin
  If Which = 2 Then
  Begin
    Map.CurrentView.ScreenToCoord(trunc(p1.x),trunc(p1.y),sp1.x,sp1.y);
    Map.CurrentView.ScreenToCoord(trunc(p2.x),trunc(p2.y),sp2.x,sp2.y);
    Map.CurrentView.ScreenToCoord(trunc(p3.x),trunc(p3.y),sp3.x,sp3.y);
    ApplySnaps(false,sp1.x,sp1.y);
    ApplySnaps(false,sp2.x,sp2.y);
    ApplySnaps(false,sp3.x,sp3.y);
    Add(sp1,sp2,sp3);
  End;
  Inherited Done;
End; // TNormalArcToolHandler.Done

Procedure TNormalArcToolHandler.Move(dx,dy: Integer);
Begin
  Inherited Move(dx,dy);
  p1.x := p1.x + dx;  p1.y := p1.y + dy;
  p2.x := p2.x + dx;  p2.y := p2.y + dy;
  p3.x := p3.x + dx;  p3.y := p3.y + dy;
End; // TNormalArcToolHandler.Move

Procedure TNormalArcToolHandler.Cancel;
Begin
  Inherited Cancel;
  Which        := 0;
  CustomCursor := crLine;
End; // TNormalArcToolHandler.Cancel

Procedure TNormalArcToolHandler.DrawCurve(sp1,sp2,sp3: CoordPoint);
Var Continue: TLineContinue;
Begin
  Continue := GetLineStyleStart(GetCurrentlySetStyles);
  DrawArc(Canvas,sp1,sp2,sp3,Continue);
End; // TNormalArcToolHandler.DrawCurve

Procedure TNormalArcToolHandler.Draw(Erase: Boolean);
Var
  OldColor : TColor;
  C        : CoordPoint;

Begin
  If Not Erase Then ApplyScreenSnapsFloat(True,EndX,EndY);
  Case Which Of
    1: Begin
         C := MakeCoordPoint(EndX,EndY);
         OldColor           := Canvas.Pen.Color;
         Canvas.Pen.Color   := CurrentColor;
         Canvas.Brush.Style := bsClear;
         Canvas.MoveTo(Round(P2.X),Round(P2.Y));
         Canvas.LineTo(Round(C.X),Round(C.Y));
         Canvas.Pen.Color   := OldColor;
       End;
    2: DrawCurve(MakeCoordPoint(EndX,EndY),P2,P3);
  End; // Case
End; // TNormalArcToolHandler.Draw

Function TNormalArcToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var OldMode:TPenMode;
Begin
  If (Button = mbRight) And (State <> tsOff) Then
  Begin
    Cancel;
    Result := True;
    Exit;
  End;

  If Button = mbLeft Then
  Begin
    State           := tsStarted;
    OldMode         := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(True);

    ApplyScreenSnaps(True,x,y);

    Case Which Of
    0: Begin
         p2.X := X;
         p2.Y := y;
         CustomCursor := crLine;
       End;
    1: Begin
         p3.X := X;
         p3.Y := y;
         CustomCursor := crCurve3;
       End;
    2: Begin
         p1.X := X;
         p1.Y := y;
         Done;
         CustomCursor := crLine;
       End;
    End; // Case

    EndX := X;
    EndY := Y;

    Which := (Which + 1) Mod 3;
    Draw(False);
    Canvas.Pen.Mode := OldMode;
    Result          := True;
  End
  Else Result := False;
End; // TNormalArcToolHandler.MouseDown

Procedure TNormalArcToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  // Don't let the default handler do it; we don't want to leave the started state.
End;

{--------------------------------------------------------------------------------}

procedure TPolyCurveToolHandler.Backspace;
var p:PCoordPoint;
  procedure XorDraw;
  var OldMode:TPenMode;
  begin
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(true);
    Canvas.Pen.Mode := OldMode;
  end;
begin
  if (which>1) or (linelist.Count=0) then begin
    inherited Backspace;
    end
  else begin
    XorDraw;

    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    p4.x:=p^.x;
    p4.y:=p^.y;
    linelist.Delete(linelist.Count-1);

    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    p3.x:=p^.x;
    p3.y:=p^.y;
    linelist.Delete(linelist.Count-1);

    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    p2.x:=p^.x;
    p2.y:=p^.y;
    linelist.Delete(linelist.Count-1);

    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    p1.x:=p^.x;
    p1.y:=p^.y;
    which:=3;

    EndX:=trunc(p3.x);
    EndY:=trunc(p3.y);
    if (linelist.Count=1) then linelist.Delete(0);
    MouseMove([],LastMouseX,LastMouseY);
    end;
end;

procedure TPolyCurveToolHandler.Start;
begin
  continue := GetLineStyleStart(GetCurrentlySetStyles);
  which := 0;
end;

procedure TPolyCurveToolHandler.AddCurveSection;
begin
  DrawCurve(p1,p2,p3,p4);

  if (linelist.Count=0) then AddPoint(p1);
  AddPoint(p2);
  AddPoint(p3);
  AddPoint(p4);
end;

procedure TPolyCurveToolHandler.Move(dx,dy:integer);
var i:integer;
    p:PCoordPoint;
begin
  inherited Move(dx,dy);

  for i:=0 to linelist.Count-1 do begin
    p:=PCoordPoint(linelist.Items[i]);
    p^.X := p^.X + dx;
    p^.Y := p^.Y + dy;
    end;
end;

procedure TPolyCurveToolHandler.Done;
var cx,cy:Coord;
    i:integer;
    p:PCoordPoint;
    list:PCoordArray;
begin
  which:=0;
  CustomCursor:=crCurve1;

  if (state<>tsStarted) then begin
    ClearList;
    state:=tsOff;
    exit;
    end;

  if linelist.Count<>0 then begin
    GetMem(list,sizeof(CoordPoint)*linelist.Count);
    for i:=0 to linelist.Count-1 do begin
      p:=PCoordPoint(linelist.Items[i]);
      Map.CurrentView.ScreenToCoord(p^.x,p^.y,cx,cy);
      ApplySnaps(true,cx,cy);
      list^[i].X := cx;
      list^[i].Y := cy;
      end;
    AddCurve(list, linelist.Count);
    ClearList;
    end;


  inherited Done;
end;

procedure TPolyCurveToolHandler.Cancel;
begin
  MainForm.DoInvalidate;
  inherited Cancel;
  Done;
end;

// JD 8-6-02

Procedure TPolyCurveToolHandler.ForceContinuity(Shift: TShiftState; Var X,Y: Integer);
Var
  C  : PCoordPoint;
  D1 : Double;
  D2 : Double;

Begin
  If (Which = 2) And (LineList.Count >= 4) And Not (ssShift In Shift) Then
  Begin
    D1 := Distance(X,Y,P1.X,P1.Y);
    C  := PCoordPoint(LineList.Items[LineList.Count - 2]);
    D2 := Distance(P1.X,P1.Y,C^.X,C^.Y);
    If D2 <> 0 Then
    Begin
      X := Round(P1.X + ((P1.X - C^.X) * D1 / D2));
      Y := Round(P1.Y + ((P1.Y - C^.Y) * D1 / D2));
    End;
  End;
End; // TPolyCurveToolHandler.ForceContinuity

procedure TPolyCurveToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
Begin
  ForceContinuity(Shift,X,Y);
  Inherited MouseMove(Shift,X,Y);
End; // TPolyCurveToolHandler.MouseMove

function TPolyCurveToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var OldMode:TPenMode;
begin
  if (Button=mbRight) then begin
    if (state=tsStarted) then begin
      if not Settings.AskForShapeClosure.Checked then begin
        if (ssShift in Shift) then CreateClosedFigure
                              else CreateOpenFigure;
        end
      else
        MainForm.ShapePopupMenu.Popup(X,Y);

      Result:=true;
      end
    else begin
      Result:=inherited MouseDown(Button, Shift, X, Y);
      end;

    exit;
    end;

  if (Button=mbLeft) then begin
    case state of
      tsOff: begin
          Start;
          ClearList;
          state  := tsStarted;
        end;
      tsStarted: begin
        end;
      end;

    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(true);

    ApplyScreenSnaps(true,x,y);

    case (which) of
      0: begin
           p1.X := X;
           p1.Y := y;
           inc(which);
           CustomCursor:=crCurve2;
         end;
      1: begin
           p4.X := X;
           p4.Y := y;
           inc(which);
           CustomCursor:=crCurve3;
         end;
      2: begin
           ForceContinuity(Shift,X,Y); // JD 8-6-02
           p2.X := X;
           p2.Y := y;
           inc(which);
           CustomCursor:=crCurve4;
         end;
      3: begin
           p3.X := X;
           p3.Y := y;
           AddCurveSection;
           which:=1;
           p1.X := p4.X;
           p1.Y := p4.Y;
           CustomCursor:=crCurve2;
         end;
      end;

    EndX:=X;
    EndY:=Y;

    Draw(false);
    Canvas.Pen.Mode := OldMode;
    Result:=true;
    end
  else
    Result:=false;
end;

constructor TPolyCurveToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  linelist:=TList.Create;
  AutoPan:=true;
  CustomCursor:=crCurve1;
end;

destructor TPolyCurveToolHandler.Destroy;
begin
  inherited Destroy;
  ClearList;
  linelist.Destroy;
end;

procedure TPolyCurveToolHandler.AddPoint(pt:CoordPoint);
var p:PCoordPoint;
begin
  New(p);
  p^.X := trunc(pt.X);
  p^.Y := trunc(pt.Y);
//  ApplyScreenSnaps(p^.X,p^.Y);
  linelist.Add(p);
end;

procedure TPolyCurveToolHandler.AddCurve(list:PCoordArray; count:integer);
begin
  Map.StartAdding(res_linetool_curvepoly_add);
  Map.AddObject(PolyCurvePrimitive.Create(list,count,GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TPolyCurveToolHandler.ClearList;
var i:integer;
begin
  for i:=0 to linelist.Count-1 do Dispose(linelist.Items[i]);
  linelist.Clear;
end;

procedure TPolyCurveToolHandler.CreateClosedFigure;
var 
  pt,PT1,PT3,PT4,PT5,PT6 : PCoordPoint;
  endpoint,C             : CoordPoint;
  D14,D40,D03,R          : Double;

begin
  MainForm.DoInvalidate;     // Make sure the ending line is undrawn

  if (linelist.Count = 0) then begin
    endpoint:=p1;
    AddPoint(p1);
    end
  else begin
    pt:=PCoordPoint(linelist.Items[0]);
    endpoint.X := pt^.X;
    endpoint.Y := pt^.Y;
    end;

  case which of
    0: Cancel;
    1: begin
         If LineList.Count >= 4 Then // JD 8-6-02
         Begin
           Pt1 := PCoordPoint(LineList.Items[LineList.Count - 4]);
           Pt3 := PCoordPoint(LineList.Items[LineList.Count - 2]);
           Pt4 := PCoordPoint(LineList.Items[LineList.Count - 1]);
           Pt5 := PCoordPoint(LineList.Items[3]);
           Pt6 := PCoordPoint(LineList.Items[1]);
           D14 := Distance(Pt1^.X,Pt1^.Y,Pt4^.X,Pt4^.Y);
           D40 := Distance(EndPoint.X,EndPoint.Y,Pt4^.X,Pt4^.Y);
           D03 := Distance(EndPoint.X,EndPoint.Y,Pt5^.X,Pt5^.Y);
           If (D14 <> 0) Then
           Begin
             R   := D40 / D14;
             C.X := Round(Pt4^.X + ((Pt4^.X - Pt3^.X) * R));
             C.Y := Round(Pt4^.Y + ((Pt4^.Y - Pt3^.Y) * R));
             AddPoint(C);
             R   := D40 / D03;
             C.X := Round(EndPoint.X - ((Pt6^.X - EndPoint.X) * R));
             C.Y := Round(EndPoint.Y - ((Pt6^.Y - EndPoint.Y) * R));
             AddPoint(C);
             AddPoint(EndPoint);
           End
           Else
           Begin
             AddPoint(endpoint);
             AddPoint(endpoint);
             AddPoint(endpoint);
           End;
         End
         Else
         Begin
           AddPoint(endpoint);
           AddPoint(endpoint);
           AddPoint(endpoint);
         End;
       end;
    2: begin
         If LineList.Count >= 4 Then // JD 8-6-02
         Begin
           Pt1 := PCoordPoint(LineList.Items[LineList.Count - 4]);
           Pt4 := PCoordPoint(LineList.Items[LineList.Count - 1]);
           Pt5 := PCoordPoint(LineList.Items[3]);
           Pt6 := PCoordPoint(LineList.Items[1]);
           D14 := Distance(Pt1^.X,Pt1^.Y,Pt4^.X,Pt4^.Y);
           D40 := Distance(EndPoint.X,EndPoint.Y,Pt4^.X,Pt4^.Y);
           D03 := Distance(EndPoint.X,EndPoint.Y,Pt5^.X,Pt5^.Y);
           If (D14 <> 0) Then
           Begin
             R   := D40 / D03;
             C.X := Round(EndPoint.X - ((Pt6^.X - EndPoint.X) * R));
             C.Y := Round(EndPoint.Y - ((Pt6^.Y - EndPoint.Y) * R));
             AddPoint(C);
             AddPoint(EndPoint);
           End
           Else
           Begin
             AddPoint(P4);
             AddPoint(endpoint);
             AddPoint(endpoint);
           End;
         End
         Else
         Begin
           AddPoint(p4);
           AddPoint(endpoint);
           AddPoint(endpoint);
         End;
       end;
    3: begin
         AddPoint(p2);
         AddPoint(p4);
         AddPoint(endpoint);
       end;
    end;

  Done;
end;

procedure TPolyCurveToolHandler.CreateOpenFigure;
begin
  if (state=tsStarted) then begin
    MainForm.DoInvalidate;     // Make sure the ending line is undrawn
    Done;
    end;
end;

{--------------------------------------------------------------------------------}

procedure TFractalPolyCurveToolHandler.Start;
begin
  inherited Start;
  seed := MainForm.SeedSpin.Value;
  roughness := MainForm.RoughnessTrackBar.Position;
end;

procedure TFractalPolyCurveToolHandler.AddCurve(list:PCoordArray; count:integer);
var itemcount:integer;
begin
  Map.StartAdding(res_linetool_curvefpoly_add);
  if Count=0 then
    itemcount:=0
  else
    itemcount:=(Count-1) div 3;

  MainForm.SeedSpin.Value:=(seed+itemcount) and 65535;
  Map.AddObject(PolyCurvePrimitive.Create(list,count,seed,roughness,GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TFractalPolyCurveToolHandler.DrawCurve(sp1,sp2,sp3,sp4:CoordPoint);
var d:double;
    continue:TLineContinue;
    itemcount:integer;
begin
  d := distance(sp1.x,sp1.y,sp2.x,sp2.y) + distance(sp4.x,sp4.y,sp3.x,sp3.y);

  if linelist.Count=0 then
    itemcount:=0
  else
    itemcount:=(linelist.Count-1) div 3;

  FractalSetSeed(seed + itemcount);
  continue:=GetLineStyleStart(GetCurrentlySetStyles);
  DrawFractalBezier(Canvas,sp1,sp2,sp3,sp4,(d * MainForm.RoughnessTrackBar.Position)/1000, continue);
end;

{--------------------------------------------------------------------------------}

constructor TNormalCircleToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crCircle;
end;

procedure TNormalCircleToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    ex,ey:Coord;
begin
  if not erase then begin
    ApplyScreenSnapsFloat(false,StartX,StartY);
    ApplyScreenSnapsFloat(true,EndX,EndY);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyProportionalSnap(1.0,StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;

    lastx1 := Round(StartX);
    lasty1 := Round(StartY);
    lastx2 := Round(EndX);
    lasty2 := Round(EndY);

    if GetKeyState(VK_CONTROL)<0 then begin
      lastx1 := lastx1 - (lastx2-lastx1);  // Make the starting point the center point
      lasty1 := lasty1 - (lasty2-lasty1);
      end;
    end;

  oldcolor:=Canvas.Pen.Color;
  Canvas.Pen.Color:=CurrentColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(lastx1, lasty1, lastx2, lasty2);
  Canvas.Pen.Color:=oldcolor;
end;

procedure TNormalCircleToolHandler.Add(cx1,cy1,cx2,cy2:Coord);
begin
  Map.StartAdding(res_linetool_circle_add);

  Map.AddObject(PolyCurvePrimitive.Ellipse(MakeCoordPoint(cx1,cy1),
                MakeCoordPoint(cx2,cy2), GetCurrentlySetStyles));
  if Settings.RedundantGrouping.Checked then Map.GroupSelected;
  Map.EndAdding;
end;

procedure TNormalCircleToolHandler.Done;
var cx1,cx2,cy1,cy2:Coord;
begin
  if (StartX=EndX) and (StartY=EndY) then begin
    Map.ClearSelection;
    Cancel;
    exit;
    end;

  Map.CurrentView.ScreenToCoord(StartX,StartY,cx1,cy1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,cx2,cy2);
  ApplySnaps(false,cx1,cy1);
  ApplySnaps(false,cx2,cy2);
  if GetKeyState(VK_SHIFT)<0 then ApplyProportionalSnap(1.0,cx1,cy1,cx2,cy2);
  if GetKeyState(VK_CONTROL)<0 then begin
    cx1 := cx1 - (cx2-cx1);
    cy1 := cy1 - (cy2-cy1);
    end;
  Add((cx1+cx2)*0.5,(cy1+cy2)*0.5,cx2,cy2);
  inherited Done;
end;

{--------------------------------------------------------------------------------}

constructor TRectangleToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crSquare;
end;

procedure TRectangleToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    style:StyleAttrib;
    ex,ey:Coord;
    continue:TLineContinue;
    temp:integer;
begin
  oldcolor:=Canvas.Pen.Color;
  Canvas.Pen.Color:=CurrentColor;
  style := GetCurrentlySetStyles;

  if not erase then begin
    ApplyScreenSnapsFloat(false,StartX,StartY);
    ApplyScreenSnapsFloat(true,EndX,EndY);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyProportionalSnap(1.0,StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;

    lastx1 := Round(StartX);
    lasty1 := Round(StartY);
    lastx2 := Round(EndX);
    lasty2 := Round(EndY);

    if GetKeyState(VK_CONTROL)<0 then begin
      lastx1 := lastx1 - (lastx2-lastx1);  // Make the starting point the center point
      lasty1 := lasty1 - (lasty2-lasty1);
      end;
    end;

  continue:=GetLineStyleStart(style);
  DrawLineContinue(Canvas, lastx1,lasty1,lastx1,lasty2, continue);
  DrawLineContinue(Canvas, lastx1,lasty2,lastx2,lasty2, continue);
  DrawLineContinue(Canvas, lastx2,lasty2,lastx2,lasty1, continue);
  DrawLineContinue(Canvas, lastx2,lasty1,lastx1,lasty1, continue);
  GetLineEnd(continue,temp);

  Canvas.Pen.Color:=oldcolor;
end;

procedure TRectangleToolHandler.Done;
var cx1,cx2,cy1,cy2:Coord;
    list:PCoordArray;
begin
  if (StartX=EndX) and (StartY=EndY) then begin
    Map.ClearSelection;
    Cancel;
    exit;
    end;

  Map.CurrentView.ScreenToCoord(StartX,StartY,cx1,cy1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,cx2,cy2);
  ApplySnaps(false,cx1,cy1);
  ApplySnaps(false,cx2,cy2);
  if GetKeyState(VK_SHIFT)<0 then ApplyProportionalSnap(1.0,cx1,cy1,cx2,cy2);
  if GetKeyState(VK_CONTROL)<0 then begin
    cx1 := cx1 - (cx2-cx1);
    cy1 := cy1 - (cy2-cy1);
    end;

  GetMem(list,sizeof(CoordPoint)*5);

  list^[0].X := cx1;   list^[0].y := cy1;
  list^[1].X := cx1;   list^[1].y := cy2;
  list^[2].X := cx2;   list^[2].y := cy2;
  list^[3].X := cx2;   list^[3].y := cy1;
  list^[4].X := cx1;   list^[4].y := cy1;

  Map.StartAdding(res_linetool_rect_add);
  Map.AddObject(PolylinePrimitive.Create(list, 5, GetCurrentlySetStyles));
  if Settings.RedundantGrouping.Checked then Map.GroupSelected;
  Map.EndAdding;

  inherited Done;
end;

{------------------------------------------------------------------------------------}

constructor TPolygonToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
  CustomCursor:=crPolygon;
end;

function TPolygonToolHandler.AskSides:boolean;
begin
  Application.CreateForm(TPolygonSidesForm, PolygonSidesForm);

  if PolygonSidesForm.ShowModal=mrOK then begin
    try
      sides:=PolygonSidesForm.Sides.Value
    except
      sides:=0;
    end;
    end
  else
    sides:=0;

  PolygonSidesForm.Free;

  Result:=(sides>1);
end;

procedure TPolygonToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    angle,delta:double;
    i:integer;
    ex,ey:Coord;
    x,y:integer;
begin
  if not erase then begin
    InitialAngle:=arctan2(EndY-StartY, EndX-StartX);
    ApplyScreenSnapsFloat(false,StartX,StartY);
    ApplyScreenSnapsFloat(true,EndX,EndY);
    if GetKeyState(VK_SHIFT)<0 then begin
      ex:=EndX;
      ey:=EndY;
      ApplyProportionalSnap(1.0,StartX,StartY,ex,ey);
      EndX:=trunc(ex);
      EndY:=trunc(ey);
      end;

    lastx1 := Round(StartX);
    lasty1 := Round(StartY);
    lastx2 := Round(EndX);
    lasty2 := Round(EndY);

    if GetKeyState(VK_CONTROL)<0 then begin
      lastx1 := lastx1 - (lastx2-lastx1);  // Make the starting point the center point
      lasty1 := lasty1 - (lasty2-lasty1);
      end;
    end;

  oldcolor:=Canvas.Pen.Color;
  Canvas.Pen.Color:=CurrentColor;
  angle:=InitialAngle;
  delta := 2*pi/sides;

  for i:=0 to sides do begin
    x:=(lastx1+lastx2) div 2 + trunc(0.5*abs(lastx1-lastx2)*cos(angle));
    y:=(lasty1+lasty2) div 2 + trunc(0.5*abs(lasty1-lasty2)*sin(angle));
    if (i=0) then Canvas.MoveTo(x,y)
             else Canvas.LineTo(x,y);
    angle:=angle+delta;
    end;
  Canvas.Pen.Color:=oldcolor;
end;

procedure TPolygonToolHandler.Add(cx1,cy1,cx2,cy2:Coord);
var //d:double;
    angle,delta:double;
    i:integer;
    list:PCoordArray;
begin
  GetMem(list,sizeof(CoordPoint)*(sides+1));

//  d:=distance(cx1,cy1,cx2,cy2);
//  angle:=arctan2(cy2-cy1, cx2-cx1);
  angle:=InitialAngle;
  delta := 2*pi/sides;
  for i:=0 to sides do begin
    list^[i].X := cx1 + abs(cx2-cx1)*cos(angle);
    list^[i].Y := cy1 + abs(cy2-cy1)*sin(angle);
    angle:=angle+delta;
    end;
  Map.StartAdding(res_linetool_polygon_add);
  Map.AddObject(PolylinePrimitive.Create(list, sides+1, GetCurrentlySetStyles));
  Map.EndAdding;
end;

procedure TPolygonToolHandler.Done;
var cx1,cx2,cy1,cy2:Coord;
begin
  if (StartX=EndX) and (StartY=EndY) then begin
    Map.ClearSelection;
    Cancel;
    exit;
    end;

  Map.CurrentView.ScreenToCoord(StartX,StartY,cx1,cy1);
  Map.CurrentView.ScreenToCoord(EndX,EndY,cx2,cy2);

  if GetKeyState(VK_SHIFT)<0 then ApplyProportionalSnap(1.0,cx1,cy1,cx2,cy2);
  if GetKeyState(VK_CONTROL)<0 then begin
    cx1 := cx1 - (cx2-cx1);
    cy1 := cy1 - (cy2-cy1);
    end;

  Add((cx1+cx2)*0.5,(cy1+cy2)*0.5,cx2,cy2);
  inherited Done;
end;

{------------------------------------------------------------------------------------}

constructor TFreehandLineToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  SnapWhenDone:=false;
  CustomCursor:=crFreehand;
end;

procedure TFreehandLineToolHandler.Cancel;
begin
  inherited Cancel;
  ClearList;
  CustomCursor:=crFreehand;
end;

function TFreehandLineToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PCoordPoint;
begin
  if (button=mbRight) then begin
    MainForm.ShapePopupMenu.Popup(X,Y);
    Result:=true;
    end
  else if (button=mbLeft) then begin
    ApplyScreenSnaps(true,X,Y);
    StartX := X;
    StartY := Y;
    state  := tsStarted;

    Start;
    ClearList;
    New(p);
    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);
    EndX := X;
    EndY := Y;
    Result:=true;
    end
  else
    Result:=false;
end;

procedure TFreehandLineToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var p,p1,p2:PCoordPoint;
    oldcolor:TColor;
    colinear:boolean;
    atanline1,atanline2:real;
    dist:real;
begin
  HideCrossHair;
  if (state=tsStarted) then begin
    { Coalese consecutive line segments that are colinear }
    if linelist.Count>=2 then begin
      p1:=PCoordPoint(linelist.Items[linelist.Count-1]);
      p2:=PCoordPoint(linelist.Items[linelist.Count-2]);
      atanline1 := ArcTan2(p2^.X-p1^.X, p2^.Y-p1^.Y);
      atanline2 := ArcTan2(p1^.X-X, p1^.Y-Y);
      dist      := Distance(p1.X,p1.Y,X,Y);
      colinear  := (atanline1 = atanline2);
      end
    else begin
      p1 := nil;
      p2 := nil;
      atanline1 := 0;
      atanline2 := 0;
      dist := 0;
      colinear:=false;
      end;

    // If we're colinear (in line with the last point), we don't create a new point,
    // no matter what freehand mode we're using.
    if not colinear then begin
      if (Settings.FreehandQuality.ItemIndex=fqSmooth) and (p1<>nil) and (p2<>nil) then begin
        // If we're smoothing, create two points parametrically weighted from the new point
        // and the last point to get a smoother line.
        New(p);
        p^.X := X*0.33 + p1^.X*0.67;
        p^.Y := Y*0.33 + p1^.Y*0.67;
        linelist.Add(p);

        New(p);
        p^.X := X*0.67 + p1^.X*0.33;
        p^.Y := Y*0.67 + p1^.Y*0.33;
        linelist.Add(p);
        end
      else if (Settings.FreehandQuality.ItemIndex=fqSelective) and (p1<>nil) and (p2<>nil) then begin
        // If we're being Selective, only create a point if we're either far away from our
        // last point, or we take a sharp turn.
        if (dist > 20) or (abs(atanline1-atanline2) > DegToRad(60)) then begin
          New(p);
          p^.X := X;
          p^.Y := Y;
          linelist.Add(p);
          end;
        end
      else begin
        // We're not smoothing or selecting: just add the user's point under the mouse
        New(p);
        p^.X := X;
        p^.Y := Y;
        linelist.Add(p);
        end;
      end
    else begin
      p:=PCoordPoint(linelist.Items[linelist.Count-1]);
      p^.X:=X;
      p^.Y:=Y;
      end;

    if not Pan(X,Y) then begin
      oldcolor:=Canvas.Pen.Color;
      Canvas.Pen.Color:=CurrentColor;
      DrawLineContinue(Canvas, EndX,EndY,X,Y, continue);
      Canvas.Pen.Color:=oldcolor;
      end
    else
      Draw(true);

    EndX := X;
    EndY := Y;
    end;
  // Keep reestablishing our cursor because our base class plays
  // games with the cursor and changes it depending on the state
  // of the line in progress.  We don't want that.
  CustomCursor:=crFreehand;
  RefreshCursor;
  ShowCrossHair(X,Y);
end;

procedure TFreehandLineToolHandler.CreateClosedFigure;
var p:PCoordPoint;
    x,y:Coord;
begin
  if state=tsStarted then begin
    MainForm.DoInvalidate;        // Make sure the ending line is undrawn

    p:=PCoordPoint(linelist.Items[0]);
    X := p^.X;
    Y := p^.Y;
    New(p);
    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);

    Done;
    end;
end;

procedure TFreehandLineToolHandler.CreateOpenFigure;
var p:PCoordPoint;
begin
  if state=tsStarted then begin
    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    ApplyScreenSnapsFloat(false,p^.X,p^.Y);
    EndX:=trunc(p^.X);
    EndY:=trunc(p^.Y);
    Done;
    end;
end;

procedure TFreehandLineToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssShift in Shift) then CreateClosedFigure
                        else CreateOpenFigure;
end;

procedure TFreehandLineToolHandler.Done;
begin
  inherited Done;
  CustomCursor:=crFreehand;
end;

procedure TFreehandLineToolHandler.Backspace;
begin
end;

{------------------------------------------------------------------------------------}
constructor TFreehandFractalToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  SnapWhenDone:=false;
  CustomCursor:=crFreehand;
end;

procedure TFreehandFractalToolHandler.Cancel;
begin
  inherited Cancel;
  ClearList;
  CustomCursor:=crFreehand;
end;

function TFreehandFractalToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var p:PCoordPoint;
begin
  if (button=mbRight) then begin
    MainForm.ShapePopupMenu.Popup(X,Y);
    Result:=true;
    end
  else if (button=mbLeft) then begin
    ApplyScreenSnaps(true,X,Y);
    StartX := X;
    StartY := Y;
    state  := tsStarted;

    Start;
    ClearList;
    New(p);
    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);
    EndX := X;
    EndY := Y;
    Result:=true;
    end
  else
    Result:=false;
end;

procedure TFreehandFractalToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var p:PCoordPoint;
    oldcolor:TColor;
    continue:TLineContinue;
begin
  HideCrosshair;
  if (state=tsStarted) then begin
    if (distance(x,y,EndX,EndY) >= 10) then begin
      FractalSetSeed(seed + linelist.Count - 1);
      New(p);
      p^.X := X;
      p^.Y := Y;
      linelist.Add(p);

      if not Pan(X,Y) then begin
        oldcolor:=Canvas.Pen.Color;
        Canvas.Pen.Color:=CurrentColor;
        continue:=GetLineStyleStart(GetCurrentlySetStyles);
        FractalLine(Canvas,EndX,EndY,X,Y,(distance(EndX,EndY,X,Y)*roughness)/1000, continue);
        Canvas.Pen.Color:=oldcolor;
        end
      else
        Draw(true);

      EndX := X;
      EndY := Y;
      end;
    end;
  // Keep reestablishing our cursor because our base class plays
  // games with the cursor and changes it depending on the state
  // of the line in progress.  We don't want that.
  CustomCursor:=crFreehand;
  RefreshCursor;
  ShowCrosshair(X,Y);
end;

procedure TFreehandFractalToolHandler.CreateClosedFigure;
var p:PCoordPoint;
    x,y:Coord;
begin
  if state=tsStarted then begin
    MainForm.DoInvalidate;        // Make sure the ending line is undrawn

    p:=PCoordPoint(linelist.Items[0]);
    X := p^.X;
    Y := p^.Y;
    New(p);
    p^.X := X;
    p^.Y := Y;
    linelist.Add(p);

    Done;
    end;
end;

procedure TFreehandFractalToolHandler.CreateOpenFigure;
var p:PCoordPoint;
begin
  if state=tsStarted then begin
    p:=PCoordPoint(linelist.Items[linelist.Count-1]);
    ApplyScreenSnapsFloat(false,p^.X,p^.Y);
    EndX:=trunc(p^.X);
    EndY:=trunc(p^.Y);
    Done;
    end;
end;


procedure TFreehandFractalToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssShift in Shift) then CreateClosedFigure
                        else CreateOpenFigure;
end;

procedure TFreehandFractalToolHandler.Done;
begin
  inherited Done;
  CustomCursor:=crFreehand;
end;

procedure TFreehandFractalToolHandler.Backspace;
begin
end;

{------------------------------------------------------------------------------------}

constructor TChartGridToolHandler.Create(cv:TCanvas);
begin
  inherited Create(cv);
  AutoPan:=true;
end;

function TChartGridToolHandler.AskGrid:boolean;
begin
  Result:=(ChartGridDialog.ShowModal=mrOk);

  { Test for the one condition that will cause us grief; the user
    checks Other, but then leaves the field blank.  If so, just act
    like the user canceled the dialog}
  if ChartGridDialog.PerimeterOther.Checked and (ChartGridDialog.Other.Text='') then
     Result:=false;

  FinishedCircle:=false;
end;


procedure TChartGridToolHandler.FreshenLineList;
const startangle = -Pi/2.0;
var delta:double;
    i,j:integer;
    skip:integer;
    p0,p1,p2:CoordPoint;
    count,endcount:integer;
    ignore:boolean;
begin
  with ChartGridDialog do begin
    count:=0;

    if Perimeter8.Checked then count := 8;
    if Perimeter16.Checked then count := 16;
    if Perimeter32.Checked then count := 32;
    if PerimeterOther.Checked then count := Other.Value;

    skip := SkipPoints.Value;
    end;

  { This is somewhat conservative in the allocation of lines... }
  if linelist=nil then begin
    allocatedlines := count * (count-1);
    GetMem(linelist,sizeof(CoordPoint)*2*allocatedlines);
    end;

  delta := 2*pi/count;
  linecount:=0;

  if isCropping and
     ((croprect.left=croprect.right) or (croprect.top=croprect.bottom)) then exit;

  for i:=0 to count-1 do begin
    endcount := i+count-skip-1;
    if endcount > count-1 then endcount:=count-1;

    p0.x:=center.x + radius*cos(startangle + i*delta);
    p0.y:=center.y + radius*sin(startangle + i*delta);

    for j:=i+skip+1 to endcount do begin
      ignore:=false;

      p1:=p0;
      p2.x:=center.x + radius*cos(startangle + j*delta);
      p2.y:=center.y + radius*sin(startangle + j*delta);

      if isCropping and ChartGridDialog.ExtendToCrop.Checked then begin
        ExtendLineToRect(p1,p2,croprect);
        end;

      if isCropping then begin
        if not CropLineOutsideRect(p1,p2,croprect) then ignore:=true;
        end;

      if not ignore then begin
        linelist^[linecount*2]:=p1;
        linelist^[linecount*2+1]:=p2;
        inc(linecount);
        end;
      end;

    Assert(linecount<allocatedlines);
    end;
end;

procedure TChartGridToolHandler.Draw(erase:boolean);
var oldcolor:TColor;
    i:integer;
    cx,cy:Coord;
    x1,y1,x2,y2:integer;
begin
  if not erase then ApplyScreenSnapsFloat(true,EndX,EndY);

  oldcolor:=Canvas.Pen.Color;

  if iscropping then begin
    Map.CurrentView.ScreenToCoord(EndX,EndY,croprect.right,croprect.bottom);
    Map.CurrentView.CoordToScreen(croprect.left,croprect.top,x1,y1);
    Map.CurrentView.CoordToScreen(croprect.right,croprect.bottom,x2,y2);
    Marquis(Canvas, x1,y1,x2,y2);
    end
  else begin
    if not FinishedCircle then begin
      Map.CurrentView.ScreenToCoord(EndX,EndY,cx,cy);
      radius:=Distance(center.X,Center.Y,cx,cy);
      end;
    end;

  Canvas.Pen.Color:=CurrentColor;

  FreshenLineList;
  for i:=0 to linecount-1 do begin
    Map.CurrentView.CoordToScreen(linelist^[i*2].X,linelist^[i*2].Y,     x1,y1);
    Map.CurrentView.CoordToScreen(linelist^[i*2+1].X,linelist^[i*2+1].Y, x2,y2);
    Canvas.MoveTo(x1,y1);
    Canvas.LineTo(x2,y2);
    end;

  Canvas.Pen.Color:=oldcolor;
end;

procedure TChartGridToolHandler.Done;
const colortable:array[0..15] of TColor=
//(clBrown,clMocha,clCafeAuLait,clParchment,
// clTangerine,clOrange,clRed,clMaroon,
// clVioletRed,clCornFlower,clFuchsia,clBlue,
// clAqua,clGreen,clSeaFoam,clYellow);
  (clBlue,clMaroon,clTangerine,
  clBlue,clMaroon,clTangerine,
  clBlue,clMaroon,clTangerine,
  clBlue,clMaroon,clTangerine,
  clBlue,clMaroon,clTangerine,
  clBlue);
var i:integer;
    OldMode:TPenMode;

{    xmin,xmax:Coord;
    ymin,ymax:Coord;}

{  function ColorMix(rgb1,rgb2:TColor; percent:single):TColor;
  var r1,g1,b1:integer;
      r2,g2,b2:integer;
      r,g,b:integer;
  begin
    r1 := (rgb1 and $FF);
    g1 := (rgb1 and $FF00) shr 8;
    b1 := (rgb1 and $FF0000) shr 16;
    r2 := (rgb2 and $FF);
    g2 := (rgb2 and $FF00) shr 8;
    b2 := (rgb2 and $FF0000) shr 16;

    r := trunc(r1*percent)+trunc(r2*(1.0-percent));
    g := trunc(g1*percent)+trunc(g2*(1.0-percent));
    b := trunc(b1*percent)+trunc(b2*(1.0-percent));

    Result:=TColor(r or (g shl 8) or (b shl 16));
  end;}

begin
  OldMode := Canvas.Pen.Mode;
  Canvas.Pen.Mode := pmNotXor;
  Draw(true);
  Canvas.Pen.Mode:=OldMode;

  FreshenLineList;

  if (linecount=0) and iscropping then begin
    iscropping:=false;
    FreshenLineList;
    end;

  if linecount<>0 then begin
    Map.StartAdding(res_linetool_rosette_add);

{    xmin:=linelist^[0].X;
    xmax:=linelist^[0].X;
    for i:=1 to linecount-1 do begin
      if (linelist^[i].X<xmin) then xmin:=linelist^[i].X;
      if (linelist^[i].X>xmax) then xmax:=linelist^[i].X;
      end;

    ymin:=linelist^[0].Y;
    ymax:=linelist^[0].Y;
    for i:=1 to linecount-1 do begin
      if (linelist^[i].Y<ymin) then ymin:=linelist^[i].Y;
      if (linelist^[i].Y>ymax) then ymax:=linelist^[i].Y;
      end;}

    for i:=0 to linecount-1 do begin
{      CurrentColor:=ColorMix(clRed,clBlue,((linelist^[i*2].x+linelist^[i*2+1].x)/2-xmin)/(xmax-xmin));
      CurrentColor:=ColorMix(CurrentColor,clWhite,0.5-abs((linelist^[i*2].y+linelist^[i*2+1].y)/2-((ymax+ymin)/2))/(ymax-ymin));}

      Map.AddObject(LinePrimitive.Create(linelist^[i*2].X,linelist^[i*2].Y,
                                         linelist^[i*2+1].X,linelist^[i*2+1].Y,
                                         GetCurrentlySetStyles));
      end;
    if Settings.RedundantGrouping.Checked then Map.GroupSelected;
    Map.EndAdding;
    end;

  FreeMem(linelist);
  linelist:=nil;

  inherited Done;

  MainForm.aSelectionExecute(Self);
end;

procedure TChartGridToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if FinishedCircle then
    CustomCursor := crRosetteClip
  else
    CustomCursor := crRosette;

  inherited MouseMove(Shift,X,Y);
end;

function TChartGridToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
var OldPenMode:TPenMode;
begin
  if (Button=mbLeft) then begin
    ApplyScreenSnaps(true,x,y);
    if state = tsStarted then begin
      OldPenMode:=Canvas.Pen.Mode;
      Canvas.Pen.Mode := pmNotXor;
      Draw(true);
      Canvas.Pen.Mode:=OldPenMode;

      iscropping:=true;
      Map.CurrentView.ScreenToCoord(x,y,croprect.left,croprect.top);
      croprect.BottomRight:=croprect.TopLeft;
      end
    else begin
      state  := tsStarted;
      iscropping:=false;
      FinishedCircle:=false;
      Map.CurrentView.ScreenToCoord(x,y,center.x,center.y);
      radius:=0;
      end;

    Result := true;
    end
  else
    Result:=false;
end;

procedure TChartGridToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (state=tsStarted) then begin
    if not ChartGridDialog.CropRosette.Checked or iscropping then
      Done
    else
      FinishedCircle:=true;
    end;
end;


{------------------------------------------------------------------------------------}

end.


