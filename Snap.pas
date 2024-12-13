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
unit Snap;

{$MODE Delphi}

interface

uses Graphics,Primitives, Geometry;

//const DesignGridScaling = 0.01;

type DirectionalSnapType=(dst90,dst45);

// JD 8-11-02

Var
  ApplyScreenSnapsDX1 : Coord;
  ApplyScreenSnapsDY1 : Coord;
  ApplyScreenSnapsDX2 : Coord;
  ApplyScreenSnapsDY2 : Coord;
  ApplyScreenSnapsEX1 : Coord;
  ApplyScreenSnapsEY1 : Coord;
  ApplyScreenSnapsEX2 : Coord;
  ApplyScreenSnapsEY2 : Coord;
  LastSnapKind        : Coord;
  LastSnapAngle       : Coord;

function  ApplySnaps(activateLED:boolean; var cx,cy:Coord; ForceType: Integer = -1; allowselected:boolean=true):boolean;
procedure ApplyScreenSnaps(activateLED:boolean; var cx,cy:integer; ForceType: Integer = -1; allowselected:boolean=true); Overload;
procedure ApplyScreenSnapsFloat(activateLED:boolean; var cx,cy:Coord; ForceType: Integer = -1; allowselected:boolean=true);
procedure ApplyDirectionalSnap(cx1,cy1:Coord; var cx2,cy2:Coord; kind:DirectionalSnapType=dst45);
procedure ApplyProportionalSnap(aspect:double; cx1,cy1:Coord; var cx2,cy2:Coord);
procedure DrawSnapGrid(force:boolean);
procedure SnapLEDsOff;
Procedure AdjustForSelBorder(CX,CY: Integer; Var DX1,DY1,DX2,DY2: Coord); // JD 8-11-02
Procedure ClearAdjust;

implementation

uses MAIN, MapObject, Math, MapSettings,SelectionTool,GraphGrid;

// JD 8-11-02

Procedure AdjustForSelBorder(CX,CY: Integer; Var DX1,DY1,DX2,DY2: Coord);
Var Sel: TSelectionToolHandler;
Begin
  Sel := MainForm.GetSelectionToolHandler;
  DX1 := Sel.Extent.Left   + SelBorder - CX;
  DX2 := Sel.Extent.Right  - SelBorder - CX - 1;
  DY1 := Sel.Extent.Top    + SelBorder - CY;
  DY2 := Sel.Extent.Bottom - SelBorder - CY - 1;
End; // AdjustForSelBorder

Procedure ClearAdjust;
Begin
  ApplyScreenSnapsDX1 := 0;
  ApplyScreenSnapsDY1 := 0;
  ApplyScreenSnapsDX2 := 0;
  ApplyScreenSnapsDY2 := 0;
End; // ClearAdjust

Procedure ClearScreenSnaps;
Begin
  ApplyScreenSnapsEX1 := 0;
  ApplyScreenSnapsEY1 := 0;
  ApplyScreenSnapsEX2 := 0;
  ApplyScreenSnapsEY2 := 0;
End; // ClearScreenSnaps

procedure ApplyScreenSnapsFloat(activateLED:boolean; var cx,cy:Coord; ForceType: Integer; allowselected:boolean);
var x,y:Coord;
begin
  if MapSettingsDialog.SnapToGrid.Checked  Or
     MapSettingsDialog.SnapToPoint.Checked Or
     MapSettingsDialog.cbSnapTo.Checked    Then
  begin
    Map.CurrentView.ScreenToCoord(cx,cy,x,y);
    if ApplySnaps(activateLED,x,y,ForceType,allowselected) 
     Then Map.CurrentView.CoordToScreen(x,y,cx,cy)
     Else ClearScreenSnaps;
  end
  Else ClearScreenSnaps;
end;

procedure ApplyScreenSnaps(activateLED:boolean; var cx,cy:integer; ForceType: Integer; allowselected:boolean);
Var X,Y: Coord;
begin
  if MapSettingsDialog.SnapToGrid.Checked  Or
     MapSettingsDialog.SnapToPoint.Checked Or
     MapSettingsDialog.cbSnapTo.Checked    Then
  begin
    Map.CurrentView.ScreenToCoord(CX,CY,X,Y);
    If ApplySnaps(ActivateLED,X,Y,ForceType,AllowSelected)
     Then Map.CurrentView.CoordToScreen(X,Y,CX,CY)
     Else ClearScreenSnaps;
  end
  Else ClearScreenSnaps;
end;

Procedure SnapToGrid(var CX,CY: Coord);

  Procedure SnapToSquareGrid;
  Var V: Coord;
  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    If V <> 0 Then
    begin
      If CX < 0
       Then CX := Int(CX / V) * V
       Else CX := Int(CX / V + 0.5) * V;

      If CY < 0
       Then CY := Int(CY / V) * v
       Else CY := Int(CY / V + 0.5) * V;
    End;
  End; // SnapToSquareGrid

  Procedure SnapToTriangleGridVertical;
  Var
    V,V2,V3 : Coord;
    I,J     : Integer;
    
  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    If V <> 0 Then
    Begin
      V2 := V / 2;
      V3 := V * Sqrt(3) / 2;
      If CX < 0
       Then I := Trunc(CX / V3)
       Else I := Trunc(CX / V3 + 0.5);
      If CY < 0
       Then J := Trunc(CY / V)
       Else J := Trunc(CY / V + 0.5);
      CX := I * V3;
      CY := J * V;
      If (I And 1) = 1 Then CY := CY + V2;
    End;
  End; // SnapToTriangleGridVertical

  Procedure SnapToTriangleGridHorizontal;
  Var
    V,V2,V3 : Coord;
    I,J     : Integer;

  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    If V <> 0 Then
    Begin
      V2 := V / 2;
      V3 := V * Sqrt(3) / 2;
      If CX < 0
       Then I := Trunc(CX / V)
       Else I := Trunc(CX / V + 0.5);
      If CY < 0
       Then J := Trunc(CY / V3)
       Else J := Trunc(CY / V3 + 0.5);
      CX := I * V;
      CY := J * V3;
      If (J And 1) = 1 Then CX := CX + V2;
    End;
  End; // SnapToTriangleGridHorizontal

  Procedure SnapToDiamondGrid;
  Var
    V,V2,V22 : Coord;
    I,J      : Integer;

  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    If V <> 0 Then
    Begin
      V2  := V * Sqrt(2);
      V22 := V2 / 2;
      If CX < 0
       Then I := Trunc(CX / V22)
       Else I := Trunc(CX / V22 + 0.5);
      If CY < 0
       Then J := Trunc(CY / V2)
       Else J := Trunc(CY / V2 + 0.5);
      CX := I * V22;
      CY := J * V2;
      If (I And 1) = 1 Then CY := CY + V22;
    End;
  End; // SnapToDiamondGrid

  Procedure SnapToHalfDiamondGrid;
  Var
    V,V2,V22,V24 : Coord;
    I,J          : Integer;

  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    If V <> 0 Then
    Begin
      V2  := V * Sqrt(2);
      V22 := V2 / 2;
      V24 := V2 / 4;
      If CX < 0
       Then I := Trunc(CX / V22)
       Else I := Trunc(CX / V22 + 0.5);
      If CY < 0
       Then J := Trunc(CY / V22)
       Else J := Trunc(CY / V22 + 0.5);
      CX := I * V22;
      CY := J * V22;
      If (I And 1) = 1 Then CY := CY + V24;
    End;
  End; // SnapToHalfDiamondGrid

Begin
 Case Map.CurrentView.Grid.GridType Of
    gtHex: SnapToTriangleGridVertical;
    gtTriangle,gtRotatedHex: SnapToTriangleGridHorizontal;
    gtDiamond: SnapToDiamondGrid;
    gtHalfDiamond: SnapToHalfDiamondGrid;
  Else
    SnapToSquareGrid;
  End; // Case
End; // SnapToGrid


procedure ApplyProportionalSnap(aspect:double; cx1,cy1:Coord; var cx2,cy2:Coord);
var sign:double;
begin
  if abs(cx1-cx2)>abs(cy1-cy2) then begin
    if (cy2<cy1) then sign:=-1 else sign:=1;
    cy2:=cy1+sign*abs(cx2-cx1)/aspect;
    end
  else begin
    if (cx2<cx1) then sign:=-1 else sign:=1;
    cx2:=cx1+sign*abs(cy2-cy1)*aspect;
    end;
end;

procedure ApplyDirectionalSnap(cx1,cy1:Coord; var cx2,cy2:Coord;
                               kind:DirectionalSnapType);
const smallestangle=pi/4;  // 45 degrees
var angle,dist:double;
begin
  case kind of
    dst90: begin
        if abs(cx1-cx2)>abs(cy1-cy2) then
          cy2:=cy1
        else
          cx2:=cx1;
      end;
    dst45: begin
          dist:=distance(cx1,cy1,cx2,cy2);
          angle:=ArcTan2(cy2-cy1,cx2-cx1);
          angle:=round(angle/smallestangle)*smallestangle;
          cx2:=cx1 + dist*cos(angle);
          cy2:=cy1 + dist*sin(angle);
      end;
    end;
end;

procedure SnapLEDsOff;
begin
  MainForm.aGridSnap.ImageIndex         := 48;
  MainForm.aGravitySnap.ImageIndex      := 49;
  MainForm.aGravitySnapAlong.ImageIndex := 64; // JD 8-13-02
end;

// Modified by JD 8-13-02

function ApplySnaps(activateLED:boolean; var cx,cy:Coord; ForceType: Integer; allowselected:boolean):boolean;
var p:CoordPoint;
    d:double;
    x,y:integer;
    kind:integer;

begin
  Result := false;
  kind   := 0;

  if MapSettingsDialog.SnapToGrid.Checked then begin
    SnapToGrid(cx,cy);
    Result:=true;
    kind:=1;
    ApplyScreenSnapsEX1 := ApplyScreenSnapsDX1;
    ApplyScreenSnapsEY1 := ApplyScreenSnapsDY1;
    ApplyScreenSnapsEX2 := ApplyScreenSnapsDX2;
    ApplyScreenSnapsEY2 := ApplyScreenSnapsDY2;
    end;

  if ((ForceType < 0) or (ForceType = 2)) And MapSettingsDialog.SnapToPoint.Checked then begin
    if Map.FindClosestPoint(cx,cy,p,allowselected) then begin
      d:=distance(cx,cy,p.x,p.y);
      Map.CurrentView.DeltaCoordToScreen(d,d,x,y);
      if (x<10) and (y<10) then begin
        cx:=p.x;
        cy:=p.y;
        Result:=true;
        kind:=2;
        ApplyScreenSnapsEX1 := ApplyScreenSnapsDX1;
        ApplyScreenSnapsEY1 := ApplyScreenSnapsDY1;
        ApplyScreenSnapsEX2 := ApplyScreenSnapsDX2;
        ApplyScreenSnapsEY2 := ApplyScreenSnapsDY2;
        end;
      end;
    end;

  if ((ForceType < 0) or (ForceType = 3)) And MapSettingsDialog.cbSnapTo.Checked then
  begin
    if Map.FindClosestPointOn(cx,cy,p,LastSnapAngle,allowselected) then
    begin
      d := distance(cx,cy,p.x,p.y);
      Map.CurrentView.DeltaCoordToScreen(d,d,x,y);
      if (x < 10) and (y < 10) then
      begin
        cx     := p.x;
        cy     := p.y;
        Result := true;
        kind   := 3;
        ApplyScreenSnapsEX1 := ApplyScreenSnapsDX1;
        ApplyScreenSnapsEY1 := ApplyScreenSnapsDY1;
        ApplyScreenSnapsEX2 := ApplyScreenSnapsDX2;
        ApplyScreenSnapsEY2 := ApplyScreenSnapsDY2;
      end;
    end;
  end;

  if activateLED then begin
    case kind of
      0: SnapLEDsOff;
      1: begin
           MainForm.aGridSnap.ImageIndex         := 52;
           MainForm.aGravitySnap.ImageIndex      := 49;
           MainForm.aGravitySnapAlong.ImageIndex := 64;
         end;
      2: begin
           MainForm.aGridSnap.ImageIndex         := 48;
           MainForm.aGravitySnap.ImageIndex      := 53;
           MainForm.aGravitySnapAlong.ImageIndex := 64;
         end;
      3: begin
           MainForm.aGridSnap.ImageIndex         := 48;
           MainForm.aGravitySnap.ImageIndex      := 49;
           MainForm.aGravitySnapAlong.ImageIndex := 65;
         end;
      end;
    end;
  LastSnapKind := Kind; // JD 8-13-02
end;

// Modified by JD 10-10-02
Procedure DrawSnapGrid(Force: Boolean);
Var CR: CoordRect;

  Procedure SquareGrid;
  Var
    v     : Coord;
    bx,by : Coord;
    ex,ey : Coord;
    cx,cy : Coord;
    x,y   : Integer;

  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;

    bx := int(cr.Left   / v) * v;
    by := int(cr.Top    / v) * v;
    ex := int(cr.Right  / v) * v;
    ey := int(cr.Bottom / v) * v;

    Map.CurrentView.DeltaCoordToScreen(v,v,x,y);
    if (x < 2) or (y < 2) then exit;

    cx := bx;
    while cx < ex do
    begin
      cy := by;
      while cy < ey do
      begin
        Map.CurrentView.CoordToScreen(cx,cy,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        cy := cy + v;
      end; // While
      cx := cx + v;
    end; // While
  End; // SquareGrid

  Procedure TriangleGridVertical;
  Var
    v,V2,V32,V3 : Coord;
    bx,by : Coord;
    ex,ey : Coord;
    cx,cy : Coord;
    x,y   : Integer;

  Begin
    V := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    V2 := V / 2;
    V3 := V * Sqrt(3);
    V32 := V3 / 2;

    bx := int(cr.Left   / v3) * v3;
    by := int(cr.Top    / v)  * v;
    ex := int(cr.Right  / v3) * v3;
    ey := int(cr.Bottom / v)  * v;

    Map.CurrentView.DeltaCoordToScreen(v,v,x,y);
    if (x < 2) or (y < 2) then exit;

    cx := bx;
    while cx < ex do
    begin
      cy := by;
      while cy < ey do
      begin
        Map.CurrentView.CoordToScreen(cx,cy,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        Map.CurrentView.CoordToScreen(cx + V32,cy + V2,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        cy := cy + v;
      end; // While
      cx := cx + v * Sqrt(3);
    end; // While
  End; // TriangleGridVertical

  Procedure TriangleGridHorizontal;
  Var
    v,V2,V32,V3 : Coord;
    bx,by : Coord;
    ex,ey : Coord;
    cx,cy : Coord;
    x,y   : Integer;

  Begin
    V   := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    V2  := V / 2;
    V3  := V * Sqrt(3);
    V32 := V3 / 2;

    bx := int(cr.Left   / v)  * v;
    by := int(cr.Top    / v3) * v3;
    ex := int(cr.Right  / v)  * v;
    ey := int(cr.Bottom / v3) * v3;

    Map.CurrentView.DeltaCoordToScreen(v,v,x,y);
    if (x < 2) or (y < 2) then exit;

    cx := bx;
    while cx < ex do
    begin
      cy := by;
      while cy < ey do
      begin
        Map.CurrentView.CoordToScreen(cx,cy,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        Map.CurrentView.CoordToScreen(cx + V2,cy + V32,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        cy := cy + V * Sqrt(3);
      end; // While
      cx := cx + v;
    end; // While
  End; // TriangleGridHorizontal

  Procedure DiamondGrid;
  Var
    v,V2,V22 : Coord;
    bx,by : Coord;
    ex,ey : Coord;
    cx,cy : Coord;
    x,y   : Integer;

  Begin
    V   := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    V2  := V * Sqrt(2);
    V22 := V2 / 2;

    bx := int(cr.Left   / v2) * v2;
    by := int(cr.Top    / v2) * v2;
    ex := int(cr.Right  / v2) * v2;
    ey := int(cr.Bottom / v2) * v2;

    Map.CurrentView.DeltaCoordToScreen(v,v,x,y);
    if (x < 2) or (y < 2) then exit;

    cx := bx;
    while cx < ex do
    begin
      cy := by;
      while cy < ey do
      begin
        Map.CurrentView.CoordToScreen(cx,cy,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        Map.CurrentView.CoordToScreen(cx + v22,cy + v22,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        cy := cy + v2;
      end; // While
      cx := cx + v2;
    end; // While
  End; // DiamondGrid

  Procedure HalfDiamondGrid;
  Var
    v,V2,V22,V24 : Coord;
    bx,by : Coord;
    ex,ey : Coord;
    cx,cy : Coord;
    x,y   : Integer;

  Begin
    V   := Map.CurrentView.Grid.CurrentGridSize / MapSettingsDialog.DesignGridUnits.Value;
    V2  := V * Sqrt(2);
    V22 := V2 / 2;
    V24 := V2 / 4;

    bx := int(cr.Left   / v2)  * v2;
    by := int(cr.Top    / v22) * v22;
    ex := int(cr.Right  / v2)  * v2;
    ey := int(cr.Bottom / v22) * v22;

    Map.CurrentView.DeltaCoordToScreen(v,v,x,y);
    if (x < 2) or (y < 2) then exit;

    cx := bx;
    while cx < ex do
    begin
      cy := by;
      while cy < ey do
      begin
        Map.CurrentView.CoordToScreen(cx,cy,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        Map.CurrentView.CoordToScreen(cx + v22,cy + v24,x,y);
        Map.CurrentView.Canvas.Pixels[x,y] := Map.CurrentView.Canvas.Pixels[x,y] xor clWhite;
        cy := cy + v22;
      end; // While
      cx := cx + v2;
    end; // While
  End; // HalfDiamondGrid

Begin
  If Force Or MapSettingsDialog.DisplayGrid.Checked Then
  Begin
    Map.CurrentView.GetCoordinateRect(cr);
    Case Map.CurrentView.Grid.GridType Of
      gtHex: TriangleGridVertical;
      gtTriangle,gtRotatedHex: TriangleGridHorizontal;
      gtDiamond: DiamondGrid;
      gtHalfDiamond: HalfDiamondGrid;
    Else
      SquareGrid;
    End; // Case
  End;
End; // DrawSnapGrid

end.
