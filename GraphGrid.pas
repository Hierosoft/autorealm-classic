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
unit GraphGrid;

interface

uses SysUtils, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
     DOM, Menus;
{ fcl_xml }
{ uses XDOM_2_3; }

const DefaultUnit=12;
      gfGridOnTop = 1;

      NumberGridStyles = 7;

type
     // If any more types are added, you MUST change GetAsDOMElement() and LoadFromDOMElement() !!! 
     TGraphGrid = (gtNone, gtSquare, gtHex, gtTriangle, gtRotatedHex, gtDiamond, gtHalfDiamond, gtPolar);

     // If any more types are added, you MUST change GetAsDOMElement() and LoadFromDOMElement() !!!
     GridPenStyle = (gpsDefault, gpsSingle, gpsDot, gpsDash, gpsDashDot, gpsDashDotDot, gpsBold);

     GridObject=class
     public
        GraphScale:double;
        GraphUnitConvert:double;
        GraphUnits:string;
        CurrentGraphUnits:integer;
        CurrentGridSize:single;
     public
        GridType:TGraphGrid;
        GridBoldUnits:integer;
        GridPosition:cardinal;
        GridFlags:byte;
        PrimaryGridStyle:GridPenStyle;
        SecondaryGridStyle:GridPenStyle;
     public
       constructor Create; overload;
       constructor Create(oldgrid:GridObject); overload;
       procedure SetMeasurementUnits(index:integer);
       procedure DrawGrid(v:TObject);
       procedure SetGraphUnits(index:integer; GridSize:single);
       function GetUnitLength:double;
       Function  GetAsDOMElement(D: TDOMDocument): TDOMElement;
       Procedure LoadFromDOMElement(E: TDOMElement);
       procedure SaveToStream(stream:TStream; version:integer);
       procedure LoadFromStream(stream:TStream; version:integer);
       procedure FillPopupMenu(Menu:TMenuItem; MeasurementMenuItemClick:TNotifyEvent);
       procedure SetMeasurementUnitChecks(Menu:TMenuItem);
       class procedure FillComboList(UnitComboBox:TComboBox);
       function Convert(dist:double; unitindex:integer):double;
       class procedure SetGridPenStyle(Canvas:TCanvas; style,defaultstyle:GridPenStyle);
       procedure bringGridForward;
       procedure sendGridBackward;
       function getGridPosition: Cardinal;
     end;


var CurrentGridColor:TColor;

implementation

uses MapObject, Primitives, Geometry, StreamUtil, LocalizedStrings;

type unittype=record
        name:string;
        factor:double;
     end;

const UnitTable:array[0..26] of unittype =
     ((name:res_graphgrid_centi;   factor:0.393700787402),
      (name:res_graphgrid_inche;   factor:1),
      (name:res_graphgrid_feet;    factor:12),
      (name:res_graphgrid_cubit;   factor:20.6),
      (name:res_graphgrid_yards;   factor:36),
      (name:res_graphgrid_meter;   factor:39.3700787402),
      (name:res_graphgrid_fatho;   factor:72),
      (name:res_graphgrid_rods;    factor:198),
      (name:res_graphgrid_chain;   factor:792),
      (name:res_graphgrid_furlo;   factor:7920),
      (name:res_graphgrid_kilom;   factor:39370.0787402),
      (name:res_graphgrid_stadi;   factor:58291.2),
      (name:res_graphgrid_miles;   factor:63360),
      (name:res_graphgrid_naumi;   factor:72913.3858268),
      (name:res_graphgrid_leagu;   factor:190080),
      (name:res_graphgrid_dbf1;    factor:316800),
      (name:res_graphgrid_dbf2;    factor:633600),
      (name:res_graphgrid_dbf3;    factor:950400),
      (name:res_graphgrid_dbw;     factor:1267200),
      (name:res_graphgrid_dbf4;    factor:1584000),
      (name:res_graphgrid_dbh1;    factor:1900800),
      (name:res_graphgrid_dbg1;    factor:2217600),
      (name:res_graphgrid_dbh2;    factor:3168000),
      (name:res_graphgrid_dbg2;    factor:4118400),
      (name:res_graphgrid_au;      factor:5.88968110236E12),
      (name:res_graphgrid_ly;      factor:3.72461748224E17),
      (name:res_graphgrid_parse;   factor:1.21483393144E18)
     );


{ ----------------------------------------------------------------- }


constructor GridObject.Create;
begin
  inherited Create;
  GraphScale:=1.0;
  CurrentGraphUnits:=-1;
  CurrentGridSize:=1.0;
  GraphUnits:='';
  GridType:=gtNone;
  GridBoldUnits:=0;
  GridFlags:=0;
  PrimaryGridStyle := gpsDefault;
  SecondaryGridStyle := gpsDefault;
end;

constructor GridObject.Create(oldgrid:GridObject);
begin
  inherited Create;
  GraphScale:=oldgrid.GraphScale;
  GraphUnitConvert:=oldgrid.GraphUnitConvert;
  GraphUnits:=oldgrid.GraphUnits;
  CurrentGraphUnits:=oldgrid.CurrentGraphUnits;
  CurrentGridSize:=oldgrid.CurrentGridSize;
  GridType:=oldgrid.GridType;
  GridBoldUnits:=oldgrid.GridBoldUnits;
  GridFlags:=oldgrid.GridFlags;
  GridPosition:=oldgrid.GridPosition;
  PrimaryGridStyle:=oldgrid.PrimaryGridStyle;
  SecondaryGridStyle:=oldgrid.SecondaryGridStyle;
end;

class procedure GridObject.SetGridPenStyle(Canvas:TCanvas; style,defaultstyle:GridPenStyle);
begin
  if style=gpsDefault then style:=defaultstyle;

  case style of
    gpsDefault:begin
                 Canvas.Pen.Style := psClear;
               end;
    gpsSingle: begin
                 Canvas.Pen.Width := 1;
                 Canvas.Pen.Style := psSolid;
               end;
    gpsDot:   begin
                 Canvas.Pen.Width := 1;
                 Canvas.Pen.Style := psDot;
               end;
    gpsDash:   begin
                 Canvas.Pen.Width := 1;
                 Canvas.Pen.Style := psDash;
               end;
    gpsDashDot:begin
                 Canvas.Pen.Width := 1;
                 Canvas.Pen.Style := psDashDot;
               end;
    gpsDashDotDot:begin
                 Canvas.Pen.Width := 1;
                 Canvas.Pen.Style := psDashDotDot;
               end;
    gpsBold:   begin
                 Canvas.Pen.Width := 3;
                 Canvas.Pen.Style := psSolid;
               end;
    end;
end;

procedure GridObject.DrawGrid(v:TObject);
var oldcolor:TColor;
    ox,oy:Coord;
    sx,sy:integer;
    left,top,right,bottom,width,height:Coord;
    View:Viewpoint;

    procedure SquareGrid(num:integer);
    var i:integer;
        size:Coord;
    begin
      size:=CurrentGridSize*num;
      ox := Int(left/size)*size;
      oy := Int(top/size)*size;

      with View.Canvas do begin
        for i:=0 to trunc((width+size)/size)+1 do begin
          View.CoordToScreen(ox+i*size,0,sx,sy);
          MoveTo(sx,ClipRect.top);
          LineTo(sx,ClipRect.bottom);
          end;

        for i:=0 to trunc((height+size)/size)+1 do begin
          View.CoordToScreen(0,oy+i*size,sx,sy);
          MoveTo(ClipRect.left,sy);
          LineTo(ClipRect.right,sy);
          end;
        end;
    end;

    procedure HexGrid(num:integer);
    { Draws hex grid with evenly spaced multiples of below pattern:
             |              A
          .:' ':.         BB FF
         |          ==>  C     G
         |               C     G
          ':. .:'         DD HH
             |              E
    }
    var w,h:Coord;
        i,j:integer;
        x,y:Coord;
        w2,hs,s,s2,iw,ih:integer;
        size:Coord;
    begin
      size:=CurrentGridSize;
      w:=size*sqrt(3);
      h:=size*3;
      w:=w*num;
      h:=h*num;
      ox := Int(left/w)*w;
      oy := Int(top/h)*h;

      { Optimization variables }
      View.DeltaCoordToScreen(w/2,(size/2)*num,w2,hs);
      View.DeltaCoordToScreen(size*num,size*2*num,s,s2);
      View.DeltaCoordToScreen(w,h,iw,ih);
      inc(iw); { Account for roundoff }

      with View.Canvas do begin
        for i:=-1 to trunc((width+w)/w)+1 do begin
          x:=i*w + ox;
          for j:=-1 to trunc((height+h)/h)+1 do begin
            y:=j*h + oy;

            View.CoordToScreen(x,y,sx,sy);

            MoveTo(sx + w2, sy);
            LineTo(sx + w2, sy + hs);       { A }
            LineTo(sx,      sy + s);        { B }
            LineTo(sx,      sy + s2);       { C }
            LineTo(sx + w2, sy + ih - hs);  { D }
            LineTo(sx + w2, sy + ih + 1);   { E }

            MoveTo(sx + w2, sy + hs);
            LineTo(sx + iw, sy + s);        { F }

            MoveTo(sx + iw, sy + s2);
            LineTo(sx + w2, sy + ih - hs);  { H }
            end;
          end;
        end;
    end;

    procedure TriangleGrid(num:integer);
    { Draws triangular grid with evenly spaced multiples of below pattern:

        \  /         B  A
        _\/_    ==>   BA
         /\          DABD
        /  \         A  B
        ¯¯¯¯         CCCC
    }
    var w,h:Coord;
        i,j:integer;
        x,y:Coord;
        size:Coord;
        iw,ih:integer;
    begin
      size:=CurrentGridSize;
      w:=size;
      h:=w*sqrt(3);
      w:=w*num;
      h:=h*num;
      ox := Int(left/w)*w;
      oy := Int(top/h)*h;

      View.DeltaCoordToScreen(w,h,iw,ih);

      with View.Canvas do begin
        for i:=-1 to trunc((width+w)/w)+1 do begin
          x:=i*w + ox;
          for j:=-1 to trunc((height+h)/h)+1 do begin
            y:=j*h + oy;
            View.CoordToScreen(x,y,sx,sy);

            MoveTo(sx + iw, sy);
            LineTo(sx,      sy + ih);  { A }
            LineTo(sx + iw, sy + ih);  { C }
            LineTo(sx,      sy);       { B }

            MoveTo(sx,      sy + ih div 2);
            LineTo(sx + iw, sy + ih div 2); { D }
            end;
          end;
        end;
    end;

    procedure RotatedHexGrid(num:integer);
    { Draws hex grid with evenly spaced multiples of below pattern:
            ___               BBB
           /   \             A   C
          /     \___ ==>    A     CDDD
          \     /           F     E
           \   /             F   E

    }
    var w,h:Coord;
        i,j:integer;
        x,y:Coord;
        w2,hs,s,s2,iw,ih:integer;
        size:Coord;
    begin
      size:=CurrentGridSize;
      w:=size*3;
      h:=size*sqrt(3);
      w:=w*num;
      h:=h*num;
      ox := Int(left/w)*w;
      oy := Int(top/h)*h;

      { Optimization variables }
      View.DeltaCoordToScreen(w/2,h/2,w2,hs);
      View.DeltaCoordToScreen(size*num,(size*num)/2,s,s2);
      View.DeltaCoordToScreen(w,h,iw,ih);
      inc(iw); { Account for roundoff }

      with View.Canvas do begin
        for i:=-1 to trunc((width+w)/w)+1 do begin
          x:=i*w + ox;
          for j:=-1 to trunc((height+h)/h)+1 do begin
            y:=j*h - (h/2) + oy;

            View.CoordToScreen(x,y,sx,sy);

            MoveTo(sx,           sy + hs);
            LineTo(sx + s2,      sy);          { A }
            LineTo(sx + w2,      sy);          { B }
            LineTo(sx + w2 + s2, sy + hs);     { C }
            LineTo(sx + iw,      sy + hs);     { D }

            MoveTo(sx + w2 + s2, sy + hs);
            LineTo(sx + w2,      sy + ih);     { E }

            MoveTo(sx,           sy + hs);
            LineTo(sx + s2,      sy + ih);     { F }
            end;
          end;
        end;
    end;

    procedure DiamondGrid(num:integer; squash:single);
    {
      Draws diamond grid (rotated square grid).  Squash is the height factor:
      1.0 draws a square grid (albeit rotated 45 degrees), where 0.5 draws a grid where the vertical diagonal
      is half the horizontal diagonal.
    }
    var i:integer;
        size:Coord;
        w,h:Coord;
        nw,nh:integer;

        // Call this function on any coordinates you want to make sure will still work under 16-bit GDI.
        // That is, Windows 2000 or Windows NT don't have problems with GDI coordinates that exceed a 16-bit integer,
        // but Windows 95 and 98 do.  Since the previous method of my creating diamond grids was rather crude,
        // it could exceed the limit of a 16-bit integer, which resulted in garbage on the display.  I do my
        // development on Windows 2000, so never saw the problem.
        // -------------------------------------
        //   function gditrunc(x:integer):integer;
        //   begin
        //     if (x<-32767) then x:=32767;
        //     if (x>32767) then x:=-32767;
        //     Result:=x;
        //   end;

    begin
      if (bottom-top=0.0) then exit;

      // Figure out each boxes width and height.
      size:=CurrentGridSize*num;
      w:=size*sqrt(2);
      h:=size*sqrt(2)*squash;

      // Get us coordinates one square over each edge,
      // snapped to the underlying grid.
      ox := Int((left-w)/w)*w;
      oy := Int((top-h)/h)*h;

      // Using +1 over width/height of clip rectangle is good enough,
      // for a full screen, but use +2 for cases when our clip rectangle
      // is an odd small size, and we need just a little extra overdraw
      nw := trunc((width+w)/w)+2;
      nh := trunc((height+h)/h)+2;

      with View.Canvas do begin
        // Horizontally:
        // Do "inverted V" lines from the bottom right to the middle top,
        // to the bottom left.
        //          +------+
        // I.e.     |/xxx\ |
        //          |//x\\\|
        //          |// \\\|
        //          +------+
        for i:=0 to nw+2 do begin
          View.CoordToScreen(ox+(i+nh)*w,oy+nh*h, sx,sy);
          MoveTo(sx,sy);
          View.CoordToScreen(ox+i*w,oy,sx,sy);
          LineTo(sx,sy);
          View.CoordToScreen(ox, oy+i*h, sx,sy);
          LineTo(sx,sy);
          end;

        // Vertically:
        // Do lines from the left edge to the bottom to fill in the remaining
        // grid lines on the left
        //          +------+
        // I.e.     |\     |
        //          |\\    |
        //          |\\\   |
        //          +------+
        for i:=1 to nh+1 do begin
          View.CoordToScreen(ox,oy+(i)*h,sx,sy);
          MoveTo(sx,sy);
          View.CoordToScreen(ox+(nw+1)*w,oy+(nw+i+1)*h, sx,sy);
          LineTo(sx,sy);
          end;

        // Vertically:
        // Do lines from the right edge to the bottom to fill in the remaining
        // grid lines on the right
        //          +------+
        // I.e.     |     /|
        //          |    //|
        //          |   ///|
        //          +------+
        for i:=2 to nh+1 do begin
          View.CoordToScreen(ox+(nw+1)*w,    oy+(i)*h,sx,sy);
          MoveTo(sx,sy);
          View.CoordToScreen(ox+(nw+1-nh+i)*w,oy+(nh)*h, sx,sy);
          LineTo(sx,sy);
          end;
      end;
    end;

    procedure PolarGrid(num:integer);
    const sectors=256;
    var i:integer;
        size:Coord;
        x,y:Coord;
        ix,iy:integer;
        radius:Coord;
        wedges:integer;
        innerring:integer;
    begin
      size:=CurrentGridSize*num;

      // Figure out how far we need to draw outwards
      radius := max(abs(top),abs(bottom));
      radius := max(radius,abs(left));
      radius := max(radius,abs(right));

      // Include a little fudge factor to account for screen diagonal
      radius := radius * 1.5;

      with View.Canvas do begin
        Brush.Style := bsClear;

        // Draw rings
        for i:=0 to trunc((radius+size)/size)+1 do begin
          View.CoordToScreen(-i*size,-i*size,sx,sy);
          View.CoordToScreen(i*size,i*size,ix,iy);
          Ellipse(sx,sy,ix,iy);
          end;

        // Draw sectors
        wedges:=sectors;

        for i:=0 to wedges-1 do begin
          // The inner edge depends on which part of the line
          // it is: we let some of the lines go all the way
          // to the center, whereas others get stopped on the
          // way out.  Basically, we let 4 into the center ring,
          // and 4* as many for each nextmost ring.
          if (i mod (sectors div 4))=0 then
            innerring := 0
          else if (i mod (sectors div 16))=0 then
            innerring := 1
          else if (i mod (sectors div 64))=0 then
            innerring := 2
          else if (i mod (sectors div 128))=0 then
            innerring := 3
          else
            innerring := 4;

          x := (size*innerring) * cos(2*pi*(i/wedges));
          y := (size*innerring) * sin(2*pi*(i/wedges));
          View.CoordToScreen(x,y,sx,sy);

          // This is the outer edge
          x := radius * cos(2*pi*(i/wedges));
          y := radius * sin(2*pi*(i/wedges));
          View.CoordToScreen(x,y,ix,iy);
          MoveTo(sx,sy);
          LineTo(ix,iy);
          end;
        end;
    end;


begin
  view:=ViewPoint(v);

  if (GridType=gtNone) then exit;

  with View.Canvas do begin
    oldcolor:=Pen.Color;
    Pen.Color := CurrentGridColor;

    View.ScreenToCoord(Cliprect.Left,Cliprect.Top,left,top);
    View.ScreenToCoord(Cliprect.Right,Cliprect.Bottom,right,bottom);
    width:=right-left + 1;
    height:=bottom-top + 1;

    SetGridPenStyle(View.Canvas, PrimaryGridStyle, gpsSingle);

    case GridType of
      gtSquare:     SquareGrid(1);
      gtHex:        HexGrid(1);
      gtTriangle:   TriangleGrid(1);
      gtRotatedHex: RotatedHexGrid(1);
      gtDiamond:    DiamondGrid(1, 1.0);
      gtHalfDiamond:DiamondGrid(1, 0.5);
      gtPolar:      PolarGrid(1);
      end;

    if (GridBoldUnits<>0) then begin
      SetGridPenStyle(View.Canvas, SecondaryGridStyle, gpsBold);

      case GridType of
        gtSquare:     SquareGrid(GridBoldUnits);
        gtHex:        HexGrid(GridBoldUnits);
        gtTriangle:   TriangleGrid(GridBoldUnits);
        gtRotatedHex: RotatedHexGrid(GridBoldUnits);
        gtDiamond:    DiamondGrid(GridBoldUnits, 1.0);
        gtHalfDiamond:DiamondGrid(GridBoldUnits, 0.5);
        gtPolar:      PolarGrid(GridBoldUnits);
        end;

    Pen.Width:=1;
    end;

    Pen.Color := oldcolor;
    end;
end;

procedure GridObject.SetGraphUnits(index:integer; GridSize:single);
begin
  { -1 lets us reset the scale for a different base unit }
  if (index<>-1) then begin
    if CurrentGraphUnits=-1 then
      GraphUnitConvert := 1/GridSize
    else begin
      GraphUnitConvert := GraphUnitConvert*(CurrentGridSize*UnitTable[CurrentGraphUnits].factor);
      GraphUnitConvert := GraphUnitConvert/(GridSize*UnitTable[index].factor);
      end;

    GraphUnits:=UnitTable[index].name;
    end;

  CurrentGraphUnits := index;
  CurrentGridSize   := GridSize;
end;

class procedure GridObject.FillComboList(UnitComboBox:TComboBox);
var i:integer;
begin
  UnitComboBox.Items.Clear;
  for i:=Low(UnitTable) to High(UnitTable) do
    UnitComboBox.Items.Add(UnitTable[i].name);
end;

procedure GridObject.SetMeasurementUnits(index:integer);
begin
  if index<>CurrentGraphUnits then Map.SetModified(modUnitType);
  SetGraphUnits(index,CurrentGridSize);
end;

procedure GridObject.SetMeasurementUnitChecks(Menu:TMenuItem);
var i:integer;
begin
  for i:=Low(UnitTable) to High(UnitTable) do begin
    Menu.Items[i].Checked := (i=CurrentGraphUnits);
    end;
end;

function GridObject.Convert(dist:double; unitindex:integer):double;
begin
  Result:=(dist/GraphScale)*CurrentGridSize*UnitTable[unitindex].factor/UnitTable[CurrentGraphUnits].factor;
end;

procedure GridObject.FillPopupMenu(Menu:TMenuItem; MeasurementMenuItemClick:TNotifyEvent);
var i:integer;
    NewItem: TMenuItem;
begin
  for i:=Low(UnitTable) to High(UnitTable) do begin
    NewItem := TMenuItem.Create(Menu);
    NewItem.Caption := UnitTable[i].name;
    NewItem.Tag:=i;
    NewItem.OnClick:=MeasurementMenuItemClick;
    Menu.Add(NewItem);
    end;
end;

function GridObject.GetUnitLength:double;
begin
  Result := GraphScale*GraphUnitConvert*CurrentGridSize;
end;

Function GridObject.GetAsDOMElement(D: TDOMDocument): TDOMElement;
Var E: TDOMElement;

  Procedure SaveGridStyle(Name: String; Style: GridPenStyle);
  Begin
    Case Style Of
      gpsDefault:    E.appendChild(NewStringProperty(D,Name,'DEFAULT'));
      gpsSingle:     E.appendChild(NewStringProperty(D,Name,'SINGLE'));
      gpsDot:        E.appendChild(NewStringProperty(D,Name,'DOT'));
      gpsDash:       E.appendChild(NewStringProperty(D,Name,'DASH'));
      gpsDashDot:    E.appendChild(NewStringProperty(D,Name,'DASHDOT'));
      gpsDashDotDot: E.appendChild(NewStringProperty(D,Name,'DASHDOTDOT'));
      gpsBold:       E.appendChild(NewStringProperty(D,Name,'BOLD'));
    Else
      E.appendChild(NewStringProperty(D,Name,'DEFAULT'));
    End; // Case
  End; // SaveGridStyle

Begin
  E := D.createElement('GRIDOBJECT');
  E.appendChild(NewDoubleProperty(D,'GRAPH_SCALE',GraphScale));
  E.appendChild(NewDoubleProperty(D,'GRAPH_UNIT_CONVERT',GraphUnitConvert));
  E.appendChild(NewStringProperty(D,'GRAPH_UNITS',GraphUnits));
  E.appendChild(NewIntegerProperty(D,'CURRENT_GRAPH_UNITS',CurrentGraphUnits));
  E.appendChild(NewDoubleProperty(D,'CURRENT_SIZE',CurrentGridSize));
  Case GridType Of
    gtNone:        E.appendChild(NewStringProperty(D,'TYPE','NONE'));
    gtSquare:      E.appendChild(NewStringProperty(D,'TYPE','SQUARE'));
    gtHex:         E.appendChild(NewStringProperty(D,'TYPE','HEX'));
    gtTriangle:    E.appendChild(NewStringProperty(D,'TYPE','TRIANGLE'));
    gtRotatedHex:  E.appendChild(NewStringProperty(D,'TYPE','ROTATEDHEX'));
    gtDiamond:     E.appendChild(NewStringProperty(D,'TYPE','DIAMOND'));
    gtHalfDiamond: E.appendChild(NewStringProperty(D,'TYPE','HALFDIAMOND'));
    gtPolar:       E.appendChild(NewStringProperty(D,'TYPE','POLAR'));
  Else
    E.appendChild(NewStringProperty(D,'TYPE','NONE'));
  End; // Case
  E.appendChild(NewIntegerProperty(D,'BOLD_UNITS',GridBoldUnits));
  E.appendChild(NewCardinalProperty(D,'FLAGS',GridFlags));
  E.appendChild(NewCardinalProperty(D,'POSITION',GridPosition));
  SaveGridStyle('PRIMARY_STYLE',PrimaryGridStyle);
  SaveGridStyle('SECONDARY_STYLE',SecondaryGridStyle);
  Result := E;
End; // GridObject.GetAsDOMElement

Procedure GridObject.LoadFromDOMElement(E: TDOMElement);
Var S: String;

  Function LoadGridStyle(Name: String): GridPenStyle;
  Var S: String;
  Begin
    S := Trim(UpperCase(GetStringProperty(E,Name)));
         If S = 'DEFAULT'    Then Result := gpsDefault
    Else If S = 'SINGLE'     Then Result := gpsSingle
    Else If S = 'DOT'        Then Result := gpsDot
    Else If S = 'DASH'       Then Result := gpsDash
    Else If S = 'DASHDOT'    Then Result := gpsDashDot
    Else If S = 'DASHDOTDOT' Then Result := gpsDashDotDot
    Else If S = 'BOLD'       Then Result := gpsBold
    Else Result := gpsDefault;
  End; // LoadGridStyle

Begin
  GraphScale        := GetDoubleProperty(E,'GRAPH_SCALE');
  GraphUnitConvert  := GetDoubleProperty(E,'GRAPH_UNIT_CONVERT');
  GraphUnits        := Trim(GetStringProperty(E,'GRAPH_UNITS'));
  CurrentGraphUnits := GetIntegerProperty(E,'CURRENT_GRAPH_UNITS');
  CurrentGridSize   := GetDoubleProperty(E,'CURRENT_SIZE');
  S                 := Trim(UpperCase(GetStringProperty(E,'TYPE')));
       If S = 'NONE'        Then GridType := gtNone
  Else If S = 'SQUARE'      Then GridType := gtSquare
  Else If S = 'HEX'         Then GridType := gtHex
  Else If S = 'TRIANGLE'    Then GridType := gtTriangle
  Else If S = 'ROTATEDHEX'  Then GridType := gtRotatedHex
  Else If S = 'DIAMOND'     Then GridType := gtDiamond
  Else If S = 'HALFDIAMOND' Then GridType := gtHalfDiamond
  Else If S = 'POLAR'       Then GridType := gtPolar
  Else GridType := gtNone;
  GridBoldUnits      := GetIntegerProperty(E,'BOLD_UNITS');
  GridFlags          := GetCardinalProperty(E,'FLAGS');
  GridPosition       := GetCardinalProperty(E,'POSITION');
  PrimaryGridStyle   := LoadGridStyle('PRIMARY_STYLE');
  SecondaryGridStyle := LoadGridStyle('SECONDARY_STYLE');
End; // GridObject.LoadFromDOMElement

procedure GridObject.SaveToStream(stream:TStream; version:integer);
var
  grid_pos_out: byte;
begin
  stream.WriteBuffer(GraphScale, sizeof(GraphScale));
  stream.WriteBuffer(GraphUnitConvert, sizeof(GraphUnitConvert));
  WriteStringToStream(stream, GraphUnits);
  stream.WriteBuffer(CurrentGraphUnits, sizeof(CurrentGraphUnits));
  stream.WriteBuffer(CurrentGridSize, sizeof(CurrentGridSize));
  stream.WriteBuffer(GridType, sizeof(GridType));
  stream.WriteBuffer(GridBoldUnits, sizeof(GridBoldUnits));

  stream.WriteBuffer(GridFlags, sizeof(GridFlags));

  // 2003/05/29 - J.Friant
  // For now files saved under the old format only have 255
  // grid positions available.  So if you want to draw more
  // primitives than that below the grid then you'll need
  // to save your file in the XML format.
  if GridPosition > 255 then
    grid_pos_out := 255
  else
    grid_pos_out := GridPosition;
  stream.WriteBuffer(grid_pos_out, sizeof(grid_pos_out));
  stream.WriteBuffer(PrimaryGridStyle, sizeof(byte));
  stream.WriteBuffer(SecondaryGridStyle, sizeof(byte));
end;

procedure GridObject.LoadFromStream(stream:TStream; version:integer);
begin
  stream.ReadBuffer(GraphScale, sizeof(GraphScale));
  stream.ReadBuffer(GraphUnitConvert, sizeof(GraphUnitConvert));
  GraphUnits := ReadStringFromStream(stream);
  stream.ReadBuffer(CurrentGraphUnits, sizeof(CurrentGraphUnits));
  stream.ReadBuffer(CurrentGridSize, sizeof(CurrentGridSize));
  stream.ReadBuffer(GridType, sizeof(GridType));
  stream.ReadBuffer(GridBoldUnits, sizeof(GridBoldUnits));

  // GridFlags was a 32-bit integer.  Now it is 16 bits,
  // and we make sure the grid styles are one byte each
  // so we haven't changed the file format.  Note that the
  // flags were also zero in the high two bytes because
  // no flags were defined there, and 0 is also our
  // default.  Happy "coincidence", huh?
  PrimaryGridStyle := gpsDefault;               // Clear these out since we only read in a byte;
  SecondaryGridStyle := gpsDefault;             // Don't want the upper 3 bytes filled with garbage
  stream.ReadBuffer(GridFlags, sizeof(GridFlags));
  // 2003/05/29 - J.Friant
  // Since we still don't need 65k flags (I believe we're
  // only using 1[!]) I'm going to split GridFlags again!
  // Also the grid position should not be affected, since
  // the only flag we've used so far is 0x01
  //
  // 2003/12/08 - J.Friant
  // BUG WORK-AROUND: This function is called by
  // MapSettingsClick in addition to the routines for loading
  // a file, so it can mess up the GridPosition by reseting
  // it to the byte max of 255 when we really want a larger
  // value...  so we'll check to make sure the value is within
  // the expected range for a byte or just leave it alone
  if ( GridPosition <= 255 ) then begin
    GridPosition := 0; // initialize to zero since we're reading a Byte into a Cardinal
    stream.ReadBuffer(GridPosition, sizeof(byte));
  end;
  stream.ReadBuffer(PrimaryGridStyle, sizeof(byte));
  stream.ReadBuffer(SecondaryGridStyle, sizeof(byte));
end;

procedure GridObject.bringGridForward;
begin
  GridPosition := GridPosition + 1;
end;

procedure GridObject.sendGridBackward;
begin
  // an side-effect of GridPosition being typed as a Cardinal
  // is that if the user tries to send the grid further back
  // than 0 they'll end up at the maximum distance in front.
  // It shouldn't cause any problems though.
  GridPosition := GridPosition - 1;
end;

function GridObject.getGridPosition: Cardinal;
begin
  getGridPosition := GridPosition;
end;

begin
  CurrentGridColor:=clAqua;
end.
