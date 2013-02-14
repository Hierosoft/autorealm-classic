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
unit TextSpecialties;

interface

uses Geometry,Windows,Classes,Graphics,SysUtils;

procedure CenterText(canvas:TCanvas; x,y:integer; text:string);
procedure XorDrawText(canvas:TCanvas; x,y:integer; text:string; format:integer; backgroundcolor:TColor);
procedure RotatedFont(var Font:TFont; angle:integer);
procedure DrawBezierText(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; text:string);
function ComputeBezierText(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; text:string; var PointList:PCoordArray; var angle:integer):integer;

implementation

uses Math,Main;

procedure CenterText(canvas:TCanvas; x,y:integer; text:string);
var w,h:integer;
begin
  w := Canvas.TextWidth(text);
  h := Canvas.TextHeight(text);
  canvas.TextOut(x-(w div 2), y-(h div 2), text);
end;

procedure RotatedFont(var Font:TFont; angle:integer);
var
  LogFont: TLogFont;
begin
  with LogFont do begin
    lfHeight := Font.Height;
    lfWidth := 0; { have font mapper choose }
    lfEscapement := angle;
    lfOrientation := lfEscapement;
    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in Font.Style);
    lfUnderline := Byte(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := Byte(Font.Charset);
    if AnsiCompareText(Font.Name, 'Default') = 0 then  // do not localize
      StrPCopy(lfFaceName, 'Arial')
    else
      StrPCopy(lfFaceName, Font.Name);
    lfQuality := DEFAULT_QUALITY;
    { Everything else as default }
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Font.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
    Font.Handle := CreateFontIndirect(LogFont);
    end;
end;

procedure XorDrawText(canvas:TCanvas; x,y:integer; text:string; format:integer; backgroundcolor:TColor);
var origr:TRect;
    bm:TBitmap;
    OldCopyMode:Integer;
    OldBrushColor,OldFontColor:TColor;
begin
  OldCopyMode  := Canvas.CopyMode;
  OldBrushColor:= Canvas.Brush.Color;
  OldFontColor := Canvas.Font.Color;

  bm:=TBitmap.Create;
  bm.Monochrome := True;

  bm.Canvas.Font.Assign(Canvas.Font);
  bm.Canvas.Font.Color:=clWhite;
  bm.Canvas.Brush.Color:=clBlack;

  Canvas.Brush.Color := Canvas.Font.Color xor backgroundcolor;
  Canvas.CopyMode := cmSrcInvert;
  { For some reason, this prevents the background from changing color }
  Canvas.Font.Color := clBlack;

  origr:=Rect(0,0,1,1);
  bm.Height:=DrawText(bm.Canvas.Handle, PChar(text), -1, origr, format or DT_CALCRECT);
  bm.Width:=origr.Right-origr.Left+1;
  DrawText(bm.Canvas.Handle, PChar(text), -1, origr, format);

  if (format and DT_CENTER)<>0 then
    x:=x-(bm.Width div 2)
  else if (format and DT_RIGHT)<>0 then
    x:=x-bm.Width;

  if (format and DT_VCENTER)<>0 then begin
    y:=y-(bm.Height div 2);
    end;

  Canvas.Draw(x,y, bm);

  bm.Free;
  Canvas.CopyMode   := OldCopyMode;
  Canvas.Brush.Color:= OldBrushColor;
  Canvas.Font.Color := OldFontColor;
end;



procedure DrawBezierText(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; text:string);
var angle:integer;
    Font:TFont;
    points:PCoordArray;
    i,count:integer;
begin
  Font:=TFont.Create;
  count:=ComputeBezierText(canvas,p1,p2,p3,p4,text,points,angle);
  Font.Assign(Canvas.Font);
  RotatedFont(Font,angle);
  Canvas.Font.Assign(Font);
  for i:=1 to count do begin
    Canvas.TextOut(trunc(points^[i-1].X),trunc(points^[i-1].Y),text[i]);
    end;
  RotatedFont(Font,0);
  Canvas.Font.Assign(Font);
  Font.Free;
  FreeMem(Points);
end;

function ComputeBezierText(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; text:string; var PointList:PCoordArray; var angle:integer):integer;
// If there is more resolution desired, split the curve, and recurse.
// The curve is split using a simple and effective equality: De Castigilu's
// Algorithm):
//
//  Conceptual
//  (coordinates)
//   p1
//     > q1
//   p2    > r1
//     > q2    > s1
//   p3    > r2
//     > q3
//   p4
//
// p1..p4 are the points of the original bezier curve.  Each parent coord. on
// the tree above is the average of the two coords below.  The point s1
// is the split point, and the curves <p1 q1 r1 s1>, and <p4 q3 r2 s1> are the
// two resulting Bezier curves.  The combination of the two curves resulting
// from splitting reproduces the original (disregarding roundoff errors).
var letterwidth:single;
    letter:string;
    spacing:double;
    n:integer;

  procedure ComputeLetterWidth;
  begin
    repeat
      letter:=copy(text,1,1);
      delete(text,1,1);
    until (letter='') or (letter[1]>=' ');
    letterwidth:=Canvas.TextWidth('W')*spacing;
  end;

  procedure DoBezier(p1,p2,p3,p4:CoordPoint);
  var q1,q2,q3:CoordPoint;
      r1,r2,s1:CoordPoint;
      d:double;
  begin
    if letter='' then exit;

    d := distance(p1.x,p1.y,p4.x,p4.y);
    if (d <= letterwidth) then begin
      PointList^[n].X:=(p1.x+p4.x)/2;
      PointList^[n].Y:=(p1.y+p4.y)/2;
      inc(n);
      ComputeLetterWidth;
      exit;
      end;

    q1:=AvePoints(p1,p2);
    q2:=AvePoints(p2,p3);
    q3:=AvePoints(p3,p4);
    r1:=AvePoints(q1,q2);
    r2:=AvePoints(q2,q3);
    s1:=AvePoints(r1,r2);

    DoBezier(p1,q1,r1,s1);
    DoBezier(s1,r2,q3,p4);
  end;

begin
  GetMem(PointList, sizeof(CoordPoint)*length(text));
  spacing:=1.5;
  angle:=3600-trunc(3600*(arctan2(p4.y-p1.y,p4.x-p1.x)/(2*pi)));
  ComputeLetterWidth;
  n:=0;
  DoBezier(p1,p2,p3,p4);
  Result:=n;
end;

{procedure ConvertTextToPolylines(s:string; var points:PCoordArray; var count:integer);
begin
Path
end; }

end.
