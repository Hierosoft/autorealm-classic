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
unit Geometry;

interface

{ uses Windows; }
uses Types; { TPoint }

type Coord = single;

     CoordPoint = record
       X: Coord;
       Y: Coord;
       end;

     CoordArray = array[0..High(Word)] of CoordPoint;

     PCoordPoint = ^CoordPoint;
     PCoordArray = ^CoordArray;

     CoordRect = record
       case Integer of
           0: (Left, Top, Right, Bottom: Coord);
           1: (TopLeft, BottomRight: CoordPoint);
       end;

     PointArray=array[0..0] of TPoint;
     PPointArray=^PointArray;

     ByteArray=array[0..0] of Byte;
     PByteArray=^ByteArray;

     IntersectType = (NoIntersect, IntersectOnLine, IntersectOffLine, IntersectOffLineBegin, IntersectOffLineEnd);

  TLineBin = Class
    PointCount : Integer;
    NumCount   : Integer;
    Points     : Array Of CoordPoint;
    IStart     : Array Of Integer;
    IEnd       : Array Of Integer;
    Child      : TLineBin;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Add(Var SourcePoints: Array Of CoordPoint; StartIndex,EndIndex: Integer);
    Procedure   AddSegment(X1,Y1,X2,Y2: Coord; New: Boolean);
    Procedure   Remove(StartIndex,EndIndex: Integer);
    Procedure   Split;
    Function    GetClosestIntersection(Var P: CoordPoint): Boolean;
  End;

function min(x,y:Coord):Coord;
function max(x,y:Coord):Coord;
function MakeCoordRect(left,top,right,bottom:Coord):CoordRect;
function MakeCoordPoint(X,Y:Coord):CoordPoint;
function PtInCoordRect(r:CoordRect; p:CoordPoint):boolean;
procedure Encompass(var b:CoordRect; pX,pY:Coord);
function  Distance(x1,y1,x2,y2:double):double;
function  Angle(x1,y1,x2,y2:double):double;
procedure UnitVector(x1,y1,x2,y2:double; var px,py:double);
procedure UnitPerpendicular(x1,y1,x2,y2:double; var px,py:double);
function AvePoints(a,b:CoordPoint):CoordPoint;
function VisibleWithin(p:CoordRect; r:CoordRect):boolean; overload;
procedure CorrectCoordRect(var r:CoordRect);
procedure CorrectRect(var r:TRect);
function VisibleWithin(p,r:TRect):boolean; overload;
procedure SwapPoints(var a,b:CoordPoint);

function DistanceToSegment(const p:CoordPoint; p1,p2:CoordPoint):double;
function Intersection(const p1,p2:CoordPoint; const p3,p4:CoordPoint):boolean;
function IntersectLine(const p1,p2:CoordPoint; const p3,p4:CoordPoint; var isect:CoordPoint):IntersectType;
//function ClipLineInsideRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
function CropLineOutsideRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
function ExtendLineToRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
function PointInPolygon(const p:CoordPoint; polyextent:CoordRect; poly:PCoordArray; polycount:integer):boolean;
Function NearestIntersection(Segments: Array Of CoordPoint; NumSegments: Array Of Integer;
                             Var ISect: CoordPoint): Boolean;


implementation

uses Math,SysUtils,Classes,           Main;

Const ArrayInc = 20;

function min(x,y:Coord):Coord;
begin
  if (x<y) then
    Result:=x
  else
    Result:=y;
end;

function max(x,y:Coord):Coord;
begin
  if (x>y) then
    Result:=x
  else
    Result:=y;
end;

function MakeCoordPoint(X,Y:Coord):CoordPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure SwapPoints(var a,b:CoordPoint);
var t:CoordPoint;
begin
  t:=a;
  a:=b;
  b:=t;
end;

procedure CorrectCoordRect(var r:CoordRect);
var temp:coord;
begin
  with r do begin
    if left > right then begin
      temp:=left;
      left:=right;
      right:=temp;
      end;

    if top > bottom then begin
      temp:=bottom;
      bottom:=top;
      top:=temp;
      end;
  end;
end;

procedure CorrectRect(var r:TRect);
var temp:integer;
begin
  with r do begin
    if left > right then begin
      temp:=left;
      left:=right;
      right:=temp;
      end;

    if top > bottom then begin
      temp:=bottom;
      bottom:=top;
      top:=temp;
      end;
  end;
end;

function MakeCoordRect(left,top,right,bottom:Coord):CoordRect;
begin
  Result.Left   := Left;
  Result.Top    := top;
  Result.Right  := Right;
  Result.Bottom := Bottom;

  CorrectCoordRect(Result);
end;

procedure Encompass(var b:CoordRect; pX,pY:Coord);
begin
  if (pX < b.Left)   then b.Left:=pX;
  if (pX > b.Right)  then b.Right:=pX;
  if (pY < b.Top)    then b.Top:=pY;
  if (pY > b.Bottom) then b.Bottom:=pY;
end;

function PtInCoordRect(r:CoordRect; p:CoordPoint):boolean;
begin
  Result := (p.Y>=r.Top) and (p.Y<=r.Bottom) and
            (p.X>=r.Left) and (p.X<=r.Right);
end;

function PtInCoordRectDelta(r:CoordRect; p:CoordPoint; delta:Coord):boolean;
begin
  Result := (p.Y>=r.Top-delta) and (p.Y<=r.Bottom+delta) and
            (p.X>=r.Left-delta) and (p.X<=r.Right+delta);
end;

function VisibleWithin(p:CoordRect; r:CoordRect):boolean;
begin
  Result:=false;

  if (p.right < r.left)  and (p.left < r.left)      then exit;
  if (p.left  > r.right) and (p.right > r.right)    then exit;
  if (p.bottom< r.top)   and (p.top < r.top)        then exit;
  if (p.top > r.bottom)  and (p.bottom > r.bottom)  then exit;

  Result:=true;
end;

function VisibleWithin(p,r:TRect):boolean;
begin
  Result:=false;

  if (p.right < r.left)  and (p.left < r.left)      then exit;
  if (p.left  > r.right) and (p.right > r.right)    then exit;
  if (p.bottom< r.top)   and (p.top < r.top)        then exit;
  if (p.top > r.bottom)  and (p.bottom > r.bottom)  then exit;

  Result:=true;
end;

function Angle(x1,y1,x2,y2:double):double;
var numerator,denominator:double;
begin
  try
    numerator   := y1-y2;
    denominator := x2-x1;

    // Try to prevent any floating-point exceptions; are they problematic for Celeron?
    if (denominator=0.0) or (abs(numerator)>264) or (abs(denominator)>264) then begin
      Angle := 0.0;
      exit;
    end;

    Result:=180.0*ArcTan2(numerator,denominator)/Pi;
    if (Result<0.0) then Result:=Result+360.0;
  except
    on EInvalidOp do Angle := 0.0;
  end;
end;


function Distance(x1,y1,x2,y2:double):double;
var hypoten:double;
begin
  try
    hypoten := sqr(x2-x1)+sqr(y2-y1);

    // Try to prevent any floating-point exceptions; are they problematic for Celeron?
    if (hypoten<0.0) then begin
      Distance := 0;
      exit;
      end;

    Distance:=sqrt(hypoten);
  except
    on EInvalidOp do Distance := 0;
  end;
end;

function AvePoints(a,b:CoordPoint):CoordPoint;
begin
  Result.X := (a.x+b.x)*0.5;
  Result.Y := (a.y+b.y)*0.5;
end;


procedure UnitVector(x1,y1,x2,y2:double; var px,py:double);
var d:double;
begin
  d:=Distance(x1,y1,x2,y2);
  try

    // Try to prevent any floating-point exceptions; are they problematic for Celeron?
    if (d=0.0) then begin
      px:=0;
      py:=0;
      exit;
    end;

    px:=(x2-x1)/d;
    py:=(y2-y1)/d;
  except
    on EInvalidOp do begin
      px:=0;
      py:=0;
      end;
  end;
end;

procedure UnitPerpendicular(x1,y1,x2,y2:double; var px,py:double);
var d:double;
begin
  d:=Distance(x1,y1,x2,y2);
  try

    // Try to prevent any floating-point exceptions; are they problematic for Celeron?
    if (d=0.0) then begin
      px:=0;
      py:=0;
      exit;
    end;

    px:=(y2-y1)/d;
    py:=(x1-x2)/d;
  except
    on EInvalidOp do begin
      px:=0;
      py:=0;
      end;
  end;
end;

{ Returns true if the lines intersect; false if the lines are parallel }
function IntersectLine(const p1,p2:CoordPoint; const p3,p4:CoordPoint; var isect:CoordPoint):IntersectType;
var m1,b1,m2,b2:double;
    p1p2vertical,p3p4vertical:boolean;
    ta,tb:double;
begin
  Result:=NoIntersect;

  try
    p1p2vertical := (p1.x=p2.x);
    p3p4vertical := (p3.x=p4.x);

    { If both lines are vertical, they won't intersect }
    if p1p2vertical and p3p4vertical then exit;

    if p1p2vertical then begin
      m2:=(p4.y-p3.y)/(p4.x-p3.x);
      b2:=p4.y - m2*p4.x;

      isect.x := p1.x;
      isect.y := m2*isect.x + b2;
      end
    else if p3p4vertical then begin
      m1:=(p2.y-p1.y)/(p2.x-p1.x);
      b1:=p2.y - m1*p2.x;

      isect.x := p3.x;
      isect.y := m1*isect.x + b1;
      end
    else begin
      m1:=(p2.y-p1.y)/(p2.x-p1.x);
      b1:=p2.y - m1*p2.x;

      m2:=(p4.y-p3.y)/(p4.x-p3.x);
      b2:=p4.y - m2*p4.x;

      // JD 10-16-02: needed to avoid a divide by zero

      If M1 <> M2 Then
      Begin
        isect.x := (b2-b1)/(m1-m2);
        isect.y := m1*isect.x + b1;
      End
      Else Exit; // The lines are the same slope; punt for now
    end;

   { Figure out if the intersection is on the lines, or is off the end
     of either of the lines }
   if (p2.x<>p1.x) then
     ta := (isect.x-p1.x)/(p2.x-p1.x)
   else if P2.Y <> P1.Y Then
     ta := (isect.y-p1.y)/(p2.y-p1.y)
   Else ta := 0;

   if (p3.x<>p4.x) then
     tb := (isect.x-p3.x)/(p4.x-p3.x)
   else If P4.Y <> P3.Y Then
     tb := (isect.y-p3.y)/(p4.y-p3.y)
   Else tb := 0;  


   if (ta>=0.0) and (ta<=1.0) and (tb>=0.0) and (tb<=1.0) then
     Result := IntersectOnLine
   else begin
     if (ta<0.0) then
       Result := IntersectOffLineBegin
     else if (ta>1.0) then
       Result := IntersectOffLineEnd
     else
       Result := IntersectOffLine;
     end;

  except
    { If we divide by 0, the slopes are equal, and the lines don't intersect. }
    Result:=NoIntersect;
  end;
end;

function DistanceToSegment(const p:CoordPoint; p1,p2:CoordPoint):double;
var m,b:double;
    im,ib:double;
    ix,iy:Coord;
    t:double;
    horz,vert:boolean;
begin
  horz:=(p1.y=p2.y);   vert:=(p1.x=p2.x);

  if horz and vert then begin  // A point (fractal can generate these)
     Result:= Distance(p.x, p.y, p1.x, p1.y);
     exit;
     end
  else if vert then begin   // A vertical line
    ix := p1.x;
    iy := p.y;
    end
  else if horz then begin    // A horizontal line
    ix := p.x;
    iy := p1.y;
    end
  else begin                        // A normal line
    m := (p2.y-p1.y)/(p2.x-p1.x);
    b := p1.y -  m*p1.x;
    im := -1/m;
    ib:= p.y  - im*p.x;
    ix:= (ib-b)/(m-im);
    iy:= im*ix + ib;
    end;
  // We now have the intersect point on the line.
  // See if it's off the end.  If so, we use the distance
  // to the endpoint.  If not, we use the distance from
  // the intersection to the point.
   if (p2.x<>p1.x) then
     t := (ix-p1.x)/(p2.x-p1.x)
   else
     t := (iy-p1.y)/(p2.y-p1.y);

   if (t<0.0) then
     Result:= Distance(p.x, p.y, p1.x, p1.y)
   else if (t>1.0) then
     Result:= Distance(p.x, p.y, p2.x, p2.y)
   else
     Result:= Distance(p.x, p.y, ix, iy);
end;

{ ClipLineInsideRect:
   Returns true if the line has survived the clip.  p1 and p2 are
      set to a line that does not contain any portion within the clip rect.
   Returns false if the line has been completely removed by the clip
     (i.e. the line is completely within the rectangle.)

  The basic algorithm is this:
    Compute the intersection between the line being tested and each edge
    of the clip rectangle.

    If there is an intersection, first test to see if the intersection is
    on the line (as opposed to the intersection occurring off the edge of the line).

    If it is on the line, then depending on which endpoint is already within
    the rectangle, truncate the line by making the other endpoint the intersection.
}
(*function ClipLineInsideRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
var isect:CoordPoint;
    line:CoordRect;
    p1_inside,p2_inside:boolean;
begin
  Result:=true;
  CorrectCoordRect(r);
  line:=MakeCoordRect(p1.x,p1.y,p2.x,p2.y);
  CorrectCoordRect(line);

  p1_inside := PtInCoordRect(r, p1);
  p2_inside := PtInCoordRect(r, p2);

  { Is the line completely within the clip area ?  If so, exit and tell the user
    that the line has been removed }
  if p1_inside and p2_inside then begin
    Result:=false;
    exit;
    end;

  { Top edge }
  if IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Right,r.Top), isect) then begin
    if PtInCoordRect(line, isect) then begin
      if p1_inside then p1 := isect else p2 := isect;
      line:=MakeCoordRect(p1.x,p1.y,p2.x,p2.y);
      CorrectCoordRect(line);
      end;
    end;

  { Left edge }
  if IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Left,r.Bottom), isect) then begin
    if PtInCoordRect(line, isect) then begin
      if p1_inside then p1 := isect else p2 := isect;
      line:=MakeCoordRect(p1.x,p1.y,p2.x,p2.y);
      CorrectCoordRect(line);
      end;
    end;

  { Right edge }
  if IntersectLine(p1,p2, MakeCoordPoint(r.Right,r.Top),r.BottomRight, isect) then begin
    if PtInCoordRect(line, isect) then begin
      if p1_inside then p1 := isect else p2 := isect;
      line:=MakeCoordRect(p1.x,p1.y,p2.x,p2.y);
      CorrectCoordRect(line);
      end;
    end;

  { Bottom edge }
  if IntersectLine(p1,p2, MakeCoordPoint(r.Left,r.Bottom),r.BottomRight, isect) then begin
    if PtInCoordRect(line, isect) then begin
      if p1_inside then p1 := isect else p2 := isect;
      end;
    end;
end;
*)


{ CropLineOutsideRect (opposite of ClipLineInsideRect):
   Returns true if the line has survived the crop.  p1 and p2 are
      set to a line that is completely within the crop rect.
   Returns false if the line has been completely removed by the crop
     (i.e. the line is outside of the crop rectangle.)

  The basic algorithm is this exactly the same as ClipLineInsideRect, except that
  the decision of which portion of the line to throw away is reversed.
}
function CropLineOutsideRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
var isect:CoordPoint;
begin
  Result:=false;
  CorrectCoordRect(r);

  { If the crop rectangle is empty, the line is removed }
  if (r.left=r.right) or (r.top=r.bottom) then exit;

  { If the line is outside the crop rectangle, then remove it entirely. }
  if not VisibleWithin(MakeCoordRect(p1.x,p1.y,p2.x,p2.y),r) then exit;

  { If the line is completely inside the crop rectangle, then no work needs to be done. }
  if PtInCoordRect(r, p1) and PtInCoordRect(r, p2) then begin
    Result:=true; exit;
    end;

  { Top edge }
  if IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Right,r.Top), isect) = IntersectOnLine then begin
    if p1.y<p2.y then p1 := isect else p2 := isect;
    Result:=true;
    end;

  { Left edge }
  if IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Left,r.Bottom), isect) = IntersectOnLine then begin
    if p1.x<p2.x then p1 := isect else p2 := isect;
    Result:=true;
    end;

  { Right edge }
  if IntersectLine(p1,p2, MakeCoordPoint(r.Right,r.Top),r.BottomRight, isect) = IntersectOnLine then begin
    if p1.x>p2.x then p1 := isect else p2 := isect;
    Result:=true;
    end;

  { Bottom edge }
  if IntersectLine(p1,p2, MakeCoordPoint(r.Left,r.Bottom),r.BottomRight, isect) = IntersectOnLine then begin
    if p1.y>p2.y then p1 := isect else p2 := isect;
    Result:=true;
    end;
end;

{ ExtendLineToRect:
   Makes the line in question extend to the edges of the given rectangle.
}
function ExtendLineToRect(var p1,p2:CoordPoint; r:CoordRect):boolean;
var isect:CoordPoint;
    it:IntersectType;

  procedure ExtendPoint;
  begin
    case it of
    IntersectOffLineBegin: begin
      p1 := isect;
      ExtendLineToRect:=true;
      end;
    IntersectOffLineEnd: begin
      p2 := isect;
      ExtendLineToRect:=true;
      end;
    end;
  end;

begin
  Result:=false;

  { If the crop rectangle is empty, the line is removed }
  if (r.left=r.right) or (r.top=r.bottom) then exit;

  CorrectCoordRect(r);

  { Top edge }
  it:=IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Right,r.Top), isect);
  ExtendPoint;

  { Left edge }
  it:=IntersectLine(p1,p2, r.TopLeft, MakeCoordPoint(r.Left,r.Bottom), isect);
  ExtendPoint;

  { Right edge }
  it:=IntersectLine(p1,p2, MakeCoordPoint(r.Right,r.Top),r.BottomRight, isect);
  ExtendPoint;

  { Bottom edge }
  it:=IntersectLine(p1,p2, MakeCoordPoint(r.Left,r.Bottom),r.BottomRight, isect);
  ExtendPoint;
end;

{ CCW (CounterClockWise)
  Determines with three points if going from the first to second to third
  travels in a counterclockwise direction.
  Returns 1 if movement is counterclockwise, 1 if not
}
function CCW(const p0, p1, p2:CoordPoint):integer;
var dx1,dx2,dy1,dy2:Coord;
begin
  dx1 := p1.x-p0.x;
  dy1 := p1.y-p0.y;
  dx2 := p2.x-p0.x;
  dy2 := p2.y-p0.y;

  // This is basically a slope comparison: we don't divide to prevent
  // having to worry about divide by 0 on pure horizontal/vertical lines.

  if (dx1*dy2 > dy1*dx2) then
    CCW := 1
  else
    CCW := -1;
end;

{
  Finds if two lines intersect, and is faster than the
  version that actually returns the intersection point.
}
function Intersection(const p1,p2:CoordPoint; const p3,p4:CoordPoint):boolean;
begin
  Intersection:=((CCW(p1,p2,p3)*CCW(p1,p2,p4)) <= 0) and
                ((CCW(p3,p4,p1)*CCW(p3,p4,p2)) <= 0);
end;

{ Returns if the point is inside the polygon.  Uses the classic method of
  hit testing by making a horizontal ray from the point, and counting the
  intersections.  An even count is outside, and odd count is inside.
}
function PointInPolygon(const p:CoordPoint; polyextent:CoordRect; poly:PCoordArray; polycount:integer):boolean;
var p2:CoordPoint;
    i:integer;
    intersections:integer;
begin
  Result:=false;
  if (polycount<2) then exit;
  if not PtInCoordRect(polyextent,p) then exit;

  p2:=p;
  p2.x := polyextent.right + 1000;

  intersections:= 0;

  for i:=0 to polycount-2 do begin
    if Intersection(p,p2,poly^[i],poly^[i+1]) then inc(intersections);
    end;

  Result:=odd(intersections);
end;

Function NearestIntersection(Segments: Array Of CoordPoint; NumSegments: Array Of Integer;
                             Var ISect: CoordPoint): Boolean;
Var
  I,J : Integer;
  Bin : TLineBin;

Begin
  Bin := TLineBin.Create;
  J   := 0;
  For I := 0 To High(NumSegments) Do
  Begin
    Bin.Add(Segments,J,J + NumSegments[I] - 1);
    Inc(J,NumSegments[I]);
  End; // For I
  Bin.Split;
  Result := Bin.GetClosestIntersection(ISect);
End; // NearestIntersection

// TLineBin

Constructor TLineBin.Create;
Begin
  PointCount := 0;
  NumCount   := 0;
  Child      := Nil;
  SetLength(Points,0);
  SetLength(IStart,0);
  SetLength(IEnd,0);
End; // TLineBin.Create

Destructor TLineBin.Destroy;
Begin
  Child.Free;
  SetLength(Points,0);
  SetLength(IStart,0);
  SetLength(IEnd,0);
End; // TLineBin.Destroy

Procedure TLineBin.Add(Var SourcePoints: Array Of CoordPoint; StartIndex,EndIndex: Integer);
Var I,J: Integer;
Begin
  J := PointCount;
  For I := StartIndex To EndIndex Do
  Begin
    If J > High(Points) Then SetLength(Points,High(Points) + ArrayInc + 1);
    Points[J] := SourcePoints[I];
    Inc(J);
  End; // For I
  If NumCount > High(IStart) Then
  Begin
    SetLength(IStart,High(IStart) + ArrayInc + 1);
    SetLength(IEnd,High(IEnd) + ArrayInc + 1);
  End;
  IStart[NumCount] := PointCount;
  IEnd[NumCount]   := IStart[NumCount] + (EndIndex - StartIndex);
  Inc(PointCount,(EndIndex - StartIndex) + 1);
  Inc(NumCount);
End; // TLineBin.Add

Procedure TLineBin.AddSegment(X1,Y1,X2,Y2: Coord; New: Boolean);
Begin
  If PointCount + 1 > High(Points) Then SetLength(Points,High(Points) + ArrayInc + 2);
  Points[PointCount].X     := X1;
  Points[PointCount].Y     := Y1;
  Points[PointCount + 1].X := X2;
  Points[PointCount + 1].Y := Y2;
  If New Or (NumCount = 0) Then
  Begin
    If NumCount > High(IStart) Then
    Begin
      SetLength(IStart,High(IStart) + ArrayInc + 1);
      SetLength(IEnd,High(IEnd) + ArrayInc + 1);
    End;
    IStart[NumCount] := PointCount;
    IEnd[NumCount]   := IStart[NumCount] + 1;
    Inc(NumCount);
  End
  Else Inc(IEnd[NumCount - 1],2);
  Inc(PointCount,2);
End; // TLineBin.AddSegment

Procedure TLineBin.Remove(StartIndex,EndIndex: Integer);
Var I,J,K: Integer;
Begin
  K := EndIndex - StartIndex + 1;
  For I := EndIndex + 1 To PointCount - 1 Do
   Points[StartIndex + I - (EndIndex + 1)] := Points[I];
  For I := 0 To NumCount - 1 Do
  Begin
    If IStart[I] >  StartIndex Then Dec(IStart[I],K);
    If IEnd[I]   >= EndIndex   Then Dec(IEnd[I],K);
  End; // For I
  I := 0;
  While I < NumCount Do
  Begin
    If IEnd[I] <= 0 Then
    Begin
      For J := I To NumCount - 2 Do
      Begin
        IStart[J] := IStart[J + 1];
        IEnd[J]   := IEnd[J + 1];
      End; // For J
      Dec(NumCount);
    End
    Else Inc(I);
  End; // While
  Dec(PointCount,K);
End; // TLineBin.Remove

Procedure TLineBin.Split;
Var
  I,J   : Integer;
  X1,Y1 : Coord;
  X2,Y2 : Coord;
  C,C1  : Coord;
  P     : CoordPoint;
  Num   : Integer;
  Last  : Integer;

Begin
  // Make sure there are at least two segments

  If (Child = Nil) And (PointCount > 1) Then
  Begin
    X1 := Points[0].X;
    Y1 := Points[0].Y;
    X2 := Points[0].X;
    Y2 := Points[0].Y;
    For I := 1 To PointCount - 1 Do
    Begin
      If Points[I].X < X1 Then X1 := Points[I].X;
      If Points[I].Y < Y1 Then Y1 := Points[I].Y;
      If Points[I].X > X2 Then X2 := Points[I].X;
      If Points[I].Y > Y2 Then Y2 := Points[I].Y;
    End; // For I

    // Simple split for now, right down the middle of the longer distance.
    // Find out if we can get rid of any segments, and do the split if so.

    J := 0;
    If X2 - X1 > Y2 - Y1 Then
    Begin
      C := (X1 + X2) / 2;
      I := 0;
      While I < PointCount Do
      Begin
        If (Points[I].X >= C) And (Points[I + 1].X >= C) Then Inc(J,2);
        Inc(I,2);
      End; // While
      If (J > 0) And (J < PointCount) Then
      Begin
        Child := TLineBin.Create;
        I     := 0;
        Last  := -1;
        Num   := 0;
        While I < PointCount Do
        Begin
          // Sort

          If Points[I].X > Points[I + 1].X Then
          Begin
            P             := Points[I];
            Points[I]     := Points[I + 1];
            Points[I + 1] := P;
          End;

          // Move the segment to the child?

          If (Points[I].X >= C) And (Points[I + 1].X >= C) Then
          Begin
            Child.AddSegment(Points[I].X,Points[I].Y,Points[I + 1].X,Points[I + 1].Y,I > Last);
            Remove(I,I + 1);
            Dec(Last,2);
            While (I > Last) And (Num < NumCount) Do
            Begin
              Last := IEnd[Num];
              Inc(Num);
            End; // While
          End
          Else If (Points[I].X < C) And (Points[I + 1].X > C) Then  // Split the segment?
          Begin
            C1 := Points[I].Y + (Points[I + 1].Y - Points[I].Y) * (C - Points[I].X) / (Points[I + 1].X - Points[I].X);
            Child.AddSegment(C,C1,Points[I + 1].X,Points[I + 1].Y,I > Last);
            Points[I + 1].X := C;
            Points[I + 1].Y := C1;
            While (I > Last) And (Num < NumCount) Do
            Begin
              Last := IEnd[Num];
              Inc(Num);
            End; // While
            Inc(I,2);
          End
          Else Inc(I,2); // Keep the segment
        End; // While
      End;
    End
    Else
    Begin
      C := (Y1 + Y2) / 2;
      I := 0;
      While I < PointCount Do
      Begin
        If (Points[I].Y >= C) And (Points[I + 1].Y >= C) Then Inc(J,2);
        Inc(I,2);
      End; // While
      If (J > 0) And (J < PointCount) Then
      Begin
        Child := TLineBin.Create;
        I     := 0;
        Last  := -1;
        Num   := 0;
        While I < PointCount Do
        Begin
          // Sort

          If Points[I].Y > Points[I + 1].Y Then
          Begin
            P             := Points[I];
            Points[I]     := Points[I + 1];
            Points[I + 1] := P;
          End;

          // Move the segment to the child?

          If (Points[I].Y >= C) And (Points[I + 1].Y >= C) Then
          Begin
            Child.AddSegment(Points[I].X,Points[I].Y,Points[I + 1].X,Points[I + 1].Y,I > Last);
            Remove(I,I + 1);
            Dec(Last,2);
            While (I > Last) And (Num < NumCount) Do
            Begin
              Last := IEnd[Num];
              Inc(Num);
            End; // While
          End
          Else If (Points[I].Y < C) And (Points[I + 1].Y > C) Then  // Split the segment?
          Begin
            C1 := Points[I].X + (Points[I + 1].X - Points[I].X) * (C - Points[I].Y) / (Points[I + 1].Y - Points[I].Y);
            Child.AddSegment(C1,C,Points[I + 1].X,Points[I + 1].Y,I > Last);
            Points[I + 1].X := C1;
            Points[I + 1].Y := C;
            While (I > Last) And (Num < NumCount) Do
            Begin
              Last := IEnd[Num];
              Inc(Num);
            End; // While
            Inc(I,2);
          End
          Else Inc(I,2); // Keep the segment
        End; // While
      End;
    End;
    If Child <> Nil Then Child.Split;
  End;
End; // TLineBin.Split

Function TLineBin.GetClosestIntersection(Var P: CoordPoint): Boolean;
Var
  P1,P2    : CoordPoint;
  I,J,K,L  : Integer;
  Found    : Boolean;
  Dist     : Double;
  D1       : Double;
  IMax     : Integer;
  JMax     : Integer;

Begin
  Found := False;
  Dist  := 0;

  If NumCount >= 2 Then
  Begin
    IMax := NumCount - 2;
    JMax := IMax + 1;
  End
  Else
  Begin
    IMax := 0;
    JMax := 0;
  End;

  For I := 0 To IMax Do
  Begin
    For J := I To JMax Do
    Begin
      K := IStart[I];
      While K < IEnd[I] Do
      Begin
        L := IStart[J];
        While L < IEnd[J] Do
        Begin
          If (J <> I) Or (L > K + 2) Or (K > L + 2) Then
          Begin
            If IntersectLine(Points[K],Points[K + 1],
                             Points[L],Points[L + 1],P2) = IntersectOnLine Then
            Begin
              D1 := Distance(P.X,P.Y,P2.X,P2.Y);
              If Not Found Then
              Begin
                Found := True;
                Dist  := D1;
                P1    := P2;
              End
              Else
              Begin
                If D1 < Dist Then
                Begin
                  Dist := D1;
                  P1   := P2;
                End;
              End;
            End;
          End;
          Inc(L,2);
        End; // While
        Inc(K,2);
      End; // While
    End; // For J
  End; // For I

  If Child <> Nil Then
  Begin
    P2 := P;
    If Child.GetClosestIntersection(P2) Then
    Begin
      D1 := Distance(P.X,P.Y,P2.X,P2.Y);
      If Not Found Then
      Begin
        Found := True;
        P1    := P2;
      End
      Else
      Begin
        If D1 <= Dist Then
        Begin
          P1   := P2;
        End;
      End;
    End;
  End;

  Result := Found;
  If Found Then P := P1;
End; // TLineBin.GetClosestIntersection

end.

