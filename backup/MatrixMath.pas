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
unit MatrixMath;

interface

uses Geometry;

type Matrix=array[1..3,1..3] of double;

function  MakeMatrix(c11,c12,c13,
                     c21,c22,c23,
                     c31,c32,c33:double):Matrix;
procedure MultiplyPointByMatrix(var x,y:Coord; Mat:Matrix);
function  MatrixMultiply(MatA,MatB:Matrix):Matrix;
procedure MatrixMultiplyBy(var MatA:Matrix; MatB:Matrix);
Function  MatrixEquals(Mat1,Mat2: Matrix): Boolean;

function  RotationMatrix(degrees:double):Matrix;
function  ScaleMatrix(xfactor,yfactor:double):Matrix;
function  OffsetMatrix(dx,dy:double):Matrix;
function  SkewMatrix(sx,sy:double):Matrix;
function  FlipMatrix(xaxis,yaxis:boolean):Matrix;
Function  IsPureOffsetMatrix(Mat: Matrix): Boolean;

implementation

uses Math;

Function MatrixEquals(Mat1,Mat2: Matrix): Boolean;
Begin
  Result := (Mat1[1,1] = Mat2[1,1]) And
            (Mat1[1,2] = Mat2[1,2]) And
            (Mat1[1,3] = Mat2[1,3]) And
            (Mat1[2,1] = Mat2[2,1]) And
            (Mat1[2,2] = Mat2[2,2]) And
            (Mat1[2,3] = Mat2[2,3]) And
            (Mat1[3,1] = Mat2[3,1]) And
            (Mat1[3,2] = Mat2[3,2]) And
            (Mat1[3,3] = Mat2[3,3]);
End; // MatrixEquals

function MakeMatrix(c11,c12,c13,
                    c21,c22,c23,
                    c31,c32,c33:double):Matrix;
begin
  Result[1,1]:=c11;  Result[1,2]:=c12;  Result[1,3]:=c13;
  Result[2,1]:=c21;  Result[2,2]:=c22;  Result[2,3]:=c23;
  Result[3,1]:=c31;  Result[3,2]:=c32;  Result[3,3]:=c33;
end;

procedure MultiplyPointByMatrix(var x,y:Coord; Mat:Matrix);
{ Note: Makes the assumption that since the matrix is homogeneous
  matrix, the last column is 0, 0, 1}
var x1,y1:Coord;
begin
  try
    x1 := x*Mat[1,1] + y*Mat[2,1] + Mat[3,1];
    y1 := x*Mat[1,2] + y*Mat[2,2] + Mat[3,2];
    x:=x1;
    y:=y1;
  except else
  end;
end;

function MatrixMultiply(MatA,MatB:Matrix):Matrix;
var row,col:Integer;
begin
  for col:=1 to 3 do
    for row:=1 to 3 do
      Result[col,row]:=MatA[col,1]*MatB[1,row] +
                       MatA[col,2]*MatB[2,row] +
                       MatA[col,3]*MatB[3,row];
end;

procedure MatrixMultiplyBy(var MatA:Matrix; MatB:Matrix);
begin
  MatA:=MatrixMultiply(MatA,MatB);
end;

function RotationMatrix(degrees:double):Matrix;
var t:double;
begin
  t:=DegToRad(-degrees);
  Result:=MakeMatrix(cos(t),  sin(t), 0,
                     -sin(t), cos(t), 0,
                     0,       0,      1);
end;

function ScaleMatrix(xfactor,yfactor:double):Matrix;
begin
  Result:=MakeMatrix(xfactor, 0,       0,
                     0,       yfactor, 0,
                     0,       0,       1);
end;

Function IsPureOffsetMatrix(Mat: Matrix): Boolean;
// Does this matrix ONLY move something, or does do anything else (e.g. rotate)?
Begin
  Result := MatrixEquals(Mat,OffsetMatrix(Mat[3,1],Mat[3,2]));
End;

function OffsetMatrix(dx,dy:double):Matrix;
begin
  Result:=MakeMatrix(1,  0,  0,
                     0,  1,  0,
                     dx, dy, 1);
end;

function SkewMatrix(sx,sy:double):Matrix;
begin
  Result:=MakeMatrix(1,  sy, 0,
                     sx, 1,  0,
                     0,  0,  1);
end;

function FlipMatrix(xaxis,yaxis:boolean):Matrix;
var sx,sy:double;
begin
  if xaxis then sx:=-1 else sx:=1;
  if yaxis then sy:=-1 else sy:=1;

  Result:=ScaleMatrix(sx,sy);
end;

end.
