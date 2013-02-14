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
unit DrawLines;

interface

uses SysUtils, Windows, Classes, Graphics, Geometry, Logging;

type
     TFullStyle = Packed Record
       Bits       : Cardinal;
       Thickness  : Coord;
       SThickness : Coord; // Never saved to disk: calculated on the fly
     End;
     // StyleAttrib = line style.  We use four sections at a byte a piece to
     // specify the style of the Line, the Fill style (if it is closed),
     // and the styles of the line tips on either end.
     StyleAttrib = packed record
       case Integer of
         0: (Line,Fill,First,Last:byte);
         1: (Bits:Cardinal);
         2: (FullStyle : TFullStyle);
       end;

     // Structure used for drawing lines.  One of these must be filled out
     // before drawing the line by calling GetLineStyleStart.
     TLineContinue=record
        Canvas:TCanvas;
        Style:StyleAttrib;
        Points:PCoordArray;
        Count:integer;
        Allocated:integer;
        Bitmask:integer;

        First:boolean;
        p1,p2:CoordPoint;

        dx,dy:integer;
        Start,Width:integer;
        Slice,NumSlices:integer;
        NoPutPixel:boolean;
        InvertLine:integer;             // 1 if normal, -1 if inverted
     end;

procedure Marquis(Canvas:TCanvas; x1,y1,x2,y2:integer);

procedure GetFractalBox(x1,y1,x2,y2:coord; rfact:double; var b:CoordRect);
procedure FractalLine(Canvas:TCanvas; x1,y1,x2,y2:coord; rfact:double; var continue:TLineContinue);
procedure FractalSetSeed(n:integer);
function  GetNumberLineStyles:integer;
function  GetNumberLineEndStyles:integer;
function  GetLineStyleStart(style:StyleAttrib):TLineContinue;
procedure DrawLineContinue(Canvas:TCanvas; x1,y1,x2,y2:Coord; var continue:TLineContinue);
function GetLineEnd(var continue:TLineContinue; var count:integer):PCoordArray;
procedure DrawLineStyle(Canvas:TCanvas; x1,y1,x2,y2:integer; style:StyleAttrib);
procedure DrawBezier(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; var continue:TLineContinue);
procedure DrawFractalBezier(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; rfact:double; var continue:TLineContinue);
procedure DrawArc(Canvas: TCanvas; P1,P2,P3: CoordPoint; Var Continue: TLineContinue);
Procedure GetArcCenter(P1,P2,P3: CoordPoint; Var X1,Y1: Single);
procedure GetBezierBox(p1,p2,p3,p4:CoordPoint; var b:CoordRect);
procedure GetFractalBezierBox(p1,p2,p3,p4:CoordPoint; rfact:double; var b:CoordRect);
function GetLineThickness(style:StyleAttrib):integer;
function IsAComplexLine(style:StyleAttrib):boolean;
function FixedRandom(n:integer):integer; // JD 9-11-02
function InvertLineStyle(style:StyleAttrib):StyleAttrib;        // AJG 1-29-03
function IsLineStyleInverted(style:StyleAttrib):boolean;        // AJG 1-29-03


// This special style indicates that we should just segment the line and
// return an array of the actual segments being drawn (used in decompose
// and related operations).
const
//  SEGMENT_STYLE:StyleAttrib=(Bits:$FFFFFFFF);
  SEGMENT_STYLE: StyleAttrib = (FullStyle:(Bits:$FFFFFFFF; Thickness:-1; SThickness:0));
  MaxRand=256*1024; // JD 9-11-02

  // Lines styles are all hardcoded: these are the number of each type.
  //
  // NOTE: If you want to add lines, you must create a new group and
  // add those lines to the end.  If you don't do this, you'll end up
  // making your files incompatible with other AutoREALM users.  Sorry.
  number_thick_styles             = 6;
  number_dithered_styles          = 9;
  number_glyph_styles             = 41;
  number_caligraphy_styles        = 12;
  number_random_styles            = 60;
  number_numeric_thickness_styles = 1;

  number_lineends                 = 20;

  // start of each style type in the total list.
  start_thick_style             = 1;
  start_dithered_style          = start_thick_style      + number_thick_styles;
  start_glyph_style             = start_dithered_style   + number_dithered_styles;
  start_caligraphy_style        = start_glyph_style      + number_glyph_styles;
  start_random_style            = start_caligraphy_style + number_caligraphy_styles;
  start_numeric_thickness_style = start_random_style     + number_random_styles;

implementation

uses Main,Printers,Math;

type GlyphData=record                    // Constant data struct for glyph line styles
       Width,Count,Start:integer;
     end;
     EndPointData=record                 // Constant data struct for line tips
       Width,Count,Start,Depth:integer;
     end;
     RandomData=record                   // Constant data struct for specifying random lines
       PatternWidth,DotCount,DotWidth,DotType:integer;
     end;

const
  AllocationGrowth = 200;      // How large we grow SEGMENT_STYLE allocations at a gulp
  fractal_depth = 16;          // number times we recurse the fractals for bounding box computations

  dashdot:array [0 .. number_dithered_styles-1] of Cardinal=(
   $55555555,       // .O.O-X-X.O.O-X-X.O.O-X-X.O.O-X-X
   $FFC3C3FF,       // OOOOXXXXOO..--XXOO..--XXOOOOXXXX
   $FF1E3CFF,       // OOOOXXXX...OXXX-..OOXX--OOOOXXXX
   $FFF3FFF3,       // OOOOXXXXOOOO--XXOOOOXXXXOOOO--XX
   $F99F33CC,       // OOOOX--XO..OXXXX..OO--XXOO..XX--
   $0F0F0F0F,       // ....XXXX....XXXX....XXXX....XXXX
   $11111111,       // ...O---X...O---X...O---X...O---X
   $10101010,       // ...O----...O----...O----...O----
   $FFFFF924        // OOOOXXXXOOOOXXXXOOOOX--X..O.-X--
   );

   GlyphBits:array[0..530] of Cardinal= (
{0}   $10,          // ---X----
      $20,          // --X-----
      $40,          // -X------
      $20,          // --X-----
      $10,          // ---X----
      $08,          // ----X---
      $04,          // -----X--
      $08,          // ----X---

{8}   $42,          // -X----X-

{9}   $24,          // --X--X--

{10}  $14,          // ---X-X--

{11}  $18,          // ---XX---
      $3C,          // --XXXX--
      $18,          // ---XX---
      $00,          // --------

{15}  $FF,          // XXXXXXXX
      $00,          // --------

{17}  $FF,          // XXXXXXXX
      $00,          // --------
      $00,          // --------
      $00,          // --------

{21}  $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $FE,          // XXXXXXX-
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{30}  $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....
      $3FFC,        // --XXXXXXXXXXXX..
      $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....
      $0420,        // ----.X..--X-....

{39}  $3C,          // --XXXX--
      $00,          // --------

{41}  $3C,          // --XXXX--
      $00,          // --------
      $00,          // --------
      $00,          // --------

{45}  $14,          // ---X-X--
      $14,          // ---X-X--
      $14,          // ---X-X--
      $14,          // ---X-X--
      $00,          // --------
      $00,          // --------

{51}  $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $FE,          // XXXXXXX-
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $00,          // --------
      $00,          // --------

{60}  $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $1E,          // ---XXXX-
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $F0,          // XXXX----
      $10,          // ---X----
      $10,          // ---X----

{72}  $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $1E,          // ---XXXX-
      $10,          // ---X----
      $1E,          // ---XXXX-
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $F0,          // XXXX----
      $10,          // ---X----
      $F0,          // XXXX----

{86}  $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $1E,          // ---XXXX-
      $10,          // ---X----
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $F0,          // XXXX----
      $10,          // ---X----
      $10,          // ---X----

{98}  $0B80,        // ----X.XXX---....
      $0580,        // ----.X.XX---....

{100} $2B80,        // --X-X.XXX---....
      $5580,        // -X-X.X.XX---....

{102} $10090000,    // ---X....----X..X----....----....
      $00090000,    // ----....----X..X----....----....
      $02490000,    // ----..X.-X--X..X----....----....
      $00410000,    // ----....-X--...X----....----....
      $10090000,    // ---X....----X..X----....----....
      $02090000,    // ----..X.----X..X----....----....
      $00490000,    // ----....-X--X..X----....----....
      $82410000,    // X---..X.-X--...X----....----....
      $00090000,    // ----....----X..X----....----....

{111} $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....
      $12490000,    // ---X..X.-X--X..X----....----....

{120} $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $7C,          // -XXXXX--
      $10,          // ---X----
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{132} $00,          // --------
      $38,          // --XXX---
      $44,          // -X---X--
      $44,          // -X---X--
      $44,          // -X---X--
      $38,          // --XXX---
      $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{144} $38,          // --XXX---
      $44,          // -X---X--
      $44,          // -X---X--
      $44,          // -X---X--
      $38,          // --XXX---
      $00,          // --------

{150} $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $38,          // --XXX---
      $7C,          // -XXXXX--
      $38,          // --XXX---
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{160} $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $38,          // --XXX---
      $7C,          // -XXXXX--
      $38,          // --XXX---
      $10,          // ---X----
      $10,          // ---X----
      $00,          // --------
      $00,          // --------

{170} $7E,          // -XXXXXX-
      $42,          // -X----X-
      $42,          // -X----X-
      $42,          // -X----X-
      $42,          // -X----X-
      $42,          // -X----X-

{176} $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $7C,          // -XXXXX--
      $7C,          // -XXXXX--
      $7C,          // -XXXXX--
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{186} $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $FE,          // XXXXXXX-
      $7C,          // -XXXXX--
      $38,          // --XXX---
      $10,          // ---X----
      $00,          // --------
      $00,          // --------

{196} $7E,          // -XXXXXX-
      $02,          // ------X-
      $02,          // ------X-
      $02,          // ------X-
      $7E,          // -XXXXXX-
      $40,          // -X------
      $40,          // -X------
      $40,          // -X------

{204} $FE,          // XXXXXXX-
      $12,          // ---X--X-
      $12,          // ---X--X-
      $12,          // ---X--X-
      $FE,          // XXXXXXX-
      $90,          // X--X----
      $90,          // X--X----
      $90,          // X--X----

{212} $00,          // --------
      $38,          // --XXX---
      $7C,          // -XXXXX--
      $38,          // --XXX---
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------
      $10,          // ---X----
      $00,          // --------

{233} $FF,          // XXXXXXXX
      $01,          // -------X
      $01,          // -------X
      $07,          // -----XXX
      $01,          // -------X
      $01,          // -------X
      $1F,          // ---XXXXX
      $01,          // -------X
      $01,          // -------X
      $07,          // -----XXX
      $01,          // -------X
      $01,          // -------X
      $7F,          // -XXXXXXX
      $01,          // -------X
      $01,          // -------X
      $07,          // -----XXX
      $01,          // -------X
      $01,          // -------X
      $1F,          // ---XXXXX
      $01,          // -------X
      $01,          // -------X
      $07,          // -----XXX
      $01,          // -------X
      $01,          // -------X

{257} $1F,          // ---XXXXX
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----
      $10,          // ---X----

{262} $00,          // --------
      $00,          // --------
      $10,          // ---X----
      $10,          // ---X----
      $1F,          // ---XXXXX
      $10,          // ---X----
      $10,          // ---X----
      $00,          // --------
      $00,          // --------

{271} $00000C00,    // ----....----....----OO..----....
      $0000DC00,    // ----....----....XX-XOO..----....
      $000DFC00,    // ----....----OO.OXXXXOO..----....
      $001FFC00,    // ----....---XOOOOXXXXOO..----....
      $007FFC00,    // ----....-XXXOOOOXXXXOO..----....
      $01FFFFC0,    // ----...OXXXXOOOOXXXXOOOOXX--....
      $007FFC00,    // ----....-XXXOOOOXXXXOO..----....
      $001FFC00,    // ----....---XOOOOXXXXOO..----....
      $000DFC00,    // ----....----OO.OXXXXOO..----....
      $0000DC00,    // ----....----....XX-XOO..----....
      $00000C00,    // ----....----....----OO..----....
      $00000000,    // ----....----....----....----....
      $00000000,    // ----....----....----....----....
      $00000000,    // ----....----....----....----....
      $00000000,    // ----....----....----....----....

{286} $3C,          // --XXXX--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $24,          // --X--X--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--
      $3C,          // --XXXX--

{304} $FE,          // XXXXXXX-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $92,          // X--X--X-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-
      $EE,          // XXX-XXX-

{322} $92,          // X--X--X-

{323} $000C0000,    // ----....----XX..----....----....
      $00070000,    // ----....----.XXX----....----....
      $0001C000,    // ----....----...XXX--....----....
      $00007000,    // ----....----....-XXX....----....
      $00001C00,    // ----....----....---XXX..----....
      $00007000,    // ----....----....-XXX....----....
      $0063C000,    // ----....-XX-..XXXX--....----....
      $00300000,    // ----....--XX....----....----....
      $000E0000,    // ----....----XXX.----....----....
      $00038000,    // ----....----..XXX---....----....
      $00070000,    // ----....----.XXX----....----....
      $001C1000,    // ----....---XXX..---X....----....
      $00701800,    // ----....-XXX....---XX...----....
      $00C00C00,    // ----....XX--....----XX..----....
      $00060600,    // ----....----.XX.----.XX.----....
      $00038700,    // ----....----..XXX---.XXX----....
      $0000DC00,    // ----....----....XX-XXX..----....
      $00007000,    // ----....----....-XXX....----....
      $00060000,    // ----....----.XX.----....----....
      $03030000,    // ----..XX----..XX----....----....
      $01838000,    // ----...XX---..XXX---....----....
      $00CE0000,    // ----....XX--XXX.----....----....
      $00387000,    // ----....--XXX...-XXX....----....
      $0003D800,    // ----....----..XXXX-XX...----....
      $000E0C00,    // ----....----XXX.----XX..----....
      $00180600,    // ----....---XX...----.XX.----....
      $00000700,    // ----....----....----.XXX----....
      $00001C00,    // ----....----....---XXX..----....
      $00003000,    // ----....----....--XX....----....
      $00001800,    // ----....----....---XX...----....
      $00070C00,    // ----....----XXX.----XX..----....
      $00039800,    // ----....----..XXX--XX...----....
      $0000F000,    // ----....----....XXXX....----....
      $00000000,    // ----....----....----....----....
      $00301800,    // ----....--XX....---XX...----....
      $001C3000,    // ----....---XXX..--XX....----....
      $00076000,    // ----....----.XXX-XX-....----....
      $0001C000,    // ----....----...XXX--....----....
      $00008000,    // ----....----....X---....----....
      $00000000,    // ----....----....----....----....
      $00060000,    // ----....----.XX.----....----....
      $00038000,    // ----....----..XXX---....----....
      $0000C000,    // ----....----....XX--....----....
      $00007000,    // ----....----....-XXX....----....
      $0C001800,    // ----XX..----X...---XX...----....
      $063C3000,    // ----.XX.--XXXX..--XX....----....
      $03676000,    // ----..XX-XX-.XXX-XX-....----....
      $01C1C000,    // ----...XXX--...XXX--....----....
      $00008000,    // ----....----....X---....----....
      $000C0000,    // ----....----XX..----....----....
      $00070000,    // ----....----.XXX----....----....
      $00018000,    // ----....----...XX---....----....
      $000E0000,    // ----....----XXX.----....----....
      $00000000,    // ----....----....----....----....

{377} $70,          // -XXX....
      $F0,          // XXXX....
      $F0,          // XXXX....
      $70,          // -XXX....
      $F0,          // XXXX....

{382} $0100,        // ----...X----....
      $0280,        // ----..X.X---....
      $0440,        // ----.X..-X--....
      $0820,        // ----X...--X-....
      $1010,        // ---X....---X....
      $2008,        // --X-....----X...

{388} $0100,        // ----...O----....
      $0380,        // ----..OOX---....
      $07C0,        // ----.OOOXX--....
      $0FE0,        // ----OOOOXXX-....
      $1FF0,        // ---XOOOOXXXX....
      $3FF8,        // --XXOOOOXXXXO...

{394} $0100,        // ----...O----....
      $0100,        // ----...O----....
      $0380,        // ----..OOX---....
      $0380,        // ----..OOX---....
      $07C0,        // ----.OOOXX--....
      $07C0,        // ----.OOOXX--....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $1FF0,        // ---XOOOOXXXX....
      $1FF0,        // ---XOOOOXXXX....

{404} $07C0,        // ----.OOOXX......

{405} $1FF0,        // ---XOOOOXXXX....

{406} $7FFC,        // -XXXOOOOXXXXOO..

{407} $2008,        // --X-....----O...
      $1010,        // ---X....---X....
      $0820,        // ----O...--X-....
      $0440,        // ----.O..-X--....
      $0280,        // ----..O.X---....
      $0100,        // ----...O----....

{413} $1010,        // ---X....---X....
      $0820,        // ----O...--X-....
      $0440,        // ----.O..-X--....
      $1290,        // ---X..O.X--X....
      $0920,        // ----O..O--X-....
      $0540,        // ----.O.O-X--....
      $1390,        // ---X..OOX--X....
      $0920,        // ----O..O--X-....
      $0540,        // ----.O.O-X--....
      $0380,        // ----..OOX---....

{423} $8002,        // X---....----..O.
      $8002,        // X---....----..O.
      $4004,        // -X--....----.O..
      $3018,        // --XX....---XO...
      $0FE0,        // ----OOOOXXX-....

{428} $0FE0,        // ----OOOOXXX-....
      $3018,        // --XX....---XO...
      $4004,        // -X--....----.O..
      $8002,        // X---....----..O.
      $8002,        // X---....----..O.

{433} $0FE0,        // ----OOOOXXX-....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0FE0,        // ----OOOOXXX-....

{440} $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....

{447} $0380,        // ----..OOX---....
      $0440,        // ----.O..-X--....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $0440,        // ----.O..-X--....
      $0380,        // ----..OOX---....

{454} $0380,        // ----..OOX---....
      $07C0,        // ----.OOOXX--....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $07C0,        // ----.OOOXX--....
      $0380,        // ----..OOX---....

{461} $0100,        // ----...O----....
      $0280,        // ----..O.X---....
      $0440,        // ----.O..-X--....
      $0820,        // ----O...--X-....
      $0440,        // ----.O..-X--....
      $0280,        // ----..O.X---....
      $0100,        // ----...O----....

{468} $0100,        // ----...O----....
      $0380,        // ----..OOX---....
      $07C0,        // ----.OOOXX--....
      $0FE0,        // ----OOOOXXX-....
      $07C0,        // ----.OOOXX--....
      $0380,        // ----..OOX---....
      $0100,        // ----...O----....

{475} $0100,        // ----...O----....
      $0100,        // ----...O----....
      $0280,        // ----..O.X---....
      $0280,        // ----..O.X---....
      $0440,        // ----.O..-X--....
      $0440,        // ----.O..-X--....
      $0820,        // ----O...--X-....
      $0820,        // ----O...--X-....
      $1010,        // ---X....---X....
      $1010,        // ---X....---X....

{485} $1FF0,        // ---XOOOOXXXX....
      $1FF0,        // ---XOOOOXXXX....
      $0FE0,        // ----OOOOXXX-....
      $0FE0,        // ----OOOOXXX-....
      $07C0,        // ----.OOOXX--....
      $07C0,        // ----.OOOXX--....
      $0380,        // ----..OOX---....
      $0380,        // ----..OOX---....
      $0100,        // ----...O----....
      $0100,        // ----...O----....

{495} $3FF8,        // --XXOOOOXXXXO...
      $1FF0,        // ---XOOOOXXXX....
      $0FE0,        // ----OOOOXXX-....
      $07C0,        // ----.OOOXX--....
      $0380,        // ----..OOX---....
      $0100,        // ----...O----....

{501} $00070000,    // ----....----.OOO----....----....
      $00180000,    // ----....---XO...----....----....
      $00600000,    // ----....-XX-....----....----....
      $008E0000,    // ----....X---OOO.----....----....
      $00F10000,    // ----....XXXX...O----....----....
      $00060000,    // ----....----.OO.----....----....
      $02380000,    // ----..O.--XXO...----....----....
      $038E0000,    // ----..OOX---OOO.----....----....
      $02710000,    // ----..O.-XXX...O----....----....
      $010F0000,    // ----...O----OOOO----....----....
      $00C00000,    // ----....XX--....----....----....
      $00300000,    // ----....--XX....----....----....
      $000E0000,    // ----....----OOO.----....----....
      $00010000,    // ----....----...O----....----....
      $00070000,    // ----....----.OOO----....----....
      $00180000,    // ----....---XO...----....----....
      $001E0000,    // ----....---XOOO.----....----....
      $06010000,    // ----.OO.----...O----....----....
      $058E0000,    // ----.O.OX---OOO.----....----....
      $04700000,    // ----.O..-XXX....----....----....
      $02000000,    // ----..O.----....----....----....
      $01C00000,    // ----...OXX--....----....----....
      $03380000,    // ----..OO--XXO...----....----....
      $0C040000,    // ----OO..----.O..----....----....
      $09C00000,    // ----O..OXX--....----....----....
      $06300000,    // ----.OO.--XX....----....----....
      $001C0000,    // ----....---XOO..----....----....
      $00600000,    // ----....-XX-....----....----....
      $00860000,    // ----....X---.OO.----....----....
      $00F90000     // ----....XXXXO..O----....----....
{531}
);

    LineEnds:array[0..number_lineends-1] of EndPointData = (
      (Width:0;  Count:0;  Start:0;    Depth:0),    // No ending
      (Width:16; Count:6;  Start:382;  Depth:0),    // Hollow arrow
      (Width:16; Count:6;  Start:388;  Depth:0),    // Filled arrow
      (Width:16; Count:10; Start:475;  Depth:0),    // Narrow Hollow arrow
      (Width:16; Count:10; Start:394;  Depth:0),    // Narrow Filled arrow
      (Width:16; Count:10; Start:413;  Depth:3),    // Arrow shaft
      (Width:16; Count:6;  Start:407;  Depth:5),    // Backwards angle
      (Width:16; Count:6;  Start:495;  Depth:0),    // Backwards Filled arrow
      (Width:16; Count:10; Start:485;  Depth:0),    // Backwards Narrow Filled arrow
      (Width:16; Count:1;  Start:404;  Depth:0),    // Narrow Beam
      (Width:16; Count:1;  Start:405;  Depth:0),    // Med Beam
      (Width:16; Count:1;  Start:406;  Depth:0),    // Wide Beam
      (Width:16; Count:5;  Start:423;  Depth:4),    // Cup
      (Width:16; Count:5;  Start:428;  Depth:0),    // Inverted Cup
      (Width:16; Count:7;  Start:433;  Depth:3),    // Hollow Box
      (Width:16; Count:7;  Start:440;  Depth:3),    // Filled Box
      (Width:16; Count:7;  Start:447;  Depth:3),    // Hollow Circle
      (Width:16; Count:7;  Start:454;  Depth:3),    // Filled Circle
      (Width:16; Count:7;  Start:461;  Depth:3),    // Hollow Diamond
      (Width:16; Count:7;  Start:468;  Depth:3)     // Filled Diamond
    );

    Glyph:array[0..number_glyph_styles-1] of GlyphData = (
      (Width:8;  Count:1;  Start:8),         // Wide double line
      (Width:8;  Count:1;  Start:10),        // Thin double line
      (Width:8;  Count:6;  Start:45),        // Dashed double line
      (Width:8;  Count:8;  Start:0),         // Sawtooth
      (Width:8;  Count:4;  Start:11),        // Balls
      (Width:8;  Count:2;  Start:15),        // Hashes
      (Width:8;  Count:4;  Start:17),        // Spaced Hashes
      (Width:8;  Count:2;  Start:39),        // Thin Hashes
      (Width:8;  Count:4;  Start:41),        // Spaced Thin Hashes
      (Width:8;  Count:12; Start:120),       // +-+-+-+-+-
      (Width:8;  Count:5;  Start:257),       // Solid "T" line
      (Width:8;  Count:5;  Start:377),       // Bark
      (Width:8;  Count:9;  Start:262),       // Dashed "T" line
      (Width:8;  Count:12; Start:132),       // o-o-o-o-o-
      (Width:8;  Count:6;  Start:144),       // oooooooooo
      (Width:8;  Count:6;  Start:170),       // Boxes
      (Width:8;  Count:10; Start:176),       // Box knotted line
      (Width:8;  Count:10; Start:186),       // Triangle knotted line
      (Width:8;  Count:10; Start:150),       // Circle knotted line
      (Width:8;  Count:21; Start:212),       // Telephone line
      (Width:8;  Count:10; Start:160),       // Dashed circle knotted line
      (Width:8;  Count:8;  Start:196),       // Castle wall
      (Width:8;  Count:8;  Start:204),       // Alternating boxes
      (Width:8;  Count:18; Start:286),       // Block Road
      (Width:8;  Count:18; Start:304),       // Double Block Road
      (Width:8;  Count:1;  Start:9),         // Hollow Road
      (Width:8;  Count:1;  Start:322),       // Double Hollow Road
      (Width:8;  Count:9;  Start:21),        // Railroad tracks
      (Width:16; Count:9;  Start:30),        // Double Railroad tracks
      (Width:8;  Count:9;  Start:51),        // Abandoned Railroad tracks
      (Width:8;  Count:12; Start:60),        // Narrow gauge
      (Width:8;  Count:14; Start:72),        // Double narrow gauge
      (Width:8;  Count:12; Start:86),        // Abandoned narrow gauge
      (Width:16; Count:2;  Start:98),        // Thin Shaded
      (Width:16; Count:2;  Start:100),       // Thick Shaded
      (Width:32; Count:9;  Start:102),       // Sparse Lake Border
      (Width:32; Count:9;  Start:111),       // Ribbon Lake Border
      (Width:8;  Count:24; Start:233),       // Ruler
      (Width:32; Count:15; Start:271),       // Timberline
      (Width:32; Count:54; Start:323),       // Packice
      (Width:32; Count:30; Start:501)        // Leaves
    );

    RandomDot:array[0..number_random_styles-1] of RandomData = (
      (PatternWidth:2; DotCount:1; DotWidth:0; DotType:0),
      (PatternWidth:4; DotCount:1; DotWidth:0; DotType:0),
      (PatternWidth:6; DotCount:1; DotWidth:0; DotType:0),
      (PatternWidth:2; DotCount:2; DotWidth:0; DotType:0),
      (PatternWidth:4; DotCount:2; DotWidth:0; DotType:0),
      (PatternWidth:6; DotCount:2; DotWidth:0; DotType:0),
      (PatternWidth:2; DotCount:3; DotWidth:0; DotType:0),
      (PatternWidth:4; DotCount:3; DotWidth:0; DotType:0),
      (PatternWidth:6; DotCount:3; DotWidth:0; DotType:0),
      (PatternWidth:2; DotCount:4; DotWidth:0; DotType:0),
      (PatternWidth:4; DotCount:4; DotWidth:0; DotType:0),
      (PatternWidth:6; DotCount:4; DotWidth:0; DotType:0),

      (PatternWidth:2; DotCount:1; DotWidth:1; DotType:1),
      (PatternWidth:4; DotCount:1; DotWidth:1; DotType:1),
      (PatternWidth:6; DotCount:1; DotWidth:1; DotType:1),
      (PatternWidth:2; DotCount:2; DotWidth:1; DotType:1),
      (PatternWidth:4; DotCount:2; DotWidth:1; DotType:1),
      (PatternWidth:6; DotCount:2; DotWidth:1; DotType:1),
      (PatternWidth:2; DotCount:3; DotWidth:1; DotType:1),
      (PatternWidth:4; DotCount:3; DotWidth:1; DotType:1),
      (PatternWidth:6; DotCount:3; DotWidth:1; DotType:1),
      (PatternWidth:2; DotCount:4; DotWidth:1; DotType:1),
      (PatternWidth:4; DotCount:4; DotWidth:1; DotType:1),
      (PatternWidth:6; DotCount:4; DotWidth:1; DotType:1),

      (PatternWidth:2; DotCount:1; DotWidth:2; DotType:1),
      (PatternWidth:4; DotCount:1; DotWidth:2; DotType:1),
      (PatternWidth:6; DotCount:1; DotWidth:2; DotType:1),
      (PatternWidth:2; DotCount:2; DotWidth:2; DotType:1),
      (PatternWidth:4; DotCount:2; DotWidth:2; DotType:1),
      (PatternWidth:6; DotCount:2; DotWidth:2; DotType:1),
      (PatternWidth:2; DotCount:3; DotWidth:2; DotType:1),
      (PatternWidth:4; DotCount:3; DotWidth:2; DotType:1),
      (PatternWidth:6; DotCount:3; DotWidth:2; DotType:1),
      (PatternWidth:2; DotCount:6; DotWidth:2; DotType:1),
      (PatternWidth:4; DotCount:6; DotWidth:2; DotType:1),
      (PatternWidth:6; DotCount:6; DotWidth:2; DotType:1),

      (PatternWidth:2; DotCount:1; DotWidth:1; DotType:2),
      (PatternWidth:4; DotCount:1; DotWidth:1; DotType:2),
      (PatternWidth:6; DotCount:1; DotWidth:1; DotType:2),
      (PatternWidth:2; DotCount:2; DotWidth:1; DotType:2),
      (PatternWidth:4; DotCount:2; DotWidth:1; DotType:2),
      (PatternWidth:6; DotCount:2; DotWidth:1; DotType:2),
      (PatternWidth:2; DotCount:3; DotWidth:1; DotType:2),
      (PatternWidth:4; DotCount:3; DotWidth:1; DotType:2),
      (PatternWidth:6; DotCount:3; DotWidth:1; DotType:2),
      (PatternWidth:2; DotCount:6; DotWidth:1; DotType:2),
      (PatternWidth:4; DotCount:6; DotWidth:1; DotType:2),
      (PatternWidth:6; DotCount:6; DotWidth:1; DotType:2),

      (PatternWidth:2; DotCount:1; DotWidth:2; DotType:2),
      (PatternWidth:4; DotCount:1; DotWidth:2; DotType:2),
      (PatternWidth:6; DotCount:1; DotWidth:2; DotType:2),
      (PatternWidth:2; DotCount:2; DotWidth:2; DotType:2),
      (PatternWidth:4; DotCount:2; DotWidth:2; DotType:2),
      (PatternWidth:6; DotCount:2; DotWidth:2; DotType:2),
      (PatternWidth:2; DotCount:3; DotWidth:2; DotType:2),
      (PatternWidth:4; DotCount:3; DotWidth:2; DotType:2),
      (PatternWidth:6; DotCount:3; DotWidth:2; DotType:2),
      (PatternWidth:2; DotCount:6; DotWidth:2; DotType:2),
      (PatternWidth:4; DotCount:6; DotWidth:2; DotType:2),
      (PatternWidth:6; DotCount:6; DotWidth:2; DotType:2)

    );


type
  PTLineContinue=^TLineContinue;

var
  RandTable:array [1..255] of integer;  // Table of Random Numbers generated for fractals
  LastSeed:integer;                     // Optimization: don't regen the table if our seed is the same.

//--------------------------------------------------------------------------
// Name: Marquis
//
// Purpose: Draws dashed line surrounding selected area.
//
// Notes: Caller must set the Canvas pen mode and color (usually pmNotXor
//        if you want to draw the actual color you've selected into the Pen).
//--------------------------------------------------------------------------
procedure Marquis(Canvas:TCanvas; x1,y1,x2,y2:integer);
var ps:TPenStyle;
begin
  ps := Canvas.Pen.Style;
  Canvas.Brush.Color:=MainForm.BackgroundColor.Color;
  Canvas.Pen.Style := psDash;

  Canvas.MoveTo(x1,y1);
  Canvas.LineTo(x2,y1);
  Canvas.LineTo(x2,y2);
  Canvas.LineTo(x1,y2);
  Canvas.LineTo(x1,y1);

  Canvas.Pen.Style := ps;
end;

//--------------------------------------------------------------------------
// Name: RndSeed
//
// Purpose: Random number generator.
//
// Notes: Used to give reliable random numbers that don't change, even if
//        we get a new version of Delphi.
//--------------------------------------------------------------------------
function RndSeed(var seed:integer; max:integer):integer;
begin
  RndSeed := seed mod max;
  seed := (seed*261) mod 65521;
  if (seed=0) then inc(seed);
end;

//--------------------------------------------------------------------------
// Name: GetNumberLineStyles
//
// Purpose: Returns total line styles available.
//
// Notes: This is essentially a fixed number since the user can't add their
//        own styles.  It's written as a function, because if we do let the
//        user add styles, they'd have to go at the bottom of the list, and
//        we'd increment our return value by the number of user styles.
//--------------------------------------------------------------------------
function GetNumberLineStyles:integer;
begin
  Result := 1 + number_thick_styles + number_dithered_styles +
                number_glyph_styles + number_caligraphy_styles +
                number_random_styles + number_numeric_thickness_styles;
end;

//--------------------------------------------------------------------------
// Name: GetNumberLineEndStyles
//
// Purpose: Returns total line end styles available.
//
// Notes: This is essentially a fixed number since the user can't add their
//        own styles.  It's written as a function, because if we do let the
//        user add styles, they'd have to go at the bottom of the list, and
//        we'd increment our return value by the number of user styles.
//--------------------------------------------------------------------------
function GetNumberLineEndStyles:integer;
begin
  Result:=number_lineends;
end;

//--------------------------------------------------------------------------
// Name: FixedRandom
//
// Purpose: Returns the nth fixed "random" number in the RandTable.
//
// Notes: After RandTable is filled with random numbers, the fractal algorithms
//        use it to get predictable random numbers from the table.
//        We can't use real random numbers here because the total numbers
//        required changes depending on our zoom level.  Since the fractals
//        are generated recursively, it would mean that without doing this
//        the shape of our fractal lines would change at each zoom level.
//        This is unacceptable.
//--------------------------------------------------------------------------
function FixedRandom(n:integer):integer;
begin
  FixedRandom:=RandTable[Low(RandTable)+n mod High(RandTable)];
end;

//--------------------------------------------------------------------------
// Name: DrawDot
//
// Purpose: Draws a dot on the canvas.
//
// Notes: If the canvas is a printer, we draw a one pixel wide line.  If we
//        canvas is a screen or bitmap, we set the pixel property instead
//        which is much faster.  We have to cheat like this on the printer
//        because we otherwise wouldn't get the same image on both canvases.
//--------------------------------------------------------------------------
procedure DrawDot(ldr:PTLineContinue; x,y:integer);
begin
  if ldr^.NoPutPixel then begin
    // Printers and metafiles don't treat pixels the same as lines;
    // they aren't overwritten by overlapping white polygons.
    // So, draw all dots to the printer as lines (yuk)...
    ldr^.Canvas.MoveTo(x,y);
    ldr^.Canvas.LineTo(x+1,y);
    end
  else
    ldr^.Canvas.Pixels[x,y] := ldr^.Canvas.Pen.Color;
end;

//--------------------------------------------------------------------------
// Name: DitherLineDDAProc
//
// Purpose: DDA routine called by Window's LineDDA procedure.
//          Called by normal dash-dot type lines.
//
// Notes: Rotates the bitmask over by one bit each time it is called
//        so the bits are sequentially applied.
//--------------------------------------------------------------------------
procedure DitherLineDDAProc(x,y:integer; ldr:PTLineContinue); stdcall;
begin
  if (ldr^.bitmask and 1)<>0 then DrawDot(ldr,x,y);

  ldr^.bitmask := (ldr^.bitmask shr 1) + (ldr^.bitmask shl 31);
end;

//--------------------------------------------------------------------------
// Name: GlyphLineDDAProc
//
// Purpose: DDA routine called by Window's LineDDA procedure.
//          Called by glyph type lines (ones with symbols in them).
//
// Notes: Draws many lines perpendicular to the original line, straddling
//        it.  The individual lines are dot-dash style lines, but
//        we set the dot-dash mask to the appropriate row of our glyph.
//        i.e.
//
//         + + + +<Individual glyph slices drawn perp. to the line
//         + + + +
//        -+-+-+-+---------------------------------------------- line
//         + + + +
//         + + + +
//
// Commentary: This isn't the best way to do this.  For one, it's pretty
//        darn slow.  For two, it leaves noticible gaps in the lines when
//        rotated: 90 degree lines are much more compact than 45 degree lines.
//        I explored several better options for glyph lines, but they all
//        suffered when it came to applying the pattern to a fractal line or
//        a curve. Specifically, joining corners when faced with zillions of
//        line stretches one pixel long (and frequently non tangential) made
//        representing the glyph lines as paths or polygons pretty darn
//        inconvient.  Doing that, they would have to be sliced into thin
//        sections, and reasonably joined when the intersection angle of the
//        lines could be pretty large.  So although not the best in terms of
//        appearance of simple straight lines, these "bitmap" lines are a
//        good all-around compromise.
//--------------------------------------------------------------------------
procedure GlyphLineDDAProc(x,y:integer; ldr:PTLineContinue); stdcall;
begin
  ldr^.Bitmask := GlyphBits[ldr^.Start+ldr^.Slice];
  LineDDA(x-ldr^.dx,y-ldr^.dy,x+ldr^.dx,y+ldr^.dy,
          @DitherLineDDAProc,Integer(ldr));
  inc(ldr^.Slice);
  if (ldr^.Slice>=ldr^.NumSlices) then ldr^.Slice:=0;
end;

//--------------------------------------------------------------------------
// Name: RandomDDAProc
//
// Purpose: DDA routine called by Window's LineDDA procedure.
//          Called by random type lines.
//
// Notes:   The dy variable is used to control the "fuzziness"
//--------------------------------------------------------------------------
procedure RandomDDAProc(x,y:integer; ldr:PTLineContinue); stdcall;
var i:integer;
begin
  for i:=1 to ldr^.NumSlices do begin
    x:=x + RndSeed(ldr^.Start, ldr^.Width) - (ldr^.Width shr 1);
    y:=y + RndSeed(ldr^.Start, ldr^.Width) - (ldr^.Width shr 1);

    case ldr^.dy of
      0:  DrawDot(ldr,x,y);
      1:  begin
            ldr^.Canvas.MoveTo(x-ldr^.dx,y-ldr^.dx);
            ldr^.Canvas.LineTo(x+ldr^.dx,y+ldr^.dx);
            ldr^.Canvas.MoveTo(x+ldr^.dx,y-ldr^.dx);
            ldr^.Canvas.LineTo(x-ldr^.dx,y+ldr^.dx);
          end;
      2:  begin
            ldr^.Canvas.MoveTo(x-ldr^.dx,y-ldr^.dx);
            ldr^.Canvas.LineTo(x-ldr^.dx,y+ldr^.dx);
            ldr^.Canvas.LineTo(x+ldr^.dx,y+ldr^.dx);
            ldr^.Canvas.LineTo(x+ldr^.dx,y-ldr^.dx);
            ldr^.Canvas.LineTo(x-ldr^.dx,y-ldr^.dx);
          end;
      end;

    end;  // End loop
end;

//--------------------------------------------------------------------------
// Name: CaligrahpyDDAProc
//
// Purpose: DDA routine called by Window's LineDDA procedure.
//          Called by caligraphy type lines.
//
// Notes:   We draw a broad stroke for each dot, centered around the real
//          line.
//--------------------------------------------------------------------------
procedure CaligrahpyDDAProc(x,y:integer; ldr:PTLineContinue); stdcall;
begin
  ldr^.Canvas.MoveTo(x - ldr^.dx,y - ldr^.dy);
  ldr^.Canvas.LineTo(x + ldr^.dx,y + ldr^.dy);
end;

//--------------------------------------------------------------------------
// Name: LineTipDDAProc
//
// Purpose: DDA routine called by Window's LineDDA procedure.
//          Called for drawing symbols at line tips.
//
// Notes:   See GlyphDDAProc for reasoning--this routine is very similar.
//--------------------------------------------------------------------------
procedure LineTipDDAProc(x,y:integer; ldr:PTLineContinue); stdcall;
begin
  if (ldr^.Slice>=ldr^.NumSlices) then exit;

  ldr^.Bitmask := GlyphBits[ldr^.Start+ldr^.Slice];

  // Occasional crash reported with a specific map (Kaje Village); failure was in
  // below LineDDA call.  Problem is that tip is being drawn off Canvas, and
  // VCL Canvas.Pixel property doesn't like being passed negative coordinates!
  // Although actual line is still onscreen, the tip will sometimes stray off
  // the corner.  A simple bounds check eliminates this boundary case.
  //
  //  LOG('LineTipDDAProc @%p, x=%d, y=%d, dx=%d, dy=%d Canvas=%p Style=%x Bitmask=%x NoPutPixel=%d',
  //                        [ldr,x,y,ldr^.dx,ldr^.dy,
  //                         @ldr^.Canvas,ldr^.Style.bits,ldr^.Bitmask,integer(ord(ldr^.NoPutPixel))]);

  if (x-ldr^.dx >= 0) and (y-ldr^.dy >= 0) then begin
    LineDDA(x-ldr^.dx,y-ldr^.dy,x+ldr^.dx,y+ldr^.dy,
            @DitherLineDDAProc,Integer(ldr));
    end;

  inc(ldr^.Slice);
end;

//--------------------------------------------------------------------------
// Name: IsAComplexLine
//
// Purpose: Figure out if the line style requires a substantial amount of
//          time to draw or not.
//
// Notes:   Called by drawing primitives if the "Quick Draw" mode is set
//          to reduce drawing time.
//--------------------------------------------------------------------------
function IsAComplexLine(style:StyleAttrib):boolean;
begin
  Result := (Style.Line >= start_glyph_style);
end;

//--------------------------------------------------------------------------
// Name: GetLineThickness
//
// Purpose: Get width of line style
//
// Notes:   Called by drawing primitives if the "Quick Draw" mode is set
//          to reduce drawing time.
//
// Comments: Could also be used to shrink the invalidation regions slightly.
//           When the canvas is manipulated, the area affected is invalidated
//           plus a several pixel border to make sure changing line styles
//           also get invalidated.  If we were "smarter", we could calculate
//           exactly how wide that border would need to be.  Needless to say,
//           it's easier just to slightly inflate the invalidation region and
//           suffer the minor (if any) cost in repaint time.
//--------------------------------------------------------------------------
function GetLineThickness(style:StyleAttrib):integer;
begin
  if style.Line > start_dithered_style
   then Result := 1
   else Result := style.Line;
end;

//--------------------------------------------------------------------------
// Name: IsLineStyleInverted
//
// Purpose: Returns true if the line style is to be inverted (used for
//          "reversing" lines)
//--------------------------------------------------------------------------
function IsLineStyleInverted(style:StyleAttrib):boolean;
begin
  if (style.Bits = SEGMENT_STYLE.Bits) then
    Result := false
  else
    Result := ((style.Last and $80) <> 0);
end;

//--------------------------------------------------------------------------
// Name: InvertLineStyle
//
// Purpose: Flips a line's style
//--------------------------------------------------------------------------
function InvertLineStyle(style:StyleAttrib):StyleAttrib;
begin
  if (style.Bits <> SEGMENT_STYLE.Bits) then begin
    style.Last := style.Last xor $80;
    end;
  Result := style;
end;

//--------------------------------------------------------------------------
// Name: CleanedLineStyle
//
// Purpose: Returns line style with any flags removed from the bitmask
//--------------------------------------------------------------------------
function CleanedLineStyle(style:StyleAttrib):StyleAttrib;
begin
  if (style.Bits <> SEGMENT_STYLE.Bits) then begin
    style.Last := style.Last and (not $80);
    end;
  Result := style;
end;


//--------------------------------------------------------------------------
// Name: CorrectPerpendicular
//
// Purpose: Used in getting lines perpendicular to the main line for
//          drawing glyph/line tips.
//
// Notes:
// Make sure the length of the line we create is in the
// direction of the perpendicular, but make the sum of the
// number of pixels in the x and y direction equals 16
// (or 1/2 our maximum width).
// Without this property, our glyph line will not always be
// centered around the correct point.
//
// The way to ensure this is to force the longest axis of the
// line (0,0)-(px,py) equal to 1 (since we're still dealing with
// unit coordinates), and scale the other axis accordingly.
//--------------------------------------------------------------------------
procedure CorrectPerpendicular(var px,py:Double);
begin
  if (abs(px)>abs(py)) then begin
    if (px>0) then begin
      py:=py*(1/px);  px:=1;
    end else begin
      py:=py*(-1/px); px:=-1;
      end;
    end
  else begin
    if (py>0) then begin
      px:=px*(1/py);  py:=1;
    end else begin
      px:=px*(-1/py); py:=-1;
      end;
    end;
end;

//--------------------------------------------------------------------------
// Name: DrawLineTip
//
// Purpose: Draws a symbol at the tip of a line.
//
// Notes:
//--------------------------------------------------------------------------
procedure DrawLineTip(x1,y1,x2,y2:Coord; kind:integer; const Canvas:TCanvas);
var px,py:Double;
    t:Double;
    continue:TLineContinue;
begin
  if (kind=0) or (kind>=number_lineends) then exit;

  with LineEnds[kind] do begin
    // Back the line segment up so the base of the tip aligns with the
    // end of the line
    UnitVector(x1,y1,x2,y2,px,py);
    x1:=x1-px*Depth;
    y1:=y1-py*Depth;

    // Make the line segment only as long as the line tip
    x2:=x1+px*Width;
    y2:=y1+py*Width;

    // Compute the perpendicular vector
    t:=px;
    px:=-py;
    py:=t;

    if (px<>0) or (py<>0) then begin
      CorrectPerpendicular(px,py);
      continue.Canvas:=Canvas;
      continue.Width:=Width div 2;
      continue.Start:=Start;
      continue.NumSlices:=Count;
      continue.Slice:=0;

      continue.dx:=trunc(px * continue.Width);
      continue.dy:=trunc(py * continue.Width);

      LineDDA(trunc(x1),trunc(y1),trunc(x2),trunc(y2),
              @LineTipDDAProc,Integer(@continue));
      end;
    end;
end;

//--------------------------------------------------------------------------
// Name: DrawLineContinue
//
// Purpose: Draws a line (x1,y1)-(x2,y2).
//
// Notes:   Requires the continue record be filled out before hand.  That
//          is, this routine does no initialization or cleanup, and doesn't
//          handle the line tips.  It is called multiple times, though for
//          polylines.
//--------------------------------------------------------------------------
procedure DrawLineContinue(Canvas:TCanvas; x1,y1,x2,y2:Coord; var continue:TLineContinue);
var
  oldpenwidth   : integer;
  r             : TRect;
  px,py         : Double;
  dots          : integer;
  _technology   : integer;
  Points        : Array[0..3] Of TPoint;
  Angle         : Double;
  Thickness     : Double;
  XOfs,YOfs     : Double;
  OldBrushColor : TColor;
  OldBrushStyle : TBrushStyle;

begin
  r.left:=trunc(x1);
  r.top:=trunc(y1);
  r.right:=trunc(x2);
  r.bottom:=trunc(y2);

  // If the line "Style" is SEGMENT_STYLE, we're actually
  // just cataloging how many times we've called DrawLineContinue
  // and with what arguments so we can perform a decompose.
  // Add the line (allocate more space if necessary).
  if continue.style.Bits=SEGMENT_STYLE.Bits then begin
    if continue.Count+2 > continue.Allocated then begin
      inc(continue.Allocated, AllocationGrowth);
      ReallocMem(continue.Points, continue.Allocated*sizeof(CoordPoint));
      end;

    if continue.Count=0 then begin
      continue.Points^[0].x := x1;
      continue.Points^[0].y := y1;
      continue.Points^[1].x := x2;
      continue.Points^[1].y := y2;
      inc(continue.Count,2);
      end
    else begin
      continue.Points^[continue.Count].x := x2;
      continue.Points^[continue.Count].y := y2;
      inc(continue.Count);
      end;
    exit;
    end;

  if continue.First then begin
    continue.Canvas := Canvas;
    DrawLineTip(x1,y1,x2,y2,continue.Style.First,Canvas);
    end;
                         
  // Set a boolean if the canvas is to a printer or metafile:
  // we need to adjust how we draw dots if it is.
  _technology := GetDeviceCaps(Canvas.Handle,TECHNOLOGY);
  continue.NoPutPixel := (_technology<>DT_RASDISPLAY);

  // Total number of actual pixels we will draw.  Used
  // in keeping clipped lines from incorrectly moving our patterns
  // up the line segment.
  dots:=abs(r.top-r.bottom)+abs(r.right-r.left)+1;

  case continue.style.Line of
    // ----------------
    // NULL line
    0: begin end;          // Useful for filled shapes with no border.

    // ----------------
    // Thick lines
    start_thick_style .. start_thick_style+number_thick_styles-1: begin
        if VisibleWithin(r,Canvas.ClipRect) then begin
          oldpenwidth := Canvas.Pen.Width;
          Canvas.Pen.Width:=GetLineThickness(continue.style);
          Canvas.MoveTo(r.left,r.top);
          Canvas.LineTo(r.right,r.bottom);
          Canvas.Pen.Width:=oldpenwidth;          end;
        end;

    // ----------------
    // Dash-dot lines
    start_dithered_style .. start_dithered_style+number_dithered_styles-1: begin
        if VisibleWithin(r,Canvas.ClipRect) then begin
          LineDDA(r.left,r.top,r.right,r.bottom,
                  @DitherLineDDAProc,Integer(@continue));
          end
        else begin
          // Even if we don't draw the line, make sure we
          // adjust the bitmask so that polylines won't get
          // messed up.  That is, rotate the bitmask for as
          // many dots as we would have drawn, but don't draw
          // it since it is out of the clipping region.
          dots := (dots and 31);
          continue.bitmask := (continue.bitmask shr Dots) + (continue.bitmask shl (32-dots));
          end;
        end;

    // ----------------
    // Glyph lines
    start_glyph_style .. start_glyph_style+number_glyph_styles-1: begin
      if VisibleWithin(r,Canvas.ClipRect) then begin
        UnitPerpendicular(x1,y1,x2,y2,px,py);
        if (px<>0) or (py<>0) then begin
          CorrectPerpendicular(px,py);
          continue.dx:=trunc(px*continue.Width) * continue.InvertLine ;
          continue.dy:=trunc(py*continue.Width) * continue.InvertLine ;

          LineDDA(r.left,r.top,r.right,r.bottom,
                  @GlyphLineDDAProc,Integer(@continue));
          end;
        end
      else begin
        // Even if we don't draw the line, make sure we
        // adjust the bitmask so that polylines won't get
        // messed up.
        continue.Slice := (continue.Slice+dots) mod continue.NumSlices;
        end;
      end;

    // ----------------
    // Caligraphy lines
    start_caligraphy_style .. start_caligraphy_style+number_caligraphy_styles-1: begin
        if VisibleWithin(r,Canvas.ClipRect) then begin
          oldpenwidth := Canvas.Pen.Width;
          Canvas.Pen.Width:=2;
          LineDDA(r.left,r.top,r.right,r.bottom,
                  @CaligrahpyDDAProc,Integer(@continue));
          Canvas.Pen.Width:=oldpenwidth;
          end;
      end;

    // ----------------
    // Random lines
    start_random_style .. start_random_style + number_random_styles - 1: begin
        if VisibleWithin(r,Canvas.ClipRect) then begin
          if (continue.Start=-1) then begin
            continue.Start := Trunc(x1) xor Trunc(y1) xor Trunc(x2) xor Trunc(y2);
            end;

          oldpenwidth := Canvas.Pen.Width;
          Canvas.Pen.Width:=1;
          LineDDA(r.left,r.top,r.right,r.bottom,
                  @RandomDDAProc,Integer(@continue));
          Canvas.Pen.Width:=oldpenwidth;
          end;
      end;

      start_numeric_thickness_style .. start_numeric_thickness_style + number_numeric_thickness_styles - 1:
      Begin
        If VisibleWithin(R,Canvas.ClipRect) Then
        Begin
          OldPenWidth        := Canvas.Pen.Width;
          OldBrushColor      := Canvas.Brush.Color;
          OldBrushStyle      := Canvas.Brush.Style;
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := Canvas.Pen.Color;
          Canvas.Pen.Width   := 0;
          Angle              := ArcTan2(R.Bottom - R.Top, R.Right - R.Left);
          Thickness          := Continue.Style.FullStyle.SThickness / 2;
          XOfs               := Thickness * Cos(Angle - (Pi / 2));
          YOfs               := Thickness * Sin(Angle - (Pi / 2));
          Points[0].X        := Round(R.Left   + XOfs);
          Points[0].Y        := Round(R.Top    + YOfs);
          Points[1].X        := Round((R.Right - 1)  + XOfs) + 1;
          Points[1].Y        := Round((R.Bottom - 1) + YOfs) + 1;
          Points[2].X        := Round((R.Right - 1)  - XOfs) + 1;
          Points[2].Y        := Round((R.Bottom - 1) - YOfs) + 1;
          Points[3].X        := Round(R.Left   - XOfs);
          Points[3].Y        := Round(R.Top    - YOfs);
          Canvas.Polygon(Points);
          Canvas.Pen.Width   := OldPenWidth;
          Canvas.Brush.Color := OldBrushColor;
          Canvas.Brush.Style := OldBrushStyle;
        End;
      End;
    end;  // of case continue.Style.Line

  continue.First:=false;
  continue.p1.x := x1;
  continue.p1.y := y1;
  continue.p2.x := x2;
  continue.p2.y := y2;
end;

//--------------------------------------------------------------------------
// Name: GetLineStyleStart
//
// Purpose: Initializes the TLineContinue struct for the beginning of a line.
//
// Notes:
//--------------------------------------------------------------------------
function GetLineStyleStart(style:StyleAttrib):TLineContinue;
var n:integer;
    w:integer;
begin
  Result.Points:=nil;
  Result.Count:=0;
  Result.Allocated:=0;
  Result.First:=true;

  if IsLineStyleInverted(style) then
     Result.InvertLine := -1
  else
     Result.InvertLine := 1;

  Result.Style := CleanedLineStyle(style);

  n:=Result.Style.Line;

  case n of

    start_dithered_style .. start_dithered_style+number_dithered_styles-1: begin
      dec(n,start_dithered_style);
      Result.bitmask := dashdot[n mod number_dithered_styles];
      end;

    start_glyph_style .. start_glyph_style+number_glyph_styles-1: begin
      dec(n,start_glyph_style);

      Result.Slice:=0;
      Result.Width:=Glyph[n].Width div 2;
      Result.Start:=Glyph[n].Start;
      Result.NumSlices:=Glyph[n].Count;
      end;

    start_caligraphy_style .. start_caligraphy_style+number_caligraphy_styles-1: begin
      dec(n,start_caligraphy_style);

      w:=(n div 4)*2 + 2;           // width of the caligraphy line
      n := n and 3;                 // Only 4 angles: / | - \

      case n of
        0: begin Result.dx:=-w; Result.dy:=w; end;      //  /
        1: begin Result.dx:=0;  Result.dy:=w; end;      //  |
        2: begin Result.dx:=w;  Result.dy:=w; end;      //  \
        3: begin Result.dx:=w;  Result.dy:=0; end;      //  -
        end;
      end;

    start_random_style .. start_random_style + number_random_styles - 1: begin
      dec(n,start_random_style);
      Result.Width     := RandomDot[n].PatternWidth;
      Result.Start     := -1;                     // Seed
      Result.NumSlices := RandomDot[n].DotCount;  // Number dots
      Result.dx        := RandomDot[n].DotWidth;  // Dot width
      Result.dy        := RandomDot[n].DotType;   // Dot Type
      end;

    end;  // case n
end;

//--------------------------------------------------------------------------
// Name: GetLineEnd
//
// Purpose: Ends a line.
//
// Notes: If a SEGMENT_STYLE line, we return the list of segments.
//--------------------------------------------------------------------------
function GetLineEnd(var continue:TLineContinue; var count:integer):PCoordArray;
begin
  if (continue.style.bits=SEGMENT_STYLE.bits) then begin
    Result:=continue.Points;
    count:=continue.Count;
    end
  else begin
    DrawLineTip(continue.p2.x,continue.p2.y,
                continue.p1.x,continue.p1.y,continue.Style.Last,continue.Canvas);
    Result:=nil;
    end;
end;

//--------------------------------------------------------------------------
// Name: DrawLineStyle
//
// Purpose: Routine for drawing a single line segment without the fuss & muss.
//
// Notes: For polylines, you can't call this, you need to do LineStart, Continue, End.
//--------------------------------------------------------------------------
procedure DrawLineStyle(Canvas:TCanvas; x1,y1,x2,y2:integer; style:StyleAttrib);
var continue:TLineContinue;
    count:integer;
    temp:PCoordArray;
begin
  continue:=GetLineStyleStart(style);
  DrawLineContinue(Canvas,x1,y1,x2,y2,continue);
  temp:=GetLineEnd(continue,count);
  if temp<>nil then FreeMem(temp);
end;

//--------------------------------------------------------------------------
// Name: GetFractalBox
//
// Purpose: Determine the bounding box for a fractal.
//
// Notes: Used in invalidating regions to find out the rectangle surrounding
//        the fractal.
//--------------------------------------------------------------------------
procedure GetFractalBox(x1,y1,x2,y2:coord; rfact:double; var b:CoordRect);

  procedure DoFractal(depth:integer; x1,y1,x2,y2:coord; rfact:double);
  var mx,my,px,py:double;
      r:double;
  begin
    Encompass(b,x1,y1);
    Encompass(b,x2,y2);
    UnitPerpendicular(x1,y1,x2,y2,px,py);

    if (depth > fractal_depth) or ((px=0) and (py=0)) then exit;

    r:=rfact*((FixedRandom(depth)/MaxRand)-0.5);
    mx:=(x2+x1)*0.5 + px*r;
    my:=(y2+y1)*0.5 + py*r;

    DoFractal(depth*2, x1,y1,mx,my,   rfact*0.5);
    DoFractal(depth*2+1, mx,my,x2,y2, rfact*0.5);
  end;

begin
  b.left  :=min(x1,x2);
  b.top   :=min(y1,y2);
  b.right :=max(x1,x2);
  b.bottom:=max(y1,y2);
  DoFractal(1,x1,y1,x2,y2,rfact);
end;

//--------------------------------------------------------------------------
// Name: FractalLine
//
// Purpose: Draws a fractal line; rfact is the "width".
//
// Notes: Assumes that FractalSetSeed has been called.
//--------------------------------------------------------------------------
procedure FractalLine(Canvas:TCanvas; x1,y1,x2,y2:coord; rfact:double; var continue:TLineContinue);
var temp:integer;

  procedure DoFractal(depth:integer; x1,y1,x2,y2:coord; rfact:double);
  var d:double;
      mx,my,px,py:double;
      r:double;
  begin
    d:=Distance(x1,y1,x2,y2);
    if (d<=2) then begin                                // 2 pixels is as small as we go: draw it.
      DrawLineContinue(Canvas,trunc(x1),trunc(y1),
                         trunc(x2),trunc(y2),continue);
      exit;
      end;

    // Take the mid-point of the line, and move us some amount above/below the line
    // along the perpendicular.
    UnitPerpendicular(x1,y1,x2,y2,px,py);
    r:=rfact*((FixedRandom(depth)/MaxRand)-0.5);
    mx:=(x2+x1)*0.5 + px*r;
    my:=(y2+y1)*0.5 + py*r;

    // Recurse, adjusting down the width.
    DoFractal(depth*2, x1,y1,mx,my, rfact*0.5);
    DoFractal(depth*2+1, mx,my,x2,y2, rfact*0.5);
  end;

begin
  DoFractal(1,x1,y1,x2,y2,rfact);
  GetLineEnd(continue,temp);
end;

//--------------------------------------------------------------------------
// Name: FractalSetSeed
//
// Purpose: Fills the RandTable: future calls to fractal routines will have
//          this "shape".
//
// Notes:
//--------------------------------------------------------------------------
procedure FractalSetSeed(n:integer);
var i:integer;
begin
  if (LastSeed<>n) then begin
    RandSeed:=n;
    for i:=Low(RandTable) to High(RandTable) do RandTable[i]:=Random(MaxRand);
    LastSeed:=n;
    end;
end;

//--------------------------------------------------------------------------
// Name: GetBezierBox
//
// Purpose: Gets a bounding box for a bezier curve
//
// Notes: Used in calculation of invalidation regions.
//--------------------------------------------------------------------------
procedure GetBezierBox(p1,p2,p3,p4:CoordPoint; var b:CoordRect);

  procedure DoBezier(depth:integer; p1,p2,p3,p4:CoordPoint);
  var q1,q2,q3:CoordPoint;
      r1,r2,s1:CoordPoint;
  begin
    if (depth<7) then begin
      q1:=AvePoints(p1,p2);
      q2:=AvePoints(p2,p3);
      q3:=AvePoints(p3,p4);
      r1:=AvePoints(q1,q2);
      r2:=AvePoints(q2,q3);
      s1:=AvePoints(r1,r2);

      Encompass(b,s1.x,s1.y);
      DoBezier(depth+1,p1,q1,r1,s1);
      DoBezier(depth+1,s1,r2,q3,p4);
      end;
  end;

begin
  b.left  := min(p1.x,p4.x);
  b.top   := min(p1.y,p4.y);
  b.right := max(p1.x,p4.x);
  b.bottom:= max(p1.y,p4.y);

  DoBezier(1,p1,p2,p3,p4);
end;

Procedure GetArcCenter(P1,P2,P3: CoordPoint; Var X1,Y1: Single);
Var
  R1,R2    : Double;
  D        : Double;
  H        : Double;
  R        : Double;
  X,Y      : Double;
  X2,Y2    : Double;

Begin
  X1 := -1;
  Y1 := -1;

  // Find out where the center of the circle is.  We can do this by thinking of
  // each point on the arc as the center of its own circle, both of which are the
  // same size.  Then we only have to find the intersection of the two circles

  R1 := Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y);
  R2 := Sqr(P3.X - P1.X) + Sqr(P3.Y - P1.Y);
  D  := Sqr(P3.X - P2.X) + Sqr(P3.Y - P2.Y);
  If R1 < R2 Then R1 := R2;
  R  := R1;
  If (D > 0) And (R1 > 0) Then
  Begin
    R := Sqrt(R);
    D := Sqrt(D);
    If D < 2 * R1 Then
    Begin

      // Solve for the two intersection points

      H  := Sqrt(Sqr(R) - Sqr(D / 2));
      X  := (P2.X + P3.X) / 2;
      Y  := (P2.Y + P3.Y) / 2;
      X1 := X + H * (P3.Y - P2.Y) / D;
      Y1 := Y - H * (P3.X - P2.X) / D;
      X2 := X - H * (P3.Y - P2.Y) / D;
      Y2 := Y + H * (P3.X - P2.X) / D;

      // Pick whichever one is closer to the cursor

      R1 := Sqr(X1 - P1.X) + Sqr(Y1 - P1.Y);
      R2 := Sqr(X2 - P1.X) + Sqr(Y2 - P1.Y);

      If R2 < R1 Then
      Begin
        X1 := X2;
        Y1 := Y2;
      End;
    End;
  End;
End; // GetArcCenter

Procedure DrawArc(Canvas: TCanvas; P1,P2,P3: CoordPoint; Var Continue: TLineContinue);
Var
  OldColor : TColor;
  R1,R2    : Double;
  A1,A2    : Double;
  D        : Double;
  H        : Double;
  R        : Double;
  X,Y      : Double;
  X1,Y1    : Double;
  X2,Y2    : Double;
  C        : CoordPoint;

Begin
  // Find out where the center of the circle is.  We can do this by thinking of
  // each point on the arc as the center of its own circle, both of which are the
  // same size.  Then we only have to find the intersection of the two circles

  R1 := Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y);
  R2 := Sqr(P3.X - P1.X) + Sqr(P3.Y - P1.Y);
  D  := Sqr(P3.X - P2.X) + Sqr(P3.Y - P2.Y);
  If R1 < R2 Then R1 := R2;
  R  := R1;
  If (D > 0) And (R1 > 0) Then
  Begin
    R := Sqrt(R);
    D := Sqrt(D);
    If D < 2 * R1 Then
    Begin

      // Solve for the two intersection points

      H  := Sqrt(Sqr(R) - Sqr(D / 2));
      X  := (P2.X + P3.X) / 2;
      Y  := (P2.Y + P3.Y) / 2;
      X1 := X + H * (P3.Y - P2.Y) / D;
      Y1 := Y - H * (P3.X - P2.X) / D;
      X2 := X - H * (P3.Y - P2.Y) / D;
      Y2 := Y + H * (P3.X - P2.X) / D;

      // Pick whichever one is closer to the cursor

      R1 := Sqr(X1 - P1.X) + Sqr(Y1 - P1.Y);
      R2 := Sqr(X2 - P1.X) + Sqr(Y2 - P1.Y);

      If R2 < R1 Then
      Begin
        X1 := X2;
        Y1 := Y2;
      End;

      // Windows wants to draw counterclockwise, so adjust if necessary

      If P2.X <> X1 Then A1 := ArcTan2(P2.Y - Y1,P2.X - X1) Else If P2.Y > Y1 Then A1 := Pi / 2 Else A1 := -Pi / 2;
      If P3.X <> X1 Then A2 := ArcTan2(P3.Y - Y1,P3.X - X1) Else If P3.Y > Y1 Then A2 := Pi / 2 Else A2 := -Pi / 2;

      While A1 < 0 Do A1 := A1 + 2 * Pi;
      While A2 < 0 Do A2 := A2 + 2 * Pi;

      R2 := A1 - A2;

      // We want to go counterclockwise, so flip if we have to

      If R2 < 0 Then
      Begin
        C  := P2;
        P2 := P3;
        P3 := C;
      End;

      // Flip again under certain circumstances

      If (R2 > Pi) Or (R2 < -Pi) Then
      Begin
        C  := P2;
        P2 := P3;
        P3 := C;
      End;

      // Draw the arc

      OldColor           := Canvas.Pen.Color;
      Canvas.Pen.Color   := CurrentColor;
      Canvas.Brush.Style := bsClear;
      Canvas.Arc(Round(X1 - R),Round(Y1 - R),
                 Round(X1 + R),Round(Y1 + R),
                 Round(P2.X),Round(P2.Y),
                 Round(P3.X),Round(P3.Y));
      Canvas.Rectangle(Round(X1) - 1,Round(Y1) - 1,Round(X1) + 1,Round(Y1) + 1);
      Canvas.Pen.Color   := OldColor;
    End;
  End;
End; // DrawArc

//--------------------------------------------------------------------------
// Name: DrawBezier
//
// Purpose: Draws a bezier curve
//
// Notes:
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
//--------------------------------------------------------------------------
procedure DrawBezier(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; var continue:TLineContinue);
var temp:integer;

  procedure DoBezier(depth:integer; p1,p2,p3,p4:CoordPoint);
  var q1,q2,q3:CoordPoint;
    r1,r2,s1:CoordPoint;
  begin
    if (depth<>1) and (abs(p1.X - p4.X)<3) and (abs(p1.Y - p4.Y)<3) then begin
//      Canvas.LineTo(trunc(p4.X),trunc(p4.Y));
      DrawLineContinue(Canvas,trunc(p1.X),trunc(p1.Y),
                       trunc(p4.X),trunc(p4.Y),continue);
      exit;
      end;

    q1:=AvePoints(p1,p2);
    q2:=AvePoints(p2,p3);
    q3:=AvePoints(p3,p4);
    r1:=AvePoints(q1,q2);
    r2:=AvePoints(q2,q3);
    s1:=AvePoints(r1,r2);

    inc(depth);
    DoBezier(depth,p1,q1,r1,s1);
    DoBezier(depth,s1,r2,q3,p4);
  end;

begin
//  Canvas.MoveTo(trunc(p1.X),trunc(p1.Y));
  DoBezier(1,p1,p2,p3,p4);
  GetLineEnd(continue,temp);
end;

//--------------------------------------------------------------------------
// Name: DrawFractalBezier
//
// Purpose: Draws fractal bezier curve.
//
// Notes: Assumes that FractalSetSeed has been called.
//--------------------------------------------------------------------------
procedure DrawFractalBezier(Canvas:TCanvas; p1,p2,p3,p4:CoordPoint; rfact:double; var continue:TLineContinue);

  procedure DoBezier(depth:integer; p1,p2,p3,p4:CoordPoint; rfact:double);
  var q1,q2,q3:CoordPoint;
      r1,r2,s1:CoordPoint;
      d:double;
      px,py:double;
      r:double;
  begin
    d:=Distance(p1.x,p1.y,p4.x,p4.y);
    if (d<=2) and (depth>1) then begin
      with Canvas do begin
//        MoveTo(trunc(p1.x),trunc(p1.y));
//        LineTo(trunc(p4.x),trunc(p4.y));
        DrawLineContinue(Canvas,trunc(p1.X),trunc(p1.Y),
                         trunc(p4.X),trunc(p4.Y),continue);
        end;
      exit;
      end;

    q1:=AvePoints(p1,p2);
    q2:=AvePoints(p2,p3);
    q3:=AvePoints(p3,p4);
    r1:=AvePoints(q1,q2);
    r2:=AvePoints(q2,q3);
    s1:=AvePoints(r1,r2);

    UnitPerpendicular(p1.x,p1.y,p4.x,p4.y,px,py);
    r:=rfact*((FixedRandom(depth)/MaxRand)-0.5);
    s1.x := s1.x + px*r;
    s1.y := s1.y + py*r;

    DoBezier(depth*2,   p1,q1,r1,s1, rfact*0.5);
    DoBezier(depth*2+1, s1,r2,q3,p4, rfact*0.5);
  end;

begin
  DoBezier(1,p1,p2,p3,p4,rfact);
end;

//--------------------------------------------------------------------------
// Name: GetFractalBezierBox
//
// Purpose: Gets a bounding box for a fractal bezier curve
//
// Notes: Used in calculation of invalidation regions.
//--------------------------------------------------------------------------
procedure GetFractalBezierBox(p1,p2,p3,p4:CoordPoint; rfact:double; var b:CoordRect);
  procedure DoBezier(depth:integer; p1,p2,p3,p4:CoordPoint; rfact:double);
  var q1,q2,q3:CoordPoint;
      r1,r2,s1:CoordPoint;
      px,py:double;
      r:double;
  begin
    if (depth > fractal_depth) then exit;

    UnitPerpendicular(p1.x,p1.y,p4.x,p4.y,px,py);
    if (px=0) and (py=0) then exit;

    q1:=AvePoints(p1,p2);
    q2:=AvePoints(p2,p3);
    q3:=AvePoints(p3,p4);
    r1:=AvePoints(q1,q2);
    r2:=AvePoints(q2,q3);
    s1:=AvePoints(r1,r2);

    r:=rfact*((FixedRandom(depth)/MaxRand)-0.5);
    s1.x := s1.x + px*r;
    s1.y := s1.y + py*r;
    Encompass(b,s1.x,s1.y);

    DoBezier(depth*2,   p1,q1,r1,s1, rfact*0.5);
    DoBezier(depth*2+1, s1,r2,q3,p4, rfact*0.5);
  end;

begin
  b.left  := min(p1.x,p4.x);
  b.top   := min(p1.y,p4.y);
  b.right := max(p1.x,p4.x);
  b.bottom:= max(p1.y,p4.y);

  DoBezier(1,p1,p2,p3,p4,rfact);
end;



begin
  LastSeed:=-1;
end.
