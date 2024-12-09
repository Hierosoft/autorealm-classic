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
unit Primitives;

interface

uses SysUtils, LCLIntf, Classes, Graphics, Forms, MatrixMath, Math, Geometry,
     GraphGrid, DrawLines, DOM,      Dialogs;
{ XDOM_2_3, DIMime }
{ LCLIntf provides cross-platform GUI classes }
const IconFontName = 'AutoREALMSymbols';
      QuickDraw_Fills=$00000001;
      QuickDraw_Lines=$00000002;
      QuickDraw_All  =$FFFFFFFF;
      QuickDraw_None =$00000000;
      SelectDistance = 2;
      VeryClose = 1E-3; // Used in IsSimilarTo() testing and is intended to handle
                        // roundoff error

type OverlaySet = set of byte;
     HandleMode = (hmAll, hmOne, hmFoundFirst);

     HyperlinkFlagValues = (hyperExecute, hyperHidden);
     THyperlinkFlags = set of HyperlinkFlagValues;

     TextAttribType = (tatText, tatFontName, tatFontSize,
                       tatFontBold, tatFontItalic, tatFontUnderline,
                       tatAlignment, tatIconSize, tatOutlineColor,
                       tatHyperlinkText, tatHyperlinkFlags);
     TextAttribSet = set of TextAttribType;
     TextAttrib = record
       Text:string;
       FontName:string;
       FontSize:integer;
       FontFillColor:TColor;
       FontOutlineColor:TColor;
       FontBold,FontItalic,FontUnderline:boolean;
       Alignment:integer;
       Valid:TextAttribSet;
       IconSize:integer;
       HyperlinkText:string;
       HyperlinkFlags:THyperlinkFlags;
       end;
     PTPoint = ^TPoint;

     PDrawPrimitive = ^DrawPrimitive;

     { ----- }

     ViewPoint = class
       private
         Area:CoordRect;
         ClientWidth,ClientHeight:integer;
       public
         Name:string;
         Canvas:TCanvas;
         VisibleOverlays:OverlaySet;
         ActiveOverlays:OverlaySet;
         QuickDraw:Cardinal;
         Grid:GridObject;
         OffScreenFullDetail:boolean;

       constructor Create; overload;
       constructor Create(rect:CoordRect; cw,ch:integer); overload;
       constructor Create(rect:CoordRect; cw,ch:integer; oldgrid:GridObject); overload;
       constructor Create(view:ViewPoint; useprinter:boolean=false); overload;
       destructor Destroy; override;
       procedure CoordToScreen(cx,cy:Coord; var sx,sy:integer); overload;
       procedure CoordToScreen(cx,cy:Coord; var sx,sy:Coord);   overload;
       procedure ScreenToCoord(sx,sy:Coord; var cx,cy:Coord);   overload;
       procedure ScreenToCoord(sx,sy:integer; var cx,cy:Coord); overload;
       procedure DeltaScreenToCoord(dx,dy:integer; var cx,cy:Coord); Overload;
       procedure DeltaScreenToCoord(dx,dy:Coord; var cx,cy:Coord); Overload;
       procedure DeltaCoordToScreen(cx,cy:Coord; var dx,dy:integer); Overload;
       procedure DeltaCoordToScreen(cx,cy:Coord; var dx,dy:Coord); Overload;

       function CoordToScreenPt(p:CoordPoint):CoordPoint;
       procedure ScreenToCoordPtArray(p:PCoordArray; Count:integer);

       procedure Zoom(center:CoordPoint; factor:single; px:single=0.5; py:single=0.5);
       procedure SetZoomPercent(percent:double);
       function  GetZoomPercent:integer;

       function CoordToScreenRect(crect:CoordRect):TRect;
       function ScreenToCoordRect(rect: TRect): CoordRect; Overload;
       function ScreenToCoordRect(rect: CoordRect): CoordRect; Overload;

       procedure SetCoordinateSize(width,height:integer; rescale:boolean);
       procedure GetCoordinateSize(var width,height:integer);
       procedure SetCoordinateRect(cr:CoordRect);
       procedure GetCoordinateRect(var cr:CoordRect);

       procedure SaveToStream(Stream:TStream);
       procedure LoadFromStream(Stream:TStream);

       Function  GetAsDOMElement(D: TDOMDocument): TDOMElement;
       Procedure LoadFromDOMElement(E: TDOMElement);

       function FixStyle(st:StyleAttrib):StyleAttrib;
       end;

     { ----- }

     FractalState = (fsUnchanged, fsSetFractal, fsSetNormal, fsFlipFractal);

     { ----- }

     DrawPrimitive = class
     private
        fOverlay  : byte;
        fSelected : boolean;

     protected
        fExtent : CoordRect;
        fColor  : TColor;

        // Absolute X,Y position of the alias.  Not used for "base" objects, though
        // it is harmless to set it for them.
        //
        // There are several reasons why Alias contains ABSOLUTE coordinates instead
        // of RELATIVE coordinates, all of which relate to GroupPrimitive objects:
        //
        // 1. Groups don't know where they are.
        //
        // GroupPrimitive objects neither contain nor use coordinate information;
        // they merely contain other DrawPrimitive objects that contain absolute
        // coordinates and therefore know where they are.  If aliases within a group
        // contained relative coordinates, they would need to know the position to
        // which they were relative, which means that groups would have to know their
        // positions (or be able to get it from their Parents--see below).
        //
        // 2. The need for "parent" information.
        //
        // Giving groups positional information is useless unless DrawPrimitives
        // know the groups to which they belong.  That means adding something like
        // a Parent field, and all the necessary code to build this information and
        // maintain its integrity.  It would also render certain information
        // meaningless, such as LinePrimitive (X1,Y1) coordinates, hyperlink
        // coordinates, and starting coordinates for curves, polycurves, etc.
        //
        // 3. SPEED!
        //
        // Many calculations have to be performed in real-time in which it is
        // necessary to know where an alias object's coordinate actually is.  For
        // instance, dragging a polycurve node requires that the program know where
        // that node is in real-time.  When alias objects contain absolute
        // coordinates, they can simply refer to their base objects and do a fast
        // calculation to find out the actual position.  When they contain relative
        // coordinates, however, they first have to walk a parent tree.  I felt that
        // this would take far too much time for many operations.

        Alias   : CoordPoint;

        // For a given "base" object, this list contains pointers to all of its
        // alias objects.  In each alias, this list is empty by definition.

        Copies  : TStringList;

        procedure SetExtent(value:CoordRect);
        Procedure MakeCopy(From: DrawPrimitive);
        Function  MakeAliasView(View: ViewPoint): ViewPoint;
        Function  GetAdjustedX(X: Coord): Coord;
        Function  GetAdjustedY(Y: Coord): Coord;
        Function  GetAdjustedPoint(P: CoordPoint): CoordPoint;
        Procedure RecalcAliasFromExtent;
        Procedure RefreshCopies;

     public

        // For alias objects, Base contains a pointer to its "base" object.  For
        // base objects, this pointer MUST contain Nil (it's how we differentiate
        // between base and alias objects).

        Base : DrawPrimitive;
        Next : DrawPrimitive;

        // MapCollection; I can't set the type here because it would cause a
        // circular unit reference.  We'll just have to typecast it where it's
        // used and deal with it.  See DrawPrimitive.SetMap() for a full
        // explanation of this field's purpose.

        MapC : TObject;

        Constructor Create;
        destructor Destroy; override;
        Procedure  SetMap(M: TObject); Virtual;
        procedure ClearChain;
        Procedure InsertIntoBaseList;
        Procedure AddToBaseOrCopies(DoChain: Boolean = False); Virtual;
        Procedure ClearThis(Deallocate: Boolean); Virtual;
        Procedure Clear;
        Procedure CopyFromBase(AliasOnly: Boolean); Virtual;

        function Copy:DrawPrimitive; virtual;

        property Extent:CoordRect read fExtent write SetExtent;
        procedure ComputeExtent; virtual;
        function  IsClosed:boolean; virtual;

        procedure SetSize(w,h:Coord);
        function Width:Coord;
        function Height:Coord;

        function IsWithin(rect:CoordRect):boolean;
        function IsTouching(rect:CoordRect):boolean;
        function IsInOverlay(var overlay:OverlaySet):boolean;
        function IsSelected:boolean;
        procedure Select(const View:ViewPoint; b:boolean);
        function SelectClick(const within:double; p:CoordPoint):boolean; virtual;

        function OnScreen(const Coord:CoordRect):boolean;
        procedure Invalidate(const Handle: THandle; const View: ViewPoint);

        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; virtual;
        procedure InvalidateHandle(const Handle: THandle; const View: ViewPoint; x, y: Coord);
        procedure DrawHandle(const View:ViewPoint; x,y:Coord);
        procedure DrawOverlayHandle(View: ViewPoint; x, y: Coord);
        function  TestHandle(const View:ViewPoint; tx,ty,px,py:Coord):boolean;
        function PointClosestInArray(x,y:Coord; points:PCoordArray; count:integer):integer;
        function PointClosestInAdjustedArray(x,y:Coord; points:PCoordArray; count:integer):integer;
        function GetLines(const View:Viewpoint; var polycount:integer):PCoordArray; virtual;

        function FindHandle(const View:ViewPoint; x,y:Coord):boolean; virtual;
        function FindHyperlink(const View:ViewPoint; x,y:Coord; var hypertext:string; var hyperflags:THyperlinkFlags):boolean; virtual;
        function FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean; virtual;
        Function FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean; Virtual; // JD 8-3-02
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; virtual;
        procedure Move(dx,dy:Coord); virtual;
        procedure Draw(View:ViewPoint); virtual;
        procedure DrawHandles(const View:ViewPoint); virtual;
        procedure DrawOverlayHandles(const View:ViewPoint); virtual;
        procedure PointClosestTo(x,y:Coord; var px,py:Coord); virtual;
        procedure InvalidateHandles(const Handle: THandle; const View: ViewPoint); virtual;
        function FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean; virtual;
        function SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean; virtual;
        procedure DeleteNode(const View:ViewPoint; index:integer); virtual;
        function SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean; virtual;

        function DisplayColor(color:TColor):TColor;
        function DisplayFillColor(color:TColor):TColor;
        procedure CopyCore(obj:DrawPrimitive);
        function ApplyMatrix(var mat:Matrix):boolean; virtual;
        function SetStyle(style:StyleAttrib):boolean; virtual;
        function  GetStyle:StyleAttrib;  virtual;
        function  SetSeed(seed:integer):boolean; virtual;
        function  GetSeed:integer;  virtual;
        function  SetRoughness(rough:integer):boolean; virtual;
        function  GetRoughness:integer;  virtual;
        function  SetColor(color:TColor):boolean; virtual;
        function  GetColor:TColor;  virtual;
        function  SetFillColor(color:TColor):boolean; virtual;
        function  GetFillColor:TColor;  virtual;
        function  SetOverlay(overlay:byte):boolean; virtual;
        function  GetOverlay:integer;  virtual;
        function  SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; virtual;
        function  GetTextAttrib:TextAttrib; virtual;
        procedure CloseFigure; virtual;
        procedure Reverse; virtual;
        function  SetFractal(state: FractalState): boolean; virtual;

        function  ChainApplyMatrix(mat:Matrix; all:boolean):boolean;
        function  ChainExtent(all:boolean):CoordRect;
        function  ChainSetStyle(style:StyleAttrib; all:boolean):boolean;
        function  ChainGetStyle(all:boolean):StyleAttrib;
        function  ChainSetSeed(seed:integer; all:boolean):boolean;
        function  ChainGetSeed(all:boolean):integer;
        function  ChainSetRoughness(rough:integer; all:boolean):boolean;
        function  ChainGetRoughness(all:boolean):integer;
        function  ChainSetColor(color:TColor; all:boolean):boolean;
        function  ChainGetColor(all:boolean):TColor;
        function  ChainSetFillColor(color:TColor; all:boolean):boolean;
        function  ChainGetFillColor(all:boolean):TColor;
        function  ChainSetOverlay(overlay:byte; all:boolean):boolean;
        function  ChainGetOverlay(all:boolean):integer;
        function  ChainSetTextAttrib(const View:ViewPoint; attrib:TextAttrib; all:boolean):boolean;
        function  ChainGetTextAttrib(all:boolean):TextAttrib;
        function  ChainSetFractal(state: FractalState; all: boolean): boolean;

        function  CountSiblings:integer;
        class function ReadChain(stream:TStream; version:integer; selected,Full,UseAliasInfo:boolean; M: TObject):DrawPrimitive;
        procedure WriteChain(stream: TStream; all,Full,UseAliasInfo: boolean; AddX: Coord = 0; AddY: Coord = 0);
        class function ReadChainFromDOMElement(E: TDOMElement; Version: Integer; Selected: Boolean; M: TObject): DrawPrimitive;
        Function  GetChainAsDOMElement(D: TDOMDocument; All,Undo: Boolean): TDOMElement;

        class function IdToObject(id:char):DrawPrimitive;
        function GetId:char;             virtual;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  virtual;
        procedure Read(stream:TStream; version:integer; Full,UseAliasInfo: Boolean); virtual;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); virtual;
        Function  ReadBaseFromDOMElement(E: TDOMElement): Boolean;
        Function  GetAsAliasDOMElement(D: TDOMDocument; E: TDOMElement): Boolean;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Virtual;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Virtual;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Virtual;
        Procedure SplitOff;
//        procedure FixExtent;             virtual;
     end;

     SymbolPrimitive = class(DrawPrimitive)
     public
        x1,y1:Coord;
        ch,cw,chx:Coord;
        Text:string;
        Font:TFont;
        size:integer;
        angle:integer;
        fOutlineColor:TColor;

        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        Procedure ClearThis(Deallocate: Boolean); Override;
        constructor CreateBlank;
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        constructor Create(const View:ViewPoint; ix,iy:Coord; symbol:string; isize:integer; outlinecolor:TColor);
        constructor CreateInternal(const View:ViewPoint; ix,iy:Coord; symbol:string; iFont:TFont;  outlinecolor:TColor);
        procedure ComputeSize(const View:ViewPoint);
        function PrepareFont(const View:ViewPoint; var sx,sy:integer; formatflags:integer):boolean;
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        procedure ComputeExtent; override;
        procedure Draw(View:ViewPoint); override;
        function ApplyMatrix(var mat:Matrix):boolean; override;
        procedure Move(dx,dy:Coord); override;
        function GetTextAttrib:TextAttrib; override;
        function SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; override;
        function RotatedBox(w,h:Coord; formatflags:integer):CoordRect;
        function SelectClick(const within:double; p:CoordPoint):boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
//        procedure FixExtent;             override;
     end;

     TextPrimitive = class(SymbolPrimitive)
        formatflags:integer;
     public
        constructor CreateBlank;
        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        constructor Create(const View:ViewPoint; ix,iy:Coord; itext:string; IFont:TFont; iformatflags:integer; outline:TColor);
        procedure Draw(View:ViewPoint); override;
        procedure ComputeExtent; override;
        function ApplyMatrix(var mat:Matrix):boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        function  SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; override;
        function  GetTextAttrib:TextAttrib; override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
     end;

     LinePrimitive = class(DrawPrimitive)
     public
        // Standard attributes
        x1,y1,x2,y2:Coord;
        style:StyleAttrib;

        // Fractal attributes
        seed,roughness:integer;
        fractal:boolean;

        constructor CreateBlank(frac:boolean);
        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;

        // Standard constructor
        constructor Create(ix1,iy1,ix2,iy2:Coord; istyle:StyleAttrib); overload;
        // Fractal constructor
        constructor Create(ix1,iy1,ix2,iy2:Coord; iseed,irough:integer; istyle:StyleAttrib); overload;
        // Dual constructor
        constructor Create(ix1,iy1,ix2,iy2:Coord; iseed,irough:integer; istyle:StyleAttrib; frac:boolean); overload;

        procedure Draw(View:ViewPoint); override;
        procedure DrawHandles(const View:ViewPoint); override;
        procedure DrawOverlayHandles(const View:ViewPoint); override;
        function ApplyMatrix(var mat:Matrix):boolean; override;
        function FindHandle(const View:ViewPoint; x,y:Coord):boolean; override;
        function FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean; override;
        Function FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean; Override; // JD 8-3-02
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        function SetStyle(new_style:StyleAttrib):boolean; override;
        function  GetStyle:StyleAttrib;  override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        procedure PointClosestTo(x,y:Coord; var px,py:Coord); override;
        function SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean; override;
        procedure Reverse; override;
        function GetLines(const View:Viewpoint; var polycount:integer):PCoordArray; override;

        // Fractal functions
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        function SetSeed(new_seed:integer):boolean; override;
        function GetSeed:integer;  override;
        function SetRoughness(rough:integer):boolean; override;
        function GetRoughness:integer;  override;
        function RFact:double;
        function SetFractal(state: FractalState): boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
        Function  GetAdjustedX1: Coord;
        Function  GetAdjustedY1: Coord;
        Function  GetAdjustedX2: Coord;
        Function  GetAdjustedY2: Coord;
     end;

     CurvePrimitive = class(DrawPrimitive)
     public
        // Standard attributes
        p1,p2,p3,p4:CoordPoint;
        style:StyleAttrib;

        // Fractal attributes
        seed,roughness:integer;
        fractal:boolean;

        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        constructor CreateBlank(frac:boolean);
        // Standard constructor
        constructor Create(ip1,ip2,ip3,ip4:CoordPoint; istyle:StyleAttrib); overload;
        // Fractal constructor
        constructor Create(ip1,ip2,ip3,ip4:CoordPoint; iseed,irough:integer; istyle:StyleAttrib); overload;
        // Dual constructor
        constructor Create(ip1,ip2,ip3,ip4:CoordPoint; iseed,irough:integer; istyle:StyleAttrib; frac:boolean); overload;

        procedure Draw(View:ViewPoint); override;
        procedure DrawHandles(const View:ViewPoint); override;
        procedure InvalidateHandles(const Handle: THandle; const View: ViewPoint); override;
        procedure DrawOverlayHandles(const View:ViewPoint); override;
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        function ApplyMatrix(var mat:Matrix):boolean; override;
        function FindHandle(const View:ViewPoint; x,y:Coord):boolean; override;
        function FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean; override;
        Function FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean; Override; // JD 8-3-02
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        function SetStyle(new_style:StyleAttrib):boolean; override;
        function  GetStyle:StyleAttrib;  override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        procedure PointClosestTo(x,y:Coord; var px,py:Coord); override;
        function SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean; override;
        procedure CloseFigure; override;
        procedure Reverse; override;
        function GetLines(const View:Viewpoint; var polycount:integer):PCoordArray; override;
        function SetSeed(new_seed:integer):boolean; override;
        function  GetSeed:integer;  override;
        function SetRoughness(rough:integer):boolean; override;
        function  GetRoughness:integer;  override;
        function RFact:double;
        function SetFractal(state: FractalState): boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
        Function  GetAdjustedP1: CoordPoint;
        Function  GetAdjustedP2: CoordPoint;
        Function  GetAdjustedP3: CoordPoint;
        Function  GetAdjustedP4: CoordPoint;
     end;

     PolyCurvePrimitive = class(DrawPrimitive)
     public
        // Standard attributes
        style:StyleAttrib;
        fillcolor:TColor;
        points:PCoordArray;
        count:integer;

        // Fractal attributes
        seed,roughness:integer;
        fractal:boolean;

        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        Procedure ClearThis(Deallocate: Boolean); Override;
        constructor CreateBlank(frac:boolean);

        // Standard constructor
        constructor Create(lp:PCoordArray; c:integer; istyle:StyleAttrib); overload;
        // Fractal constructor
        constructor Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib); overload;
        // Dual constructor
        constructor Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib; frac:boolean); overload;

        constructor Ellipse(center:CoordPoint; edge:CoordPoint; istyle:StyleAttrib);
//        constructor Ellipse(center:CoordPoint; edge:CoordPoint; iseed,irough:integer; istyle:StyleAttrib);
        constructor Arc(Center: CoordPoint; Edge1,Edge2: CoordPoint; iStyle: StyleAttrib); // JD 7-31-02
        function  IsClosed:boolean; override;
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        procedure Draw(View:ViewPoint); override;
        procedure DrawHandles(const View:ViewPoint); override;
        procedure InvalidateHandles(const Handle: THandle; const View: ViewPoint); override;
        procedure DrawOverlayHandles(const View:ViewPoint); override;
        function  ApplyMatrix(var mat:Matrix):boolean; override;
        function  FindHandle(const View:ViewPoint; x,y:Coord):boolean; override;
        function FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean; override;
        Function FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean; Override; // JD 8-3-02
        function  MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        function  SetStyle(new_style:StyleAttrib):boolean; override;
        function  GetStyle:StyleAttrib;  override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        procedure PointClosestTo(x,y:Coord; var px,py:Coord); override;
        function  SetFillColor(color:TColor):boolean; override;
        function  GetFillColor:TColor;  override;
        function GetLines(const View:Viewpoint; var polycount:integer):PCoordArray; override;

        function  SetSeed(new_seed:integer):boolean; override;
        function  GetSeed:integer;  override;
        function  SetRoughness(rough:integer):boolean; override;
        function  GetRoughness:integer;  override;

        procedure CloseFigure; override;
        function FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean; override;
        function SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean; override;
        procedure DeleteNode(const View:ViewPoint; index:integer); override;
        function SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean; override;
        procedure Reverse; override;
        function SetFractal(state: FractalState): boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
     end;

     TextCurvePrimitive = class(CurvePrimitive)
        Text:string;
        Font:TFont;
        size:integer;
        ch:Coord;
        fOutlineColor:TColor;
     public
        function    Copy:DrawPrimitive; override;
        Procedure   CopyFromBase(AliasOnly: Boolean); Override;
        constructor CreateBlank;
        constructor Create(const View:ViewPoint; ip1,ip2,ip3,ip4:CoordPoint; itext:string; IFont:TFont; outlinecolor:TColor);
        procedure   Draw(View:ViewPoint); override;
        procedure   ComputeSize(const View:ViewPoint);
        function    ApplyMatrix(var mat:Matrix):boolean; override;
        procedure   ComputeExtent; override;
        function    Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;

        function    GetId:char;             override;
        procedure   DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure   Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure   ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function    GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        function    SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; override;
        function    GetTextAttrib:TextAttrib; override;
        Function    IsSimilarTo(D: DrawPrimitive): Boolean; Override;
        Procedure   ClearThis(Deallocate: Boolean); Override;
     end;

     PolyLinePrimitive = class(DrawPrimitive)
     public
        // Standard attributes
        fillcolor:TColor;
        points:PCoordArray;
        count:integer;
        style:StyleAttrib;

        // Fractal attributes
        seed,roughness:integer;
        fractal:boolean;

        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        Procedure ClearThis(Deallocate: Boolean); Override;
        constructor CreateBlank(frac:boolean);
        // Standard constructor
        constructor Create(lp:PCoordArray; c:integer; istyle:StyleAttrib); overload;
        // Fractal constructor
        constructor Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib); overload;
        // Dual constructor
        constructor Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib; frac:boolean); overload;

        function  IsClosed:boolean; override;
        procedure CloseFigure; override;
        function  Inside(x,y:Coord):boolean;
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        procedure Draw(View:ViewPoint); override;
        procedure DrawHandles(const View:ViewPoint); override;
        procedure DrawOverlayHandles(const View:ViewPoint); override;
        function  ApplyMatrix(var mat:Matrix):boolean; override;
        function  FindHandle(const View:ViewPoint; x,y:Coord):boolean; override;
        function FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean; override;
        Function FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean; Override; // JD 8-3-02
        function  MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        function  SetStyle(new_style:StyleAttrib):boolean; override;
        function  GetStyle:StyleAttrib;  override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        procedure PointClosestTo(x,y:Coord; var px,py:Coord); override;
        function  SetFillColor(color:TColor):boolean; override;
        function  GetFillColor:TColor;  override;
        function GetLines(const View:Viewpoint; var polycount:integer):PCoordArray; override;

        function SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean; override;
        function FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean; override;
        function SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean; override;
        procedure DeleteNode(const View:ViewPoint; index:integer); override;
        procedure Reverse; override;
        function SetFractal(state: FractalState): boolean; override;

        function  SetSeed(new_seed:integer):boolean; override;
        function  GetSeed:integer;  override;
        function  SetRoughness(rough:integer):boolean; override;
        function  GetRoughness:integer;  override;

        function  GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
     end;

     GroupPrimitive = class(DrawPrimitive)
     private
        Head : DrawPrimitive;
     public
        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        Procedure  SetMap(M: TObject); Override;
        Procedure  AddToBaseOrCopies(DoChain: Boolean = False); Override;
        constructor CreateBlank;
        constructor Create(starting_head:DrawPrimitive);
        Procedure ClearThis(Deallocate: Boolean); Override;
        function Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean=true):boolean; override;
        property  First:DrawPrimitive read head write head;
        procedure Draw(View:ViewPoint); override;
        function  ApplyMatrix(var mat:Matrix):boolean; override;
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        procedure Move(dx,dy:Coord); override;
        function  SetStyle(new_style:StyleAttrib):boolean; override;
        function  GetStyle:StyleAttrib;  override;
        function  SetSeed(new_seed:integer):boolean; override;
        function  GetSeed:integer;  override;
        function  SetRoughness(rough:integer):boolean; override;
        function  GetRoughness:integer;  override;
        function  SetColor(color:TColor):boolean; override;
        function  GetColor:TColor;  override;
        function  SetOverlay(overlay:byte):boolean; override;
        function  GetOverlay:integer;  override;
        function  SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; override;
        function  GetTextAttrib:TextAttrib; override;
        function  SetFillColor(color:TColor):boolean; override;
        function  GetFillColor:TColor;  override;
        procedure ComputeExtent; override;
        function SelectClick(const within:double; p:CoordPoint):boolean; override;
        procedure Reverse; override;
        function SetFractal(state: FractalState): boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
     end;

     BitmapPrimitive = class(DrawPrimitive)
     private
        image:TBitmap;
        corners:CoordRect;
     public
        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        Procedure ClearThis(Deallocate: Boolean); Override;
        constructor CreateBlank;
        constructor Create(const start:TBitmap; InitialRect:CoordRect);
        procedure Draw(View:ViewPoint); override;
        function  ApplyMatrix(var mat:Matrix):boolean; override;
        function MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean; override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        function SelectClick(const within:double; p:CoordPoint):boolean; override;
        function  SetFillColor(color:TColor):boolean; override;
        function  GetFillColor:TColor;  override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
     end;

     HyperlinkPrimitive = class(DrawPrimitive)
     private
        x1,y1:Coord;
        flags:THyperlinkFlags;
        text:string;
     public
        function Copy:DrawPrimitive; override;
        Procedure CopyFromBase(AliasOnly: Boolean); Override;
        constructor CreateBlank;
        constructor Create(const x,y:Coord; str:string; flag:THyperlinkFlags);
        procedure Draw(View:ViewPoint); override;
        function  ApplyMatrix(var mat:Matrix):boolean; override;
        procedure Move(dx,dy:Coord); override;
        procedure ComputeExtent; override;
        function SelectClick(const within:double; p:CoordPoint):boolean; override;
        function GetTextAttrib:TextAttrib; override;
        function SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean; override;
        function FindHyperlink(const View:ViewPoint; x,y:Coord; var hypertext:string; var hyperflags:THyperlinkFlags):boolean; override;

        function GetId:char;             override;
        procedure DoRead(stream:TStream; version:integer; Full,UseAliasInfo: Boolean);  override;
        procedure Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX: Coord = 0; AddY: Coord = 0); override;
        Procedure ReadFromDOMElement(E: TDOMElement; Version: Integer);  Override;
        Function  GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement; Override;
        Function  IsSimilarTo(D: DrawPrimitive): Boolean; Override;
        Function  GetAdjustedX1: Coord;
        Function  GetAdjustedY1: Coord;
     end;

     procedure StartNewHandleDraw;
     procedure ReadOverlayColors;
     function CombineObjects(o1,o2:DrawPrimitive):DrawPrimitive;
     function ConvertObject(obj:DrawPrimitive; newtype:char):DrawPrimitive;
     function AdjustToPrinterPage(ViewRect:CoordRect):CoordRect;

var OverlayMainColor:array [0..29] of TColor;
    OverlayFillColor:array [0..29] of TColor;
    HyperlinkBullet:TBitmap;

implementation

uses MapObject,Main,TextSpecialties,SettingsDialog, Printers, ColorButton, StreamUtil, LocalizedStrings, Bezier;

var LastHandleX,LastHandleY:Coord;
    FillPalette:HPalette;

Function XMLIDFromCharID(C: Char): String;
Begin
  Case C Of
    'L','l': Result := 'LINE';
    'C','c': Result := 'CURVE';
    'P','p': Result := 'POLYLINE';
    'K','k': Result := 'POLYCURVE';

    'S': Result := 'SYMBOL';
    'T': Result := 'TEXT';
    't': Result := 'TEXTCURVE';
    'G': Result := 'GROUP';
    'B': Result := 'BITMAP';
    'H': Result := 'HYPERLINK';
  Else
    Result := 'UNKNOWN';
  End; // Case
End; // XMLIDFromCharID

Function CharIDFromXMLID(S: String; Fractal: Boolean): Char;
Var C: Char;
Begin
       If S = 'LINE'      Then C := 'L'
  Else If S = 'CURVE'     Then C := 'C'
  Else If S = 'POLYLINE'  Then C := 'P'
  Else If S = 'POLYCURVE' Then C := 'K'
  Else If S = 'SYMBOL'    Then C := 'S'
  Else If S = 'TEXT'      Then C := 'T'
  Else If S = 'TEXTCURVE' Then C := 't'
  Else If S = 'GROUP'     Then C := 'G'
  Else If S = 'BITMAP'    Then C := 'B'
  Else If S = 'HYPERLINK' Then C := 'H'
  Else C := #0;
  If Fractal Then
  Begin
    Case C Of
      'L': C := 'l';
      'C': C := 'c';
      'P': C := 'p';
      'K': C := 'k';
    End; // Case
  End;
  Result := C;
End; // CharIDFromXMLID

procedure ReadOverlayColors;
var i:integer;
    b:TBitmap;
begin
  b:=TBitmap.Create;
  for i:=0 to 29 do begin
    MainForm.OverlayColors.GetBitmap(i,b);
    OverlayMainColor[i]:=b.Canvas.Pixels[0,0];
    OverlayFillColor[i]:=b.Canvas.Pixels[1,1];
    end;
  b.Free;
end;

{ ------------------------------------------------------------ }

function AdjustToPrinterPage(ViewRect:CoordRect):CoordRect;
var vr:CoordRect;
    vw,vh:Coord;
    printerpix,screenpix,adjust:Double;
begin
  vr:=ViewRect;
  vw:=vr.Right-vr.Left;
  vh:=vr.Bottom-vr.Top;

  screenpix:=vw/vh;
  printerpix:=Printer.PageWidth/Printer.PageHeight;

  if (round(screenpix*100000)<>round(printerpix*100000)) then begin
    if (screenpix<printerpix) then begin {Coordinate Width < Screen Width }
      adjust:=0.5*vw*printerpix/screenpix;
      vr.left := (ViewRect.left+ViewRect.right)/2 - adjust;
      vr.right:= (ViewRect.left+ViewRect.right)/2 + adjust;
      end
    else if (screenpix>printerpix) then begin {Coordinate Height < Screen Height }
      adjust:=0.5*vh*screenpix/printerpix;
      vr.top   := (ViewRect.top+ViewRect.bottom)/2 - adjust;
      vr.bottom:= (ViewRect.top+ViewRect.bottom)/2 + adjust;
      end;
    end;

  Result:=vr;
end;

constructor ViewPoint.Create;
begin
  inherited Create;
  Name:='';
  VisibleOverlays:=[];
  ActiveOverlays:=[];
  Canvas:=nil;
  Grid:=GridObject.Create;
  QuickDraw:=QuickDraw_None;
  OffScreenFullDetail:=false;
end;

constructor ViewPoint.Create(rect:CoordRect; cw,ch:integer);
begin
  inherited Create;
  ClientWidth:=cw;
  ClientHeight:=ch;
  Area:=rect;
  Name:='';
  VisibleOverlays:=[];
  ActiveOverlays:=[];
  Canvas:=nil;
  Grid:=GridObject.Create;
  QuickDraw:=QuickDraw_None;
end;

constructor ViewPoint.Create(rect:CoordRect; cw,ch:integer; oldgrid:GridObject);
begin
  inherited Create;
  ClientWidth:=cw;
  ClientHeight:=ch;
  Area:=rect;
  Name:='';
  VisibleOverlays:=[];
  ActiveOverlays:=[];
  Canvas:=nil;
  Grid:=GridObject.Create(oldgrid);
  QuickDraw:=QuickDraw_None;
end;

constructor ViewPoint.Create(view:ViewPoint; useprinter:boolean);
begin
  inherited Create;
  Name:=view.Name;

  if useprinter then begin
     Area:=AdjustToPrinterPage(view.Area);
     ClientWidth:=Printer.PageWidth;
     ClientHeight:=Printer.PageHeight;
     Canvas := Printer.Canvas;
     end
  else begin
     Area:=view.Area;
     ClientWidth:=view.ClientWidth;
     ClientHeight:=view.ClientHeight;
     Canvas:=view.Canvas;
     end;

  VisibleOverlays:=view.VisibleOverlays;
  ActiveOverlays:=view.ActiveOverlays;
  SetCoordinateSize(ClientWidth, ClientHeight, false);
  Grid:=GridObject.Create(view.Grid);
  QuickDraw:=QuickDraw_None;
end;

destructor ViewPoint.Destroy;
begin
  Grid.Free;
  inherited Destroy;
end;

Function ViewPoint.GetAsDOMElement(D: TDOMDocument): TDOMElement;
Var
  E : TDOMElement;
  I : Integer;
  S : String;

Begin
  E := D.createElement('VIEWPOINT');
  E.appendChild(NewStringProperty(D,'NAME',MimeEncodeString(Name)));
  E.appendChild(NewIntegerProperty(D,'CLIENTWIDTH',ClientWidth));
  E.appendChild(NewIntegerProperty(D,'CLIENTHEIGHT',ClientHeight));
  E.appendChild(NewCoordRectProperty(D,'AREA',Area));
  S := '';
  For I := 0 To 255 Do
  Begin
    If I In VisibleOverlays Then
    Begin
      If S <> '' Then S := S + ',';
      S := S + IntToStr(I);
    End;
  End; // For I
  E.appendChild(NewStringProperty(D,'VISIBLE_OVERLAYS',S));
  S := '';
  For I := 0 To 255 Do
  Begin
    If I In ActiveOverlays Then
    Begin
      If S <> '' Then S := S + ',';
      S := S + IntToStr(I);
    End;
  End; // For I
  E.appendChild(NewStringProperty(D,'ACTIVE_OVERLAYS',S));
  E.appendChild(Grid.GetAsDOMElement(D));
  Result := E;
End; // ViewPoint.GetAsDOMElement

Procedure ViewPoint.LoadFromDOMElement(E: TDOMElement);
Var
  S   : String;
  St  : String;
  I,J : Integer;
  E1  : TDOMElement;

Begin
  Name         := MimeDecodeString(Trim(GetStringProperty(E,'NAME')));
  ClientWidth  := GetIntegerProperty(E,'CLIENTWIDTH');
  ClientHeight := GetIntegerProperty(E,'CLIENTHEIGHT');
  Area         := GetCoordRectProperty(E,'AREA');
  S            := Trim(GetStringProperty(E,'VISIBLE_OVERLAYS'));
  VisibleOverlays := [];
  While S <> '' Do
  Begin
    I := Pos(',',S);
    If I <> 0 Then
    Begin
      St := Trim(Copy(S,1,I - 1));
      S  := Trim(Copy(S,I + 1,Length(S)));
    End
    Else
    Begin
      St := S;
      S  := '';
    End;
    Val(St,I,J);
    If (J = 0) And (I >= 0) And (I < 256) Then VisibleOverlays := VisibleOverlays + [I]; 
  End; // While
  S            := Trim(GetStringProperty(E,'ACTIVE_OVERLAYS'));
  ActiveOverlays := [];
  While S <> '' Do
  Begin
    I := Pos(',',S);
    If I <> 0 Then
    Begin
      St := Trim(Copy(S,1,I - 1));
      S  := Trim(Copy(S,I + 1,Length(S)));
    End
    Else
    Begin
      St := S;
      S  := '';
    End;
    Val(St,I,J);
    If (J = 0) And (I >= 0) And (I < 256) Then ActiveOverlays := ActiveOverlays + [I]; 
  End; // While
  E1 := E.getFirstChildElement('GRIDOBJECT');
  If E1 <> Nil Then Grid.LoadFromDOMElement(E1);
End; // ViewPoint.LoadFromDOMElement

procedure ViewPoint.SaveToStream(Stream:TStream);
begin
  WriteStringToStream(Stream, Name);
  stream.WriteBuffer(ClientWidth, sizeof(ClientWidth));
  stream.WriteBuffer(ClientHeight, sizeof(ClientHeight));
  stream.WriteBuffer(Area, sizeof(Area));
  stream.WriteBuffer(VisibleOverlays, sizeof(VisibleOverlays));
  stream.WriteBuffer(ActiveOverlays, sizeof(ActiveOverlays));
  Grid.SaveToStream(stream, 0);
end;

procedure ViewPoint.LoadFromStream(Stream:TStream);
begin
  Name:=ReadStringFromStream(Stream);
  stream.ReadBuffer(ClientWidth, sizeof(ClientWidth));
  stream.ReadBuffer(ClientHeight, sizeof(ClientHeight));
  stream.ReadBuffer(Area, sizeof(Area));
  stream.ReadBuffer(VisibleOverlays, sizeof(VisibleOverlays));
  stream.ReadBuffer(ActiveOverlays, sizeof(ActiveOverlays));
  Grid.LoadFromStream(stream, 0);
end;

procedure ViewPoint.CoordToScreen(cx,cy:Coord; var sx,sy:Coord);
begin
  sx := ((cx-Area.Left)/(Area.right-Area.left))*ClientWidth;
  sy := ((cy-Area.Top)/(Area.bottom-Area.top))*ClientHeight;
end;

procedure ViewPoint.CoordToScreen(cx,cy:Coord; var sx,sy:integer);
var tempx,tempy:int64;
    fsx,fsy:Coord;
begin
  CoordToScreen(cx,cy, fsx, fsy);
  tempx := trunc(fsx);
  tempy := trunc(fsy);

  if (tempx > 32767) then
    sx:=32767
  else if (tempx < -32768) then
    sx:=-32768
  else sx:=tempx;

  if (tempy > 32767) then
    sy:=32767
  else if (tempy < -32768) then
    sy:=-32768
  else sy:=tempy;
end;

procedure ViewPoint.ScreenToCoord(sx,sy:integer; var cx,cy:Coord);
begin
  ScreenToCoord(1.0*sx,1.0*sy,cx,cy);
end;

procedure ViewPoint.ScreenToCoord(sx,sy:Coord; var cx,cy:Coord);
begin
  cx := Area.Left + (sx/ClientWidth)*(Area.right-Area.left);
  cy := Area.Top +  (sy/ClientHeight)*(Area.bottom-Area.top);
end;

procedure ViewPoint.DeltaScreenToCoord(dx,dy: integer; var cx,cy:Coord);
begin
  cx := (dx/ClientWidth)*(Area.right-Area.left);
  cy := (dy/ClientHeight)*(Area.bottom-Area.top);
end;

procedure ViewPoint.DeltaScreenToCoord(dx,dy: Coord; var cx,cy:Coord);
begin
  cx := (dx/ClientWidth)*(Area.right-Area.left);
  cy := (dy/ClientHeight)*(Area.bottom-Area.top);
end;

procedure ViewPoint.DeltaCoordToScreen(cx,cy:Coord; var dx,dy:integer);
begin
  dx := trunc((cx/(Area.right-Area.left))*ClientWidth);
  dy := trunc((cy/(Area.bottom-Area.top))*ClientHeight);
end;

procedure ViewPoint.DeltaCoordToScreen(cx,cy:Coord; var dx,dy:Coord);
begin
  dx := (cx/(Area.right-Area.left))*ClientWidth;
  dy := (cy/(Area.bottom-Area.top))*ClientHeight;
end;

function ViewPoint.CoordToScreenRect(crect:CoordRect):TRect;
begin
  CoordToScreen(crect.left,crect.top,Result.left,Result.top);
  CoordToScreen(crect.right,crect.bottom,Result.right,Result.bottom);
end;

function ViewPoint.ScreenToCoordRect(rect: TRect): CoordRect;
begin
  ScreenToCoord(rect.left,rect.top,Result.left,Result.top);
  ScreenToCoord(rect.right,rect.bottom,Result.right,Result.bottom);
end;

function ViewPoint.ScreenToCoordRect(rect: CoordRect): CoordRect;
begin
  ScreenToCoord(rect.left,rect.top,Result.left,Result.top);
  ScreenToCoord(rect.right,rect.bottom,Result.right,Result.bottom);
end;

function ViewPoint.CoordToScreenPt(p:CoordPoint):CoordPoint;
var x,y:integer;
begin
  CoordToScreen(p.x,p.y,x,y);
  Result.x:=x;
  Result.y:=y;
end;

procedure ViewPoint.ScreenToCoordPtArray(p:pCoordArray; Count:integer);
var i:integer;
begin
  for i:=0 to Count-1 do ScreenToCoord(trunc(p[i].x),trunc(p[i].y),p[i].x,p[i].y);
end;

procedure ViewPoint.SetCoordinateSize(width,height:integer; rescale:boolean);
var r:CoordRect;
begin
  if (ClientWidth<>0) and (ClientHeight<>0) and rescale then begin
    Area.Bottom := Area.Bottom*height/ClientHeight;
    Area.Right  := Area.Right*width/ClientWidth;
    end;

  if (width<>ClientWidth) or (height<>ClientHeight) then begin
    ClientWidth := width;
    ClientHeight:= height;

    GetCoordinateRect(r);
    SetCoordinateRect(r);
    end;
end;

procedure ViewPoint.GetCoordinateSize(var width,height:integer);
begin
  width:=ClientWidth;
  height:=ClientHeight;
end;

procedure ViewPoint.SetCoordinateRect(cr:CoordRect);
var width,height:Coord;
    screen,pix,adjust:Coord;
begin
  { If they zoom too close, refuse to do anything }
  if (abs(cr.bottom-cr.top)<=0.01) or
     (abs(cr.right-cr.left)<=0.01) then begin

     // If the existing area is "null", fix it up so we don't create
     // div 0 errors.  Otherwise, leave the area alone if we got here
     // by repeated zoom-in commands.
     if (Area.Right-Area.Left = 0.0) or
        (Area.Bottom-Area.Top = 0.0) then begin
       Area.Left := 0;
       Area.Right := 1;       // Set a coordinate space of (0,0)-(1,1)
       Area.Top := 0;         // Preventative measure for invalid
       Area.Bottom:= 1;       // operations on zero width/height viewpoints.
       end;

     exit;
     end;

  Area:=cr;

  width:=cr.right-cr.left;
  height:=cr.bottom-cr.top;

//  if (width=0) and (height=0) then exit;

  screen:=width/height;
  pix:=ClientWidth/ClientHeight;

  if (round(screen*100000)=round(pix*100000)) then exit;

  if (screen<pix) then begin {Coordinate Width < Screen Width }
    adjust:=0.5*width*pix/screen;
    Area.left := (cr.left+cr.right)/2 - adjust;
    Area.right:= (cr.left+cr.right)/2 + adjust;
    end
  else if (screen>pix) then begin {Coordinate Height < Screen Height }
    adjust:=0.5*height*screen/pix;
    Area.top   := (cr.top+cr.bottom)/2 - adjust;
    Area.bottom:= (cr.top+cr.bottom)/2 + adjust;
    end;

end;

procedure ViewPoint.GetCoordinateRect(var cr:CoordRect);
begin
  cr:=Area;
end;

function ViewPoint.FixStyle(st:StyleAttrib):StyleAttrib;
begin
  Result:=st;
  if ((QuickDraw and QuickDraw_Lines)<>0) and
     ((GetLineThickness(st)>1) or IsAComplexLine(st))
  then Result.Line:=1;
end;

procedure ViewPoint.Zoom(center:CoordPoint; factor:single; px,py:Coord);
var w,h:Coord;
    ScreenCoord:CoordRect;
begin
  GetCoordinateRect(ScreenCoord);
  w:=ScreenCoord.right-ScreenCoord.Left;
  h:=ScreenCoord.bottom-ScreenCoord.Top;

  w := w * factor;
  h := h * factor;

  ScreenCoord.left := center.x - w*px;
  ScreenCoord.right:= center.x + w*(1-px);
  ScreenCoord.top  := center.y - h*py;
  ScreenCoord.bottom:= center.y + h*(1-py);

  SetCoordinateRect(ScreenCoord);
end;

procedure ViewPoint.SetZoomPercent(percent:double);
var cr:CoordRect;
    center:CoordPoint;
    width,height:integer;
begin
    // A Zoom level of 100% has a one-to-one relationship between pixels and coordinates.
    // Center the zoom region on the current screen center.
    GetCoordinateRect(cr);
    center.X := (cr.Right + cr.Left) / 2;
    center.Y := (cr.Bottom + cr.Top) / 2;
    GetCoordinateSize(width,height);

    // More useful to use a real scalar instead of percent.
    // The amount we scale is also reciprocal to the percentage (i.e.
    // bigger percents are larger zooms, which are correspondingly smaller
    // coordinate sizes).
    percent := 1.0 / (percent * 0.01);

    cr.Left := center.X - width*percent*0.5;
    cr.Right := center.X + width*percent*0.5;
    cr.Top := center.Y - height*percent*0.5;
    cr.Bottom := center.Y + height*percent*0.5;

    SetCoordinateRect(cr);
end;

function ViewPoint.GetZoomPercent:integer;
var cx,cy:Coord;
    zoom:single;
    z:integer;
begin
  // Figure out our zoom by looking at how big a single pixel is
  DeltaScreenToCoord(1,1,cx,cy);

  zoom := 100.0/cx;   // 100.0 * Reciprocal = percentage
  z:=Round(zoom);     // Round: don't show fractional zooms

  // Don't allow our displayed zoom to drop below 1% to 0%
  if (z < 1) then z := 1;
  Result:=z;
end;

{ ----------------------------------------------------------------- }

function GetThickLines(Const Points: PCoordArray; Var Count: Integer;
                       Const Style: StyleAttrib;
                       GetAverageSlope,ThickEnds,Closed: Boolean): PCoordArray;
// ------------------------------------------------------------------------
// For an array of coordinates, this routine calculates a new set of
// coordinates that form an "envelope" around the original coordinates.
// The purpose of this routine is to allow rendering lines and curves
// where the width is a fixed amount (in map coordinates).  Such objects
// always maintain a constant width relative to their size.
//
// Input:
//
//   Points ............ The original point list
//   Count ............. Contains the number of points in the original list
//   Style ............. The object's style (this now contains the optional
//                       numeric line width as well)
//   GetAverageSlope ... Used for curves and polycurves, this makes sure
//                       that the resulting "envelope" is smooth
//   ThickEnds ......... Not used for now, this extends the endpoints by
//                       half the line width
//   Closed ............ If the polycurve is closed, this allows the
//                       original points to "wrap", and is necessary for
//                       GetAverageAngle to get the correct slope at the
//                       ends of the closed polycurve.
//
// Output:
//
//   Count ............. Returns the number of points in the new list
//   Function Result ... Returns a list of points that form an "envelope"
//                       around the original object, with the specified
//                       numerical line width.  This represents a single
//                       polygon to be filled.
// ------------------------------------------------------------------------
Var
  Total       : Integer;
  I,K         : Integer;
  ThickPoints : Array Of CoordPoint;
  X1,Y1       : Coord;
  X2,Y2       : Coord;
  X3,Y3       : Coord;
  Angle1      : Double;
  Angle2      : Double;
  Angle4      : Double;
  Thickness   : Double;
  XOfs1,YOfs1 : Double;
  XOfs2,YOfs2 : Double;
  NewPoints   : PCoordArray;

  Function GetAverageAngle(I,Start,Last,Step: Integer): Double;
  // ------------------------------------------------------------------------
  // Starting at the line segment represented by points at I and I + Step,
  // this routine tries to get the average angle by looking at the previous
  // two and next two line segments if possible.
  //
  // Input:
  //
  //   I .............. The point index
  //   Start .......... The index of the first point in the list.  For the
  //                    first side of the "envelope" this should be 0, and
  //                    for the next side it should be count - 1.
  //   Last ........... The index of the last point in the list.  For the
  //                    first side of the "envelope" this should be count - 1,
  //                    and for the next side it should be 0.
  //   Step ........... This should be 1 for the first side of the
  //                    "envelope" and -1 for the next side.
  //
  // Output:
  //
  //   Returns the average angle (which represents the average slope of the
  //   line segment) in radians.
  // ------------------------------------------------------------------------
  Var
    J     : Integer;
    I0,I1 : Integer;
    X,Y   : Coord;
    X1,Y1 : Coord;
    X2,Y2 : Coord;
    Angle : Double;

  Begin
    I0 := I - 2 * Step;
    If Step > 0 Then
    Begin
      If Closed Then
      Begin
        If I0 < Start Then I0 := Last + Step - (Start - I0);
        I1 := I0 + 5 * Step;
        If I1 > Last + Step Then I1 := Start + (I1 - (Last + Step));
      End
      Else
      Begin
        If I0 < Start Then I0 := Start;
        I1 := I0 + 5 * Step;
        If I1 > Last + Step Then I1 := Last + Step;
      End;
    End
    Else
    Begin
      If Closed Then
      Begin
        If I0 > Start Then I0 := Last + Step + (I0 - Start);
        I1 := I0 + 5 * Step;
        If I1 < Last + Step Then I1 := Start - ((Last + Step) - I1);
      End
      Else
      Begin
        If I0 > Start Then I0 := Start;
        I1 := I0 + 5 * Step;
        If I1 < Last + Step Then I1 := Last + Step;
      End;
    End;
    I     := I0;
    Angle := 0;
    X     := 0;
    Y     := 0;
    While (I <> I1) And (I + Step <> I1) Do
    Begin
      J  := I + Step;
      If Closed And (J = Last + Step) Then J := Start;
      X1 := Points^[I].X;
      Y1 := Points^[I].Y;
      X2 := Points^[J].X;
      Y2 := Points^[J].Y;
      X  := X + (X2 - X1);
      Y  := Y + (Y2 - Y1);
      I  := J;
    End; // While
    If (X <> 0) Or (Y <> 0) Then Angle := ArcTan2(Y,X);
    Result := Angle;
  End; // GetAverageAngle

  Procedure Calc(Var I: Integer; Start,Last,Step: Integer);
  // ------------------------------------------------------------------------
  // For a given point whose index is I, this routine calculates the next
  // appropriate point in the object's "envelope" and adds it to the point
  // list.
  //
  // Input:
  //
  //   I .............. The point index
  //   Start .......... The index of the first point in the list.  For the
  //                    first side of the "envelope" this should be 0, and
  //                    for the next side it should be count - 1.
  //   Last ........... The index of the last point in the list.  For the
  //                    first side of the "envelope" this should be count - 1,
  //                    and for the next side it should be 0.
  //   Step ........... This should be 1 for the first side of the
  //                    "envelope" and -1 for the next side.
  // ------------------------------------------------------------------------
  Var Scale1,Scale2: Double;
  Begin
    X1 := Points^[I].X;
    Y1 := Points^[I].Y;
    If (I <> Last) Then
    Begin
      X2     := Points^[I + Step].X;
      Y2     := Points^[I + Step].Y;
      Angle1 := ArcTan2(Y2 - Y1, X2 - X1);
    End
    Else
    Begin
      X2     := X1;
      Y2     := Y1;
      Angle1 := 0;
    End;
    If (I <> Last) And (I + Step <> Last) Then
    Begin
      X3     := Points^[I + Step * 2].X;
      Y3     := Points^[I + Step * 2].Y;
      Angle2 := ArcTan2(Y3 - Y2, X3 - X2);
    End
    Else
    Begin
      X3     := X2;
      Y3     := Y2;
      Angle2 := Angle1;
    End;

    If GetAverageSlope Then
    Begin
      Angle1 := GetAverageAngle(I,Start,Last,Step);
      Angle2 := GetAverageAngle(I + Step,Start,Last,Step);
    End;

    If ThickEnds Then
    Begin
      If Closed Then
      Begin
        XOfs1 := Thickness * Cos(Angle1 - 3 * (Pi / 4)) * Sqrt(2);
        YOfs1 := Thickness * Sin(Angle1 - 3 * (Pi / 4)) * Sqrt(2);
        XOfs2 := Thickness * Cos(Angle2 -     (Pi / 4)) * Sqrt(2);
        YOfs2 := Thickness * Sin(Angle2 -     (Pi / 4)) * Sqrt(2);
      End
      Else
      Begin
        XOfs1 := Thickness * Cos(Angle1 - 3 * (Pi / 4)) * Sqrt(2);
        YOfs1 := Thickness * Sin(Angle1 - 3 * (Pi / 4)) * Sqrt(2);
        XOfs2 := Thickness * Cos(Angle1 -     (Pi / 4)) * Sqrt(2);
        YOfs2 := Thickness * Sin(Angle1 -     (Pi / 4)) * Sqrt(2);
      End;
    End
    Else
    Begin
      If Closed Then
      Begin
        XOfs1 := Thickness * Cos(Angle1 - (Pi / 2));
        YOfs1 := Thickness * Sin(Angle1 - (Pi / 2));
        XOfs2 := Thickness * Cos(Angle2 - (Pi / 2));
        YOfs2 := Thickness * Sin(Angle2 - (Pi / 2));
      End
      Else
      Begin
        XOfs1 := Thickness * Cos(Angle1 - (Pi / 2));
        YOfs1 := Thickness * Sin(Angle1 - (Pi / 2));
        XOfs2 := Thickness * Cos(Angle1 - (Pi / 2));
        YOfs2 := Thickness * Sin(Angle1 - (Pi / 2));
      End;
    End;

    // Handle the very first point

    If I = Start Then
    Begin
      ThickPoints[K].X := X1 + XOfs1;
      ThickPoints[K].Y := Y1 + YOfs1;
      Inc(K);
    End;

    // The last point is a special case

    If (I = Last) Or (I = Last - Step) Then
    Begin
      ThickPoints[K].X := X2 + XOfs2;
      ThickPoints[K].Y := Y2 + YOfs2;
      Inc(K);
      I := Last + Step; // We're done: push the point index past the end
    End
    Else
    Begin
      // Important to force the angles positive or the averaging doesn't
      // work right
      
      While Angle1 < 0 Do Angle1 := Angle1 + 2 * Pi;
      While Angle2 < 0 Do Angle2 := Angle2 + 2 * Pi;

      // We form the envelope by first getting the average angle between
      // the two segments (note that their angles might in fact be the average
      // angle in their local area).

      Angle4 := (Angle1 + Angle2) / 2;

      // Because the angles will differ, we have to rescale the thickness to
      // to keep the overall thickness constant.

      Scale1 := Cos(Angle4 - Angle1);
      Scale2 := Cos(Angle4 - Angle2);
      If Scale2 < Scale1 Then Scale1 := Scale2;
      If Scale1 <> 0 Then Scale1 := 1 / Scale1 Else Scale1 := 1;

      // Calculate the envelope point and add it to the list

      ThickPoints[K].X := X2 + Thickness * Cos(Angle4 - (Pi / 2)) * Scale1;
      ThickPoints[K].Y := Y2 + Thickness * Sin(Angle4 - (Pi / 2)) * Scale1;
      Inc(K);      // Increment the point counter
      Inc(I,Step); // Go to the next point index
    End;
  End; // Calc

begin
  // It's more efficient to pre-allocate the maximum possible number of points
  // and then shrink the array to the actual number of points once we're done

  Total     := (Count - 2) * 4 + 4; // Maximum number of points
  SetLength(ThickPoints,Total);
  Thickness := Style.FullStyle.SThickness / 2; // Half-thickness on each side
  K         := 0; // Counter for the number of points actually created

  // Form one-half of the polygon by traversing the points

  I := 0;
  While I < Count Do Calc(I,0,Count - 1,1);

  // Now traverse in reverse order to finish the polygon

  I := Count - 1;
  While I >= 0 Do Calc(I,Count - 1,0,-1);

  // Make the output array, shrinking it to the number of points
  // actually created

  GetMem(NewPoints,SizeOf(CoordPoint) * K);
  For I := 0 To K - 1 Do NewPoints^[I] := ThickPoints[I];

  // Cleanup

  SetLength(ThickPoints,0);
  Count  := K;
  Result := NewPoints;
End; // GetThickLines

procedure DrawEnclosedFigure(Canvas:TCanvas; points:PCoordArray; count:integer; closed:boolean;
                             Var style:StyleAttrib; EdgeColor, FillColor:TColor; View: ViewPoint;
                             GetAverageSlope: Boolean);
var sx1,sy1,sx2,sy2:integer;
    oldcolor:TColor;
    oldbrushcolor:TColor;
    I,K : integer;
    continue:TLineContinue;
    poly:PPointArray;
    brushbitmap:TBitmap;
    BrushHandle:HGDIOBJ;
    IsPrinter:boolean;

    T1,T2         : Coord;
    ThickPoints   : Array Of TPoint;
    NewPoints     : PCoordArray;
    OldBrushStyle : TBrushStyle;
    OldPenWidth   : Integer;
    Total         : Integer;
    NewStyle      : StyleAttrib;

  function CreatePaletteBrush:HGDIOBJ;
  type MyBrush=packed record
         header:TBitmapInfoHeader;
         palette:array[0..1] of WORD;
         bits:array[0..7] of DWORD;
       end;
  var PackedDIB:MyBrush;
      i:integer;
  begin
    with PackedDib do begin
      header.biSize := sizeof(header);
      header.biWidth:=8;
      header.biHeight:=8;
      header.biPlanes:=1;
      header.biBitCount:=1;
      header.biCompression:=BI_RGB;
      header.biSizeImage:=0;
      header.biXPelsPerMeter:=8;
      header.biYPelsPerMeter:=8;
      header.biClrUsed:=2;
      header.biClrImportant:=0;
      palette[0]:=GetNearestPaletteIndex(FillPalette,EdgeColor);
      palette[1]:=GetNearestPaletteIndex(FillPalette,FillColor);
      for i:=0 to 7 do
        bits[7-i]:=PBYTE(brushbitmap.ScanLine[i])^;
      end;
    Result:=CreateDIBPatternBrushPt(@PackedDIB,DIB_PAL_COLORS);
  end;

  function CreateHiresBrush:HGDIOBJ;
  type MyBrush=packed record
         header:TBitmapInfoHeader;
         palette:array[0..1] of DWORD;
         bits:array[0..7] of DWORD;
       end;
  var PackedDIB:MyBrush;
      i:integer;
    function ColorToRGBQuad(c:TColor):DWORD;
    begin
      Result:=((c and $FF) shl 16) or
               (c and $FF00) or
              ((c and $FF0000) shr 16);
    end;
  begin
    with PackedDib do begin
      header.biSize := sizeof(header);
      header.biWidth:=8;
      header.biHeight:=8;
      header.biPlanes:=1;
      header.biBitCount:=1;
      header.biCompression:=BI_RGB;
      header.biSizeImage:=0;
      header.biXPelsPerMeter:=8;
      header.biYPelsPerMeter:=8;
      header.biClrUsed:=2;
      header.biClrImportant:=0;
      palette[0]:=ColorToRGBQuad(EdgeColor);
      palette[1]:=ColorToRGBQuad(FillColor);
      for i:=0 to 7 do
        bits[7-i]:=PBYTE(brushbitmap.ScanLine[i])^;
      end;
    Result:=CreateDIBPatternBrushPt(@PackedDIB,DIB_RGB_COLORS);
  end;

begin
  if (count=0) then exit;
  GetMem(poly,count*sizeof(TPoint));

  for i:=0 to count-1 do begin
    poly^[i].X:=trunc(points^[i].X);
    poly^[i].Y:=trunc(points^[i].Y);
    end;
  oldcolor := Canvas.Pen.Color;

  if (FillColor<>clNone) and Closed then begin
    IsPrinter := GetDeviceCaps(Canvas.Handle,TECHNOLOGY)<>DT_RASDISPLAY;

    Canvas.Pen.Style:=psClear;
    if IsPrinter and (Style.Fill=0) then begin
      oldbrushcolor:=Canvas.Brush.Color;
      Canvas.Brush.Color := FillColor;
      Windows.Polygon(Canvas.Handle, poly^, Count);
      Canvas.Brush.Color:=oldbrushcolor
      end
    else begin
      brushbitmap:=MainForm.FillPattern.GetMonochromeBitmap(Style.Fill);
      if IsPaletteDevice then
        BrushHandle:=CreatePaletteBrush
      else
        BrushHandle:=CreateHiresBrush;

      Canvas.Brush.Handle:=BrushHandle;
      Windows.Polygon(Canvas.Handle, poly^, Count);
      brushbitmap.Free;
      Canvas.Brush.Handle:=0;
      DeleteObject(BrushHandle);
      end;

    Canvas.Pen.Style:=psSolid;
    end;

  Canvas.Pen.Color := EdgeColor;

  sx1:=poly^[0].X;
  sy1:=poly^[0].Y;

  If Style.Line <> start_numeric_thickness_style Then
  Begin
    NewStyle := View.FixStyle(Style);
    continue:=GetLineStyleStart(NewStyle);
    for i:=1 to Count-1 do begin
      sx2:=poly^[i].X;
      sy2:=poly^[i].Y;
      DrawLineContinue(Canvas,sx1,sy1,sx2,sy2,continue);
      sx1:=sx2;
      sy1:=sy2;
      end;
    GetLineEnd(continue,i);
  End
  Else
  Begin
    // This style lets us specify a hard numeric value for a line thickness. This is
    // useful for drawing things like roads, where we want the lines to grow when we
    // zoom in and shrink when we zoom out.  The way to accomplish this is to render
    // the line as a polygon, filling the entire polygon with our line color.

    OldPenWidth        := Canvas.Pen.Width;
    OldBrushColor      := Canvas.Brush.Color;
    OldBrushStyle      := Canvas.Brush.Style;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.Pen.Width   := 0;

    // It's more efficient to pre-allocate the maximum possible number of points
    // and then shrink the array to the actual number of points once we're done

    Total              := (Count - 2) * 4 + 4; // Maximum number of points
    SetLength(ThickPoints,Total);
    T1                 := Style.FullStyle.Thickness;
    T2                 := View.Grid.GraphUnitConvert * View.Grid.GraphScale;
    If T2 <> 0 Then T1 := T1 / T2;
    View.DeltaCoordToScreen(T1,0,T1,T2);
    Style.FullStyle.SThickness := T1;

    K         := Count;
    NewPoints := GetThickLines(Points,K,Style,GetAverageSlope,False,Closed);
    SetLength(ThickPoints,K);
    For I := 0 To K - 1 Do
    Begin
      ThickPoints[I].X := Round(NewPoints^[I].X);
      ThickPoints[I].Y := Round(NewPoints^[I].Y);
    End; // For I
    FreeMem(NewPoints);

    SetPolyFillMode(Canvas.Handle,WINDING);
    Canvas.Polygon(ThickPoints);
    SetPolyFillMode(Canvas.Handle,ALTERNATE);
    Canvas.Pen.Width   := OldPenWidth;
    Canvas.Brush.Color := OldBrushColor;
    Canvas.Brush.Style := OldBrushStyle;
    SetLength(ThickPoints,0);
  End;

  Canvas.Pen.Color := oldcolor;
  FreeMem(poly);
end;


function CombineObjects(o1,o2: DrawPrimitive): DrawPrimitive;
Var
  PLP,PLP1,PLP2 : PolyLinePrimitive;
  PCP,PCP1,PCP2 : PolyCurvePrimitive;

  procedure Realloc(var newpoints: PCoordArray; var newcount: integer;
                    first: PCoordArray; s1 :integer;
                    second: PCoordArray; s2 :integer);
  var
    i,n         : integer;
    upone,uptwo : boolean;

    function Match(n1,n2: integer): boolean;
    var c1x,c1y,c2x,c2y: integer;
    begin
      Map.CurrentView.CoordToScreen(first[n1].X,first[n1].Y,c1x,c1y);
      Map.CurrentView.CoordToScreen(second[n2].X,second[n2].Y,c2x,c2y);
      Result := (abs(c1x - c2x) <= 1) and (abs(c1y - c2y) <= 1);
    end; // Match

  begin
    newcount := s1 + s2 - 1;
    GetMem(NewPoints, sizeof(CoordPoint) * newcount);

    { Based on the matching endpoints, copy the stuff into the new array }
    if Match(0,0) then
    begin
      upone := true;
      uptwo := false;
    end
    else if Match(0,s2-1) then
    begin
      upone := true;
      uptwo := true;
    end
    else if Match(s1-1,0) then
    begin
      upone := false;
      uptwo := false;
    end
    else if Match(s1-1,s2-1) then
    begin
      upone := false;
      uptwo := true;
    end
    else
    begin
      raise EInvalidOperation.Create(res_primitives_combine1);
    end;

    n := 0;
    if upone then
    begin
      for i := s1 - 1 downto 0 do
      Begin
        NewPoints^[n] := first^[i];
        inc(n);
      End; // For I
    end
    else
    begin
      for i := 0 to s1 - 1 do
      Begin
        NewPoints^[n] := first^[i];
        inc(n);
      End; // For I
    end;

    if uptwo then
    begin
      for i := s2 - 2 downto 0 do
      Begin
        NewPoints^[n] := second^[i];
        inc(n);
      End; // For I
    end
    else
    begin
      for i := 1 to s2 - 1 do
      Begin
        NewPoints^[n] := second^[i];
        inc(n);
      End; // For I
    end;
  end; // Realloc

begin
  Assert(o1.GetId = o2.GetId, res_primitives_types);

  Result := o1.Copy;
  Result.SplitOff;

  case o1.GetId of
    'P','p':
    begin // Polyline & Fractal polyline
      PLP  := Result As PolyLinePrimitive;
      PLP1 := O1     As PolyLinePrimitive;
      PLP2 := O2     As PolyLinePrimitive;
      Realloc(PLP.Points,  PLP.Count,
              PLP1.Points, PLP1.Count,
              PLP2.Points, PLP2.Count);
    end;
    'K','k':
    begin // Polycurve and Fractal polycurve
      PCP  := Result As PolyCurvePrimitive;
      PCP1 := O1     As PolyCurvePrimitive;
      PCP2 := O2     As PolyCurvePrimitive;
      Realloc(PCP.Points,  PCP.Count,
              PCP1.Points, PCP1.Count,
              PCP2.Points, PCP2.Count);
    end;
    else
      raise EInvalidOperation.Create(res_primitives_combine2);
  end; // Case
  Result.ComputeExtent;
end; // CombineObjects

function ConvertObject(obj:DrawPrimitive; newtype:char):DrawPrimitive;
var pcoord:PCoordArray;
    line:LinePrimitive;
    frac:LinePrimitive;
    curve:CurvePrimitive;
    fraccurve:CurvePrimitive;
    PolyLine : PolyLinePrimitive;
    I        : Integer;

    function DoubleDecompose(obj:DrawPrimitive):DrawPrimitive;
    var objchain,nextobjchain:DrawPrimitive;
        polyobj,newpolyobj:DrawPrimitive;
    begin
      { First decompose into a series of intermediates }
      obj.Decompose(MapCollection(Obj.MapC).CurrentView,objchain);
      { Now, decompose all those intermediates into polylines and merge them}
      objchain.Decompose(MapCollection(Obj.MapC).CurrentView,Result);
      nextobjchain:=objchain.Next;
      objchain.Destroy;
      objchain:=nextobjchain;

      while (objchain<>nil) do begin
        objchain.Decompose(MapCollection(Obj.MapC).CurrentView,polyobj);
        newpolyobj:=CombineObjects(Result,polyobj);
        Result.Destroy;
        polyobj.Destroy;
        Result:=newpolyobj;

        nextobjchain:=objchain.Next;
        objchain.Destroy;
        objchain:=nextobjchain;
        end;
    end;

begin
  case obj.GetId of
    'L': begin     { LINE PRIMITIVE }
         line:=(obj as LinePrimitive);
         case newtype of
           'P': begin   { Convert a line into a polyline }
                  GetMem(pcoord,sizeof(CoordPoint)*2);
                  pcoord^[0].X:=line.x1;
                  pcoord^[0].Y:=line.y1;
                  pcoord^[1].X:=line.x2;
                  pcoord^[1].Y:=line.y2;
                  Result:=PolyLinePrimitive.Create(pcoord, 2, line.Style);
                  Result.CopyCore(obj);
                end;
           'K': begin   { Convert a line into a polycurve }
                  GetMem(pcoord,sizeof(CoordPoint)*4);
                  pcoord^[0].X:=line.x1;
                  pcoord^[0].Y:=line.y1;
                  pcoord^[3].X:=line.x2;
                  pcoord^[3].Y:=line.y2;
                  pcoord^[1] := pcoord^[0];
                  pcoord^[2] := pcoord^[3];
                  Result:=PolyCurvePrimitive.Create(pcoord, 4, line.Style);
                  Result.CopyCore(obj);
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'l': begin     { FRACTAL LINE PRIMITIVE }
         frac:=(obj as LinePrimitive);
         case newtype of
           'P': begin   { Convert a fractal line into a polyline }
                  frac.Decompose(MapCollection(Obj.MapC).CurrentView,Result);
                end;
           'p': begin   { Convert a fractal line into a poly fractal line }
                  GetMem(pcoord,sizeof(CoordPoint)*2);
                  pcoord^[0].X:=frac.x1;
                  pcoord^[0].Y:=frac.y1;
                  pcoord^[1].X:=frac.x2;
                  pcoord^[1].Y:=frac.y2;
                  Result:=PolylinePrimitive.Create(pcoord, 2, frac.seed,frac.roughness,frac.Style);
                  Result.CopyCore(obj);
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'P': begin  { POLYLINE PRIMITIVE }
           PolyLine := Obj As PolyLinePrimitive;
           { Polyline isn't converted to anything but itself }
           Case NewType Of
             'P': Result := Obj.Copy;
             'K': Begin
                    GetMem(PCoord,SizeOf(CoordPoint) * (PolyLine.Count * 3 - 2));
                    For I := 0 To PolyLine.Count - 1 Do
                    Begin
                      PCoord^[I * 3].X := PolyLine.Points^[I].X;
                      PCoord^[I * 3].Y := PolyLine.Points^[I].Y;
                      If I > 0 Then PCoord^[I * 3 - 1] := PCoord^[I * 3];
                      If I < PolyLine.Count - 1 Then PCoord^[I * 3 + 1] := PCoord^[I * 3];
                    End; // For I
                    Result := PolyCurvePrimitive.Create(PCoord, PolyLine.Count * 3 - 2, PolyLine.Style);
                    Result.CopyCore(Obj);
                  End;
           Else Raise EInvalidOperation.Create(res_primitives_convert);
           End; // Case

//           if (newtype<>'P') then raise EInvalidOperation.Create(res_primitives_convert);

//           Result:=obj.Copy;
         end;
    'p': begin  { FRACTAL POLYLINE PRIMITIVE }
         case newtype of
           'P': begin   { Convert a fractal polyline into a polyline }
                  Result:=DoubleDecompose(obj);
                end;
           'p': begin   { Convert a fractal polyline into a fractal polyline }
                  Result:=obj.Copy;
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'C': begin   { CURVE PRIMITIVE }
         curve:=(obj as curvePrimitive);
         case newtype of
           'P': begin   { Convert a curve into a polyline }
                  curve.Decompose(MapCollection(Obj.MapC).CurrentView,Result);
                end;
           'K': begin   { Convert a curve into a polycurve}
                  GetMem(pcoord,sizeof(CoordPoint)*4);
                  pcoord^[0] := curve.p1;
                  pcoord^[1] := curve.p2;
                  pcoord^[2] := curve.p3;
                  pcoord^[3] := curve.p4;
                  Result:=PolyCurvePrimitive.Create(pcoord, 4, curve.Style);
                  Result.CopyCore(obj);
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'c': begin   { FRACTAL CURVE PRIMITIVE }
         fraccurve:=(obj as CurvePrimitive);
         case newtype of
           'P': begin   { Convert a fractal curve into a polyline }
                  fraccurve.Decompose(MapCollection(Obj.MapC).CurrentView,Result);
                end;
           'K': begin   { Convert a fractal curve into a fractal polycurve}
                  GetMem(pcoord,sizeof(CoordPoint)*4);
                  pcoord^[0] := fraccurve.p1;
                  pcoord^[1] := fraccurve.p2;
                  pcoord^[2] := fraccurve.p3;
                  pcoord^[3] := fraccurve.p4;
                  Result:=PolyCurvePrimitive.Create(pcoord, 4,
                         fraccurve.Seed, fraccurve.Roughness,
                         fraccurve.Style);
                  Result.CopyCore(obj);
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'K': begin  { POLYCURVE PRIMITIVE }
         case newtype of
           'P': begin   { Convert a polycurve into a polyline }
                  Result:=DoubleDecompose(obj);
                end;
           'K': begin   { Convert a polycurve into a polycurve }
                  Result:=obj.Copy;
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    'k': begin  { FRACTAL POLYCURVE PRIMITIVE }
         case newtype of
           'P': begin   { Convert a fractal polycurve into a polyline }
                  Result:=DoubleDecompose(obj);
                end;
           'K': begin   { Convert a fractal polycurve into a fractal polycurve }
                  Result:=obj.Copy;
                end;
           else raise EInvalidOperation.Create(res_primitives_convert);
           end;
         end;
    else raise EInvalidOperation.Create(res_primitives_convert);
    end;
end;

function SetFractalState(current: boolean; state: FractalState): boolean;
begin
  Result := false;

  case state of
    fsUnchanged:   Result := current;
    fsSetFractal:  Result := true;
    fsSetNormal:   Result := false;
    fsFlipFractal: Result := not current;
  end; // Case
end; // SetFractalState

{------------------------------------------------------------------------------}

//procedure DrawPrimitive.FixExtent;
//begin
//     // Don't do anything except for the single symbol.
//end;

Procedure DrawPrimitive.InsertIntoBaseList;
// If we're not already in the base list, find the first free base slot to
// use, or make a new one if none are empty.  Base slots can become empty as
// objects are deleted.
Var
  I : Integer;
  B : Boolean;

Begin
  If MapCollection(MapC).BasePrimitives.IndexOfObject(Self) < 0 Then
  Begin
    I := 0;
    B := False;
    While (I < MapCollection(MapC).BasePrimitives.Count) And Not B Do
    Begin
      If MapCollection(MapC).BasePrimitives.Objects[I] = Nil Then B := True Else Inc(I);
    End; // While
    If B
     Then MapCollection(MapC).BasePrimitives.Objects[I] := Self
     Else MapCollection(MapC).BasePrimitives.AddObject('',Self);
  End;
End; // DrawPrimitive.InsertIntoBaseList

Procedure DrawPrimitive.AddToBaseOrCopies(DoChain: Boolean);
// This routine checks a DrawPrimitive and finds out whether it is a unique
// object or is in fact a copy of something that already exists.  If the object
// is unique, it is added to the base object list, and if it is a copy then it
// is turned into an alias.  Generally this can be a time-consuming process,
// since it calls IsSimilarTo() for each object it finds in the base list, and
// should only be used at certain times (loading a binary map file, adding
// new objects, etc.).  Since XML files are designed to preserve base/alias
// information, this doesn't need to be called when they are loaded, which
// makes loading XML files very fast!  The main place where this routine is
// used is when binary maps are loaded and objects are made into base and alias
// objects.  It makes loading binary maps take longer but once they're converted
// to XML maps the pain is gone :)
Var
  I,J : Integer;
  B   : Boolean;
  P   : TObject;

Begin
  I := 0;
  B := False;

  // We need to explicitly call SplitOff because we might be trying to add something
  // that is already an alias object (real-world example: group a series of objects,
  // copy the group, resize the new copy, and then group it again.).  The call to
  // ComputeExtent won't be accurate unless we first ensure that this object is
  // NOT an alias object.

  SplitOff;
  ComputeExtent;

  // Check all the base primitives and see if we match any of them.  This is the
  // most time-consuming part.

  While (I < MapCollection(MapC).BasePrimitives.Count) And Not B Do
  Begin
    P := MapCollection(MapC).BasePrimitives.Objects[I];
    If (P <> Nil) And (P <> Self) Then B := B Or IsSimilarTo(P As DrawPrimitive);
    If Not B Then Inc(I);
  End; // While

  // Set the alias's position.  It doesn't hurt to set it for base objects as well.
  // (See DrawPrimitive.ReadBaseFromDOMElement() for important note)

  Alias.X := fExtent.Left;
  Alias.Y := fExtent.Top;

  // Is it unique or isn't it?

  If Not B Then
  Begin
    // If we don't match any existing object, put this object into the base list

    InsertIntoBaseList;
  End
  Else
  Begin
    // Get rid of any dynamically allocated pieces, since we will become an alias

    Clear;

    // It is possible that something that was a base is becoming another
    // primitive's alias.  This can happen when objects are decomposed into
    // their constituent parts and those parts are injected back into the
    // map (decomposing sometimes calls SplitOff()).  In that case, we need to
    // find this object's base slot and clear it.

    J := MapCollection(MapC).BasePrimitives.IndexOfObject(Self);
    If J >= 0 Then MapCollection(MapC).BasePrimitives.Objects[J] := Nil;

    // Connect to our new base object and mirror the basic stuff that every
    // DrawPrimitive needs, then calculate our new extent

    Base := MapCollection(MapC).BasePrimitives.Objects[I] As DrawPrimitive;
    Base.Copies.AddObject('',Self);
    CopyFromBase(True);
    ComputeExtent;
  End;
End; // DrawPrimitive.AddToBaseOrCopies

Procedure DrawPrimitive.ClearThis(Deallocate: Boolean);
// ClearThis() is used to clear (and especially deallocate) anything specific
// to a particular DrawPrimitive type.  For instance, polylines and polycurves
// have to get rid of their point arrays, other objects have to get rid of
// fonts, bitmaps, etc.  Splitting the method off is important, since Clear() has
// to work differently depending on whether a DrawPrimitive is a base or an alias.
// Generally speaking, ClearThis() should never be called directly.  It is
// intended only for use by DrawPrimitive.Clear().
//
// Deallocate will cause ClearThis() to actually deallocate allocated memory if
// true.  Otherwise, only variables will be cleared to default values if necessary.
// This is mainly needed for dealing with group primitives.
Begin
  // To be overriden in child classes. 
End; // DrawPrimitive.ClearThis

procedure DrawPrimitive.Clear;
// Clear() is intended to completely empty a DrawPrimitive.  However, since a
// DrawPrimitive could be either a base or alias, this method has to handle the
// two cases intelligently.
//
// NB: One must be careful calling this routine.  Since it changes a DrawPrimitive,
// by rights the primitive should then either be made into a base object in its own
// right or become another object's alias.  Normally this would be done by calling
// AddToBaseOrCopies(), but since this routine is called BY AddToBaseOrCopies() that
// would be very bad :).  When calling Clear() from any normal routine, one should
// always call SplitOff() first to ensure that this object is unique.
Var
  I : Integer;
  G : DrawPrimitive;

Begin
  If Base = Nil Then
  Begin

    // This object is a base object.  Only fully clear and deallocate it if it has
    // no aliases.  If there are aliases, then the first alias has to become the new
    // base and this object can be cleared WITHOUT deallocating anything common to
    // all aliases.

    If Copies.Count > 0 Then
    Begin
      G      := Copies.Objects[0] As DrawPrimitive;
      G.Base := Nil;
      G.Copies.Clear;
      For I := 1 To Copies.Count - 1 Do
      Begin
        G.Copies.AddObject('',Copies.Objects[I]);
        (Copies.Objects[I] As DrawPrimitive).Base := G;
      End; // For I
      MapCollection(MapC).BasePrimitives.Objects[MapCollection(MapC).BasePrimitives.IndexOfObject(Self)] := G;
      Copies.Clear;
      ClearThis(False);
    End
    Else ClearThis(True);
  End
  Else
  Begin

    // This object is an alias object.  Clear it and remove it from its base
    // object's alias list.

    Base.Copies.Delete(Base.Copies.IndexOfObject(Self));
    Base := Nil;
    ClearThis(False);
  End;
End; // DrawPrimitive.Clear

function DrawPrimitive.DisplayColor(color:TColor):TColor;
begin
  if Settings.VisualOverlays.Checked and Settings.ColorCoded.Checked then begin
    Result:=OverlayMainColor[fOverlay mod (High(OverlayMainColor)+1)];
    end
  else
    Result:=color;
end;

function DrawPrimitive.DisplayFillColor(color:TColor):TColor;
begin
  if Settings.VisualOverlays.Checked and Settings.ColorCoded.Checked then begin
    Result:=OverlayFillColor[fOverlay mod (High(OverlayMainColor)+1)];
    end
  else
    Result:=color;
end;

procedure DrawPrimitive.CloseFigure;
begin
end;

procedure DrawPrimitive.SetSize(w,h:Coord);
var mat:Matrix;
    xc,yc:Coord;
    xf,yf:double;
begin
  xc:=Width/2;
  yc:=Height/2;
  mat:=OffsetMatrix(-(Extent.Left+xc),-(Extent.Top+yc));

  if (w<>0) and (Width<>0) then  begin
    xf:=w/Width;
    xc:=w/2;
    end
  else
    xf:=1.0;

  if (h<>0) and (Height<>0) then begin
    yf:=h/Height;
    xc:=h/2;
    end
  else
    yf:=1.0;

  MatrixMultiplyBy(mat,ScaleMatrix(xf,yf));
  MatrixMultiplyBy(mat,OffsetMatrix(Extent.Left+xc,Extent.Top+yc));
  ApplyMatrix(mat);
  ComputeExtent;
end;

function DrawPrimitive.Width:Coord;
begin
  Result:=Extent.Right-Extent.left;
end;

function DrawPrimitive.Height:Coord;
begin
  Result:=Extent.Bottom-Extent.Top;
end;

function  DrawPrimitive.IsClosed:boolean;
begin
  Result:=false;
end;

procedure DrawPrimitive.Invalidate(const Handle: THandle; const View: ViewPoint);
var
  r: TRect;
begin
  // Convert view extent to screen rectangle
  r := View.CoordToScreenRect(Extent);

  // Invalidate the specified rectangle on the Handle
  InvalidateRect(Handle, @r, True);

  // Call a helper method to invalidate related handles
  InvalidateHandles(Handle, View);
end;

function DrawPrimitive.ChainApplyMatrix(mat:Matrix; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.ApplyMatrix(mat) then begin
        p.ComputeExtent;
        Result:=true;
        end;
      end;
    p:=p.Next;
    end;
end;

function DrawPrimitive.ChainExtent(all:boolean):CoordRect;
var p:DrawPrimitive;
    r:CoordRect;
    c:integer;
begin
  r := MakeCoordRect(0,0,0,0);
  p := self;
  c := 0;

  while p <> nil do
  begin
    if all or p.IsSelected then
    begin
      inc(c);
      if c = 1 then r := p.Extent
      else
      begin
        r.Left   := min(r.Left,p.Extent.Left);
        r.Top    := min(r.Top,p.Extent.Top);
        r.Right  := max(r.Right,p.Extent.Right);
        r.Bottom := max(r.Bottom,p.Extent.Bottom);
      end;
    end;

    p := p.Next;
  end; // While

  Result := r;
end;

function DrawPrimitive.ChainSetStyle(style:StyleAttrib; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetStyle(style) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function DrawPrimitive.ChainSetFractal(state:FractalState; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do
  begin
    if all or p.IsSelected then
    begin
      if p.SetFractal(state) then Result:=true;
    end;
    p:=p.Next;
  end;
end;

function  DrawPrimitive.ChainGetStyle(all:boolean):StyleAttrib;
var p:DrawPrimitive;
    t,style:StyleAttrib;
begin
  p:=self;
  style.bits:=$FFFFFFFF;
  Style.FullStyle.Thickness   := -1;
  Style.FullStyle.SThickness  := 0;
  Result.bits:=0;
  Result.FullStyle.Thickness  := 0;
  Result.FullStyle.SThickness := 0;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetStyle;

      // Line style
      if (t.Line<>$FF) then begin
        if (style.Line=$FF) then
          style.Line:=t.Line
        else
          if (style.Line<>t.Line) then Result.Line:=$FF;
        end;

      // Fill style
      if (t.Fill<>$FF) then begin
        if (style.Fill=$FF) then
          style.Fill:=t.Fill
        else
          if (style.Fill<>t.Fill) then Result.Fill:=$FF;
        end;

      // Line start style
      if (t.First<>$FF) then begin
        if (style.First=$FF) then
          style.First:=t.First
        else
          if (style.First<>t.First) then Result.First:=$FF;
        end;

      // Line end style
      if (t.Last<>$FF) then begin
        if (style.Last=$FF) then
          style.Last:=t.Last
        else
          if (style.Last<>t.Last) then Result.Last:=$FF;
        end;

      // Line thickness
      if t.FullStyle.Thickness >= 0 then
      begin
        if style.FullStyle.Thickness = -1 then
        Begin
          style.FullStyle.Thickness  := t.FullStyle.Thickness;
          style.FullStyle.SThickness := t.FullStyle.SThickness;
        end
        else if style.FullStyle.Thickness <> t.FullStyle.Thickness then
         Result.FullStyle.Thickness := -1;
      end;

      end;

    p:=p.Next;
    end;

  if Result.Line  <> $FF then Result.Line  := Style.Line;
  if Result.Fill  <> $FF then Result.Fill  := Style.Fill;
  if Result.First <> $FF then Result.First := Style.First;
  if Result.Last  <> $FF then Result.Last  := Style.Last;
  If Result.FullStyle.Thickness >= 0 Then
  Begin
    Result.FullStyle.Thickness  := Style.FullStyle.Thickness;
    Result.FullStyle.SThickness := Style.FullStyle.SThickness;
  End;
end;

function DrawPrimitive.ChainSetSeed(seed:integer; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetSeed(seed) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function  DrawPrimitive.ChainGetSeed(all:boolean):integer;
var p:DrawPrimitive;
    t,seed:integer;
begin
  p:=self;
  seed:=-1;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetSeed;
      if (t<>-1) then begin
        if (seed=-1) then
          seed:=t
        else
          if (seed<>t) then begin
            Result:=-1;
            exit;
            end;
        end;
      end;

    p:=p.Next;
    end;

  Result:=seed;
end;


function DrawPrimitive.ChainSetRoughness(rough:integer; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetRoughness(rough) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function  DrawPrimitive.ChainGetRoughness(all:boolean):integer;
var p:DrawPrimitive;
    t,rough:integer;
begin
  p:=self;
  rough:=-1;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetRoughness;
      if (t<>-1) then begin
        if (rough=-1) then
          rough:=t
        else
          if (rough<>t) then begin
            Result:=-1;
            exit;
            end;
        end;
      end;

    p:=p.Next;
    end;

  Result:=rough;
end;

function DrawPrimitive.ChainSetColor(color:TColor; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetColor(color) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function  DrawPrimitive.ChainGetColor(all:boolean):TColor;
var p:DrawPrimitive;
    t,color:TColor;
begin
  p:=self;
  color:=clNone;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetColor;
      if (t<>clNone) then begin
        if (color=clNone) then
          color:=t
        else
          if (color<>t) then begin
            Result:=clNone;
            exit;
            end;
        end;
      end;

    p:=p.Next;
    end;

  Result:=color;
end;

function DrawPrimitive.ChainSetFillColor(color:TColor; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetFillColor(color) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function DrawPrimitive.ChainGetFillColor(all:boolean):TColor;
var p:DrawPrimitive;
    t,color:TColor;
begin
  p:=self;
  color:=clNone;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetFillColor;
      if (t<>clNone) then begin
        if (color=clNone) then
          color:=t
        else
          if (color<>t) then begin
            Result:=clNone;
            exit;
            end;
        end;
      end;

    p:=p.Next;
    end;

  Result:=color;
end;

function DrawPrimitive.ChainSetOverlay(overlay:byte; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetOverlay(overlay) then Result:=true;
      end;
    p:=p.Next;
    end;
end;

function  DrawPrimitive.ChainGetOverlay(all:boolean):integer;
var p:DrawPrimitive;
    t,overlay:integer;
begin
  p:=self;
  overlay:=-1;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      t:=p.GetOverlay;
      if (t<>-1) then begin
        if (overlay=-1) then
          overlay:=t
        else
          if (overlay<>t) then begin
            Result:=-1;
            exit;
            end;
        end;
      end;

    p:=p.Next;
    end;

  Result:=overlay;
end;

function DrawPrimitive.ChainSetTextAttrib(const View:ViewPoint; attrib:TextAttrib; all:boolean):boolean;
var p:DrawPrimitive;
begin
  Result:=false;
  p:=self;

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      if p.SetTextAttrib(View, attrib) then begin
        // Only allow the first text/hyperlink string to be changed so the others aren't
        // all wiped out by accident.
        attrib.Valid := attrib.Valid - [tatText, tatHyperlinkText];
        Result:=true;
        end;
      end;
    p:=p.Next;
    end;
end;

function DrawPrimitive.ChainGetTextAttrib(all:boolean):TextAttrib;
var p:DrawPrimitive;
    temp,attrib:TextAttrib;
    ignore:TextAttribSet;

    procedure CheckFontName;
    begin
      if (tatFontName in temp.Valid) and not (tatFontName in ignore) then begin
        if (tatFontName in attrib.Valid) then begin
          if (attrib.FontName<>temp.FontName) then begin
            ignore:=ignore+[tatFontName];
            attrib.Valid:=attrib.Valid-[tatFontName];
            end;
          end
        else begin
          attrib.FontName:=temp.FontName;
          attrib.Valid:=attrib.Valid+[tatFontName];
          end;
      end;
    end;

    procedure CheckText;
    begin
      if (tatText in temp.Valid) and not (tatText in ignore) then begin
        if (tatText in attrib.Valid) then begin
          // Text is a special case; don't merge multiple texts into one;
          // we only allow one text to pass.  More than one will be returned
          // as not the same to prevent us from changing millions of text strings
          // at once accidentally.
          ignore:=ignore+[tatText];
          attrib.Valid:=attrib.Valid-[tatText];
          end
        else begin
          attrib.Text:=temp.Text;
          attrib.Valid:=attrib.Valid+[tatText];
          end;
      end;
    end;

    procedure CheckHyperlinkText;
    begin
      if (tatHyperlinkText in temp.Valid) and not (tatHyperlinkText in ignore) then begin
        if (tatHyperlinkText in attrib.Valid) then begin
          // Treat Hyperlink text just like normal text
          ignore:=ignore+[tatHyperlinkText];
          attrib.Valid:=attrib.Valid-[tatHyperlinkText];
          end
        else begin
          attrib.HyperLinkText:=temp.HyperlinkText;
          attrib.Valid:=attrib.Valid+[tatHyperlinkText];
          end;
      end;
    end;

    procedure CheckHyperlinkFlags;
    begin
      if (tatHyperlinkFlags in temp.Valid) and not (tatHyperlinkFlags in ignore) then begin
        if (tatHyperlinkFlags in attrib.Valid) then begin
          if (attrib.HyperlinkFlags<>temp.HyperlinkFlags) then begin
            ignore:=ignore+[tatHyperlinkFlags];
            attrib.Valid:=attrib.Valid-[tatHyperlinkFlags];
            end;
          end
        else begin
          attrib.HyperlinkFlags :=temp.HyperlinkFlags;
          attrib.Valid:=attrib.Valid+[tatHyperlinkFlags];
          end;
      end;
    end;

    procedure CheckFontSize;
    begin
      if (tatFontSize in temp.Valid) and not (tatFontSize in ignore) then begin
        if (tatFontSize in attrib.Valid) then begin
          if (attrib.FontSize<>temp.FontSize) then begin
            ignore:=ignore+[tatFontSize];
            attrib.Valid:=attrib.Valid-[tatFontSize];
            end;
          end
        else begin
          attrib.FontSize:=temp.FontSize;
          attrib.Valid:=attrib.Valid+[tatFontSize];
          end;
      end;
    end;

    procedure CheckIconSize;
    begin
      if (tatIconSize in temp.Valid) and not (tatIconSize in ignore) then begin
        if (tatIconSize in attrib.Valid) then begin
          if (attrib.IconSize<>temp.IconSize) then begin
            ignore:=ignore+[tatIconSize];
            attrib.Valid:=attrib.Valid-[tatIconSize];
            end;
          end
        else begin
          attrib.IconSize:=temp.IconSize;
          attrib.Valid:=attrib.Valid+[tatIconSize];
          end;
      end;
    end;

    procedure CheckFontBold;
    begin
      if (tatFontBold in temp.Valid) and not (tatFontBold in ignore) then begin
        if (tatFontBold in attrib.Valid) then begin
          if (attrib.FontBold<>temp.FontBold) then begin
            ignore:=ignore+[tatFontBold];
            attrib.Valid:=attrib.Valid-[tatFontBold];
            end;
          end
        else begin
          attrib.FontBold:=temp.FontBold;
          attrib.Valid:=attrib.Valid+[tatFontBold];
          end;
      end;
    end;

    procedure CheckFontItalic;
    begin
      if (tatFontItalic in temp.Valid) and not (tatFontItalic in ignore) then begin
        if (tatFontItalic in attrib.Valid) then begin
          if (attrib.FontItalic<>temp.FontItalic) then begin
            ignore:=ignore+[tatFontItalic];
            attrib.Valid:=attrib.Valid-[tatFontItalic];
            end;
          end
        else begin
          attrib.FontItalic:=temp.FontItalic;
          attrib.Valid:=attrib.Valid+[tatFontItalic];
          end;
      end;
    end;

    procedure CheckFontUnderline;
    begin
      if (tatFontUnderline in temp.Valid) and not (tatFontUnderline in ignore) then begin
        if (tatFontUnderline in attrib.Valid) then begin
          if (attrib.FontUnderline<>temp.FontUnderline) then begin
            ignore:=ignore+[tatFontUnderline];
            attrib.Valid:=attrib.Valid-[tatFontUnderline];
            end;
          end
        else begin
          attrib.FontUnderline:=temp.FontUnderline;
          attrib.Valid:=attrib.Valid+[tatFontUnderline];
          end;
      end;
    end;

    procedure CheckAlignment;
    begin
      if (tatAlignment in temp.Valid) and not (tatAlignment in ignore) then begin
        if (tatAlignment in attrib.Valid) then begin
          if (attrib.Alignment<>temp.Alignment) then begin
            ignore:=ignore+[tatAlignment];
            attrib.Valid:=attrib.Valid-[tatAlignment];
            end;
          end
        else begin
          attrib.Alignment:=temp.Alignment;
          attrib.Valid:=attrib.Valid+[tatAlignment];
          end;
      end;
    end;

    procedure CheckFontOutline;
    begin
      if (tatOutlineColor in temp.Valid) and not (tatOutlineColor in ignore) then begin
        if (tatOutlineColor in attrib.Valid) then begin
          if (attrib.FontOutlineColor<>temp.FontOutlineColor) then begin
            ignore:=ignore+[tatOutlineColor];
            attrib.Valid:=attrib.Valid-[tatOutlineColor];
            end;
          end
        else begin
          attrib.FontOutlineColor:=temp.FontOutlineColor;
          attrib.Valid:=attrib.Valid+[tatOutlineColor];
          end;
      end;
    end;

begin
  p:=self;
  attrib.Valid:=[];
  ignore:=[];

  while (p<>nil) do begin
    if all or p.IsSelected then begin
      temp:=p.GetTextAttrib;

      CheckText;
      CheckFontName;
      CheckFontSize;
      CheckFontBold;
      CheckFontItalic;
      CheckFontUnderline;
      CheckAlignment;
      CheckIconSize;
      CheckFontOutline;
      CheckHyperlinkText;
      CheckHyperlinkFlags;
      end;

    p:=p.Next;
    end;

  Result:=attrib;
end;

function DrawPrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
begin
  Result:=false;
end;

function DrawPrimitive.SetFractal(state:FractalState):boolean;
begin
  Result:=false;
end;

function DrawPrimitive.CountSiblings:integer;
var p:DrawPrimitive;
begin
  p:=self;
  Result:=0;

  while (p<>nil) do begin
    inc(Result);
    p:=p.Next;
    end;
end;

class function DrawPrimitive.IdToObject(id:char):DrawPrimitive;
begin
  case id of
    'L': Result := LinePrimitive.CreateBlank(false);
    'l': Result := LinePrimitive.CreateBlank(true);
    'C': Result := CurvePrimitive.CreateBlank(false);
    'c': Result := CurvePrimitive.CreateBlank(true);
    'K': Result := PolyCurvePrimitive.CreateBlank(false);
    'k': Result := PolyCurvePrimitive.CreateBlank(true);
    'P': Result := PolylinePrimitive.CreateBlank(false);
    'p': Result := PolylinePrimitive.CreateBlank(true);

    'S': Result := SymbolPrimitive.CreateBlank;
    'T': Result := TextPrimitive.CreateBlank;
    't': Result := TextCurvePrimitive.CreateBlank;
    'G': Result := GroupPrimitive.CreateBlank;
    'B': Result := BitmapPrimitive.CreateBlank;
    'H': Result := HyperlinkPrimitive.CreateBlank;
    else
      Result := nil;
    end;
end;

class function DrawPrimitive.ReadChain(stream:TStream; version:integer;
                                       selected,Full,UseAliasInfo: boolean;
                                       M: TObject):DrawPrimitive;
// Full tells the DrawPrimitive Read() methods whether to use every new extension
// in the binary format, or to stick to the 1.21 binary file-format.  This option
// is important for Undo/Redo functionality in the event that DrawObjects are
// given extended functionality.  Undo/Redo set this to True so that any extra
// parameters are saved and restored, whereas functions that load and save binary
// map file set it to false to retain backwards compatibility.  It's important to
// note that any extensions to primitives would be lost if the map is saved to an
// old-format binary file (hence the reason to move to XML maps).
//
// UseAliasInfo allows saving and restoring of base and alias information.  This
// is absolutely vital for Undo/Redo functionality, and is set to true during
// those functions.  Likewise, it is set to false when 1.21 binary map files are
// loaded and saved.
var last,p:DrawPrimitive;
    id:char;
begin
  Result:=nil;
  last:=nil;

  repeat
    stream.ReadBuffer(id, sizeof(id));
    if (id<>#0) then begin
      p:=IdToObject(id);
      if (p=nil) then exit;
      P.SetMap(M);

      p.Read(stream, version, Full, UseAliasInfo);
//!!      p.ComputeExtent;
      p.Select(nil, selected);

      if (last = nil) then
        Result:=p
      else
        last.next := p;

      last:=p;
      end;
  until (id=#0);
end;

procedure DrawPrimitive.WriteChain(stream: TStream; all,Full,UseAliasInfo: boolean; AddX,AddY: Coord);
// Full tells the DrawPrimitive Read() methods whether to use every new extension
// in the binary format, or to stick to the 1.21 binary file-format.  This option
// is important for Undo/Redo functionality in the event that DrawObjects are
// given extended functionality.  Undo/Redo set this to True so that any extra
// parameters are saved and restored, whereas functions that load and save binary
// map file set it to false to retain backwards compatibility.  It's important to
// note that any extensions to primitives would be lost if the map is saved to an
// old-format binary file (hence the reason to move to XML maps).
//
// UseAliasInfo allows saving and restoring of base and alias information.  This
// is absolutely vital for Undo/Redo functionality, and is set to true during
// those functions.  Likewise, it is set to false when 1.21 binary map files are
// loaded and saved.
//
// The only time AddX and AddY ever come into play (i.e. are nonzero) is when
// saving to an old-style 1.21 binary file.  The reason is because aliased groups
// point to their BASE groups and, when their individual pieces are saved, they
// need to know where they're actually located.  This isn't a problem when groups
// are drawn on the map because MakeAliasView() is used to shift the ViewPoint first,
// and it isn't normally possible to operate on individual elements in a group
// without first ungrouping.  The one exception (at this time) is color translation,
// which handles this by first calling SplitOff().
var p:DrawPrimitive;
    id:char;
begin
  p:=self;
  while (p<>nil) do begin
    if all or (p.IsSelected) then begin
      id:=p.GetId;
      stream.WriteBuffer(id, sizeof(id));
      p.Write(stream,Full,UseAliasInfo,AddX,AddY);
      end;
    p:=p.Next;
    end;

  id:=#0;
  stream.WriteBuffer(id,sizeof(id));
end;

Class Function DrawPrimitive.ReadChainFromDOMElement(E: TDOMElement; Version: Integer;
                                                     Selected: Boolean;
                                                     M : TObject): DrawPrimitive;
// Works just like ReadChain(), but reads from an XML tree
Var
  Last  : DrawPrimitive;
  P     : DrawPrimitive;
  ID    : Char;
  Chain : TDOMElement;
  E1    : TDOMElement;
  Count : Integer;
  I     : Integer;
  EA    : Array Of TDOMElement;

Begin
  Result := Nil;
  Last   := Nil;
  Chain  := E.getFirstChildElement('DRAWCHAIN');
  If Chain <> Nil Then
  Begin
    Count := GetIntegerProperty(Chain,'COUNT');
    If Count > 0 Then
    Begin
      // Load up the chain, preserving the ordering

      E1 := Chain.findFirstChildElement;
      SetLength(EA,Count);
      For I := 0 To Count - 1 Do EA[I] := Nil;
      While E1 <> Nil Do
      Begin
        If E1.tagName <> 'COUNT' Then
        Begin

          // Get the ID, which preserves back-to-front draw ordering

          I  := GetIntegerProperty(E1,'CID');
          If (I >= 0) And (I < Count) Then EA[I] := E1;
        End;
        E1 := E1.findNextSiblingElement;
      End; // While

      For I := 0 To Count - 1 Do
      Begin
        E1 := EA[I];
        If E1 <> Nil Then
        Begin
          ID := CharIDFromXMLID(E1.tagName,E1.getFirstChildElement('FRACTAL') <> Nil);
          If ID <> #0 Then
          Begin
            P := IdToObject(ID);
            If P = Nil Then Exit;
            P.SetMap(M);
            P.ReadFromDOMElement(E1,Version);
//!!            p.ComputeExtent;
            P.Select(Nil, Selected);
            If Last = Nil Then Result := P Else Last.Next := P;
            Last := P;
          End;
        End;
      End; // For I
      SetLength(EA,0);
    End;
  End;
End; // DrawPrimitive.ReadChainFromDOMElement

Function DrawPrimitive.GetChainAsDOMElement(D: TDOMDocument; All,Undo: Boolean): TDOMElement;
// Works just like WriteChain(), but creates an XML tree
Var
  P  : DrawPrimitive;
  E  : TDOMElement;
  E1 : TDOMElement;
  I  : Integer;

Begin
  E := D.createElement('DRAWCHAIN');
  P := Self;
  I := 0;
  While P <> Nil Do
  Begin
    E1 := P.GetAsDOMElement(D,Undo);
    E.appendChild(E1);

    // Save an ID so we can preserve back-to-front draw ordering

    E1.appendChild(NewIntegerProperty(D,'CID',I));
    Inc(I);
    P := P.Next;
  End; // While
  E.appendChild(NewIntegerProperty(D,'COUNT',I));
  Result := E;
End; // DrawPrimitive.GetChainAsDOMElement

Procedure DrawPrimitive.SplitOff;
// This is one of THE MOST IMPORTANT methods with regard to object base/aliasing.
// It takes a DrawPrimitive and makes it "unique", that is, it will become a base
// object with NO aliases.  The purpose of this routine is to prepare an object for
// some sort of modification that would make it fundamentally different from any
// of its duplicates (e.g. stretching, moving a node, changing a color, etc.).
// Only translating (moving) an object should do so without calling this first.
Var
  I : Integer;
  G : DrawPrimitive;
  E : CoordRect;

Begin
  // If this object is already unique, we don't have to do anything

  If (Base <> Nil) Or (Copies.Count > 0) Then
  Begin
    // Is it already a base, or is it an alias?

    If Base <> Nil Then
    Begin
      // This object is an alias.  First let's perform a FULL copy of its base
      // (which means also get anything dynamically allocated, e.g. CoordPoint
      // arrays, fonts, child objects, etc.)

      CopyFromBase(False); // AliasOnly is False, copy EVERYTHING

      // We will no longer be an alias

      Base.Copies.Delete(Base.Copies.IndexOfObject(Self));

      // We are going to become a base in our own right

      Base := Nil;
    End
    Else
    Begin

      // We are a base object.  The first of our aliases will become the new
      // base for the rest of our aliases.

      G := Copies.Objects[0] As DrawPrimitive;

      // All dynamically allocated information already exists, so simply copy the
      // static stuff into the first of our aliases and have it simply point to
      // the dynamic stuff (we'll make our own copy later).

      G.CopyFromBase(True);

      // Switch the base object slot to point to the alias and make it the new base

      MapCollection(MapC).BasePrimitives.Objects[MapCollection(MapC).BasePrimitives.IndexOfObject(Self)] := G;
      G.Base := Nil;

      // The rest of our aliases need to become aliases for the new base

      G.Copies.Clear;

      // Since the new base sits at a different location, it has to move.  However,
      // we also need to allocate a new dynamic copy for this object so we will
      // temporarily make the new base object our base so our dynamic copy will be at
      // the right position.

      Base    := G;
      E       := fExtent;
      Alias.X := E.Left;
      Alias.Y := E.Top;
      G.Move(G.Alias.X - E.Left,G.Alias.Y - E.Top);
      CopyFromBase(False); // FULL copy

      // Relocate the other aliases to the new base

      For I := 1 To Copies.Count - 1 Do
      Begin
        G.Copies.AddObject('',Copies.Objects[I]);
        (Copies.Objects[I] As DrawPrimitive).Base := G;
      End; // For I

      // No more base or copies for us; we are to be unique

      Copies.Clear;
      Base := Nil;

      // Just for kicks

      G.Alias.X := G.fExtent.Left;
      G.Alias.Y := G.fExtent.Top;
    End;

    // It doesn't hurt to set these coordinates (and perhaps it might be useful
    // to make setting them standard in the future)
    // (See DrawPrimitive.ReadBaseFromDOMElement() for important note)

    Alias.X := fExtent.Left;
    Alias.Y := fExtent.Top;

    // Add the new unique object to the base object list

    InsertIntoBaseList;
  End;
End; // DrawPrimitive.SplitOff

function DrawPrimitive.OnScreen(const Coord:CoordRect):boolean;
begin
  Result := (fExtent.Left   < Coord.Right)  and
            (fExtent.Right  > Coord.Left)   and
            (fExtent.Top    < Coord.Bottom) and
            (fExtent.Bottom > Coord.Top);
end;

procedure DrawPrimitive.SetExtent(value:CoordRect);
begin
  fExtent.left   := min(value.left,value.right);
  fExtent.top    := min(value.top,value.bottom);
  fExtent.right  := max(value.left,value.right);
  fExtent.bottom := max(value.top,value.bottom);
end;

procedure DrawPrimitive.Move(dx,dy:Coord);
begin
  fExtent.left   := fExtent.left   + dx;
  fExtent.right  := fExtent.right  + dx;
  fExtent.top    := fExtent.top    + dy;
  fExtent.bottom := fExtent.bottom + dy;
  Alias.X        := Alias.X + DX;
  Alias.Y        := Alias.Y + DY;
  ComputeExtent;
end;

function DrawPrimitive.FindHandle(const View:ViewPoint; x,y:Coord):boolean;
begin
  Result := TestHandle(View, fExtent.left, fExtent.top,    x,y) or
            TestHandle(View, fExtent.left, fExtent.bottom, x,y) or
            TestHandle(View, fExtent.right,fExtent.top,    x,y) or
            TestHandle(View, fExtent.right,fExtent.bottom, x,y);
end;

function DrawPrimitive.FindHyperlink(const View:ViewPoint; x,y:Coord; var hypertext:string; var hyperflags:THyperlinkFlags):boolean;
begin
  Result := false;
end;

function DrawPrimitive.GetLines(const View:Viewpoint; var polycount:integer):PCoordArray;
begin
  polycount := 0;
  Result    := nil;
end;

function DrawPrimitive.SelectClick(const within:double; p:CoordPoint):boolean;
var fracpoints:PCoordArray;
    polycount:integer;
    i:integer;
begin
  // GetLines() is smart enough to take base/alias status into account and
  // return correct coordinates

  fracpoints := GetLines(nil,polycount);
  Result     := false;

  if (polycount=0) then exit;

  if IsClosed then begin
    Result:=PointInPolygon(p, fExtent, fracpoints, polycount);
    end
  else begin
    for i:=1 to polycount-1 do begin
      if (DistanceToSegment(p,fracpoints[i-1],fracpoints[i]) < within) then begin
        Result:=true; break;
        end;
      end;
    end;

  FreeMem(fracpoints);
end;

Function DrawPrimitive.GetAdjustedX(X: Coord): Coord;
// Alias objects, by definition, either contain or point to the same coordinates
// as their base objects.  Therefore, when we need to determine where a coordinate
// REALLY lies, we call routines like this to determine the truth.
Begin
  If Base <> Nil
   Then Result := X + (Alias.X - Base.fExtent.Left)
   Else Result := X;
End; // DrawPrimitive.GetAdjustedX

Function DrawPrimitive.GetAdjustedY(Y: Coord): Coord;
// Alias objects, by definition, either contain or point to the same coordinates
// as their base objects.  Therefore, when we need to determine where a coordinate
// REALLY lies, we call routines like this to determine the truth.
Begin
  If Base <> Nil
   Then Result := Y + (Alias.Y - Base.fExtent.Top)
   Else Result := Y;
End; // DrawPrimitive.GetAdjustedY

Function DrawPrimitive.GetAdjustedPoint(P: CoordPoint): CoordPoint;
// Alias objects, by definition, either contain or point to the same coordinates
// as their base objects.  Therefore, when we need to determine where a coordinate
// REALLY lies, we call routines like this to determine the truth.
Begin
  If Base <> Nil Then
  Begin
    P.X := P.X + (Alias.X - Base.fExtent.Left);
    P.Y := P.Y + (Alias.Y - Base.fExtent.Top);
  End;
  Result := P;
End; // DrawPrimitive.GetAdjustedPoint

Function DrawPrimitive.MakeAliasView(View: ViewPoint): ViewPoint;
// This function should be called sparingly and with caution.  It reads a ViewPoint
// and allocates a new, SHIFTED ViewPoint that points to an object's base object.
// It's primary purpose is for when drawing a DrawPrimitive.  Is is generally better
// to call GetAdjustedX(), GetAdjustedY(), etc. rather than MakeAliasView(), except
// in cases where it is more efficient to simply shift the viewpoint (such as when
// drawing).
//
// NB: The routine that calls this is responsible for deallocating the ViewPoint it
// returns.  Note also that in the case where this object is actually a base object,
// a ViewPoint is NOT allocated and the original one is merely returned.  Routines
// that call this therefore have to make sure to only deallocate the ViewPoint if it
// is in fact different.
Var
  X,Y : Coord;
  V   : ViewPoint;

Begin
  If Base <> Nil Then
  Begin
    V             := ViewPoint.Create(View);
    X             := Alias.X - Base.fExtent.Left;
    Y             := Alias.Y - Base.fExtent.Top;
    V.Area.Left   := V.Area.Left   - X;
    V.Area.Top    := V.Area.Top    - Y;
    V.Area.Right  := V.Area.Right  - X;
    V.Area.Bottom := V.Area.Bottom - Y;
    Result        := V;
  End
  Else Result := View;
End; // DrawPrimitive.MakeAliasView

procedure DrawPrimitive.Draw(View:ViewPoint);
begin
end;

function DrawPrimitive.FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean;
begin
  Result := false;
end;

Function DrawPrimitive.FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean;
Begin
  Result := False;
End; // DrawPrimitive.FindPointOn

Procedure DrawPrimitive.RecalcAliasFromExtent;
Begin
  ComputeExtent;
  Alias.X := fExtent.Left;
  Alias.Y := fExtent.Top;
End; // DrawPrimitive.RecalcAliasFromExtent

function DrawPrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
begin
  if FindHandle(View,origx,origy) then
  begin
    fExtent.left   := fExtent.left   + dx;
    fExtent.right  := fExtent.right  + dx;
    fExtent.top    := fExtent.top    + dy;
    fExtent.bottom := fExtent.bottom + dy;
    Result := true;
  end
  else Result := false;
end;

procedure DrawPrimitive.InvalidateHandles(const Handle:HWND; const View:ViewPoint);
begin
end;

function DrawPrimitive.FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean;
begin
  Result := false;
end;

function DrawPrimitive.SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean;
begin
  Result := false;
end;

procedure DrawPrimitive.Reverse;
begin
end;

function DrawPrimitive.SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean;
begin
  Result := false;
end;

procedure DrawPrimitive.DeleteNode(const View:ViewPoint; index:integer);
begin
end;

function DrawPrimitive.SetStyle(style:StyleAttrib):boolean;
begin
  Result:=false;
end;

function  DrawPrimitive.GetStyle:StyleAttrib;
begin
  Result.bits                 := $FFFFFFFF;
  Result.FullStyle.Thickness  := -1;
  Result.FullStyle.SThickness := 0;
end;

function DrawPrimitive.SetSeed(seed:integer): boolean;
begin
  Result:=false;
end;

function  DrawPrimitive.GetSeed: integer;
begin
  Result:=-1;
end;

function DrawPrimitive.SetRoughness(rough:integer):boolean;
begin
  Result:=false;
end;

function  DrawPrimitive.GetRoughness:integer;
begin
  Result:=-1;
end;

function DrawPrimitive.SetColor(color:TColor):boolean;
begin
  SplitOff;
  fColor := color;
  Result := true;
end;

function  DrawPrimitive.GetColor:TColor;
begin
  Result := fColor;
end;

function DrawPrimitive.SetFillColor(color:TColor):boolean;
begin
  Result:=false;
end;

function  DrawPrimitive.GetFillColor:TColor;
begin
  Result := clNone;
end;

function DrawPrimitive.SetOverlay(overlay:byte):boolean;
begin
  SplitOff;
  fOverlay := overlay;
  Result:=true;
end;

function  DrawPrimitive.GetOverlay:integer;
begin
  Result:=fOverlay;
end;

Function DrawPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Begin
  Result := (fColor = D.fColor);
End; // DrawPrimitive.IsSimilarTo

function  DrawPrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  Result:=false;
end;

function  DrawPrimitive.GetTextAttrib:TextAttrib;
begin
  Result.Valid:=[];
end;

function DrawPrimitive.ApplyMatrix(var mat:Matrix):boolean;
begin
  Result:=false;
end;

function DrawPrimitive.IsWithin(rect:CoordRect):boolean;
begin
  Result:=PtInCoordRect(rect, fExtent.TopLeft) and
          PtInCoordRect(rect, fExtent.BottomRight);
end;

function DrawPrimitive.IsTouching(rect:CoordRect):boolean;
begin
  Result:=PtInCoordRect(rect, fExtent.TopLeft) or
          PtInCoordRect(rect, fExtent.BottomRight) or
          PtInCoordRect(fExtent, rect.TopLeft) or
          PtInCoordRect(fExtent, rect.BottomRight);
end;

function DrawPrimitive.IsInOverlay(var overlay:OverlaySet):boolean;
begin
  Result := (fOverlay in overlay);
end;

procedure DrawPrimitive.ComputeExtent;
begin
  fExtent:=MakeCoordRect(0,0,0,0);
end;

function DrawPrimitive.IsSelected:boolean;
begin
  Result := fSelected;
end;

procedure DrawPrimitive.Select(const View:ViewPoint; b:boolean);
begin
  if (b <> fSelected) then begin
    fSelected:=b;
    if (View<>nil) and (View.Canvas<>nil) then Draw(View);
    end;
end;

Procedure DrawPrimitive.CopyFromBase(AliasOnly: Boolean);
// Makes an aliased copy from this object's Base object,
// without copying anything specific to this one (like
// its position).
Begin
  If Base <> Nil Then
  Begin
    fColor   := Base.fColor;
    MapC     := Base.MapC;
  End;
End; // DrawPrimitive.CopyFromBase

procedure DrawPrimitive.CopyCore(obj:DrawPrimitive);
begin
  fOverlay  := obj.fOverlay;
  fSelected := obj.fSelected;
  fColor    := obj.fColor;
  MapC      := Obj.MapC;
end;

Constructor DrawPrimitive.Create;
Var N: Integer;
Begin
  // By default, our map is the drawing map.  If this is actually going to go into
  // a different map (e.g. a symbol catalog), we don't have to worry about it here.
  // MapCollections always call SetMap whenever an object is added to their linked
  // lists.

  MapC    := Map;

  // Start off by saying we are a base object, but don't add ourselves to the base
  // object list.  Routines like AddToBaseOrCopies(), DoRead(), or
  // ReadBaseFromDOMElement() will determine the truth and take appropriate steps.

  Base    := Nil; 
  Copies  := TStringList.Create;
  n       := MainForm.ActiveOverlay.ItemIndex;
  if n = -1 then n := 0;

  fOverlay  := n;
  fSelected := false;
  fExtent   := MakeCoordRect(0,0,0,0);
  fColor    := CurrentColor;
  Next      := nil;
  Alias.X   := 0;
  Alias.Y   := 0;
End;

Procedure DrawPrimitive.SetMap(M: TObject);
// Since we now have this mechanism for base and alias objects, and since the
// base object list is contained in a MapCollection, each DrawPrimitive MUST know
// the MapCollection to which it belongs.  This is critical because there are
// actually several MapCollections active at any one time (symbol catalogs have
// their own distinct MapCollections).
Begin
  MapC := M;
End; // DrawPrimitive.SetMap

function DrawPrimitive.Copy: DrawPrimitive;
begin
  Result := nil;
end;

Procedure DrawPrimitive.MakeCopy(From: DrawPrimitive);
// Copies the most basic DrawPrimitive information and establishes this object
// as another object's alias (or sibling if that object is itself an alias).
// This routine is generally for internal use only (look for cases to get an
// idea of where it's appropriate).
Begin
  If From.Base <> Nil Then
  Begin
    Base := From.Base;
    From.Base.Copies.AddObject('',Self);
  End
  Else
  Begin
    From.Copies.AddObject('',Self);
    Base := From;
  End;
  fExtent  := From.fExtent;
  Alias.X  := From.fExtent.Left;
  Alias.Y  := From.fExtent.Top;
  MapC     := From.MapC;
  fOverlay := From.fOverlay;
  fColor   := From.fColor;
End;

destructor DrawPrimitive.Destroy;
Var I: Integer;
begin
  // First we have to completely decouple this object from any bases or aliases

  SplitOff;

  // Then we can clear it, and, since it's now "unique", it will deallocate
  // anything that was dynamically allocated

  Clear;

  // Since this object is unique, it is by definition a base object.  We'll put a
  // Nil in its base object slot for now, and the next time any other object calls
  // InsertIntoBaseList() this slot will be available for use.

  I := MapCollection(MapC).BasePrimitives.IndexOfObject(Self);
  If (I >= 0) And (I < MapCollection(MapC).BasePrimitives.Count) Then MapCollection(MapC).BasePrimitives.Objects[I] := Nil;

  // SplitOff() caused our alias list to be empty, by definition, so we can simply
  // get rid of it

  Copies.Free;

  // Bye-bye :)

  inherited Destroy;
end;

procedure DrawPrimitive.ClearChain;
var p,f:DrawPrimitive;
begin
  p:=Next;

  while (p<>nil) do begin
    f:=p;
    p:=p.Next;
    f.Free;
    end;

  Next:=nil;
end;

procedure StartNewHandleDraw;
begin
  LastHandleX:=9E99;
  LastHandleY:=9E99;
end;

procedure DrawPrimitive.InvalidateHandle(const Handle: THandle;
  const View: ViewPoint; x, y: Coord);
var
  sx, sy: Integer;
  rect: TRect;
begin
  // Convert logical coordinates to screen coordinates
  View.CoordToScreen(x, y, sx, sy);

  // Define a rectangle around the point
  rect.Left := sx - 3;
  rect.Top := sy - 3;
  rect.Right := sx + 3;
  rect.Bottom := sy + 3;

  // Invalidate the rectangle on the handle
  InvalidateRect(Handle, @rect, True);
end;

procedure DrawPrimitive.DrawHandle(const View:ViewPoint; x,y:Coord);
var sx,sy:integer;
    size,offset:integer;
begin
  if View<>nil then begin
    if (x=LastHandleX) and (y=LastHandleY) then begin
      size:=7;
      offset:=-3;
      end
    else begin
      size:=5;
      offset:=-2;
      end;

    View.Canvas.Pen.Color:=clBlack;
    View.Canvas.Brush.Color := clBlack;
    View.CoordToScreen(x,y,sx,sy);
    PatBlt(View.Canvas.Handle,sx+offset,sy+offset,size,size,DSTINVERT);
    end;

  LastHandleX:=x;
  LastHandleY:=y;
  //  View.Canvas.FillRect(Rect(sx-2,sy-2,sx+3,sy+3));
end;

function DrawPrimitive.TestHandle(const View:ViewPoint; tx,ty,px,py:Coord):boolean;
var stx,sty,spx,spy:integer;
begin
  View.CoordToScreen(tx,ty,stx,sty);
  View.CoordToScreen(px,py,spx,spy);
  Result:=PtInRect(Rect(stx-2,sty-2,stx+3,sty+3), Point(spx,spy));
end;

procedure DrawPrimitive.DrawOverlayHandle(View: ViewPoint; x, y: Coord);
var
  sx, sy: Integer;
  V: ViewPoint;
begin
  // Create a shifted ViewPoint, if necessary
  V := MakeAliasView(View);

  // Convert logical coordinates to screen coordinates
  V.CoordToScreen(x, y, sx, sy);

  // Use Lazarus-compatible drawing code
  if Assigned(MainForm.OverlayImages) then
    MainForm.OverlayImages.Draw(V.Canvas, sx, sy,
      MainForm.OverlayList.ImageIndex[fOverlay]);

  // Deallocate the shifted ViewPoint if it was created
  if V <> View then
    V.Free;
end;


procedure DrawPrimitive.DrawOverlayHandles(const View:ViewPoint);
begin
  DrawOverlayHandle(View, Extent.Left, Extent.Top);
end;

procedure DrawPrimitive.DrawHandles(const View:ViewPoint);
begin
  DrawHandle(View, Extent.Left,Extent.Top);
  DrawHandle(View, Extent.Right,Extent.Top);
  DrawHandle(View, Extent.Left,Extent.Bottom);
  DrawHandle(View, Extent.Right,Extent.Bottom);
end;

function DrawPrimitive.PointClosestInArray(x,y:Coord; points:PCoordArray; count:integer):integer;
var i:integer;
    d,temp:double;
begin
  Result:=-1;
  if (count=0) then exit;

  d:=Distance(x,y, points^[0].x,points^[0].y);
  Result:=0;

  for i:=1 to count-1 do begin
    temp:=Distance(x,y, points^[i].x, points^[i].y);
    if (temp<d) then begin
      d:=temp;
      Result:=i;
      end;
    end;
end;

function DrawPrimitive.PointClosestInAdjustedArray(x,y:Coord; points:PCoordArray; count:integer):integer;
// This serves the same purpose as PointClosestInArray, but adjusts the points for
// base/alias status.
var i:integer;
    d,temp:double;
begin
  Result:=-1;
  if (count=0) then exit;

  d:=Distance(x,y, GetAdjustedX(points^[0].x),GetAdjustedY(points^[0].y));
  Result:=0;

  for i:=1 to count-1 do begin
    temp:=Distance(x,y, GetAdjustedX(points^[i].x), GetAdjustedY(points^[i].y));
    if (temp<d) then begin
      d:=temp;
      Result:=i;
      end;
    end;
end;

procedure DrawPrimitive.PointClosestTo(x,y:Coord; var px,py:Coord);
var corner:array[0..3] of CoordPoint;
    n:integer;
begin
  corner[0].x := Extent.Left;  corner[0].y := Extent.Top;
  corner[1].x := Extent.Right; corner[1].y := Extent.Top;
  corner[2].x := Extent.Left;  corner[2].y := Extent.Bottom;
  corner[3].x := Extent.Right; corner[3].y := Extent.Bottom;

  n:=PointClosestInArray(x,y,@corner,4);

  px:=corner[n].x;
  py:=corner[n].y;
end;


function DrawPrimitive.GetId:char;
begin
  Result:=#0;
end;

Procedure DrawPrimitive.RefreshCopies;
// The main purpose of this routine is to "fixup" pointers in alias objects
// for dynamically allocated content (e.g. point arrays) so that they mirror
// the pointers in their base objects.  A full explanation for the reasons
// behind this can be found in DrawPrimitive.ReadBaseFromDOMElement().
Var
  I  : Integer;
  DP : DrawPrimitive;

Begin
  If Base = Nil Then
   For I := 0 To Copies.Count - 1 Do
   Begin
     DP := Copies.Objects[I] As DrawPrimitive; // The "as" operator incurs a performance hit so let's minimize its use
     DP.CopyFromBase(True);
     DP.ComputeExtent;
   End; // For I
  ComputeExtent; // Important
End; // DrawPrimitive.RefreshCopies   

procedure DrawPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
// The logic in this method is very similar to DrawPrimitive.ReadBaseFromDOMElement().
Var
  I,J : Integer;
  B   : Boolean;
  P   : TObject;
  L   : TStringList;
  D   : DrawPrimitive;

begin
  stream.ReadBuffer(fColor,sizeof(fColor));
  stream.ReadBuffer(fOverlay,sizeof(fOverlay));
  stream.ReadBuffer(fExtent,sizeof(fExtent));    //!!

  // If we're simply loading a 1.21 binary file, UseAliasInfo will be false
  // for backward compatibility and we'll skip the next part

  If UseAliasInfo Then
  Begin
    Stream.ReadBuffer(B,SizeOf(B));  // Is this a base or an alias?

    // If this is a base, then I represents the INDEX in the base object list
    // where this object should be placed.  If this object is an alias, it
    // represents the INDEX of its base object in the base object list.

    Stream.ReadBuffer(I,SizeOf(I));

    // Where is this object located?  This is only strictly necessary for
    // aliases, though it doesn't hurt to set/save/retrieve it for bases as well
    // (it simply is ignored in that case).

    Stream.ReadBuffer(Alias.X,SizeOf(Alias.X));
    Stream.ReadBuffer(Alias.Y,SizeOf(Alias.Y));

    // Objects are read back as they were written, where the order is based on the
    // MapCollection's linked list.  This ordering is based on back-to-front drawing
    // than on base object index, and in general the base indexes will jump around
    // as the objects are read from the stream.  For indexes that we haven't read
    // in yet (up to and INCLUDING this one), put a TStringList in which will be
    // used later.  If all the code works as designed, there should NEVER be any
    // leftover TStringLists once everything has been read.

    While MapCollection(MapC).BasePrimitives.Count <= I Do
     MapCollection(MapC).BasePrimitives.AddObject('',TStringList.Create);

    // Base or alias?

    If B Then
    Begin
      // This object is a base object

      Base := Nil;

      // There HAS to be a TStringList occupying our base slot!

      If MapCollection(MapC).BasePrimitives.Objects[I] Is TStringList Then
      Begin
        L := TStringList(MapCollection(MapC).BasePrimitives.Objects[I]);

        // If any DrawPrimitives have already been read in that are supposed to
        // be aliases of this object, add them to our alias list

        For J := 0 To L.Count - 1 Do
        Begin
          Copies.AddObject('',L.Objects[J]);
          (L.Objects[J] As DrawPrimitive).Base := Self;
        End; // For J

        // Get rid of the TStringList since the base object is here now and has
        // an alias list of its own.  Put this object in its base list slot.

        L.Free;
        MapCollection(MapC).BasePrimitives.Objects[I] := Self;

        // Now that it's been read, calculate the extent

        ComputeExtent;
      End;
    End
    Else
    Begin
      // Get this alias object's base object.  It might not have been read in yet,
      // and in that case is a mere TStringList placeholder.

      P := MapCollection(MapC).BasePrimitives.Objects[I];
      If P Is DrawPrimitive Then
      Begin
      
        // The base object HAS been read in already, so make it our base and put this
        // object in its alias list

        D    := MapCollection(MapC).BasePrimitives.Objects[I] As DrawPrimitive;
        Base := D;
        D.Copies.AddObject('',Self);
        CopyFromBase(True);
        ComputeExtent;
      End
      Else
      Begin
        // The base object hasn't been read in yet, so all we have is this
        // TStringList placeholder.  Add this object to the placeholder and, when
        // the base object is finally read in, it will gather all the aliases that
        // have been read in already and deal with them.

        (P As TStringList).AddObject('',Self);

        // DoRead() (this routine) is the first thing called from
        // DrawPrimitive.Read() method, after which the rest of the parameters for
        // that particular object are then read in.  However, there is an issue: we
        // KNOW this is to be an alias object, but its base object hasn't been read
        // in yet.  This is a problem for DrawPrimitive objects that dynamically
        // allocate memory (e.g. polylines and polycurves), since we don't want to
        // do that for alias objects.  Normally we would merely check to see if
        // Base is Nil or not, but Base hasn't been read in yet!  Our solution is
        // to point Base to THIS DrawObject to fool its DoRead() method into
        // skipping the dynamic allocation part.  The dynamically allocated parts
        // will have to be made to point to those of the base object AFTER the base
        // object has been read in.  This is accomplished in DrawPrimitive.Read()
        // where the base object calls RefreshCopies().

        Base := Self; // Do this to fool PolyLine/PolyCurve.ReadFromDOMElement()
      End;
    End;
  End;
end; // DrawPrimitive.DoRead

procedure DrawPrimitive.Read(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  // We're splitting off the bulk of the read functionality so that the last line
  // in this routine can be made common to all reads

  DoRead(stream, version, Full,UseAliasInfo);

  // The aliases may have been read in before the base object (objects are stored
  // in order of back-to-front drawing, NOT in order of base index, so this is
  // normal).  In that case, calling RefreshCopies causes the aliases to point
  // their dynamically allocated pointers to those of their bases (think of it as
  // a fixup).

  If Not UseAliasInfo Then AddToBaseOrCopies Else RefreshCopies;
end;

procedure DrawPrimitive.Write(stream: TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
Var
  I : Integer;
  B : Boolean;

begin
  stream.WriteBuffer(fColor,sizeof(fColor));
  stream.WriteBuffer(fOverlay,sizeof(fOverlay));
  stream.WriteBuffer(fExtent,sizeof(fExtent));   //!!
  If UseAliasInfo Then
  Begin
    B := (Base = Nil);
    Stream.WriteBuffer(B,SizeOf(B));
    If B
     Then I := MapCollection(MapC).BasePrimitives.IndexOfObject(Self)
     Else I := MapCollection(MapC).BasePrimitives.IndexOfObject(Base);
    Stream.WriteBuffer(I,SizeOf(I));
    Stream.WriteBuffer(Alias.X,SizeOf(Alias.X));
    Stream.WriteBuffer(Alias.Y,SizeOf(Alias.Y));
  End;
end;

Function DrawPrimitive.ReadBaseFromDOMElement(E: TDOMElement): Boolean;
// Given an XML tree or subtree, this routine first determines if the new object
// is a base or alias object.  It then takes appropriate action as described below.
Var
  I,J : Integer;
  B   : Boolean;
  P   : TObject;
  D   : DrawPrimitive;
  L   : TStringList;

Begin
  B := GetBooleanProperty(E,'BASE');      // Is this a base or an alias?

  // If this is a base, then BASEINDEX represents the INDEX in the base object list
  // where this object should be placed.  If this object is an alias, it represents
  // the INDEX of its base object in the base object list.

  I := GetIntegerProperty(E,'BASEINDEX');

  // Objects are read back as they were written, where the order is based on the
  // MapCollection's linked list.  This ordering is based on back-to-front drawing
  // than on base object index, and in general the base indexes will jump around
  // as the objects are read from the XML tree.  For indexes that we haven't read
  // in yet (up to and INCLUDING this one), put a TStringList in which will be
  // used later.  If all the code works as designed, there should NEVER be any
  // leftover TStringLists once everything has been read.

  While MapCollection(MapC).BasePrimitives.Count <= I Do
   MapCollection(MapC).BasePrimitives.AddObject('',TStringList.Create);

  // Base or alias?

  If B Then
  Begin
    // This object is a base object

    Base := Nil;

    // There HAS to be a TStringList occupying our base slot!

    If MapCollection(MapC).BasePrimitives.Objects[I] Is TStringList Then
    Begin
      L := TStringList(MapCollection(MapC).BasePrimitives.Objects[I]);

      // If any DrawPrimitives have already been read in that are supposed to
      // be aliases of this object, add them to our alias list

      For J := 0 To L.Count - 1 Do
      Begin
        Copies.AddObject('',L.Objects[J]);
        (L.Objects[J] As DrawPrimitive).Base := Self;
      End; // For J

      // Get rid of the TStringList since the base object is here now and has
      // an alias list of its own.  Put this object in its base list slot.

      L.Free;
      MapCollection(MapC).BasePrimitives.Objects[I] := Self;

      // Now that it's been read, calculate the extent

      ComputeExtent;
    End;
  End
  Else
  Begin
    // This object is an alias object.  First read in where this alias is to be
    // placed.
    //
    // Note that this differs from DrawPrimitive.DoRead() where Alias is ALWAYS
    // saved and read back in, even for base objects.  Due to the size and memory
    // footprint of XML files, I'm putting it here to save memory and disk space.
    // If the program is ever changed such that Alias is needed for both base and
    // alias objects, then this needs to be moved (see DrawPrimitive.DoRead() to
    // see now it's handled differently).

    Alias := GetCoordPointProperty(E,'POS');

    // Get this alias object's base object.  It might not have been read in yet,
    // and in that case is a mere TStringList placeholder.

    P     := MapCollection(MapC).BasePrimitives.Objects[I];
    If P Is DrawPrimitive Then
    Begin
      // The base object HAS been read in already, so make it our base and put this
      // object in its alias list

      D    := MapCollection(MapC).BasePrimitives.Objects[I] As DrawPrimitive;
      Base := D;
      D.Copies.AddObject('',Self);
      CopyFromBase(True);
      ComputeExtent;
    End
    Else
    Begin
      // The base object hasn't been read in yet, so all we have is this
      // TStringList placeholder.  Add this object to the placeholder and, when
      // the base object is finally read in, it will gather all the aliases that
      // have been read in already and deal with them.

      (P As TStringList).AddObject('',Self);

      // ReadBaseFromDOMElement (this routine) is generally the first thing called
      // from a DrawPrimitive's ReadFromDOMElement() method, after which the rest
      // of the parameters for that particular object are then read in.  However,
      // there is an issue: we KNOW this is to be an alias object, but its base
      // object hasn't been read in yet.  This is a problem for DrawPrimitive
      // objects that dynamically allocate memory (e.g. polylines and polycurves),
      // since we don't want to do that for alias objects.  Normally we would
      // merely check to see if Base is Nil or not, but Base hasn't been read in
      // yet!  Our solution is to point Base to THIS DrawObject to fool its
      // DoRead() method into skipping the dynamic allocation part.  The
      // dynamically allocated parts will have to be made to point to those of
      // the base object AFTER the base object has been read in.  This is
      // accomplished in the DrawPrimitive DoRead() methods where the base object
      // calls RefreshCopies().

      Base := Self; // Do this to fool PolyLine/PolyCurve.ReadFromDOMElement()
    End;
  End;
  Result := Not B;
End; // DrawPrimitive.ReadBaseFromDOMElement

Function DrawPrimitive.GetAsAliasDOMElement(D: TDOMDocument; E: TDOMElement): Boolean;
// This simply reads the basic base/alias information from the XML tree.  It's generally
// the first thing read in for a DrawPrimitive.
Begin
  E.appendChild(NewBooleanProperty(D,'BASE',Base = Nil));
  If Base <> Nil Then
  Begin
    E.appendChild(NewIntegerProperty(D,'BASEINDEX',MapCollection(MapC).BasePrimitives.IndexOfObject(Base)));
    E.appendChild(NewCoordPointProperty(D,'POS',Alias));
    Result := True;
  End
  Else
  Begin
    E.appendChild(NewIntegerProperty(D,'BASEINDEX',MapCollection(MapC).BasePrimitives.IndexOfObject(Self)));
    Result := False;
  End;
End; // DrawPrimitive.GetAsAliasDOMElement

Procedure DrawPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Base = Nil Then FColor := TColor(GetCardinalProperty(E,'COLOR'));
  fOverlay := GetCardinalProperty(E,'OVERLAY_ID');
  fExtent  := GetCoordRectProperty(E,'EXTENT');
End; // DrawPrimitive.ReadFromDOMElement

Function DrawPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(XMLIDFromCharID(GetID));
  If Base = Nil Then E.appendChild(NewCardinalProperty(D,'COLOR',Cardinal(fColor)));
  E.appendChild(NewCardinalProperty(D,'OVERLAY_ID',fOverlay));
  E.appendChild(NewCoordRectProperty(D,'EXTENT',fExtent));
  If Undo Then E.appendChild(NewBooleanProperty(D,'SELECTED',IsSelected));
  Result := E;
End; // DrawPrimitive.GetAsDOMElement

{------------------------------------------------------------------------------}
constructor SymbolPrimitive.CreateBlank;
begin
  inherited Create;
  Font          := TFont.Create;
  Font.Color    := clBlack;
  Font.Name     := IconFontName;
  Font.Size     := 1;
  Font.Style    := [];
  Size          := 0;
  Angle         := 0;
  CH            := 0;
  CW            := 0;
  CHX           := 0;
  X1            := 0;
  Y1            := 0;
  Text          := '';
  fOutlineColor := clBlack;
  // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
  Font.Charset  := DEFAULT_CHARSET;
end;

constructor SymbolPrimitive.CreateInternal(const View:ViewPoint; ix,iy:Coord; symbol:string; iFont:TFont;  outlinecolor:TColor);
begin
  inherited Create;

  fColor        := iFont.Color;
  fOutlineColor := outlinecolor;
  Font          := TFont.Create;
  Font.Assign(iFont);
  Text          := symbol;
  Size          := 0;
  Angle         := 0;

  if View <> nil then ComputeSize(View)
  else
  begin
    cw  := 0;
    ch  := 0;
    chx := 0;
  end;

  x1 := ix;
  y1 := iy;

  ComputeExtent;
end;

Procedure SymbolPrimitive.CopyFromBase(AliasOnly: Boolean);
Var SP: SymbolPrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    SP   := Base As SymbolPrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    ch   := SP.ch;
    cw   := SP.cw;
    chx  := SP.chx;
    Text := SP.Text;
    If AliasOnly Then
    Begin
      If (Font <> SP.Font) And (Font <> Nil) Then Font.Free;
      Font := SP.Font;
    End
    Else
    Begin
      If (Font <> SP.Font) And (Font <> Nil) Then Font.Free;
      Font := TFont.Create;
      Font.Assign(SP.Font);
    End;
    Size          := SP.Size;
    Angle         := SP.Angle;
    fOutlineColor := SP.fOutlineColor;
    X1            := SP.X1;
    Y1            := SP.Y1;
    If Not AliasOnly Then
    Begin
      X1 := X1 + (Alias.X - Base.fExtent.Left);
      Y1 := Y1 + (Alias.Y - Base.fExtent.Top);
    End;
  End;
End; // SymbolPrimitive.CopyFromBase

function SymbolPrimitive.SelectClick(const within:double; p:CoordPoint):boolean;
begin
  Result:=PtInCoordRect(fExtent, p);
end;

procedure SymbolPrimitive.ComputeSize(const View:ViewPoint);
var w,h:integer;
begin
  View.Canvas.Font.Assign(Font);

  { Defeat the built-in logic for scaling up fonts for the printer,
    since we automatically do that as a result of matching the font
    size to the zoom level. Reset the font height after assigning
    the font into the canvas.}
  View.Canvas.Font.Height:=Font.Height;

  w:=View.Canvas.TextWidth(text);
  h:=View.Canvas.TextHeight(text);

  View.DeltaScreenToCoord(w,h,cw,ch);
  chx:=ch;
end;


constructor SymbolPrimitive.Create(const View:ViewPoint; ix,iy:Coord; symbol:string; isize:integer; outlinecolor:TColor);
begin
  inherited Create;

  fColor         := CurrentColor; {CurrentIconColor}
  fOutlineColor  := outlinecolor;
  Font           := TFont.Create;
  Font.Color     := clBlack;
  Text           := symbol;

  Font.Name      := IconFontName;
  // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
  Font.Charset   := DEFAULT_CHARSET;
  Font.Height    := isize;
  Font.Size      := 1;
  Font.Style     := [];
  size           := isize;
  angle          := 0;

  if View <> nil then ComputeSize(View)
  else
  begin
    ch  := 0;
    cw  := 0;
    chx := 0;
  end;

  x1 := ix;
  y1 := iy;

  ComputeExtent;
end;

function SymbolPrimitive.Copy: DrawPrimitive;
begin
  Result:=SymbolPrimitive.Create(Map.CurrentView, x1, y1, Text, size, fOutlineColor);
  Result.MakeCopy(Self);
{
  Result.CopyCore(Self);
  Result.fExtent := fExtent;
  (Result as SymbolPrimitive).angle:=angle;
  (Result as SymbolPrimitive).ch:=ch;
  (Result as SymbolPrimitive).chx:=chx;
  (Result as SymbolPrimitive).cw:=cw;
}
end;

function SymbolPrimitive.RotatedBox(w,h:Coord; formatflags:integer):CoordRect;
var ox,oy,cx,cy:Coord;
    dist,theta:double;
begin
  {Start marking extent at upper left x,y}
  ox:=x1; oy:=y1;
  theta:=(-angle*2*pi)/3600;
  { Adjust the initial point depending on the format flags }
  if (formatflags and DT_CENTER)<>0 then begin
    ox:=ox - (cos(theta)*w)/2;
    oy:=oy - (sin(theta)*w)/2;
    end
  else if (formatflags and DT_RIGHT)<>0 then begin
    ox:=ox - cos(theta)*w;
    oy:=oy - sin(theta)*w;
    end;

  if (formatflags and DT_VCENTER)<>0 then begin
    ox:=ox - (cos(theta+pi/2)*h)/2;
    oy:=oy - (sin(theta+pi/2)*h)/2;
    end;

  Result:=MakeCoordRect(ox,oy,ox,oy);

  { Compute the height vector of the text, and include in the extent }
  cx:=ox + h*(cos(theta+pi/2));
  cy:=oy + h*(sin(theta+pi/2));
  Encompass(Result, cx,cy);

  { Compute the width vector of the text, and include in the extent }
  cx:=ox + w*(cos(theta));
  cy:=oy + w*(sin(theta));
  Encompass(Result, cx,cy);

  { Compute the diagonal vector of the text, and include in the extent }
  dist  := distance(0, 0, w, h);
  theta := theta + arctan2(h, w);
  cx:=ox + dist*(cos(theta));
  cy:=oy + dist*(sin(theta));
  Encompass(Result, cx,cy);
end;

procedure SymbolPrimitive.ComputeExtent;
var NewChain:PolyLinePrimitive;
    Skip:DrawPrimitive;
    X,Y: Coord;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    if Decompose(Map.CurrentView, DrawPrimitive(NewChain), false) then begin
      fExtent:=NewChain.Extent;

      while (NewChain<>nil) do begin
        Skip:=NewChain.Next;
        NewChain.Free;
        NewChain:=PolylinePrimitive(Skip);
        if (NewChain<>nil) then begin
          Encompass(fExtent, NewChain.Extent.left,NewChain.Extent.top);
          Encompass(fExtent, NewChain.Extent.right,NewChain.Extent.bottom);
          end;
        end;
      end
    else Extent:=RotatedBox(cw,ch,DT_CENTER or DT_VCENTER);
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

Function SymbolPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: SymbolPrimitive;
Begin
  If D Is SymbolPrimitive Then
  Begin
    P := SymbolPrimitive(D);

    // Perhaps sometime in the future this check could take objects' relative
    // size into account, or at least scale VeryClose based on their size to
    // handle very small objects.  I'm not sure on this at the moment...

    Result := (Abs(CW - P.CW) < VeryClose) And
              (Abs(CH - P.CH) < VeryClose) And
              (Abs(CHX - P.CHX) < VeryClose) And
              (Size = P.Size) And
              (Text = P.Text) And
              (Angle = P.Angle) And
              (fOutlineColor = P.fOutlineColor) And
              (Font.Name = P.Font.Name) And
              (Font.Size = P.Font.Size) And
              (Font.Color = P.Font.Color) And
              (Font.Style = P.Font.Style) And Inherited IsSimilarTo(D);
  End
  Else Result := False;
End; // SymbolPrimitive.IsSimilarTo

function SymbolPrimitive.ApplyMatrix(var mat:Matrix):boolean;
var wx,wy:Coord;
//    cwx,cwy:Coord;
//    hx,hy:Coord;
    f:Coord;

begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;      
    End;
  End;

//  hx:=x1;       hy:=y1+ch;
  wx:=x1+cw;    wy:=y1;
//  cwx:=x1+chx;  cwy:=y1;
  MultiplyPointByMatrix(x1,y1, mat);
  MultiplyPointByMatrix(wx,wy, mat);
//  MultiplyPointByMatrix(hx,hy, mat);
//  MultiplyPointByMatrix(cwx,cwy, mat);
//  cw:=Round(distance(x1,y1,wx,wy));
//  ch:=Round(distance(x1,y1,hx,hy));
//  chx:=Round(distance(x1,y1,cwx,cwy));
  if ((mat[1,1]<>0.0) or (mat[2,2]<>0.0)) and (mat[1,2]=0.0) and (mat[2,1]=0.0) then begin
    f:= (mat[1,1]+mat[2,2])*0.5;
    cw := cw * f;
    ch := ch * f;
    chx:= chx* f;
    end;

  angle:=angle-trunc(3600*(arctan2(wy-y1,wx-x1)/(2*pi)));
  Result:=true;
end;

function SymbolPrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
begin
  if inherited MoveHandle(View,mode,origx,origy,dx,dy) then
  begin
    x1:=x1+dx;
    y1:=y1+dy;
    Result:=true;
  end
  else
  Result:=false;
end;

Procedure SymbolPrimitive.ClearThis(Deallocate: Boolean);
Begin
  If (Font <> Nil) And Deallocate Then
  Begin
    Font.Free;
    Font := Nil;
  End;
End; // SymbolPrimitive.ClearThis

function SymbolPrimitive.PrepareFont(const View:ViewPoint; var sx,sy:integer; formatflags:integer):boolean;
var w,h:integer;
    pt:CoordPoint;

  function FindCenterPoint(w,h:double):CoordPoint;
  var theta:double;
  begin
    {Start marking extent at upper left x,y}
    Result.x:=x1; Result.y:=y1;
    theta:=(-angle*2*pi)/3600;
    { Adjust the initial point depending on the format flags }
    if (formatflags and DT_CENTER)<>0 then begin
      Result.x:=Result.x - (cos(theta)*w)/2;
      Result.y:=Result.y - (sin(theta)*w)/2;
      end
    else if (formatflags and DT_RIGHT)<>0 then begin
      Result.x:=Result.x - cos(theta)*w;
      Result.y:=Result.y - sin(theta)*w;
      end;

    if (formatflags and DT_VCENTER)<>0 then begin
      Result.x:=Result.x - (cos(theta+pi/2)*h)/2;
      Result.y:=Result.y - (sin(theta+pi/2)*h)/2;
      end;
  end;

begin
  Result:=false;

  View.DeltaCoordToScreen(chx,ch,w,h);
  if (w<h) then h:=w;
  if (h<6) then exit;

  if formatflags=0 then begin
    View.CoordToScreen(x1,y1,sx,sy);
    end
  else begin
    pt:=FindCenterPoint(cw,ch);
    View.CoordToScreen(pt.x,pt.y,sx,sy);
    end;

  Font.Height := h;

  Font.Color:=DisplayColor(GetColor);

  if (angle<>0) then begin
    RotatedFont(Font,angle);
    View.Canvas.Font.Handle:=Font.Handle;
    View.Canvas.Font.Color:=DisplayColor(GetColor);
    end
  else begin
    View.Canvas.Font.Assign(Font);

    { Defeat the built-in logic for scaling up fonts for the printer,
      since we automatically do that as a result of matching the font
      size to the zoom level. Reset the font height after assigning
      the font into the canvas.}
    View.Canvas.Font.Height:=h;
    end;

  View.Canvas.Brush.Style := bsClear;
  Result:=true;
end;

function SymbolPrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var sx,sy:integer;
    count:integer;
    pointtype:PByteArray;
    points:PPointArray;
    i,pos,subsize:integer;
    n,tail:PolylinePrimitive;
    pcoord:PCoordArray;
    style:StyleAttrib;
    total:integer;
    inside:PolylinePrimitive;
    solid:boolean;
begin
  NewChain := nil;
  Result   := true;
  if PrepareFont(View,sx,sy,DT_CENTER or DT_VCENTER) then
  begin
    { Draw the text into a path }
    BeginPath(View.Canvas.Handle);
    View.Canvas.TextOut(sx, sy, Text);
    EndPath(View.Canvas.Handle);
    { Convert curves to straight lines }
    FlattenPath(View.Canvas.Handle);

    { Get the lines }
    points    := nil;
    pointtype := nil;
    count     := GetPath(View.Canvas.Handle, points^, pointtype^, 0);

    GetMem(pointtype, count*sizeof(char));
    GetMem(points, count*sizeof(TPoint));
    GetPath(View.Canvas.Handle, points^, pointtype^, count);

    { Convert into polylines }
    style.Bits := 0;
    style.Line := 1;
    Style.FullStyle.Thickness  := 0;
    Style.FullStyle.SThickness := 0;
    tail       := nil;
    pos        := 0;
    total      := 0;
    NewChain   := nil;

    while pos < count do
    begin
      subsize := 0;
      while (pos + subsize < count) and
            ((pointtype^[pos + subsize] and PT_CLOSEFIGURE) = 0) do
      begin
        inc(subsize);
      end; // While
      { Count the last point }
      inc(subsize);

      GetMem(pcoord,sizeof(CoordPoint) * (subsize + 1));
      for i := 0 to subsize - 1 do
      begin
        View.ScreenToCoord(GetAdjustedX(points^[i + pos].X),GetAdjustedY(points^[i + pos].Y),
                           pcoord^[i].X,pcoord^[i].Y);
      end; // For i
      inc(pos,subsize);

      pcoord^[subsize] := pcoord^[0];

      inc(total);
      n := PolyLinePrimitive.Create(pcoord, subsize + 1, Style);
      n.CopyCore(self);
      if (tail = nil) then NewChain := n else tail.Next := n;
      tail := n;
    end; // While

    FreeMem(pointtype);
    FreeMem(points);

    if testinside then
    begin
      { Now color the objects based on whether or not the polygon
        in question is inside another one. }
      n := (NewChain as PolylinePrimitive);
      for i := 1 to total do
      begin
        tail   := (NewChain as PolylinePrimitive);
        inside := nil;
        while tail <> n do
        begin
          if tail.Inside(n.points^[0].X,n.points^[0].Y) then inside := tail;
          tail := (tail.Next as PolylinePrimitive);
        end; // While

        if inside = nil
         then solid := true
         else solid := (inside.fillcolor <> fColor);

        if solid then
        begin
          n.fillcolor := fColor;
          n.fColor    := fOutlineColor;
        end
        else
        begin
          n.fillcolor := MainForm.BackgroundColor.Color;
          n.fColor    := fOutlineColor;
        end;

        { You can select none as a outline color but that's
          illegal for a "normal" color.  If that's the case, then
          make the outline the same as the fill so it disappears. }
        if n.fColor = clNone then
        begin
          n.fColor := n.fillcolor;
        end;
        n := (n.Next as PolylinePrimitive);
      end;
    end;
  end;
  { In the pathological case where a decompose actually
    would destroy the object (i.e., decomposing a space),
    fail the decompose. }
  if NewChain = nil then Result := false;
end;

procedure SymbolPrimitive.Draw(View:ViewPoint);
var sx,sy:integer;
    OldWidth:integer;
    OldColor:TColor;
    V : ViewPoint;
begin
  if PrepareFont(View,sx,sy,DT_CENTER or DT_VCENTER) then
  begin
    V := MakeAliasView(View);
    if ((V.QuickDraw and QuickDraw_Fills)=0) and (fOutlineColor<>clNone) then
    begin
      BeginPath(V.Canvas.Handle);
      V.Canvas.TextOut(sx, sy, Text);
      EndPath(V.Canvas.Handle);
      OldWidth := V.Canvas.Pen.Width;
      OldColor := V.Canvas.Pen.Color;
      V.Canvas.Pen.Width:=2;
      V.Canvas.Pen.Color:=DisplayFillColor(fOutlineColor);
      StrokePath(V.Canvas.Handle);
      V.Canvas.Pen.Width:=OldWidth;
      V.Canvas.Pen.Color:=OldColor;
      AbortPath(V.Canvas.Handle);
    end;
    V.Canvas.TextOut(sx, sy, Text);
    If V <> View Then V.Free;
  end;
end;

procedure SymbolPrimitive.Move(dx,dy:Coord);
begin
  x1 := x1 + dx;
  y1 := y1 + dy;
  inherited Move(dx,dy);
end;

function SymbolPrimitive.GetTextAttrib:TextAttrib;
begin
  Result.Valid := [tatIconSize,tatOutlineColor];
  Result.IconSize:=Size;
  Result.FontOutlineColor:=fOutlineColor;
end;

function SymbolPrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  Result:=false;
  if tatIconSize in attrib.Valid then begin
    SplitOff;
    cw:=(cw*attrib.IconSize)/Size;
    ch:=(ch*attrib.IconSize)/Size;
    chx:=(chx*attrib.IconSize)/Size;
    Size:=attrib.IconSize;
    Result:=true;
    end;
  if tatOutlineColor in attrib.Valid then begin
    If Not Result Then SplitOff;
    fOutlineColor := attrib.FontOutlineColor;
    Result:=true;
    end;
  if Result=true then ComputeExtent;
end;

function SymbolPrimitive.GetId:char;
begin
  Result:='S';
end;

procedure SymbolPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(x1,sizeof(x1));
  stream.ReadBuffer(y1,sizeof(y1));
  stream.ReadBuffer(cw,sizeof(cw));
  stream.ReadBuffer(ch,sizeof(ch));
  stream.ReadBuffer(chx,sizeof(chx));
//!!    chx:=ch;
//!!    x1:=x1+cw/2;
//!!    y1:=y1+ch/2;
  stream.ReadBuffer(size,sizeof(size));
  stream.ReadBuffer(angle,sizeof(angle));
  Text:=ReadStringFromStream(stream);
  stream.ReadBuffer(fOutlineColor,sizeof(fOutlineColor));
end;

procedure SymbolPrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
Var X,Y: Coord; // Must be Coords!!!
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  X := GetAdjustedX(X1) + AddX;
  Y := GetAdjustedY(Y1) + AddY;
  stream.WriteBuffer(X,sizeof(x1));
  stream.WriteBuffer(Y,sizeof(y1));
  stream.WriteBuffer(cw,sizeof(cw));
  stream.WriteBuffer(ch,sizeof(ch));
  stream.WriteBuffer(chx,sizeof(chx));
  stream.WriteBuffer(size,sizeof(size));
  stream.WriteBuffer(angle,sizeof(angle));
  WriteStringToStream(stream,Text);
  stream.WriteBuffer(fOutlineColor,sizeof(fOutlineColor));
end;

Procedure SymbolPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    X1            := GetCoordProperty(E,'X1');
    Y1            := GetCoordProperty(E,'Y1');
    CW            := GetCoordProperty(E,'CW');
    CH            := GetCoordProperty(E,'CH');
    CHX           := GetCoordProperty(E,'CHX');
    Size          := GetIntegerProperty(E,'SIZE');
    Angle         := GetIntegerProperty(E,'ANGLE');
    Text          := MimeDecodeString(Trim(GetStringProperty(E,'TEXT')));
    fOutlineColor := TColor(GetCardinalProperty(E,'OUTLINE_COLOR'));
    RefreshCopies;
  End;
End; // SymbolPrimitive.ReadFromDOMElement

Function SymbolPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordProperty(D,'X1',X1));
    E.appendChild(NewCoordProperty(D,'Y1',Y1));
    E.appendChild(NewCoordProperty(D,'CW',CW));
    E.appendChild(NewCoordProperty(D,'CH',CH));
    E.appendChild(NewCoordProperty(D,'CHX',CHX));
    E.appendChild(NewIntegerProperty(D,'SIZE',Size));
    E.appendChild(NewIntegerProperty(D,'ANGLE',Angle));
    E.appendChild(NewStringProperty(D,'TEXT',MimeEncodeString(Text)));
    E.appendChild(NewCardinalProperty(D,'OUTLINE_COLOR',fOutlineColor));
  End;
  Result := E;
End; // SymbolPrimitive.GetAsDOMElement

//procedure SymbolPrimitive.FixExtent;
//var s:string;
//begin
//(*  s := 'x1=' + FloatToStr(x1) + #13#10 +
//       'y1=' + FloatToStr(y1) + #13#10 +
//       'cw=' + FloatToStr(cw) + #13#10 +
//       'ch=' + FloatTostr(ch) + #13#10 +
//       'chx='+ FloatTostr(chx) + #13#10 +
//       'size=' + FloatTostr(size) + #13#10 +
//       'Font.Size=' + FloatTostr(Font.Size) + #13#10 +
//       'fExtent.Left=' + FloatTostr(fExtent.Left) + #13#10 +
//       'fExtent.Top=' + FloatTostr(fExtent.Top) + #13#10 +
//       'fExtent.Right=' + FloatTostr(fExtent.Right) + #13#10 +
//       'fExtent.Bottom=' + FloatTostr(fExtent.Bottom) + #13#10;
//
//  MessageBox(0,PChar(s),'Info',MB_OK);
//
//*)
//
//  // This gross hack is to make sure that we always
//  // have our old-style font symbols in the symbol
//  // library with a known quantity for sizing, which
//  // makes them amenable to displaying properly in
//  // the Favorites tabs.
//  x1:=  0;
//  y1:=  0;
//  cw:=  1250;
//  chx:= 2500;
//  ch:=  2500;
//  size:= 36;
//  angle:= 0;
//  fExtent.Left  := -1250;
//  fExtent.Top   := -1250;
//  fExtent.Right := 1250;
//  fExtent.Bottom:= 1250;
//end;

{------------------------------------------------------------------------------}
constructor TextPrimitive.CreateBlank;
begin
  inherited CreateBlank;
end;

constructor TextPrimitive.Create(const View:ViewPoint; ix,iy:Coord; itext:string; IFont:TFont; iformatflags:integer; outline:TColor);
begin
  inherited CreateInternal(View, ix,iy, itext, IFont, outline);
  formatflags:=iformatflags;
  //Size:=IFont.Size;
  ComputeExtent;
end;

Procedure TextPrimitive.CopyFromBase(AliasOnly: Boolean);
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    FormatFlags := (Base As TextPrimitive).FormatFlags;
  End;
End; // TextPrimitive.CopyFromBase

function TextPrimitive.Copy:DrawPrimitive;
begin
  Result:=TextPrimitive.Create(Map.CurrentView, x1, y1, Text, Font, formatflags, fOutlineColor);
  Result.MakeCopy(Self);
{
  Result.CopyCore(Self);
  Result.fExtent := fExtent;
  (Result as SymbolPrimitive).angle:=angle;
  (Result as SymbolPrimitive).ch:=ch;
  (Result as SymbolPrimitive).chx:=chx;
  (Result as SymbolPrimitive).cw:=cw;
}
end;

function TextPrimitive.ApplyMatrix(var mat:Matrix):boolean;
//var oldch:Coord;
begin
//  oldch := ch;
  Result:=inherited ApplyMatrix(mat);

//  if (oldch<>0) then begin
//    size := Trunc(size * (ch/oldch));
//    end;
end;

procedure TextPrimitive.ComputeExtent;
var tw,th:Coord;
    fScale:double;
    r:TRect;
    ncw,nch:Coord;
    X,Y: Coord;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    Font.Size:=Size;
    Map.CurrentView.Canvas.Font.Assign(Font);
    r:=Rect(0,0,1,1);
    DrawText(Map.CurrentView.Canvas.Handle, PChar(text), -1, r, DT_CALCRECT or formatflags);

    Map.CurrentView.DeltaScreenToCoord(r.right-r.left,r.bottom-r.top,tw,th);

    Map.CurrentView.DeltaScreenToCoord(0,Map.CurrentView.Canvas.TextHeight(text),ncw,nch);
    if (nch<>0) then begin
      fScale := ch/nch;
      tw:=tw*fScale;
      th:=th*fScale;
      end;

    Extent:=RotatedBox(tw,th,formatflags);
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

procedure TextPrimitive.Draw(View:ViewPoint);
var sx,sy:integer;
    r:TRect;
    OldWidth:integer;
    OldColor:TColor;
    V : ViewPoint;
begin
  if PrepareFont(View,sx,sy,0) then
  begin
    r := Rect(sx,sy,sx+1,sy+1);
    V := MakeAliasView(View);
    if ((V.QuickDraw and QuickDraw_Fills)=0) and (fOutlineColor<>clNone) then
    begin
      BeginPath(V.Canvas.Handle);
      DrawText(V.Canvas.Handle, PChar(text), -1, r, DT_NOCLIP or formatflags);
      EndPath(V.Canvas.Handle);
      OldWidth := V.Canvas.Pen.Width;
      OldColor := V.Canvas.Pen.Color;
      V.Canvas.Pen.Width:=5;
      V.Canvas.Pen.Color:=DisplayFillColor(fOutlineColor);
      StrokePath(V.Canvas.Handle);
      V.Canvas.Pen.Width:=OldWidth;
      V.Canvas.Pen.Color:=OldColor;
    end;
    DrawText(V.Canvas.Handle, PChar(text), -1, r, DT_NOCLIP or formatflags);
    If V <> View Then V.Free;
  end;
end;

function TextPrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  Result:=false;
  if tatFontSize in attrib.Valid then begin
    SplitOff;
    if Settings.RelativeTextSize.Checked then begin
      cw:=(cw*attrib.FontSize)/Size;
      ch:=(ch*attrib.FontSize)/Size;
      chx:=(chx*attrib.FontSize)/Size;
      Size:=attrib.FontSize;
    end else begin
      Font.Size:=attrib.FontSize;
      ComputeSize(View);
    end;
    Result:=true;
    end;
  if tatText in attrib.Valid then begin
    If Not Result Then SplitOff;
    Text:=attrib.Text;
    Result:=true;
    end;
  if tatFontName in attrib.Valid then begin
    If Not Result Then SplitOff;
    Font.Name:=attrib.FontName;
    // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
    Font.Charset:=DEFAULT_CHARSET;
    Result:=true;
    end;
  if tatFontBold in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontBold then Font.Style := Font.Style + [fsBold]
                       else Font.Style := Font.Style - [fsBold];
    Result:=true;
    end;
  if tatFontItalic in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontItalic then Font.Style := Font.Style + [fsItalic]
                         else Font.Style := Font.Style - [fsItalic];
    Result:=true;
    end;
  if tatFontUnderline in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontUnderline then Font.Style := Font.Style + [fsUnderline]
                            else Font.Style := Font.Style - [fsUnderline];
    Result:=true;
    end;
  if tatAlignment in attrib.Valid then begin
    If Not Result Then SplitOff;
    formatflags := (formatflags and (not DT_CENTER+DT_RIGHT+DT_LEFT)) or attrib.Alignment;
    Result:=true;
    end;
  if tatOutlineColor in attrib.Valid then begin
    If Not Result Then SplitOff;
    fOutlineColor := attrib.FontOutlineColor;
    Result:=true;
    end;

  if Result=true then ComputeExtent;
end;

function  TextPrimitive.GetTextAttrib:TextAttrib;
begin
  Result.Valid := [tatText, tatFontName, tatFontSize,
                  tatFontBold, tatFontItalic, tatFontUnderline,
                  tatAlignment, tatOutlineColor];

  Result.Text:=text;
  Result.FontName:=Font.Name;
  Result.FontSize:=Size;
  Result.FontBold := fsBold in Font.Style;
  Result.FontItalic := fsItalic in Font.Style;
  Result.FontUnderline := fsUnderline in Font.Style;
  Result.Alignment := formatflags and (DT_LEFT OR DT_RIGHT OR DT_CENTER);
  Result.FontOutlineColor := fOutlineColor;
end;

function TextPrimitive.GetId:char;
begin
  Result:='T';
end;

procedure TextPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
var fontstyle:TFontStyles;
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  Font.Name:=ReadStringFromStream(stream);
  // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
  Font.Charset:=DEFAULT_CHARSET;
  stream.ReadBuffer(fontstyle, sizeof(fontstyle));
  Font.Style:=fontstyle;
  stream.ReadBuffer(formatflags,sizeof(formatflags));
end;

procedure TextPrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
var fontstyle:TFontStyles;
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  WriteStringToStream(stream,Font.Name);
  fontstyle:=Font.Style;
  stream.WriteBuffer(fontstyle, sizeof(fontstyle));
  stream.WriteBuffer(formatflags,sizeof(formatflags));
end;

Procedure TextPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    Font.Name := MimeDecodeString(Trim(GetStringProperty(E,'FONT')));
    // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
    Font.Charset := DEFAULT_CHARSET;
    Font.Style   := [];
    If GetBooleanProperty(E,'BOLD')      Then Font.Style := Font.Style + [fsBold];
    If GetBooleanProperty(E,'ITALIC')    Then Font.Style := Font.Style + [fsItalic];
    If GetBooleanProperty(E,'UNDERLINE') Then Font.Style := Font.Style + [fsUnderline];
    If GetBooleanProperty(E,'STRIKEOUT') Then Font.Style := Font.Style + [fsStrikeOut];
    FormatFlags := GetIntegerProperty(E,'FORMATFLAGS');
    RefreshCopies;
  End;
End; // TextPrimitive.ReadFromDOMElement

Function TextPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewStringProperty(D,'FONT',MimeEncodeString(Font.Name)));
    E.appendChild(NewBooleanProperty(D,'BOLD',fsBold In Font.Style));
    E.appendChild(NewBooleanProperty(D,'ITALIC',fsItalic In Font.Style));
    E.appendChild(NewBooleanProperty(D,'UNDERLINE',fsUnderline In Font.Style));
    E.appendChild(NewBooleanProperty(D,'STRIKEOUT',fsStrikeOut In Font.Style));
    E.appendChild(NewIntegerProperty(D,'FORMATFLAGS',FormatFlags));
  End;  
  Result := E;
End; // TextPrimitive.GetAsDOMElement

Function TextPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: TextPrimitive;
Begin
  If D Is TextPrimitive Then
  Begin
    P := TextPrimitive(D);
    Result := (FormatFlags = P.FormatFlags) And Inherited IsSimilarTo(P);
  End
  Else Result := False;
End; // TextPrimitive.IsSimilarTo

{------------------------------------------------------------------------------}
procedure LinePrimitive.Reverse;
var swp:Coord;
begin
  SplitOff;
  swp:=x1;
  x1:=x2;
  x2:=swp;

  swp:=y1;
  y1:=y2;
  y2:=swp;

  // Swapping endpoints of a normal line doesn't change the size, but it
  // can for a fractal line.
  if (fractal) then ComputeExtent;
end;

Procedure LinePrimitive.CopyFromBase(AliasOnly: Boolean);
Var LP: LinePrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    LP        := Base As LinePrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Style     := LP.Style;
    Seed      := LP.Seed;
    Roughness := LP.Roughness;
    Fractal   := LP.Fractal;
    X1        := LP.X1;
    Y1        := LP.Y1;
    X2        := LP.X2;
    Y2        := LP.Y2;
    If Not AliasOnly Then
    Begin
      X1 := X1 + (Alias.X - Base.fExtent.Left);
      Y1 := Y1 + (Alias.Y - Base.fExtent.Top);
      X2 := X2 + (Alias.X - Base.fExtent.Left);
      Y2 := Y2 + (Alias.Y - Base.fExtent.Top);
    End;
  End;
End; // LinePrimitive.CopyFromBase

function LinePrimitive.SetFractal(state:FractalState):boolean;
begin
  SplitOff;
  fractal := SetFractalState(fractal, state);
  ComputeExtent;
  Result:=true;
end;

function LinePrimitive.SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean;
var isect:CoordPoint;
    pts:PCoordArray;
    XX1,YY1,XX2,YY2 : Coord;
begin
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  if IntersectLine(MakeCoordPoint(XX1,YY1),
                   MakeCoordPoint(XX2,YY2),s1,s2,isect) = IntersectOnLine then
  begin
    GetMem(pts,sizeof(CoordPoint)*3);
    pts^[0].x := XX1;
    pts^[0].y := YY1;
    pts^[1].x := isect.x;
    pts^[1].y := isect.y;
    pts^[2].x := XX2;
    pts^[2].y := YY2;
    np:=PolyLinePrimitive.Create(pts,3,seed,roughness,style, fractal);
    np.CopyCore(self);
    Result:=true;
    end
  else Result:=false;
end;

procedure LinePrimitive.PointClosestTo(x,y:Coord; var px,py:Coord);
var corner:array[0..1] of CoordPoint;
    n:integer;
begin
  corner[0].x := GetAdjustedX1;    corner[0].y := GetAdjustedY1;
  corner[1].x := GetAdjustedX2;    corner[1].y := GetAdjustedY2;

  n:=PointClosestInArray(x,y,@corner,2);

  px:=corner[n].x;
  py:=corner[n].y;
end;

function LinePrimitive.GetLines(const View:Viewpoint; var polycount:integer):PCoordArray;
var p1,p2:CoordPoint;
    continue:TLineContinue;
    x1c,y1c,x2c,y2c:Coord;
begin
  p1 := MakeCoordPoint(GetAdjustedX1,GetAdjustedY1);
  p2 := MakeCoordPoint(GetAdjustedx2,GetAdjustedY2);

  if (View<>nil) then
  begin
    p1 := View.CoordToScreenPt(p1);
    p2 := View.CoordToScreenPt(p2);
  end;

  if not fractal then begin
    polycount:=2;
    GetMem(Result, polycount*sizeof(CoordPoint));
    Result^[0] := p1;
    Result^[1] := p2;
    end
  else begin
    FractalSetSeed(seed);
    continue:=GetLineStyleStart(SEGMENT_STYLE);

    x1c:=p1.x; y1c:=p1.y;
    x2c:=p2.x; y2c:=p2.y;

    FractalLine(nil,x1c,y1c,x2c,y2c,(distance(x1c,y1c,x2c,y2c)*roughness)/1000, continue);
    Result:=GetLineEnd(continue,polycount);
    end;
end;

Function LinePrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: LinePrimitive;
Begin
  If D Is LinePrimitive Then
  Begin
    P := LinePrimitive(D);

    // Perhaps sometime in the future this check could take objects' relative
    // size into account, or at least scale VeryClose based on their size to
    // handle very small objects.  I'm not sure on this at the moment...

    Result := (Abs((X2 - X1) - (P.X2 - P.X1)) < VeryClose) And
              (Abs((Y2 - Y1) - (P.Y2 - P.Y1)) < VeryClose) And
              (Style.Bits = P.Style.Bits) And
              (Style.FullStyle.Thickness = P.Style.FullStyle.Thickness) And
              (Fractal = P.Fractal)       And
              ((Not Fractal) Or
               ((Seed = P.Seed) And
                (Roughness = P.Roughness))) And Inherited IsSimilarTo(D);
  End
  Else Result := False;
End; // LinePrimitive.IsSimilarTo

function LinePrimitive.ApplyMatrix(var mat:Matrix):boolean;
Var XX1,YY1,XX2,YY2,Scale: Coord;
begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;      
    End;
  End;
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  MultiplyPointByMatrix(XX1,YY1,Mat);
  MultiplyPointByMatrix(XX2,YY2,Mat);
  X1 := XX1;
  Y1 := YY1;
  X2 := XX2;
  Y2 := YY2;
  If Style.Line = start_numeric_thickness_style Then
  Begin
    Scale := (mat[1,1] + mat[2,2]) / 2;
    If Style.FullStyle.Thickness  >= 0 Then Style.FullStyle.Thickness  := Style.FullStyle.Thickness  * Scale;
    If Style.FullStyle.SThickness >= 0 Then Style.FullStyle.SThickness := Style.FullStyle.SThickness * Scale;
  End;
  Result := true;
end;

procedure LinePrimitive.Draw(View:ViewPoint);
Var
  SX1,SY1,SX2,SY2 : Integer;
  OldColor        : TColor;
  Continue        : TLineContinue;
  NewStyle        : StyleAttrib;
  T1,T2           : Coord;
  Poly            : PCoordArray;
  PolyPoints      : Integer;

begin
  If Style.Line = start_numeric_thickness_style Then
  Begin
    Poly := GetLines(View, polypoints);
    DrawEnclosedFigure(View.Canvas, Poly, Polypoints, False, Style,
                       DisplayColor(GetColor), DisplayColor(GetColor), View,Fractal);
    ComputeExtent;
    FreeMem(Poly);
  End
  Else
  Begin
    oldcolor:=View.Canvas.Pen.Color;
    View.Canvas.Pen.Color := DisplayColor(GetColor);
    View.CoordToScreen(GetAdjustedX1,GetAdjustedY1,sx1,sy1);
    View.CoordToScreen(GetAdjustedX2,GetAdjustedY2,sx2,sy2);

    NewStyle := View.FixStyle(Style);
    T1       := Style.FullStyle.Thickness;
    T2       := View.Grid.GraphUnitConvert * View.Grid.GraphScale;
    If T2 <> 0 Then T1 := T1 / T2;
    View.DeltaCoordToScreen(T1,0,T1,T2);
    NewStyle.FullStyle.Thickness := T1;
    Style.FullStyle.SThickness   := T1;

    if not fractal then begin
      DrawLineStyle(View.Canvas, sx1,sy1,sx2,sy2, NewStyle);
      end
    else begin
      FractalSetSeed(seed);
      continue:=GetLineStyleStart(NewStyle);
      FractalLine(View.Canvas,sx1,sy1,sx2,sy2,(distance(sx1,sy1,sx2,sy2)*roughness)/1000, continue);
    end;
    View.Canvas.Pen.Color := oldcolor;
  End;
end;

procedure LinePrimitive.DrawHandles(const View:ViewPoint);
begin
  DrawHandle(View,GetAdjustedX1,GetAdjustedY1);
  DrawHandle(View,GetAdjustedX2,GetAdjustedY2);
end;

procedure LinePrimitive.DrawOverlayHandles(const View:ViewPoint);
begin
  DrawOverlayHandle(View,GetAdjustedX1,GetAdjustedX1);
end;

procedure LinePrimitive.Move(dx,dy:Coord);
begin
  If Base = Nil Then
  Begin
    SplitOff;
    x1 := x1 + dx;
    x2 := x2 + dx;
    y1 := y1 + dy;
    y2 := y2 + dy;
  End;
  inherited Move(dx,dy);
end;

constructor LinePrimitive.CreateBlank(frac:boolean);
begin
  inherited Create;
  fractal := frac;
end;

procedure LinePrimitive.ComputeExtent;
Var
  X,Y,XX1,YY1,XX2,YY2 : Coord;
  Pts,Pts1            : PCoordArray;
  NumCount            : Integer;
  I                   : Integer;
  B                   : CoordRect;

begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    XX1 := GetAdjustedX1;
    YY1 := GetAdjustedY1;
    XX2 := GetAdjustedX2;
    YY2 := GetAdjustedY2;
    If Style.Line = start_numeric_thickness_style Then
    Begin
      Pts1 := GetLines(Nil,NumCount);
      Pts  := GetThickLines(Pts1,NumCount,Style,False,False,False);

      fExtent.left   := pts^[0].X;
      fExtent.right  := pts^[0].X;
      fExtent.top    := pts^[0].Y;
      fExtent.bottom := pts^[0].Y;

      for i:=1 to NumCount-1 do begin
        if not fractal then begin
          Encompass(fExtent,pts^[i].X,pts^[i].Y);
          end
        else begin
          FractalSetSeed(seed + i - 1);
          GetFractalBox(pts^[i-1].X,pts^[i-1].Y,
                        pts^[i].X,pts^[i].Y,
                        (distance(pts^[i-1].X,pts^[i-1].Y,pts^[i].X,pts^[i].Y)*roughness)/1000,
                        b);

          Encompass(fExtent, b.left,b.top);
          Encompass(fExtent, b.right,b.bottom);
          end;
        end;

      FreeMem(Pts);
      FreeMem(Pts1);
    End
    Else
    Begin
      if not fractal then
        Extent:=MakeCoordRect(XX1,YY1,XX2,YY2)
      else begin
        FractalSetSeed(seed);
        GetFractalBox(XX1,YY1,XX2,YY2, RFact, fExtent);
      end;
    End;
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

Function LinePrimitive.GetAdjustedX1: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := (Base As LinePrimitive).X1 + (Alias.X - Base.fExtent.Left)
   Else Result := X1;
End; // LinePrimitive.GetAdjustedX1

Function LinePrimitive.GetAdjustedY1: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := (Base As LinePrimitive).Y1 + (Alias.Y - Base.fExtent.Top)
   Else Result := Y1;
End; // LinePrimitive.GetAdjustedY1

Function LinePrimitive.GetAdjustedX2: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := (Base As LinePrimitive).X2 + (Alias.X - Base.fExtent.Left)
   Else Result := X2;
End; // LinePrimitive.GetAdjustedX2

Function LinePrimitive.GetAdjustedY2: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := (Base As LinePrimitive).Y2 + (Alias.Y - Base.fExtent.Top)
   Else Result := Y2;
End; // LinePrimitive.GetAdjustedY2

constructor LinePrimitive.Create(ix1,iy1,ix2,iy2:Coord; istyle:StyleAttrib);
begin
  inherited Create;
  x1 := ix1;
  y1 := iy1;
  x2 := ix2;
  y2 := iy2;
  style:= istyle;
  seed:=0;
  roughness:=0;
  fractal:=false;
  ComputeExtent;
end;

constructor LinePrimitive.Create(ix1,iy1,ix2,iy2:coord; iseed,irough:integer; istyle:StyleAttrib);
begin
  inherited Create;
  x1 := ix1;
  y1 := iy1;
  x2 := ix2;
  y2 := iy2;
  style:= istyle;
  seed:=iseed;
  roughness:=irough;
  fractal:=true;
  ComputeExtent;
end;

constructor LinePrimitive.Create(ix1,iy1,ix2,iy2:coord; iseed,irough:integer; istyle:StyleAttrib; frac:boolean);
begin
  inherited Create;
  x1 := ix1;
  y1 := iy1;
  x2 := ix2;
  y2 := iy2;
  style:= istyle;
  seed:=iseed;
  roughness:=irough;
  fractal:=frac;
  ComputeExtent;
end;

function LinePrimitive.Copy: DrawPrimitive;
begin
  Result := LinePrimitive.Create(GetAdjustedX1,GetAdjustedY1,GetAdjustedX2,GetAdjustedY2,
                                 seed,roughness,style, fractal);
  Result.MakeCopy(Self);
end;

function LinePrimitive.SetStyle(new_style:StyleAttrib):boolean;
begin
  Result:=false;

  if (style.Line <> new_style.Line) and (new_style.Line<>$FF) then begin
    SplitOff;
    style.Line:=new_style.Line;
    Result:=true;
    end;

  if (style.Fill <> new_style.Fill) and (new_style.Fill<>$FF) then begin
    If Not Result Then SplitOff;
    style.Fill:=new_style.Fill;
    Result:=true;
    end;

  if (style.First <> new_style.First) and (new_style.First<>$FF) then begin
    If Not Result Then SplitOff;
    style.First:=new_style.First;
    Result:=true;
    end;

  if (style.Last <> new_style.Last) and (new_style.Last<>$FF) then begin
    If Not Result Then SplitOff;
    style.Last:=new_style.Last;
    Result:=true;
    end;

  If (Style.FullStyle.Thickness <> New_Style.FullStyle.Thickness) And
     (New_Style.FullStyle.Thickness >= 0) Then
  Begin
    If Not Result Then SplitOff;
    Style.FullStyle.Thickness  := New_Style.FullStyle.Thickness;
    Style.FullStyle.SThickness := New_Style.FullStyle.SThickness;
    Result := True;
  End;
end;

function LinePrimitive.GetStyle:StyleAttrib;
begin
  Result:=style;
end;

function LinePrimitive.FindHandle(const View:ViewPoint; x,y:Coord):boolean;
Var XX1,YY1,XX2,YY2: Coord;
begin
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  Result := TestHandle(View, XX1,YY1,x,y) Or
            TestHandle(View, XX2,YY2,x,y);
end;

function LinePrimitive.FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean;
Var XX1,YY1,XX2,YY2: Coord;
begin
  Result := false;
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  if TestHandle(View, XX1,YY1,x,y) then
  begin
    x:=XX1;
    y:=YY1;
    Result:=true;
  end
  Else if TestHandle(View, XX2,YY2,x,y) then
  begin
    x:=XX2;
    y:=YY2;
    Result:=true;
  end;
end;

// Reused by several fractal FindPointOn routines
Function NormalLineFindPointOn(Var X,Y,Angle: Coord; X1,Y1,X2,Y2: Coord): Boolean;
Var
  DX,DY : Double;
  T     : Double;
  XI,YI : Double;

Begin
  DX := X2 - X1;
  DY := Y2 - Y1;
  If (DX <> 0) Or (DY <> 0) Then
  Begin
    T := (DX * (X - X1) + DY * (Y - Y1)) / (Sqr(DX) + Sqr(DY));
    If (T >= 0) And (T <= 1) Then
    Begin
      XI     := X1 + T * DX;
      YI     := Y1 + T * DY;
      X      := XI;
      Y      := YI;
      Angle  := ArcTan2(DY,DX);
      Result := True;
    End
    Else Result := False;
  End
  Else
  Begin
    X      := X1;
    Y      := Y1;
    Result := True;
    If DX = 0 Then
    Begin
      If DY > 0 Then Angle := Pi / 2 Else Angle := 3 * Pi / 2;
    End
    Else
    Begin
      If DX < 0 Then Angle := Pi Else Angle := 0;
    End;
  End;
End; // NormalLineFindPointOn

Function FractalLineFindPointOn(Var X,Y,Angle: Coord; X1,Y1,X2,Y2: Coord; Seed,Roughness: Integer; Const View: ViewPoint): Boolean;
Var
  X0,Y0 : Coord;
  SX1   : Integer;
  SY1   : Integer;
  SX2   : Integer;
  SY2   : Integer;
  Dist  : Double;

  Function DoFractal(Var X,Y,Angle: Coord; Depth: Integer; X1,Y1,X2,Y2: Coord; RFact: Double; Var Dist: Double): Boolean;
  Var
    D     : Double;
    MX,MY : Double;
    PX,PY : Double;
    R     : double;
    B1    : Boolean;
    B2    : Boolean;
    D1    : Double;
    D2    : Double;
    XX1   : Coord;
    YY1   : Coord;
    AA1   : Coord;
    XX2   : Coord;
    YY2   : Coord;
    AA2   : Coord;

  Begin
    D := Distance(X1,Y1,X2,Y2);


    If D <= 2 Then
    Begin
      XX1    := X0;
      YY1    := Y0;
      Result := NormalLineFindPointOn(XX1,YY1,Angle,X1,Y1,X2,Y2);
      X      := XX1;
      Y      := YY1;
      Dist   := Distance(X0,Y0,X,Y);
    End
    Else
    Begin
      // Take the mid-point of the line, and move us some amount above/below the line
      // along the perpendicular.
      UnitPerpendicular(X1,Y1,X2,Y2,PX,PY);
      R  := RFact * ((FixedRandom(Depth) / MaxRand) - 0.5);
      MX := (X2 + X1) * 0.5 + PX * R;
      MY := (Y2 + Y1) * 0.5 + PY * R;

      // Recurse, adjusting down the width.
      XX1 := X0;
      YY1 := Y0;
      D1  := Dist;
      XX2 := X0;
      YY2 := Y0;
      D2  := Dist;
      B1  := DoFractal(XX1,YY1,AA1,Depth * 2,     X1,Y1,MX,MY, RFact * 0.5,D1);
      B2  := DoFractal(XX2,YY2,AA2,Depth * 2 + 1, MX,MY,X2,Y2, RFact * 0.5,D2);
      If B2 And ((Not B1) Or (D2 < D1)) Then
      Begin
        XX1 := XX2;
        YY1 := YY2;
        AA1 := AA2;
        D1  := D2;
        B1  := B2;
      End;
      If B1 And (D1 < Dist) Then
      Begin
        X      := XX1;
        Y      := YY1;
        Angle  := AA1;
        Dist   := D1;
        Result := True;
      End
      Else Result := False;
    End;
  End; // DoFractal

Begin
  View.CoordToScreen(X,Y,X0,Y0);
  FractalSetSeed(Seed);
  View.CoordToScreen(X1,Y1,SX1,SY1);
  View.CoordToScreen(X2,Y2,SX2,SY2);
  Dist   := MaxDouble;
  Result := DoFractal(X0,Y0,Angle,1,SX1,SY1,SX2,SY2,(Distance(SX1,SY1,SX2,SY2) * Roughness) / 1000,Dist);
  If Result Then View.ScreenToCoord(X0,Y0,X,Y);
End; // FractalLineFindPointOn

Function LinePrimitive.FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean;
Var XX1,YY1,XX2,YY2: Coord;
Begin
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  If Fractal
   Then Result := FractalLineFindPointOn(X,Y,Angle,XX1,YY1,XX2,YY2,Seed,Roughness,View)
   Else Result := NormalLineFindPointOn(X,Y,Angle,XX1,YY1,XX2,YY2);
End; // LinePrimitive.FindPointOn

function LinePrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
Var XX1,YY1,XX2,YY2: Coord;
begin
  Result:=false;
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  XX2 := GetAdjustedX2;
  YY2 := GetAdjustedY2;
  if TestHandle(View,XX1,YY1,origx,origy) then
  begin
    SplitOff;
    x1 := origx + dx;
    y1 := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;

  if TestHandle(View,XX2,YY2,origx,origy) then
  begin
    If Not Result Then SplitOff;
    x2 := origx + dx;
    y2 := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;
end;

function LinePrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var fracpoints:PCoordArray;
    polycount:integer;
begin
  // Fractal line can decompose into a polyline
  if fractal then begin
    fracpoints:=GetLines(View,polycount);

    View.ScreenToCoordPtArray(fracpoints, polycount);
    NewChain:=PolyLinePrimitive.Create(fracpoints, polycount, style);
    NewChain.CopyCore(self);
    (NewChain as PolyLinePrimitive).fillcolor:=clNone;
    Result:=true;
    end
  else begin
    // Standard line is a simple as it gets
    Result:=false;
    end;
end;

function LinePrimitive.RFact:double;
begin
  if fractal then
    Result := (distance(GetAdjustedX1,GetAdjustedY1,GetAdjustedX2,GetAdjustedY2)*roughness)/1000
  else
    Result := 0;
end;

function LinePrimitive.GetId:char;
begin
  if not fractal then
    Result:='L'
  else
    Result:='l';
end;

Procedure LinePrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    X1                         := GetCoordProperty(E,'X1');
    Y1                         := GetCoordProperty(E,'Y1');
    X2                         := GetCoordProperty(E,'X2');
    Y2                         := GetCoordProperty(E,'Y2');
    Style.Bits                 := GetCardinalProperty(E,'STYLE');
    Style.FullStyle.Thickness  := GetCoordProperty(E,'THICKNESS');
    Fractal                    := GetBooleanProperty(E,'FRACTAL');
    Style.FullStyle.SThickness := GetCoordProperty(E,'STHICKNESS');
    If Fractal Then
    Begin
      Seed      := GetIntegerProperty(E,'SEED');
      Roughness := GetIntegerProperty(E,'ROUGHNESS');
    End;
    RefreshCopies;
  End;
End; // LinePrimitive.ReadFromDOMElement

Function LinePrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordProperty(D,'X1',X1));
    E.appendChild(NewCoordProperty(D,'Y1',Y1));
    E.appendChild(NewCoordProperty(D,'X2',X2));
    E.appendChild(NewCoordProperty(D,'Y2',Y2));
    E.appendChild(NewCardinalProperty(D,'STYLE',Style.Bits));
    E.appendChild(NewCoordProperty(D,'THICKNESS',Style.FullStyle.Thickness));
    E.appendChild(NewBooleanProperty(D,'FRACTAL',Fractal));
    E.appendChild(NewCoordProperty(D,'STHICKNESS',Style.FullStyle.SThickness));
    If Fractal Then
    Begin
      E.appendChild(NewIntegerProperty(D,'SEED',Seed));
      E.appendChild(NewIntegerProperty(D,'ROUGHNESS',Roughness));
    End;
  End;
  Result := E;
End; // LinePrimitive.GetAsDOMElement

procedure LinePrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(x1,sizeof(x1));
  stream.ReadBuffer(y1,sizeof(y1));
  stream.ReadBuffer(x2,sizeof(x2));
  stream.ReadBuffer(y2,sizeof(y2));
  stream.ReadBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.ReadBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.ReadBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;

  if fractal then
  begin
    stream.ReadBuffer(seed,sizeof(seed));
    stream.ReadBuffer(roughness,sizeof(roughness));
  end;
end;

procedure LinePrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
Var XX1,YY1,XX2,YY2: Coord; // Must be Coords!!!
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  XX1 := GetAdjustedX1 + AddX;
  YY1 := GetAdjustedY1 + AddY;
  XX2 := GetAdjustedX2 + AddX;
  YY2 := GetAdjustedY2 + AddY;
  stream.WriteBuffer(XX1,sizeof(x1));
  stream.WriteBuffer(YY1,sizeof(y1));
  stream.WriteBuffer(XX2,sizeof(x2));
  stream.WriteBuffer(YY2,sizeof(y2));
  stream.WriteBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.WriteBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.WriteBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;

  if fractal then begin
    stream.WriteBuffer(seed,sizeof(seed));
    stream.WriteBuffer(roughness,sizeof(roughness));
    end;
end;

function LinePrimitive.SetSeed(new_seed:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetSeed(new_seed);
    end
  else begin
    if (seed<>new_seed) then begin
      SplitOff;
      seed:=new_seed;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function LinePrimitive.GetSeed:integer;
begin
  if not fractal then begin
    Result:=inherited GetSeed;
    end
  else begin
    Result:=seed;
    end;
end;

function LinePrimitive.SetRoughness(rough:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetRoughness(rough);
    end
  else begin
    if roughness<>rough then begin
      SplitOff;
      roughness:=rough;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function  LinePrimitive.GetRoughness:integer;
begin
  if not fractal then begin
    Result:=inherited GetRoughness;
    end
  else begin
    Result:=roughness;
    end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

function CurvePrimitive.SetFractal(state:FractalState):boolean;
begin
  SplitOff;
  fractal := SetFractalState(fractal, state);
  ComputeExtent;
  Result:=true;
end;

procedure CurvePrimitive.Reverse;
begin
  SplitOff;
  SwapPoints(p1,p4);
  SwapPoints(p2,p3);

  if fractal then ComputeExtent;
end;

Procedure CurvePrimitive.CopyFromBase(AliasOnly: Boolean);
Var CP: CurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    CP        := Base As CurvePrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Style     := CP.Style;
    Seed      := CP.Seed;
    Roughness := CP.Roughness;
    Fractal   := CP.Fractal;
    P1        := CP.P1;
    P2        := CP.P2;
    P3        := CP.P3;
    P4        := CP.P4;
    If Not AliasOnly Then
    Begin
      P1.X := P1.X + (Alias.X - Base.fExtent.Left);
      P1.Y := P1.Y + (Alias.Y - Base.fExtent.Top);
      P2.X := P2.X + (Alias.X - Base.fExtent.Left);
      P2.Y := P2.Y + (Alias.Y - Base.fExtent.Top);
      P3.X := P3.X + (Alias.X - Base.fExtent.Left);
      P3.Y := P3.Y + (Alias.Y - Base.fExtent.Top);
      P4.X := P4.X + (Alias.X - Base.fExtent.Left);
      P4.Y := P4.Y + (Alias.Y - Base.fExtent.Top);
    End;    
  End;
End; // CurvePrimitive.CopyFromBase

procedure CurvePrimitive.CloseFigure;
begin
  SplitOff;
  p4 := p1;
  ComputeExtent;
end;

Function CurvePrimitive.SliceAlong(S1,S2:CoordPoint; Var NP: DrawPrimitive): Boolean;
Var
  X,Y,XN,YN : Array Of Double;
  I,J       : Integer;
  Pts       : PCoordArray;
  PP1,PP2,PP3,PP4 : CoordPoint;

begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  SetLength(X,4);
  SetLength(Y,4);
  SetLength(XN,13);
  SetLength(YN,13);
  X[0] := PP1.X;
  X[1] := PP2.X;
  X[2] := PP3.X;
  X[3] := PP4.X;
  Y[0] := PP1.Y;
  Y[1] := PP2.Y;
  Y[2] := PP3.Y;
  Y[3] := PP4.Y;
  I    := SliceAlongLine(X,Y,S1.X,S1.Y,S2.X,S2.Y,XN,YN);
  If I > 4 Then
  Begin
    GetMem(Pts,SizeOf(CoordPoint) * I);
    For J := 0 To I - 1 Do
    Begin
      Pts^[J].X := XN[J];
      Pts^[J].Y := YN[J];
    End; // For J
    NP := PolyCurvePrimitive.Create(Pts,I,Seed,Roughness,Style,Fractal);
    NP.CopyCore(Self);
    Result := True;
  End
  Else Result := False;
  SetLength(X,0);
  SetLength(Y,0);
  SetLength(XN,0);
  SetLength(YN,0);
end;

procedure CurvePrimitive.InvalidateHandles(const Handle: THandle;
  const View: ViewPoint);
var
  PP2, PP3: CoordPoint;
begin
  // Get adjusted points for the curve
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;

  // Invalidate the area around each point
  InvalidateHandle(Handle, View, PP2.x, PP2.y);
  InvalidateHandle(Handle, View, PP3.x, PP3.y);
end;


function CurvePrimitive.GetLines(const View:ViewPoint; var polycount:integer):PCoordArray;
var continue:TLineContinue;
    p1c,p2c,p3c,p4c:CoordPoint;
    d:double;
    PP1,PP2,PP3,PP4 : CoordPoint;
begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  if fractal then FractalSetSeed(seed);

  continue:=GetLineStyleStart(SEGMENT_STYLE);

  if View <> nil then
  begin
    p1c := View.CoordToScreenPt(PP1);
    p2c := View.CoordToScreenPt(PP2);
    p3c := View.CoordToScreenPt(PP3);
    p4c := View.CoordToScreenPt(PP4);
  end
  else
  begin
    p1c := PP1;
    p2c := PP2;
    p3c := PP3;
    p4c := PP4;
  end;

  if not fractal then begin
    DrawBezier(nil, p1c,p2c,p3c,p4c, continue);
    end
  else begin
    d := distance(p1c.x,p1c.y,p2c.x,p2c.y) + distance(p4c.x,p4c.y,p3c.x,p3c.y);
    DrawFractalBezier(nil, p1c,p2c,p3c,p4c, (d*roughness)/1000, continue);
  end;

  Result:=GetLineEnd(continue,polycount);
end;

function CurvePrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var fracpoints:PCoordArray;
    polycount:integer;
begin
  fracpoints:=GetLines(View,polycount);
  View.ScreenToCoordPtArray(fracpoints,polycount);
  NewChain:=PolyLinePrimitive.Create(fracpoints, polycount, style);
  NewChain.CopyCore(self);
  (NewChain as PolyLinePrimitive).fillcolor:=clNone;
  Result:=true;
end;

procedure CurvePrimitive.Draw(View:ViewPoint);
var sp1,sp2,sp3,sp4:CoordPoint;
    ix,iy:integer;
    oldcolor:TColor;
    continue:TLineContinue;
    d:double;
    PP1,PP2,PP3,PP4 : CoordPoint;
    NewStyle : StyleAttrib;
    T1,T2    : Coord;
    poly     : PCoordArray;
    polypoints:integer;

begin
  If Style.Line = start_numeric_thickness_style Then
  Begin
    poly := GetLines(View, polypoints);

    DrawEnclosedFigure(View.Canvas, poly, polypoints,
                       ((View.QuickDraw and QuickDraw_Fills)=0) and IsClosed,
                       Style,
                       DisplayColor(GetColor), DisplayFillColor(0),
                       View,True);

    FreeMem(poly);
  End
  Else
  Begin
    PP1 := GetAdjustedP1;
    PP2 := GetAdjustedP2;
    PP3 := GetAdjustedP3;
    PP4 := GetAdjustedP4;
    oldcolor := View.Canvas.Pen.Color;
    View.Canvas.Pen.Color := DisplayColor(GetColor);
    View.CoordToScreen(PP1.x,PP1.y,ix,iy);
    sp1.x:=ix;
    sp1.y:=iy;
    View.CoordToScreen(PP2.x,PP2.y,ix,iy);
    sp2.x:=ix;
    sp2.y:=iy;
    View.CoordToScreen(PP3.x,PP3.y,ix,iy);
    sp3.x:=ix;
    sp3.y:=iy;
    View.CoordToScreen(PP4.x,PP4.y,ix,iy);
    sp4.x:=ix;
    sp4.y:=iy;

    NewStyle := View.FixStyle(Style);
    T1       := Style.FullStyle.Thickness;
    T2       := View.Grid.GraphUnitConvert * View.Grid.GraphScale;
    If T2 <> 0 Then T1 := T1 / T2;
    View.DeltaCoordToScreen(T1,0,T1,T2);
    NewStyle.FullStyle.Thickness := T1;
    Style.FullStyle.SThickness := T1;

    continue:=GetLineStyleStart(NewStyle);

    if not fractal then begin
      DrawBezier(View.Canvas, sp1,sp2,sp3,sp4, continue);
      end
    else begin
      FractalSetSeed(seed);
      d := distance(sp1.x,sp1.y,sp2.x,sp2.y) + distance(sp4.x,sp4.y,sp3.x,sp3.y);
      DrawFractalBezier(View.Canvas, sp1,sp2,sp3,sp4, (d*roughness)/1000, continue);
      end;

    View.Canvas.Pen.Color := oldcolor;
  End;
end;

procedure CurvePrimitive.DrawHandles(const View:ViewPoint);
Var PP1,PP2,PP3,PP4: CoordPoint;
begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  DrawHandle(View,PP1.x,PP1.y);
  DrawHandle(View,PP2.x,PP2.y);
  DrawHandle(View,PP3.x,PP3.y);
  DrawHandle(View,PP4.x,PP4.y);
end;

procedure CurvePrimitive.PointClosestTo(x,y:Coord; var px,py:Coord);
var corner:array[0..3] of CoordPoint;
    n:integer;
    PP1,PP2,PP3,PP4 : CoordPoint;
begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  corner[0] := PP1;
  corner[1] := PP2;
  corner[2] := PP3;
  corner[3] := PP4;

  n:=PointClosestInArray(x,y,@corner,4);

  px:=corner[n].x;
  py:=corner[n].y;
end;

procedure CurvePrimitive.DrawOverlayHandles(const View:ViewPoint);
Var P: CoordPoint;
begin
  P := GetAdjustedP1;
  DrawOverlayHandle(View,P.x,P.y);
end;

Function CurvePrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: CurvePrimitive;
Begin
  If D Is CurvePrimitive Then
  Begin
    P := CurvePrimitive(D);
    ComputeExtent;
    P.ComputeExtent;

    // Perhaps sometime in the future this check could take objects' relative
    // size into account, or at least scale VeryClose based on their size to
    // handle very small objects.  I'm not sure on this at the moment...

    Result := (Abs((P2.X - P1.X) - (P.P2.X - P.P1.X)) < VeryClose)      And
              (Abs((P2.Y - P1.Y) - (P.P2.Y - P.P1.Y)) < VeryClose)      And
              (Abs((P3.X - P1.X) - (P.P3.X - P.P1.X)) < VeryClose)      And
              (Abs((P3.Y - P1.Y) - (P.P3.Y - P.P1.Y)) < VeryClose)      And
              (Abs((P4.X - P1.X) - (P.P4.X - P.P1.X)) < VeryClose)      And
              (Abs((P4.Y - P1.Y) - (P.P4.Y - P.P1.Y)) < VeryClose)      And
              (Style.Bits = P.Style.Bits)                               And
              (Style.FullStyle.Thickness = P.Style.FullStyle.Thickness) And
              (Fractal = P.Fractal)                                     And
              ((Not Fractal) Or
               ((Seed = P.Seed) And
                (Roughness = P.Roughness))) And Inherited IsSimilarTo(D);
  End
  Else Result := False;
End; // CurvePrimitive.IsSimilarTo

function CurvePrimitive.ApplyMatrix(var mat:Matrix):boolean;
Var
  PP1,PP2,PP3,PP4 : CoordPoint;
  Scale           : Coord;

begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;
    End;
  End;
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  MultiplyPointByMatrix(PP1.x,PP1.y,Mat);
  MultiplyPointByMatrix(PP2.x,PP2.y,Mat);
  MultiplyPointByMatrix(PP3.x,PP3.y,Mat);
  MultiplyPointByMatrix(PP4.x,PP4.y,Mat);
  P1 := PP1;
  P2 := PP2;
  P3 := PP3;
  P4 := PP4;
  If Style.Line = start_numeric_thickness_style Then
  Begin
    Scale := (mat[1,1] + mat[2,2]) / 2;
    If Style.FullStyle.Thickness  >= 0 Then Style.FullStyle.Thickness  := Style.FullStyle.Thickness  * Scale;
    If Style.FullStyle.SThickness >= 0 Then Style.FullStyle.SThickness := Style.FullStyle.SThickness * Scale;
  End;
  Result:=true;
end;

procedure CurvePrimitive.Move(dx,dy:Coord);
begin
  If Base = Nil Then
  Begin
    SplitOff;
    p1.x := p1.x + dx;
    p2.x := p2.x + dx;
    p3.x := p3.x + dx;
    p4.x := p4.x + dx;
    p1.y := p1.y + dy;
    p2.y := p2.y + dy;
    p3.y := p3.y + dy;
    p4.y := p4.y + dy;
  End;  
  inherited Move(dx,dy);
end;

constructor CurvePrimitive.CreateBlank(frac:boolean);
begin
  inherited Create;
  p1.X := 0; p1.Y := 0;
  p2.X := 0; p2.Y := 0;
  p3.X := 0; p3.Y := 0;
  p4.X := 0; p4.Y := 0;
  style.Bits := 0;
  Style.FullStyle.Thickness  := 0;
  Style.FullStyle.SThickness := 0;
  fractal:=frac;
  ComputeExtent;
end;

constructor CurvePrimitive.Create(ip1,ip2,ip3,ip4:CoordPoint; istyle:StyleAttrib);
begin
  inherited Create;
  p1        := ip1;
  p2        := ip2;
  p3        := ip3;
  p4        := ip4;
  style     := istyle;
  seed      := 0;
  roughness := 0;
  fractal   := false;
  ComputeExtent;
end;

constructor CurvePrimitive.Create(ip1,ip2,ip3,ip4:CoordPoint; iseed,irough:integer; istyle:StyleAttrib);
begin
  inherited Create;
  p1        := ip1;
  p2        := ip2;
  p3        := ip3;
  p4        := ip4;
  style     := istyle;
  seed      := iseed;
  roughness := irough;
  fractal   := true;
  ComputeExtent;
end;

constructor CurvePrimitive.Create(ip1,ip2,ip3,ip4:CoordPoint; iseed,irough:integer; istyle:StyleAttrib; frac:boolean);
begin
  inherited Create;
  p1        := ip1;
  p2        := ip2;
  p3        := ip3;
  p4        := ip4;
  style     := istyle;
  seed      := iseed;
  roughness := irough;
  fractal   := frac;
  ComputeExtent;
end;

Function CurvePrimitive.GetAdjustedP1: CoordPoint;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Var CP: CurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    CP       := Base As CurvePrimitive;
    Result.X := CP.P1.X + (Alias.X - Base.fExtent.Left);
    Result.Y := CP.P1.Y + (Alias.Y - Base.fExtent.Top);
  End
  Else Result := P1;
End; // CurvePrimitive.GetAdjustedP1

Function CurvePrimitive.GetAdjustedP2: CoordPoint;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Var CP: CurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    CP       := Base As CurvePrimitive;
    Result.X := CP.P2.X + (Alias.X - Base.fExtent.Left);
    Result.Y := CP.P2.Y + (Alias.Y - Base.fExtent.Top);
  End
  Else Result := P2;
End; // CurvePrimitive.GetAdjustedP2

Function CurvePrimitive.GetAdjustedP3: CoordPoint;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Var CP: CurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    CP       := Base As CurvePrimitive;
    Result.X := CP.P3.X + (Alias.X - Base.fExtent.Left);
    Result.Y := CP.P3.Y + (Alias.Y - Base.fExtent.Top);
  End
  Else Result := P3;
End; // CurvePrimitive.GetAdjustedP3

Function CurvePrimitive.GetAdjustedP4: CoordPoint;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Var CP: CurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    CP       := Base As CurvePrimitive;
    Result.X := CP.P4.X + (Alias.X - Base.fExtent.Left);
    Result.Y := CP.P4.Y + (Alias.Y - Base.fExtent.Top);
  End
  Else Result := P4;
End; // CurvePrimitive.GetAdjustedP4

function CurvePrimitive.Copy:DrawPrimitive;
begin
  Result := CurvePrimitive.Create(GetAdjustedP1,GetAdjustedP2,GetAdjustedP3,GetAdjustedP4,
                                  seed,roughness,style,fractal);
  Result.MakeCopy(Self);
end;

function CurvePrimitive.SetStyle(new_style:StyleAttrib):boolean;
begin
  Result:=false;

  if (style.Line <> new_style.Line) and (new_style.Line<>$FF) then begin
    SplitOff;
    style.Line:=new_style.Line;
    Result:=true;
    end;

  if (style.Fill <> new_style.Fill) and (new_style.Fill<>$FF) then begin
    If Not Result Then SplitOff;
    style.Fill:=new_style.Fill;
    Result:=true;
    end;

  if (style.First <> new_style.First) and (new_style.First<>$FF) then begin
    If Not Result Then SplitOff;
    style.First:=new_style.First;
    Result:=true;
    end;

  if (style.Last <> new_style.Last) and (new_style.Last<>$FF) then begin
    If Not Result Then SplitOff;
    style.Last:=new_style.Last;
    Result:=true;
    end;

  If (Style.FullStyle.Thickness <> New_Style.FullStyle.Thickness) And
     (New_Style.FullStyle.Thickness >= 0) Then
  Begin
    If Not Result Then SplitOff;
    Style.FullStyle.Thickness  := New_Style.FullStyle.Thickness;
    Style.FullStyle.SThickness := New_Style.FullStyle.SThickness;
    Result := True;
  End;    
end;

function CurvePrimitive.GetStyle: StyleAttrib;
begin
  Result := style;
end;

function CurvePrimitive.FindHandle(const View:ViewPoint; x,y:Coord):boolean;
Var PP1,PP2,PP3,PP4: CoordPoint;
begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  Result := TestHandle(View, PP1.x,PP1.y,x,y) or
            TestHandle(View, PP2.x,PP2.y,x,y) or
            TestHandle(View, PP3.x,PP3.y,x,y) or
            TestHandle(View, PP4.x,PP4.y,x,y);
end;

function CurvePrimitive.FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean;
Var PP1,PP4: CoordPoint;
begin
  Result := false;
  PP1    := GetAdjustedP1;
  PP4    := GetAdjustedP4;
  if TestHandle(View, PP1.x,PP1.y,x,y) then
  begin
    x      := PP1.x;
    y      := PP1.y;
    Result := true;
  end
  Else if TestHandle(View, PP4.x,PP4.y,x,y) then
  begin
    x      := PP4.x;
    y      := PP4.y;
    Result := true;
  end;
end;

// Reused for curves and polycurves
Function NormalCurveFindPointOn(Var X,Y,Angle: Coord; P1,P2,P3,P4: CoordPoint): Boolean;
Var
  P    : CPoint;
  V    : Array Of CPoint;
  Root : Double;

Begin
  P.X := X;
  P.Y := Y;
  SetLength(V,4);
  V[0].X := P1.X;
  V[0].Y := P1.Y;
  V[1].X := P2.X;
  V[1].Y := P2.Y;
  V[2].X := P3.X;
  V[2].Y := P3.Y;
  V[3].X := P4.X;
  V[3].Y := P4.Y;
  P      := FindNearestPoint(P,V,Root);
  Angle  := FindAngleAt(V,2 * Root - 1);
  SetLength(V,0);
  X      := P.X;
  Y      := P.Y;
  Result := True;
End; // NormalCurveFindPointOn

Function FractalCurveFindPointOn(Var X,Y,Angle: Coord; P1,P2,P3,P4: CoordPoint; Seed,Roughness: Integer; Const View: ViewPoint): Boolean;
Var
  X0,Y0 : Coord;
  SP1   : CoordPoint;
  SP2   : CoordPoint;
  SP3   : CoordPoint;
  SP4   : CoordPoint;
  Dist  : Double;
  D     : Double;
  IX,IY : Integer;

  Function DoFractal(Var X,Y,Angle: Coord; Depth: Integer; P1,P2,P3,P4: CoordPoint; RFact: Double; Var Dist: Double): Boolean;
  Var
    D     : Double;
    Q1    : CoordPoint;
    Q2    : CoordPoint;
    Q3    : CoordPoint;
    R1    : CoordPoint;
    R2    : CoordPoint;
    S1    : CoordPoint;
    PX,PY : Double;
    R     : double;
    B1    : Boolean;
    B2    : Boolean;
    D1    : Double;
    D2    : Double;
    XX1   : Coord;
    YY1   : Coord;
    AA1   : Coord;
    XX2   : Coord;
    YY2   : Coord;
    AA2   : Coord;

  Begin
    D := Distance(P1.X,P1.Y,P4.X,P4.Y);
    If (D <= 2) And (Depth > 1) Then
    Begin
      XX1    := X0;
      YY1    := Y0;
      Result := NormalLineFindPointOn(XX1,YY1,Angle,P1.X,P1.Y,P4.X,P4.Y);
      X      := XX1;
      Y      := YY1;
      Dist   := Distance(X0,Y0,X,Y);
    End
    Else
    Begin
      Q1 := AvePoints(P1,P2);
      Q2 := AvePoints(P2,P3);
      Q3 := AvePoints(P3,P4);
      R1 := AvePoints(Q1,Q2);
      R2 := AvePoints(Q2,Q3);
      S1 := AvePoints(R1,R2);

      UnitPerpendicular(P1.X,P1.Y,P4.X,P4.Y,PX,PY);
      R    := RFact * ((FixedRandom(Depth) / MaxRand) - 0.5);
      S1.X := S1.X + PX * R;
      S1.Y := S1.Y + PY * R;

      // Recurse, adjusting down the width.
      XX1 := X0;
      YY1 := Y0;
      D1  := Dist;
      XX2 := X0;
      YY2 := Y0;
      D2  := Dist;
      B1  := DoFractal(XX1,YY1,AA1,Depth * 2,     P1,Q1,R1,S1, RFact * 0.5,D1);
      B2  := DoFractal(XX2,YY2,AA2,Depth * 2 + 1, S1,R2,Q3,P4, RFact * 0.5,D2);
      If B2 And ((Not B1) Or (D2 < D1)) Then
      Begin
        XX1 := XX2;
        YY1 := YY2;
        AA1 := AA2;
        D1  := D2;
        B1  := B2;
      End;
      If B1 And (D1 < Dist) Then
      Begin
        X      := XX1;
        Y      := YY1;
        Angle  := AA1;
        Dist   := D1;
        Result := True;
      End
      Else Result := False;
    End;
  End; // DoFractal

Begin
  View.CoordToScreen(X,Y,X0,Y0);
  FractalSetSeed(Seed);
  View.CoordToScreen(P1.X,P1.Y,IX,IY);
  SP1.X := IX;
  SP1.Y := IY;
  View.CoordToScreen(P2.X,P2.Y,IX,IY);
  SP2.X := IX;
  SP2.Y := IY;
  View.CoordToScreen(P3.X,P3.Y,IX,IY);
  SP3.X := IX;
  SP3.Y := IY;
  View.CoordToScreen(P4.X,P4.Y,IX,IY);
  SP4.X := IX;
  SP4.Y := IY;
  Dist   := MaxDouble;
  D      := Distance(SP1.X,SP1.Y,SP2.X,SP2.Y) + Distance(SP4.X,SP4.Y,SP3.X,SP3.Y);
  Result := DoFractal(X0,Y0,Angle,1,SP1,SP2,SP3,SP4,(D * Roughness) / 1000,Dist);
  If Result Then View.ScreenToCoord(X0,Y0,X,Y);
End; // FractalCurveFindPointOn

Function CurvePrimitive.FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean;
Var PP1,PP2,PP3,PP4: CoordPoint;
Begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  If Fractal
   Then Result := FractalCurveFindPointOn(X,Y,Angle,PP1,PP2,PP3,PP4,Seed,Roughness,View)
   Else Result := NormalCurveFindPointOn(X,Y,Angle,PP1,PP2,PP3,PP4);
End; // CurvePrimitive.FindPointOn

function CurvePrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
Var PP1,PP2,PP3,PP4: CoordPoint;
begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  Result := false;
  if TestHandle(View,PP1.x,PP1.y,origx,origy) then
  begin
    SplitOff;
    P1.x := origx + dx;
    P1.y := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;
  if TestHandle(View,PP2.x,PP2.y,origx,origy) then
  begin
    If Not Result Then SplitOff;
    P2.x := origx + dx;
    P2.y := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;
  if TestHandle(View,PP3.x,PP3.y,origx,origy) then
  begin
    If Not Result Then SplitOff;
    P3.x := origx + dx;
    P3.y := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;
  if TestHandle(View,PP4.x,PP4.y,origx,origy) then
  begin
    If Not Result Then SplitOff;
    P4.x := origx + dx;
    P4.y := origy + dy;
    RecalcAliasFromExtent;
    Result:=true;
    if (mode=hmOne) then Exit;
  end;
end;

procedure CurvePrimitive.ComputeExtent;
Var
  X,Y      : Coord;
  Pts,Pts1 : PCoordArray;
  NumCount : Integer;
  I        : Integer;
  B        : CoordRect;

begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    If Style.Line = start_numeric_thickness_style Then
    Begin
      Pts1 := GetLines(Nil,NumCount);
      Pts  := GetThickLines(Pts1,NumCount,Style,True,False,False);

      fExtent.left   := pts^[0].X;
      fExtent.right  := pts^[0].X;
      fExtent.top    := pts^[0].Y;
      fExtent.bottom := pts^[0].Y;

      for i:=1 to NumCount-1 do begin
        if not fractal then begin
          Encompass(fExtent,pts^[i].X,pts^[i].Y);
          end
        else begin
          FractalSetSeed(seed + i - 1);
          GetFractalBox(pts^[i-1].X,pts^[i-1].Y,
                        pts^[i].X,pts^[i].Y,
                        (distance(pts^[i-1].X,pts^[i-1].Y,pts^[i].X,pts^[i].Y)*roughness)/1000,
                        b);

          Encompass(fExtent, b.left,b.top);
          Encompass(fExtent, b.right,b.bottom);
          end;
        end;

      FreeMem(Pts);
      FreeMem(Pts1);
    End
    Else
    Begin
      if not fractal then GetBezierBox(p1,p2,p3,p4, fExtent)
      else
      begin
        FractalSetSeed(seed);
        GetFractalBezierBox(p1,p2,p3,p4,RFact, fExtent);
      end;
    End;
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

function CurvePrimitive.GetId:char;
begin
  if not fractal then Result := 'C' else Result := 'c';
end;

procedure CurvePrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(p1.x,sizeof(p1.x));
  stream.ReadBuffer(p1.y,sizeof(p1.y));
  stream.ReadBuffer(p2.x,sizeof(p2.x));
  stream.ReadBuffer(p2.y,sizeof(p2.y));
  stream.ReadBuffer(p3.x,sizeof(p3.x));
  stream.ReadBuffer(p3.y,sizeof(p3.y));
  stream.ReadBuffer(p4.x,sizeof(p4.x));
  stream.ReadBuffer(p4.y,sizeof(p4.y));
  stream.ReadBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.ReadBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.ReadBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;  

  if fractal then begin
    stream.ReadBuffer(seed,sizeof(seed));
    stream.ReadBuffer(roughness,sizeof(roughness));
    end;
end;

procedure CurvePrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
Var PX1,PY1,PX2,PY2,PX3,PY3,PX4,PY4: Coord; // Must be Coords!!!
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  PX1 := GetAdjustedX(P1.X) + AddX;
  PY1 := GetAdjustedY(P1.Y) + AddY;
  PX2 := GetAdjustedX(P2.X) + AddX;
  PY2 := GetAdjustedY(P2.Y) + AddY;
  PX3 := GetAdjustedX(P3.X) + AddX;
  PY3 := GetAdjustedY(P3.Y) + AddY;
  PX4 := GetAdjustedX(P4.X) + AddX;
  PY4 := GetAdjustedY(P4.Y) + AddY;
  stream.WriteBuffer(PX1,sizeof(p1.x));
  stream.WriteBuffer(PY1,sizeof(p1.y));
  stream.WriteBuffer(PX2,sizeof(p2.x));
  stream.WriteBuffer(PY2,sizeof(p2.y));
  stream.WriteBuffer(PX3,sizeof(p3.x));
  stream.WriteBuffer(PY3,sizeof(p3.y));
  stream.WriteBuffer(PX4,sizeof(p4.x));
  stream.WriteBuffer(PY4,sizeof(p4.y));
  stream.WriteBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.WriteBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.WriteBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;

  if fractal then begin
    stream.WriteBuffer(seed,sizeof(seed));
    stream.WriteBuffer(roughness,sizeof(roughness));
    end;
end;

Procedure CurvePrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    P1                         := GetCoordPointProperty(E,'P1');
    P2                         := GetCoordPointProperty(E,'P2');
    P3                         := GetCoordPointProperty(E,'P3');
    P4                         := GetCoordPointProperty(E,'P4');
    Style.Bits                 := GetCardinalProperty(E,'STYLE');
    Style.FullStyle.Thickness  := GetCoordProperty(E,'THICKNESS');
    Fractal                    := GetBooleanProperty(E,'FRACTAL');
    Style.FullStyle.SThickness := GetCoordProperty(E,'STHICKNESS');
    If Fractal Then
    Begin
      Seed      := GetIntegerProperty(E,'SEED');
      Roughness := GetIntegerProperty(E,'ROUGHNESS');
    End;
    RefreshCopies;
  End;
End; // CurvePrimitive.ReadFromDOMElement

Function CurvePrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordPointProperty(D,'P1',P1));
    E.appendChild(NewCoordPointProperty(D,'P2',P2));
    E.appendChild(NewCoordPointProperty(D,'P3',P3));
    E.appendChild(NewCoordPointProperty(D,'P4',P4));
    E.appendChild(NewCardinalProperty(D,'STYLE',Style.Bits));
    E.appendChild(NewCoordProperty(D,'THICKNESS',Style.FullStyle.Thickness));
    E.appendChild(NewBooleanProperty(D,'FRACTAL',Fractal));
    E.appendChild(NewCoordProperty(D,'STHICKNESS',Style.FullStyle.SThickness));
    If Fractal Then
    Begin
      E.appendChild(NewIntegerProperty(D,'SEED',Seed));
      E.appendChild(NewIntegerProperty(D,'ROUGHNESS',Roughness));
    End;
  End;  
  Result := E;
End; // CurvePrimitive.GetAsDOMElement

function CurvePrimitive.SetSeed(new_seed:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetSeed(new_seed);
    end
  else begin
    if seed<>new_seed then begin
      SplitOff;
      seed:=new_seed;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function  CurvePrimitive.GetSeed:integer;
begin
  if not fractal then begin
    Result:=inherited GetSeed;
    end
  else begin
    Result:=seed;
    end;
end;

function CurvePrimitive.SetRoughness(rough:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetRoughness(rough);
    end
  else begin
    if roughness<>rough then begin
      SplitOff;
      roughness:=rough;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function CurvePrimitive.GetRoughness:integer;
begin
  if not fractal then
    Result:=inherited GetRoughness
  else
    Result:=roughness;
end;

function CurvePrimitive.RFact: double;
var
  D               : Double;
  PP1,PP2,PP3,PP4 : CoordPoint;

begin
  PP1    := GetAdjustedP1;
  PP2    := GetAdjustedP2;
  PP3    := GetAdjustedP3;
  PP4    := GetAdjustedP4;
  D      := Distance(PP1.x,PP1.y,PP2.x,PP2.y) + Distance(PP4.x,PP4.y,PP3.x,PP3.y);
  Result := (D * Roughness) / 1000;
end;

{------------------------------------------------------------------------------}
constructor PolyCurvePrimitive.Ellipse(center:CoordPoint; edge:CoordPoint; istyle:StyleAttrib);
// Magic constant was derived from Solver equation for finding X=cos(45 deg)
// with Bezier formula.
const circ=0.552284749826;
var dw,dh:double;
begin
  CreateBlank(False);
  dw := abs(edge.x - center.x);
  dh := abs(edge.y - center.y);

  Count := 13;
  GetMem(points,sizeof(CoordPoint)*Count);

  points^[0] := MakeCoordPoint(center.x+dw,center.y);
  points^[1] := MakeCoordPoint(center.x+dw,center.y-dh*circ);
  points^[2] := MakeCoordPoint(center.x+dw*circ,center.y-dh);
  points^[3] := MakeCoordPoint(center.x,center.y-dh);

  points^[4] := MakeCoordPoint(center.x-dw*circ,center.y-dh);
  points^[5] := MakeCoordPoint(center.x-dw,center.y-dh*circ);
  points^[6] := MakeCoordPoint(center.x-dw,center.y);

  points^[7] := MakeCoordPoint(center.x-dw,center.y+dh*circ);
  points^[8] := MakeCoordPoint(center.x-dw*circ,center.y+dh);
  points^[9] := MakeCoordPoint(center.x,center.y+dh);

  points^[10]:= MakeCoordPoint(center.x+dw*circ,center.y+dh);
  points^[11]:= MakeCoordPoint(center.x+dw,center.y+dh*circ);
  points^[12]:= MakeCoordPoint(center.x+dw,center.y);

  style      := istyle;
  fillcolor  := CurrentFillColor;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;  
end;

Constructor PolyCurvePrimitive.Arc(Center: CoordPoint; Edge1,Edge2: CoordPoint; iStyle: StyleAttrib);
// Magic constant was derived from Solver equation for finding X=cos(45 deg)
// with Bezier formula.
//
// JD: For an arc of angle a (where 0 <= a <= pi / 4), the handle length must be:
//
//            2  a
//     8 * sin  ---
//               4
//     ------------
//              a
//     3 * sin ---
//              2
//
//    that is: (8 / 3) * (sin(a / 4)^2) / sin(a / 2)

Var
  A1,A2 : Double;
  D     : Double;
  Num   : Integer;
  C     : CoordPoint;
  I     : Integer;
  R     : Double;
  X     : Double;
  Y     : Double;
  L     : Double;

Begin
  CreateBlank(False);

  // Get the distance to the first endpoint as the circle's radius

  R := Sqr(Edge1.X - Center.X) + Sqr(Edge1.Y - Center.Y);
  If R > 0 Then
  Begin
    R := Sqrt(R);

    // Get the angles to the two endpoints, and sort them if necessary

         If Edge1.X <> Center.X Then A1 := ArcTan2(Edge1.Y - Center.Y,Edge1.X - Center.X)
    Else If Edge1.Y > Center.Y Then A1 := Pi / 2 Else A1 := -Pi / 2;

         If Edge2.X <> Center.X Then A2 := ArcTan2(Edge2.Y - Center.Y,Edge2.X - Center.X)
    Else If Edge2.Y > Center.Y Then A2 := Pi / 2 Else A2 := -Pi / 2;

    While A1 < 0 Do A1 := A1 + 2 * Pi;
    While A2 < 0 Do A2 := A2 + 2 * Pi;

    D := A2 - A1;

    // We want to go counterclockwise, so flip if we have to

    If D < 0 Then
    Begin
      C     := Edge1;
      Edge1 := Edge2;
      Edge2 := C;
    End;

    // Flip again under certain circumstances

    If (D > Pi) Or (D < -Pi) Then
    Begin
      C     := Edge1;
      Edge1 := Edge2;
      Edge2 := C;
    End;

         If D >  Pi Then D := 2 * Pi - D
    Else If D < -Pi Then D := D + 2 * Pi
    Else If D <   0 Then D := -D;

    // Get the angle to the first endpoint

         If Edge1.X <> Center.X Then A1 := ArcTan2(Edge1.Y - Center.Y,Edge1.X - Center.X)
    Else If Edge1.Y > Center.Y Then A1 := Pi / 2 Else A1 := -Pi / 2;

    // Find out how many segments we need to make the arc, making sure that no
    // segment sweeps more than pi / 4 (90 degrees)

    Num := Trunc(D / (Pi / 2));
    If Num * Pi / 2 < D Then Inc(Num);
    If Num > 0 Then
    Begin
      Count := 3 * Num + 1;
      GetMem(Points,SizeOf(CoordPoint) * Count);

      // Fill in the first point, then iterate to fill in the remaining points

      Points^[0] := Edge1;                                // First point

      For I := 0 To Num - 1 Do
      Begin
        X := Points^[I * 3].X;                            // First point location
        Y := Points^[I * 3].Y;
        If D > Pi / 2 Then A2 := Pi / 2 Else A2 := D;     // Get the angle sweep
        L := (8 / 3) * Sqr(Sin(A2 / 4)) / Sin(A2 / 2);    // Calculate handle length

        // First handle

        Points^[I * 3 + 1] := MakeCoordPoint(X - R * L * Sin(A1),Y + R * L * Cos(A1));

        X := Center.X + R * Cos(A1 + A2);                 // Calculate the second point location
        Y := Center.Y + R * Sin(A1 + A2);

        // Second handle

        Points^[I * 3 + 2] := MakeCoordPoint(X + R * L * Sin(A1 + A2),Y - R * L * Cos(A1 + A2));
        Points^[I * 3 + 3] := MakeCoordPoint(X,Y);        // Second point/first point of the next segment
        D  := D - A2;
        A1 := A1 + A2;
      End; // For I
      Style     := iStyle;
      FillColor := CurrentFillColor;
      ComputeExtent;
      Alias.X    := fExtent.Left;
      Alias.Y    := fExtent.Top;      
    End;
  End;
End; // PolyCurvePrimitive.Arc

Procedure PolyCurvePrimitive.CopyFromBase(AliasOnly: Boolean);
Var
  I   : Integer;
  PCP : PolyCurvePrimitive;

Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    PCP       := Base As PolyCurvePrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Style     := PCP.Style;
    FillColor := PCP.FillColor;
    Count     := PCP.Count;
    Seed      := PCP.Seed;
    Roughness := PCP.Roughness;
    Fractal   := PCP.Fractal;
    If AliasOnly Then Points := PCP.Points
    Else
    Begin
      GetMem(Points,SizeOf(CoordPoint) * Count);
      System.Move(PCP.Points^,Points^,SizeOf(CoordPoint) * Count);
      For I := 0 To Count - 1 Do
      Begin
        Points^[I].X := Points^[I].X + (Alias.X - Base.fExtent.Left);
        Points^[I].Y := Points^[I].Y + (Alias.Y - Base.fExtent.Top);
      End; // For I
    End;
  End;
End; // PolyCurvePrimitive.CopyFromBase

procedure PolyCurvePrimitive.Reverse;
var i:integer;
begin
  SplitOff;
  for i:=0 to (Count-1) div 2 do begin
    SwapPoints(points^[i],points^[Count-1-i]);
    end;

  if fractal then ComputeExtent;
end;

function PolyCurvePrimitive.SetFractal(state:FractalState):boolean;
begin
  SplitOff;
  fractal := SetFractalState(fractal, state);
  ComputeExtent;
  Result:=true;
end;

procedure PolyCurvePrimitive.CloseFigure;
begin
  SplitOff;
  if Count > 1 then points^[Count-1] := points^[0];
  ComputeExtent;
end;

function PolyCurvePrimitive.SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean;
Var
  X,Y,XN,YN,XP,YP : Array Of Double;
  I,J,K     : Integer;
  Pts       : PCoordArray;

begin
  SetLength(XP,0);
  SetLength(YP,0);
  SetLength(X,4);
  SetLength(Y,4);
  SetLength(XN,13);
  SetLength(YN,13);
  For K := 0 To (Count - 2) Div 3 Do
  Begin
    X[0] := GetAdjustedX(Points^[K * 3 + 0].X);
    X[1] := GetAdjustedY(Points^[K * 3 + 1].X);
    X[2] := GetAdjustedX(Points^[K * 3 + 2].X);
    X[3] := GetAdjustedY(Points^[K * 3 + 3].X);
    Y[0] := GetAdjustedX(Points^[K * 3 + 0].Y);
    Y[1] := GetAdjustedY(Points^[K * 3 + 1].Y);
    Y[2] := GetAdjustedX(Points^[K * 3 + 2].Y);
    Y[3] := GetAdjustedY(Points^[K * 3 + 3].Y);
    I    := SliceAlongLine(X,Y,S1.X,S1.Y,S2.X,S2.Y,XN,YN);
    SetLength(XP,High(XP) + I);
    SetLength(YP,High(YP) + I);
    For J := 0 To I - 2 Do
    Begin
      XP[High(XP) - I + J + 2] := XN[J];
      YP[High(YP) - I + J + 2] := YN[J];
    End; // For J
  End; // For K
  SetLength(X,0);
  SetLength(Y,0);
  SetLength(XN,0);
  SetLength(YN,0);
  If High(XP) + 1 > Count Then
  Begin
    GetMem(Pts,SizeOf(CoordPoint) * (High(XP) + 2));
    For J := 0 To High(XP) Do
    Begin
      Pts^[J].X := XP[J];
      Pts^[J].Y := YP[J];
    End; // For J
    Pts^[High(XP) + 1].X := Points^[Count - 1].X;
    Pts^[High(XP) + 1].Y := Points^[Count - 1].Y;
    NP := PolyCurvePrimitive.Create(Pts,High(XP) + 2,Seed,Roughness,Style,Fractal);
    NP.CopyCore(Self);
    Result := True;
  End
  Else Result := False;
  SetLength(XP,0);
  SetLength(YP,0);
end;

procedure PolyCurvePrimitive.InvalidateHandles(const Handle: THandle;
  const View: ViewPoint);
var
  i: Integer;
begin
  i := 0;
  while i < Count - 1 do
  begin
    // Invalidate the area around the first adjusted point
    InvalidateHandle(Handle, View,
      GetAdjustedX(points^[i + 1].x),
      GetAdjustedY(points^[i + 1].y));

    // Invalidate the area around the second adjusted point
    InvalidateHandle(Handle, View,
      GetAdjustedX(points^[i + 2].x),
      GetAdjustedY(points^[i + 2].y));

    // Increment index for the next set of points
    Inc(i, 3);
  end;
end;


function PolyCurvePrimitive.FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean;
var i:integer;
begin
  if (Count<=4) then begin
    Result:=false; exit;
    end;

  Result:=true;

  // Find all points that are not control points, except for the
  // very first and very last (except if it is closed).

  // JD 10-18-02: Figured out how to safely delete the endpoints

//  if IsClosed then i:=0 else i:=3;
  i := 0;
  while (i<Count{-3}) do
  begin
    if (TestHandle(View,x,y,GetAdjustedX(points^[i].x),GetAdjustedY(points^[i].y))) then
    begin
      index := i;
      exit;
    end;
    inc(i,3);
  end; // While
  Result:=false;
end;

function PolyCurvePrimitive.SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean;
var p:PolyCurvePrimitive;
    i,n:integer;
    pt:PCoordArray;
    dx,dy:Coord;
    a1,a2:double;
begin
  // THIS IS ESSENTIALLY AN EXACT COPY OF POLYLINE.SEPARATENODE.
  // (SIGH; ONLY IF MULTIPLE INHERITANCE...)
  if IsClosed then begin
    SplitOff;
    GetMem(pt, sizeof(CoordPoint)*count);

    // Roll the array around so that the index point is the first and last
    n:=0;
    for i:=index to count-1 do begin
      pt^[n] := points^[i];
      inc(n);
      end;
    for i:=1 to index-1 do begin
      pt^[n] := points^[i];
      inc(n);
      end;
    pt^[count-1] := pt^[0];

    // Fix up the first and last point to "separate"
    View.DeltaScreenToCoord(5,5,dx,dy);
    a1:=ArcTan2(pt^[1].y-pt^[0].y,pt^[1].x-pt^[0].x);
    a2:=ArcTan2(pt^[count-2].y-pt^[0].y,pt^[count-2].x-pt^[0].x);

    pt^[0].x:=pt^[0].x+cos(a1)*dx;
    pt^[0].y:=pt^[0].y+sin(a1)*dy;
    pt^[count-1].x:=pt^[count-1].x+cos(a2)*dx;
    pt^[count-1].y:=pt^[count-1].y+sin(a2)*dy;

    FreeMem(points);
    points:=pt;
    ComputeExtent;
    Result:=false;
    end
  else begin
    If (Index > 0) And (Index < Count - 1) Then
    Begin
      // Make a copy, and splice it into the list
      p:=Copy as PolyCurvePrimitive;
      p.Next:=Next;
      Next:=p;
      P.SplitOff;

      // Truncate this polycurve at index
      SplitOff;
      Count:=index+1;
      ComputeExtent;

      // Truncate all before this index in the new polycurve
      for i:=index to p.Count-1 do begin
        p.points^[i-index]:=p.points^[i];
        end;
      p.Count:=p.Count-index;
      p.ComputeExtent;
      NewObject := p;
      NewObject.SetSeed(Seed + Index Div 3); // JD: Fixed the seed setting
      P.AddToBaseOrCopies;
//      NewObject.SetSeed(Seed + Count - 1);
      Result:=true;
    End
    Else Result := False;
  end;
end;

procedure PolyCurvePrimitive.DeleteNode(const View:ViewPoint; index:integer);
var i:integer;
    WasClosed:boolean;
    p1,p2:CoordPoint;
begin
  SplitOff;
  WasClosed:=IsClosed;

  if (index=0) then begin
    p1:=AvePoints(points^[Count-2],points^[Count-3]);
    p2:=AvePoints(points^[index+1],points^[index+2]);
    end
  else begin
    p1:=AvePoints(points^[index-1],points^[index-2]);
    p2:=AvePoints(points^[index+1],points^[index+2]);
    end;

  for i:=index to Count-4 do points^[i]:=points^[i+3];

  if (index=0) then begin
    points^[Count-2]:=p1;
    points^[Count-1]:=p2;
    end
  else begin
    points^[index-2]:=p1;
    points^[index-1]:=p2;
    end;

  dec(Count,3);

  if WasClosed then points^[Count-1]:=points^[0];

  // JD 10-18-02: Correction to protect the remaining fractal shape

  If (Index = 0) And Fractal Then Inc(Seed);

  ComputeExtent;
end;

constructor PolyCurvePrimitive.CreateBlank(frac:boolean);
begin
  inherited Create;
  Count     := 0;
  points    := nil;
  fractal   := frac;
  Seed      := 0;
  Roughness := 0;
end;

constructor PolyCurvePrimitive.Create(lp:PCoordArray; c:integer; istyle:StyleAttrib);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor := CurrentFillColor;
  fractal   := false;
  seed      := 0;
  roughness := 0;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

constructor PolyCurvePrimitive.Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor := CurrentFillColor;
  seed      := iseed;
  roughness := irough;
  fractal   := true;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

constructor PolyCurvePrimitive.Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib; frac:boolean);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor :=CurrentFillColor;
  seed      := iseed;
  roughness := irough;
  fractal   := frac;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

function PolyCurvePrimitive.Copy:DrawPrimitive;
//var pt:PCoordArray;
begin
//  GetMem(pt, sizeof(CoordPoint)*count);
//  System.Move(points^, pt^, sizeof(CoordPoint)*count);
  Result:=PolyCurvePrimitive.Create(points,count,seed,roughness,style,fractal);
  Result.MakeCopy(Self);
//  Result.CopyCore(Self);
//  (Result as PolyCurvePrimitive).fillcolor:=FillColor;
end;

Procedure PolyCurvePrimitive.ClearThis(Deallocate: Boolean);
Begin
  If (Points <> Nil) And Deallocate Then
  Begin
    FreeMem(Points);
    Points := Nil;
  End;
End; // PolyCurvePrimitive.ClearThis

function PolyCurvePrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var i:integer;
    n,tail:CurvePrimitive;
    seedstart:integer;
begin
  tail:=nil;
  i:=0;
  seedstart:=seed;
  while (i<Count-1) do begin
     n:=CurvePrimitive.Create(GetAdjustedPoint(points^[i]),GetAdjustedPoint(points^[i+1]),
                              GetAdjustedPoint(points^[i+2]),GetAdjustedPoint(points^[i+3]),
                              seedstart,roughness,style, fractal);
     n.CopyCore(self);

     if (tail=nil) then NewChain:=n else tail.Next:=n;
     tail:=n;

     inc(i,3);
     inc(seedstart);
     end;

  Result:=true;
end;

function PolyCurvePrimitive.IsClosed:boolean;
begin
  If Count <= 1
   Then Result := False
   Else Result := (points^[0].X = points^[Count-1].X) and
                  (points^[0].Y = points^[Count-1].Y);
end;

function PolyCurvePrimitive.GetLines(const View:ViewPoint; var polycount:integer):PCoordArray;
var i:integer;
    continue:TLineContinue;
    curvepoints:PCoordArray;
    sx,sy:integer;
    ScreenCoord:CoordRect;
    tempseed:integer;
    d:double;
begin
  continue:=GetLineStyleStart(SEGMENT_STYLE);

  if (View<>nil) then View.GetCoordinateRect(ScreenCoord);

  GetMem(curvepoints, sizeof(CoordPoint)*Count);
  for i:=0 to Count-1 do begin
    if View<>nil then begin
      View.CoordToScreen(GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y),sx,sy);
      curvepoints^[i].X := sx;
      curvepoints^[i].Y := sy;
      end
    else begin
      curvepoints^[i] := GetAdjustedPoint(points^[i]);
      end;
    end;

  tempseed:=seed;
  i:=0;
  while (i<Count-1) do begin
     if not fractal then begin
       DrawBezier(nil,curvepoints^[i],curvepoints^[i+1],
                      curvepoints^[i+2],curvepoints^[i+3], continue);
       end
     else begin
       FractalSetSeed(tempseed);
       if (View<>nil) and
          ((not View.OffScreenFullDetail) and
           (not VisibleWithin(MakeCoordRect(points^[i].X,points^[i].Y,points^[i+3].X,points^[i+3].Y),
                            ScreenCoord))) then begin
         { If it is off screen, don't bother doing anything fancy, just connect the line
           so we can do a fill properly if needed }
         DrawLineContinue(nil, trunc(curvepoints^[i].X),trunc(curvepoints^[i].Y),
                               trunc(curvepoints^[i+3].X),trunc(curvepoints^[i+3].Y), continue);
         end
       else begin
         d:=distance(trunc(curvepoints^[i].X),trunc(curvepoints^[i].Y),
                     trunc(curvepoints^[i+3].X),trunc(curvepoints^[i+3].Y));
         DrawFractalBezier(nil,curvepoints^[i],curvepoints^[i+1],curvepoints^[i+2],curvepoints^[i+3],
                               (d*roughness)/1000, continue);
         end;
       end;

     inc(i,3);
     inc(tempseed);
     end;

  FreeMem(curvepoints);

  Result:=GetLineEnd(continue,polycount);
end;

procedure PolyCurvePrimitive.DrawHandles(const View:ViewPoint);
var i: integer;
begin
  for i := 0 to Count - 1 - ord(IsClosed) do
   DrawHandle(View, GetAdjustedX(points^[i].X), GetAdjustedY(points^[i].Y));
end;

procedure PolyCurvePrimitive.DrawOverlayHandles(const View:ViewPoint);
begin
  if Count <> 0 then DrawOverlayHandle(View, GetAdjustedX(points^[0].X), GetAdjustedX(points^[0].Y));
end;

function PolyCurvePrimitive.ApplyMatrix(var mat:Matrix):boolean;
var
  i     : integer;
  Scale : Coord;
  
begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;
    End;
  End;

  for i:=0 to Count-1 do MultiplyPointByMatrix(points^[i].X, points^[i].Y, Mat);
  If Style.Line = start_numeric_thickness_style Then
  Begin
    Scale := (mat[1,1] + mat[2,2]) / 2;
    If Style.FullStyle.Thickness  >= 0 Then Style.FullStyle.Thickness  := Style.FullStyle.Thickness  * Scale;
    If Style.FullStyle.SThickness >= 0 Then Style.FullStyle.SThickness := Style.FullStyle.SThickness * Scale;
  End;
  Result:=true;
end;

Function PolyCurvePrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var
  P : PolyCurvePrimitive;
  B : Boolean;
  I : Integer;
  X1,Y1,X2,Y2 : Coord;

Begin
  If D Is PolyCurvePrimitive Then
  Begin
    P := PolyCurvePrimitive(D);
    B := (Count = P.Count)                                         And
         (FillColor = P.FillColor)                                 And
         (Style.Bits = P.Style.Bits)                               And
         (Style.FullStyle.Thickness = P.Style.FullStyle.Thickness) And
         (Fractal = P.Fractal)                                     And
         ((Not Fractal) Or
          ((Seed = P.Seed) And
           (Roughness = P.Roughness))) And Inherited IsSimilarTo(D);
    If Count > 0 Then
    Begin
      X1 := Points^[0].X;
      Y1 := Points^[0].Y;
      X2 := P.Points^[0].X;
      Y2 := P.Points^[0].Y;
      I  := 1;

      // Perhaps sometime in the future this check could take objects' relative
      // size into account, or at least scale VeryClose based on their size to
      // handle very small objects.  I'm not sure on this at the moment...

      While (I < Count) And B Do
      Begin
        B := B And (Abs((Points^[I].X - X1) - (P.Points^[I].X - X2)) < VeryClose) And
                   (Abs((Points^[I].Y - Y1) - (P.Points^[I].Y - Y2)) < VeryClose);
        Inc(I);
      End; // While
    End;
    Result := B;
  End
  Else Result := False;
End; // PolyCurvePrimitive.IsSimilarTo

function PolyCurvePrimitive.FindHandle(const View:ViewPoint; x,y:Coord):boolean;
var i:integer;
begin
  Result := true;
  for i:=0 to Count-1 do
  begin
    if TestHandle(View,
                  GetAdjustedX(points^[i].X),
                  GetAdjustedY(points^[i].Y),
                  x,y) then Exit;
  end;
  Result := false;
end;

function PolyCurvePrimitive.FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean;
begin
  Result := false;
  if (Count<2) or IsClosed then exit;
  if TestHandle(View, GetAdjustedX(points^[0].x),GetAdjustedY(points^[0].y),x,y) then
  begin
    x:=GetAdjustedX(points^[0].x);
    y:=GetAdjustedY(points^[0].y);
    Result:=true;
  end
  Else if TestHandle(View, GetAdjustedX(points^[Count-1].x),GetAdjustedY(points^[Count-1].y),x,y) then
  begin
    x:=GetAdjustedX(points^[Count-1].x);
    y:=GetAdjustedY(points^[Count-1].y);
    Result:=true;
  end;
end;

Function PolyCurvePrimitive.FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean;
Var
  Found  : Boolean;
  P1,P2  : CoordPoint;
  P3,P4  : CoordPoint;
  X0,Y0  : Coord;
  X1,Y1  : Coord;
  XX,YY  : Coord;
  AA     : Coord;
  I      : Integer;
  D,Dist : Double;
  B      : Boolean;
  ISeed  : Integer;

Begin
  I     := 0;
  Found := False;
  View.CoordToScreen(X,Y,X0,Y0);
  X1    := X0;
  Y1    := Y0; 
  Dist  := MaxDouble;
  ISeed := 0;
  While I <= Count - 4 Do
  Begin
    View.CoordToScreen(GetAdjustedX(Points^[I + 0].X),GetAdjustedY(Points^[I + 0].Y),P1.X,P1.Y);
    View.CoordToScreen(GetAdjustedX(Points^[I + 1].X),GetAdjustedY(Points^[I + 1].Y),P2.X,P2.Y);
    View.CoordToScreen(GetAdjustedX(Points^[I + 2].X),GetAdjustedY(Points^[I + 2].Y),P3.X,P3.Y);
    View.CoordToScreen(GetAdjustedX(Points^[I + 3].X),GetAdjustedY(Points^[I + 3].Y),P4.X,P4.Y);
    XX := X0;
    YY := Y0;

    If Fractal
     Then B := FractalCurveFindPointOn(XX,YY,AA,P1,P2,P3,P4,Seed + ISeed,Roughness,View)
     Else B := NormalCurveFindPointOn(XX,YY,AA,P1,P2,P3,P4);

    If (I = 0) Or Not Found Then
    Begin
      If B Then
      Begin
        Dist  := Distance(X0,Y0,XX,YY);
        X1    := XX;
        Y1    := YY;
        Angle := AA;
      End;
    End
    Else
    Begin
      If B Then
      Begin
        D := Distance(X0,Y0,XX,YY);
        If D < Dist Then
        Begin
          Dist  := D;
          X1    := XX;
          Y1    := YY;
          Angle := AA;
        End;
      End;
    End;
    Inc(I,3);
    Inc(ISeed);
    Found := Found Or B;
  End; // While
  Result := Found;
  If Result Then View.ScreenToCoord(X1,Y1,X,Y);
End; // PolyCurvePrimitive.FindPointOn

function PolyCurvePrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
var i:integer;
begin
  Result := false;
  for i:=0 to Count-1 do
  begin
    if TestHandle(View, GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y),origx,origy) then
    begin
      If Not Result Then SplitOff;
      points^[i].X:=origx+dx;
      points^[i].Y:=origy+dy;
      RecalcAliasFromExtent;
      Result:=true;
      if (mode=hmOne) then Exit;
    end;
  end;
end;

function PolyCurvePrimitive.SetStyle(new_style:StyleAttrib):boolean;
begin
  Result:=false;

  if (style.Line <> new_style.Line) and (new_style.Line<>$FF) then begin
    SplitOff;
    style.Line:=new_style.Line;
    Result:=true;
    end;

  if (style.Fill <> new_style.Fill) and (new_style.Fill<>$FF) then begin
    If Not Result Then SplitOff;
    style.Fill:=new_style.Fill;
    Result:=true;
    end;

  if (style.First <> new_style.First) and (new_style.First<>$FF) then begin
    If Not Result Then SplitOff;
    style.First:=new_style.First;
    Result:=true;
    end;

  if (style.Last <> new_style.Last) and (new_style.Last<>$FF) then begin
    If Not Result Then SplitOff;
    style.Last:=new_style.Last;
    Result:=true;
    end;

  If (Style.FullStyle.Thickness <> New_Style.FullStyle.Thickness) And
     (New_Style.FullStyle.Thickness >= 0) Then
  Begin
    If Not Result Then SplitOff;
    Style.FullStyle.Thickness  := New_Style.FullStyle.Thickness;
    Style.FullStyle.SThickness := New_Style.FullStyle.SThickness;
    Result := True;
  End;    
end;

function PolyCurvePrimitive.GetStyle:StyleAttrib;
begin
  Result:=style;
end;

procedure PolyCurvePrimitive.Move(dx,dy:Coord);
var i:integer;
begin
  If Base = Nil Then
   for i:=0 to Count-1 do
   begin
     points^[i].X := points^[i].X + dx;
     points^[i].Y := points^[i].Y + dy;
   end; // For I

  inherited Move(dx,dy);
end;

procedure PolyCurvePrimitive.ComputeExtent;
var i:integer;
    b:CoordRect;
    d:double;
    X,Y: Coord;
    Pts,Pts1 : PCoordArray;
    NumCount : Integer;

begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    if (points=nil) or (Count<4) then exit;

    Pts      := Points;
    NumCount := Count;
    If Style.Line = start_numeric_thickness_style Then
    Begin
      Pts1 := GetLines(Nil,NumCount);
      Pts  := GetThickLines(Pts1,NumCount,Style,True,False,IsClosed);
      FreeMem(Pts1);
    End;

    fExtent.left   := pts^[0].X;
    fExtent.right  := pts^[0].X;
    fExtent.top    := pts^[0].Y;
    fExtent.bottom := pts^[0].Y;

    i := 0;
    while i < NumCount - 1 do
    begin
      If Style.Line = start_numeric_thickness_style Then
      Begin
        Encompass(fExtent,Pts^[I].X,Pts^[I].Y);
        Inc(I);
      End
      Else
      Begin
        if not fractal then
        begin
          GetBezierBox(pts^[i],pts^[i+1],pts^[i+2],pts^[i+3], b);
        end
        else
        begin
          FractalSetSeed(Seed + I Div 3);  // JD 10-18-02: Seed correction
          d:=distance(pts^[i].X,pts^[i].Y,pts^[i+3].X,pts^[i+3].Y);
          GetFractalBezierBox(pts^[i],pts^[i+1],pts^[i+2],pts^[i+3], d*roughness/1000, b);
        end;
        Encompass(fExtent, b.left,b.top);
        Encompass(fExtent, b.right,b.bottom);
        inc(i,3);
      End;
    end;
    If Pts <> Points Then FreeMem(Pts);
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

procedure PolyCurvePrimitive.PointClosestTo(x,y:Coord; var px,py:Coord);
var n:integer;
begin
  n:=PointClosestInAdjustedArray(x,y,points,Count);
  px:=Points^[n].x;
  py:=Points^[n].y;
end;

function PolyCurvePrimitive.SetFillColor(color:TColor):boolean;
begin
  SplitOff;
  fillcolor:=color;
  Result:=true;
end;

function PolyCurvePrimitive.GetFillColor:TColor;
begin
  Result:=fillcolor;
end;

procedure PolyCurvePrimitive.Draw(View:ViewPoint);
var polypoints:PCoordArray;
    polycount:integer;
begin
  polypoints:=GetLines(View,polycount);

  DrawEnclosedFigure(View.Canvas, polypoints, polycount,
                     ((View.QuickDraw and QuickDraw_Fills)=0) and IsClosed,
                     Style,
                     DisplayColor(GetColor), DisplayFillColor(fillcolor),
                     View,True);

  If Style.Line = start_numeric_thickness_style Then ComputeExtent;

  FreeMem(polypoints);
end;

function PolyCurvePrimitive.GetId:char;
begin
  if not fractal then
    Result:='K'
  else
    Result:='k';
end;

procedure PolyCurvePrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
var i:integer;
begin
  inherited DoRead(stream, version, Full, UseAliasInfo);

  stream.ReadBuffer(fillcolor,sizeof(fillcolor));
  stream.ReadBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.ReadBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.ReadBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;  
  stream.ReadBuffer(Count,sizeof(Count));

  If (Base = Nil) Or Not UseAliasInfo Then
  Begin
    If Points <> Nil Then FreeMem(Points);
    GetMem(points,Count*sizeof(CoordPoint));
    for i := 0 to Count - 1 do
    begin
      stream.ReadBuffer(points^[i].X,sizeof(points^[i].X));
      stream.ReadBuffer(points^[i].Y,sizeof(points^[i].Y));
    end;
  End;

  if fractal then
  begin
    stream.ReadBuffer(seed,sizeof(seed));
    stream.ReadBuffer(roughness,sizeof(roughness));
  end;
end;

procedure PolyCurvePrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
var
  i   : integer;
  X,Y : Coord; // Must be Coords!!!

begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  stream.WriteBuffer(fillcolor,sizeof(fillcolor));
  stream.WriteBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.WriteBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.WriteBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;
  stream.WriteBuffer(Count,sizeof(Count));
  If (Base = Nil) Or Not UseAliasInfo Then
   for i := 0 to Count - 1 do
   begin
     X := GetAdjustedX(Points^[I].X) + AddX;
     Y := GetAdjustedY(Points^[I].Y) + AddY;
     stream.WriteBuffer(X,sizeof(points^[i].X));
     stream.WriteBuffer(Y,sizeof(points^[i].Y));
   end; // For I

  if fractal then
  begin
    stream.WriteBuffer(seed,sizeof(seed));
    stream.WriteBuffer(roughness,sizeof(roughness));
  end;
end;

Procedure PolyCurvePrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    FillColor := TColor(GetCardinalProperty(E,'FILLCOLOR'));
    Count     := GetIntegerProperty(E,'COUNT');
    If Base = Nil Then
    Begin
      If Points <> Nil Then FreeMem(Points);
      GetMem(Points,Count * SizeOf(CoordPoint));
    End;
    GetCoordPointsProperty(E,'POINTS',Points,Count);
    Style.Bits                 := GetCardinalProperty(E,'STYLE');
    Style.FullStyle.Thickness  := GetCoordProperty(E,'THICKNESS');
    Fractal                    := GetBooleanProperty(E,'FRACTAL');
    Style.FullStyle.SThickness := GetCoordProperty(E,'STHICKNESS');
    If Fractal Then
    Begin
      Seed      := GetIntegerProperty(E,'SEED');
      Roughness := GetIntegerProperty(E,'ROUGHNESS');
    End;
    RefreshCopies;
  End;
End; // PolyCurvePrimitive.ReadFromDOMElement

Function PolyCurvePrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCardinalProperty(D,'FILLCOLOR',Cardinal(FillColor)));
    E.appendChild(NewIntegerProperty(D,'COUNT',Count));
    E.appendChild(NewCoordPointsProperty(D,'POINTS',Points,Count));
    E.appendChild(NewCardinalProperty(D,'STYLE',Style.Bits));
    E.appendChild(NewCoordProperty(D,'THICKNESS',Style.FullStyle.Thickness));
    E.appendChild(NewBooleanProperty(D,'FRACTAL',Fractal));
    E.appendChild(NewCoordProperty(D,'STHICKNESS',Style.FullStyle.SThickness));
    If Fractal Then
    Begin
      E.appendChild(NewIntegerProperty(D,'SEED',Seed));
      E.appendChild(NewIntegerProperty(D,'ROUGHNESS',Roughness));
    End;
  End;
  Result := E;
End; // PolyCurvePrimitive.GetAsDOMElement

function PolyCurvePrimitive.SetSeed(new_seed:integer):boolean;
begin
  if not fractal then begin
    Result := inherited SetSeed(new_seed);
    end
  else begin
    if seed<>new_seed then begin
      SplitOff;
      seed:=new_seed;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function PolyCurvePrimitive.GetSeed:integer;
begin
  if not fractal then begin
    Result := inherited GetSeed;
    end
  else begin
    Result:=seed;
    end;
end;

function PolyCurvePrimitive.SetRoughness(rough:integer):boolean;
begin
  if not fractal then begin
    Result := inherited SetRoughness(rough);
    end
  else begin
    if roughness<>rough then begin
      SplitOff;
      roughness:=rough;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function PolyCurvePrimitive.GetRoughness: integer;
begin
  if not fractal
   then Result := inherited GetRoughness
   else Result := roughness;
end;

{------------------------------------------------------------------------------}

function TextCurvePrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var angle:integer;
    points:PCoordArray;
    i,count:integer;
    n,tail:TextPrimitive;
    PP1,PP2,PP3,PP4: CoordPoint;

begin
  PP1    := GetAdjustedPoint(P1);
  PP2    := GetAdjustedPoint(P2);
  PP3    := GetAdjustedPoint(P3);
  PP4    := GetAdjustedPoint(P4);
  Result := true;
  count  := ComputeBezierText(View.Canvas,PP1,PP2,PP3,PP4,text,points,angle);
  tail   := nil;

  for i := 1 to count do
  begin
    n := TextPrimitive.Create(View,points^[i-1].X,points^[i-1].Y,
                              text[i],Font,0,fOutlineCOlor);
    n.Angle := angle;
    n.CopyCore(self);
    if tail = nil then NewChain := n else tail.Next := n;
    tail := n;
  end;

  FreeMem(Points);
end;

constructor TextCurvePrimitive.CreateBlank;
begin
  inherited CreateBlank(false);
  Font          := TFont.Create;
  Text          := '';
  Size          := 1;
  Ch            := 0;
  fOutlineColor := clBlack;
end;

Procedure TextCurvePrimitive.ClearThis(Deallocate: Boolean);
Begin
  If (Font <> Nil) And Deallocate Then
  Begin
    Font.Free;
    Font := Nil;
  End;
End; // TextCurvePrimitive.ClearThis

Procedure TextCurvePrimitive.CopyFromBase(AliasOnly: Boolean);
Var TCP: TextCurvePrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    TCP  := Base As TextCurvePrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Text := TCP.Text;
    If AliasOnly Then
    Begin
      If (Font <> TCP.Font) And (Font <> Nil) Then Font.Free;
      Font := TCP.Font;
    End
    Else
    Begin
      If (Font <> TCP.Font) And (Font <> Nil) Then Font.Free;
      Font := TFont.Create;
      Font.Assign(TCP.Font);
    End;
    Size          := TCP.Size;
    Ch            := TCP.Ch;
    fOutlineColor := TCP.fOutlineColor;
  End;
End; // TextCurvePrimitive.CopyFromBase

procedure TextCurvePrimitive.ComputeExtent;
Var X,Y: Coord;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    inherited ComputeExtent;
    fExtent.Left   := fExtent.Left   - ch;
    fExtent.Right  := fExtent.Right  + ch;
    fExtent.Top    := fExtent.Top    - ch;
    fExtent.Bottom := fExtent.Bottom + ch;
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

procedure TextCurvePrimitive.ComputeSize(const View:ViewPoint);
var w,h:integer;
    cw:Coord;
begin
  View.Canvas.Font.Assign(Font);
  w := View.Canvas.TextWidth(text);
  h := View.Canvas.TextHeight(text);
  View.DeltaScreenToCoord(w,h,cw,ch);
end;

constructor TextCurvePrimitive.Create(const View:ViewPoint; ip1,ip2,ip3,ip4:CoordPoint;
                                      itext:string; IFont:TFont; outlinecolor:TColor);
var st: StyleAttrib;
begin
  st.Bits       := 0;
  St.FullStyle.Thickness  := 0;
  St.FullStyle.SThickness := 0;
  inherited Create(ip1,ip2,ip3,ip4,st);
  Text          := itext;
  Font          := TFont.Create;
  Font.Assign(IFont);
  Size          := IFont.Size;
  ComputeSize(View);
  fOutlineColor := outlinecolor;
  ComputeExtent;
end;

function TextCurvePrimitive.Copy: DrawPrimitive;
begin
  Result := TextCurvePrimitive.Create(Map.CurrentView,
                                      GetAdjustedP1,
                                      GetAdjustedP2,
                                      GetAdjustedP3,
                                      GetAdjustedP4,
                                      text,Font,foutlinecolor);
  Result.MakeCopy(Self);
end;

procedure TextCurvePrimitive.Draw(View:ViewPoint);
var w,h:integer;
    sp1,sp2,sp3,sp4:CoordPoint;
    ix,iy:integer;
    OldWidth:integer;
    OldColor:TColor;
    PP1,PP2,PP3,PP4 : CoordPoint;

begin
  PP1 := GetAdjustedP1;
  PP2 := GetAdjustedP2;
  PP3 := GetAdjustedP3;
  PP4 := GetAdjustedP4;
  View.DeltaCoordToScreen(0,ch,w,h);
  if h < 6 then exit;

  Font.Height := h;
  Font.Color  := DisplayColor(GetColor);
  View.Canvas.Font.Assign(Font);
  { Defeat the built-in logic for scaling up fonts for the printer,
    since we automatically do that as a result of matching the font
    size to the zoom level. Reset the font height after assigning
    the font into the canvas.}
  View.Canvas.Font.Height := h;
  View.Canvas.Brush.Style := bsClear;

  View.CoordToScreen(PP1.x,PP1.y,ix,iy);
  sp1.x := ix;
  sp1.y := iy;
  View.CoordToScreen(PP2.x,PP2.y,ix,iy);
  sp2.x := ix;
  sp2.y := iy;
  View.CoordToScreen(PP3.x,PP3.y,ix,iy);
  sp3.x := ix;
  sp3.y := iy;
  View.CoordToScreen(PP4.x,PP4.y,ix,iy);
  sp4.x := ix;
  sp4.y := iy;

  if ((View.QuickDraw and QuickDraw_Fills) = 0) and (fOutlineColor <> clNone) then
  begin
    BeginPath(View.Canvas.Handle);
    DrawBezierText(View.Canvas, sp1,sp2,sp3,sp4, Text);
    EndPath(View.Canvas.Handle);
    OldWidth := View.Canvas.Pen.Width;
    OldColor := View.Canvas.Pen.Color;
    View.Canvas.Pen.Width := 5;
    View.Canvas.Pen.Color := DisplayFillColor(fOutlineColor);
    StrokePath(View.Canvas.Handle);
    View.Canvas.Pen.Width := OldWidth;
    View.Canvas.Pen.Color := OldColor;
  end;
  DrawBezierText(View.Canvas, sp1,sp2,sp3,sp4, Text);
end;

Function TextCurvePrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: TextCurvePrimitive;
Begin
  If D Is TextCurvePrimitive Then
  Begin
    P := TextCurvePrimitive(D);
    Result := (Text = P.Text)     And
              (Font.Name = P.Font.Name)     And
              (Font.Style = P.Font.Style) And
              (Font.Color = P.Font.Color)       And
              (Font.Size = P.Font.Size) And
              (Ch = P.Ch) And
              (fOutlineColor = P.fOutlineColor) And
              (Size = P.Size) And Inherited IsSimilarTo(D);
  End
  Else Result := False;
End; // TextCurvePrimitive.IsSimilarTo

function TextCurvePrimitive.ApplyMatrix(var mat:Matrix):boolean;
var hx,hy:Coord;
    x1,y1:Coord;
begin
  inherited ApplyMatrix(mat);
  x1 := p1.x;
  y1 := p1.y;
  hx := x1;
  hy := y1 + ch;
  MultiplyPointByMatrix(x1,y1, mat);
  MultiplyPointByMatrix(hx,hy, mat);
  ch     := distance(x1,y1,hx,hy);
  Result := true;
end;

function TextCurvePrimitive.GetId:char;
begin
  Result := 't';
end;

procedure TextCurvePrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
var fontstyle:TFontStyles;
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(ch,sizeof(ch));
  stream.ReadBuffer(size,sizeof(size));
  Text:=ReadStringFromStream(stream);
  Font.Name:=ReadStringFromStream(stream);
  // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
  Font.Charset:=DEFAULT_CHARSET;
  stream.ReadBuffer(fontstyle, sizeof(fontstyle));
  Font.Style:=fontstyle;
  stream.ReadBuffer(fOutlineColor,sizeof(fOutlineColor));  //!!
end;

procedure TextCurvePrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
var fontstyle:TFontStyles;
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  stream.WriteBuffer(ch,sizeof(ch));
  stream.WriteBuffer(size,sizeof(size));
  WriteStringToStream(stream,Text);
  WriteStringToStream(stream,Font.Name);
  fontstyle:=Font.Style;
  stream.WriteBuffer(fontstyle, sizeof(fontstyle));
  stream.WriteBuffer(fOutlineColor,sizeof(fOutlineColor));
end;

Procedure TextCurvePrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    Ch        := GetCoordProperty(E,'CH');
    Size      := GetIntegerProperty(E,'SIZE');
    Text      := MimeDecodeString(Trim(GetStringProperty(E,'TEXT')));
    Font.Name := MimeDecodeString(Trim(GetStringProperty(E,'FONT')));
    // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
    Font.Charset := DEFAULT_CHARSET;
    Font.Style   := [];
    If GetBooleanProperty(E,'BOLD')      Then Font.Style := Font.Style + [fsBold];
    If GetBooleanProperty(E,'ITALIC')    Then Font.Style := Font.Style + [fsItalic];
    If GetBooleanProperty(E,'UNDERLINE') Then Font.Style := Font.Style + [fsUnderline];
    If GetBooleanProperty(E,'STRIKEOUT') Then Font.Style := Font.Style + [fsStrikeOut];
    fOutlineColor := TColor(GetCardinalProperty(E,'OUTLINE_COLOR'));
    RefreshCopies;
  End;
End; // TextCurvePrimitive.ReadFromDOMElement

Function TextCurvePrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordProperty(D,'CH',Ch));
    E.appendChild(NewIntegerProperty(D,'SIZE',Size));
    E.appendChild(NewStringProperty(D,'TEXT',MimeEncodeString(Text)));
    E.appendChild(NewStringProperty(D,'FONT',MimeEncodeString(Font.Name)));
    E.appendChild(NewBooleanProperty(D,'BOLD',fsBold In Font.Style));
    E.appendChild(NewBooleanProperty(D,'ITALIC',fsItalic In Font.Style));
    E.appendChild(NewBooleanProperty(D,'UNDERLINE',fsUnderline In Font.Style));
    E.appendChild(NewBooleanProperty(D,'STRIKEOUT',fsStrikeOut In Font.Style));
    E.appendChild(NewCardinalProperty(D,'OUTLINE_COLOR',Cardinal(fOutlineColor)));
  End;  
  Result := E;
End; // TextCurvePrimitive.GetAsDOMElement

function  TextCurvePrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  Result:=false;
  if tatFontSize in attrib.Valid then begin
    SplitOff;
    if Settings.RelativeTextSize.Checked then begin
      ch:=(ch*attrib.FontSize)/Size;
      Size:=attrib.FontSize;
    end else begin
      Font.Size:=attrib.FontSize;
      ComputeSize(View);
    end;
    Result:=true;
    end;
  if tatText in attrib.Valid then begin
    If Not Result Then SplitOff;
    Text:=attrib.Text;
    Result:=true;
    end;
  if tatFontName in attrib.Valid then begin
    If Not Result Then SplitOff;
    Font.Name:=attrib.FontName;
    // Changed below line to display symbols properly on Japanese systems.  Ryuichi Sakamoto.
    Font.Charset:=DEFAULT_CHARSET;
    Result:=true;
    end;
  if tatFontBold in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontBold then Font.Style := Font.Style + [fsBold]
                       else Font.Style := Font.Style - [fsBold];
    Result:=true;
    end;
  if tatFontItalic in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontItalic then Font.Style := Font.Style + [fsItalic]
                         else Font.Style := Font.Style - [fsItalic];
    Result:=true;
    end;
  if tatFontUnderline in attrib.Valid then begin
    If Not Result Then SplitOff;
    if attrib.FontUnderline then Font.Style := Font.Style + [fsUnderline]
                            else Font.Style := Font.Style - [fsUnderline];
    Result:=true;
    end;
  if tatOutlineColor in attrib.Valid then begin
    If Not Result Then SplitOff;
    fOutlineColor:=attrib.FontOutlineColor;
    Result:=true;
    end;
  if Result=true then ComputeExtent;
end;

function TextCurvePrimitive.GetTextAttrib:TextAttrib;
begin
  Result.Valid := [tatText, tatFontName, tatFontSize,
                  tatFontBold, tatFontItalic, tatFontUnderline,
                  tatOutlineColor];

  Result.Text             := text;
  Result.FontName         := Font.Name;
  Result.FontSize         := Size;
  Result.FontBold         := fsBold      in Font.Style;
  Result.FontItalic       := fsItalic    in Font.Style;
  Result.FontUnderline    := fsUnderline in Font.Style;
  Result.FontOutlineColor := fOutlineColor;
end;

{------------------------------------------------------------------------------}

function PolyLinePrimitive.SetFractal(state:FractalState):boolean;
begin
  SplitOff;
  fractal := SetFractalState(fractal, state);
  ComputeExtent;
  Result:=true;
end;

procedure PolyLinePrimitive.Reverse;
var i:integer;
begin
  SplitOff;
  for i:=0 to (Count-1) div 2 do begin
    SwapPoints(points^[i],points^[Count-1-i]);
    end;

  // Fractal polylines can change their bounding rectangle by reversing
  if fractal then begin
    ComputeExtent;
  end;
end;

Procedure PolyLinePrimitive.CopyFromBase(AliasOnly: Boolean);
Var
  I   : Integer;
  PLP : PolyLinePrimitive;

Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    PLP       := Base As PolyLinePrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Style     := PLP.Style;
    FillColor := PLP.FillColor;
    Count     := PLP.Count;
    Seed      := PLP.Seed;
    Roughness := PLP.Roughness;
    Fractal   := PLP.Fractal;
    If AliasOnly Then Points := PLP.Points
    Else
    Begin
      GetMem(Points,SizeOf(CoordPoint) * Count);
      System.Move(PLP.Points^,Points^,SizeOf(CoordPoint) * Count);
      For I := 0 To Count - 1 Do
      Begin
        Points^[I].X := Points^[I].X + (Alias.X - Base.fExtent.Left);
        Points^[I].Y := Points^[I].Y + (Alias.Y - Base.fExtent.Top);
      End; // For I
    End;
  End;
End; // PolyLinePrimitive.CopyFromBase

procedure PolyLinePrimitive.CloseFigure;
var pts:PCoordArray;
    i:integer;
begin
  if (not IsClosed) and (Count>1) then
  begin
    SplitOff;
    GetMem(pts,sizeof(CoordPoint)*(Count+1));
    for i:=0 to Count-1 do pts^[i]:=points^[i];
    pts^[Count] := points^[0];
    FreeMem(points);
    points:=pts;
    Count:=Count+1;
    ComputeExtent;
  end;
end;

function PolyLinePrimitive.SliceAlong(s1,s2:CoordPoint; var np:DrawPrimitive):boolean;
var isect:CoordPoint;
    pts:PCoordArray;
    i,n,hits:integer;
    PLP : PolyLinePrimitive;

begin
  hits:=0;
  for i:=0 to Count-2 do begin
    if IntersectLine(GetAdjustedPoint(points^[i]),GetAdjustedPoint(points^[i+1]),
                     s1,s2,isect)=IntersectOnLine then
      inc(hits);
    end;

  if hits=0 then begin
    Result:=false;
    exit;
    end;

  GetMem(pts,sizeof(CoordPoint)*(Count+hits));
  pts^[0]:=points^[0];
  n:=1;
  for i:=1 to Count-1 do begin
    if IntersectLine(GetAdjustedPoint(points^[i-1]),GetAdjustedPoint(points^[i]),
                     s1,s2,isect)=IntersectOnLine then begin
      pts^[n]:=isect;
      inc(n);
      end;
    pts^[n]:=points^[i];
    inc(n);
    end;

  np := Copy;
  np.SplitOff;
  PLP := np As PolyLinePrimitive;
  FreeMem(PLP.Points);
  PLP.Points := pts;
  PLP.Count  := Count + hits;
  Result     := true;
end;

function PolyLinePrimitive.FindScalpelPoint(const View:ViewPoint; x,y:Coord; var index:integer):boolean;
var i,i0,i1:integer;
begin
  if (Count<=2) then begin
    Result:=false; exit;
    end;

  Result:=true;

  // JD 10-18-02: Added the ability to remove endpoints of open polylines

  if isClosed Then
  Begin
    i0 := 0;
    i1 := Count - 2;
  End
  Else
  Begin
    i0 := 0;
    i1 := Count - 1;
  End;
  For I := I0 To I1 Do
  Begin
//  for i:=1-ord(IsClosed) to Count-2 do begin
    if TestHandle(View,x,y,GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y)) then
    begin
      index := i;
      exit;
    end;
  end;

  Result:=false;
end;

function PolyLinePrimitive.SeparateNode(const View:ViewPoint; index:integer; var NewObject:DrawPrimitive):boolean;
var p:PolyLinePrimitive;
    i,n:integer;
    pt:PCoordArray;
    dx,dy:Coord;
    a1,a2:double;
begin
  if IsClosed then begin
    SplitOff;
    GetMem(pt, sizeof(CoordPoint)*count);

    // Roll the array around so that the index point is the first and last
    n:=0;
    for i:=index to count-1 do begin
      pt^[n] := points^[i];
      inc(n);
      end;
    for i:=1 to index-1 do begin
      pt^[n] := points^[i];
      inc(n);
      end;
    pt^[count-1] := pt^[0];

    // Fix up the first and last point to "separate"
    View.DeltaScreenToCoord(5,5,dx,dy);
    a1:=ArcTan2(pt^[1].y-pt^[0].y,pt^[1].x-pt^[0].x);
    a2:=ArcTan2(pt^[count-2].y-pt^[0].y,pt^[count-2].x-pt^[0].x);

    pt^[0].x:=pt^[0].x+cos(a1)*dx;
    pt^[0].y:=pt^[0].y+sin(a1)*dy;
    pt^[count-1].x:=pt^[count-1].x+cos(a2)*dx;
    pt^[count-1].y:=pt^[count-1].y+sin(a2)*dy;

    FreeMem(points);
    points:=pt;
    ComputeExtent;
    Result:=false;
    end
  else begin
    If (Index > 0) And (Index < Count - 1) Then
    Begin
      // Make a copy, and splice it into the list
      p:=Copy as PolyLinePrimitive;
      p.Next:=Next;
      Next:=p;
      P.SplitOff;

      // Truncate this polyline at index
      SplitOff;
      Count:=index+1;
      ComputeExtent;

      // Truncate all before this index in the new polyline
      for i:=index to p.Count-1 do begin
        p.points^[i-index]:=p.points^[i];
        end;
      p.Count:=p.Count-index;
      p.ComputeExtent;
      NewObject := p;
      // Advance the seed so that the portion split off will start with the
      // same seed it when it belonged to the main line.
      NewObject.SetSeed(Seed + Count - 1);
      P.AddToBaseOrCopies;
      Result:=true;
    End
    Else Result := False;
  end;
end;

procedure PolyLinePrimitive.DeleteNode(const View:ViewPoint; index:integer);
var i:integer;
    WasClosed:boolean;
begin
  SplitOff;
  WasClosed:=IsClosed;
  for i:=index to Count-2 do points^[i]:=points^[i+1];
  dec(Count);
  // As long as the final result doesn't equal a straight line,
  // keep the box closed.
  if WasClosed then begin
    if (Count>3) then points^[Count-1]:=points^[0] else dec(Count);
    end;

  // JD 10-18-02: Correction to protect the remaining fractal shape

  If (Index = 0) And Fractal Then Inc(Seed);

  ComputeExtent;
end;

constructor PolyLinePrimitive.CreateBlank(frac:boolean);
begin
  inherited Create;
  points:=nil;
  count:=0;
  style.Bits:=0;
  Style.FullStyle.Thickness  := 0;
  Style.FullStyle.SThickness := 0;
  fillcolor:=CurrentFillColor;
  fractal := frac;
  ComputeExtent;
end;

constructor PolyLinePrimitive.Create(lp:PCoordArray; c:integer; istyle:StyleAttrib);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor := CurrentFillColor;
  seed      := 0;
  roughness := 0;
  fractal   := false;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

constructor PolylinePrimitive.Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor := CurrentFillColor;
  seed      := iseed;
  roughness := irough;
  fractal   := true;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

constructor PolylinePrimitive.Create(lp:PCoordArray; c:integer; iseed,irough:integer; istyle:StyleAttrib; frac:boolean);
begin
  inherited Create;
  points    := lp;
  count     := c;
  style     := istyle;
  fillcolor := CurrentFillColor;
  seed      := iseed;
  roughness := irough;
  fractal   := frac;
  ComputeExtent;
  Alias.X    := fExtent.Left;
  Alias.Y    := fExtent.Top;
end;

function PolyLinePrimitive.Copy:DrawPrimitive;
//var pt:PCoordArray;
begin
//  GetMem(pt, sizeof(CoordPoint)*count);
//  System.Move(points^, pt^, sizeof(CoordPoint)*count);
  Result:=PolylinePrimitive.Create(points,count,seed,roughness,style,fractal);
  Result.MakeCopy(Self);
//  Result.CopyCore(Self);
//  (Result as PolyLinePrimitive).fillcolor:=FillColor;
end;

Procedure PolyLinePrimitive.ClearThis(Deallocate: Boolean);
Begin
  If (Points <> Nil) And Deallocate Then
  Begin
    FreeMem(Points);
    Points := Nil;
  End;
End; // PolyLinePrimitive.ClearThis

function PolyLinePrimitive.IsClosed:boolean;
begin
  If Count <= 1
   Then Result := False
   Else Result := (points^[0].X=points^[Count-1].X) and
                  (points^[0].Y=points^[Count-1].Y);
end;

function PolyLinePrimitive.Inside(x,y:Coord):boolean;
var i,n:integer;
    tp1,tp2:CoordPoint;

  function intersect(p1,p2,p3,p4:CoordPoint):boolean;
  var it:IntersectType;
      ipoint:CoordPoint;
  begin
    it:=IntersectLine(p1,p2,p3,p4,ipoint);
    Result:=(it=IntersectOnLine);
  end;

begin
  if not IsClosed then begin
    Result:=false;
    exit;
    end;

  { Create the test line }
  tp1.X:=X;
  tp1.Y:=Y;
  tp2.X:=X;
  tp2.Y:=Y;

  for i:=0 to Count-1 do begin
    tp2.X:=max(tp2.X,points^[i].X);
    tp2.Y:=max(tp2.Y,points^[i].Y);
    end;

  tp2.X:=tp2.X*pi;
  tp2.Y:=tp2.Y*10;

  { Now, see where the test line intersects with the sides
    of the polygon.  Note that this simple form of the
    procedure doesn't work if the polygon vertex lies exactly
    on the test line.  That's why the test line is computed
    from the max of the polygon points * pi to minimize the
    chance of this happening.  This just creates a very odd
    sloped line very far away--odds are poor that we'll have
    an exact intersection. }
  n:=0;
  for i:=0 to Count-2 do begin
    n := n + ord(intersect(points^[i],points^[i+1],tp1,tp2));
    end;

  Result:=odd(n);
end;

function PolyLinePrimitive.GetLines(const View:Viewpoint; var polycount:integer):PCoordArray;

  function GetNormalLines:PCoordArray;
  var i:integer;
      poly:PCoordArray;
      sx,sy:integer;
  begin
    GetMem(poly,sizeof(CoordPoint)*Count);
    for i:=0 to Count-1 do begin
      if (View<>nil) then begin
        View.CoordToScreen(GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y),sx,sy);
        poly^[i].X:=sx;
        poly^[i].Y:=sy;
        end
      else begin
        poly^[i]:=GetAdjustedPoint(points^[i]);
        end;
      end;
    Result:=poly;
    polycount:=Count;
  end;

  function GetFractalLines:PCoordArray;
  var s1,s2:CoordPoint;
      i:integer;
      continue:TLineContinue;
      ScreenCoord:CoordRect;
  begin
    continue:=GetLineStyleStart(SEGMENT_STYLE);
    if (View<>nil) then begin
      View.GetCoordinateRect(ScreenCoord);
      s1:=View.CoordToScreenPt(GetAdjustedPoint(points^[0]));
      end
    else begin
      s1:=GetAdjustedPoint(points^[0]);
      end;

    for i:=1 to Count-1 do begin
       if (View<>nil) then begin
         s2:=View.CoordToScreenPt(GetAdjustedPoint(points^[i]));
         end
       else begin
         s2:=GetAdjustedPoint(points^[i]);
         end;

       FractalSetSeed(seed + i - 1);

       if (View<>nil) and
          ((not View.OffScreenFullDetail) and
           (not VisibleWithin(MakeCoordRect(GetAdjustedX(points^[i-1].X),
                                            GetAdjustedY(points^[i-1].Y),
                                            GetAdjustedX(points^[i].X),
                                            GetAdjustedY(points^[i].Y)),
                            ScreenCoord))) then begin
         { If it is off screen, don't bother doing anything fancy, just connect the line
           so we can do a fill properly if needed }
         DrawLineContinue(nil, s1.x,s1.y,s2.x,s2.y,continue);
         end
       else begin
         FractalLine(nil,s1.x,s1.y,s2.x,s2.y,(distance(s1.x,s1.y,s2.x,s2.y)*roughness)/1000, continue);
         end;
       s1:=s2;
       end;

    Result:=GetLineEnd(continue,polycount);
  end;

begin 
  if not fractal then
    Result:=GetNormalLines
  else
    Result:=GetFractalLines;
end;


procedure PolyLinePrimitive.Draw(View:ViewPoint);
var poly:PCoordArray;
    polypoints:integer;
begin
  poly := GetLines(View, polypoints);
  DrawEnclosedFigure(View.Canvas, poly, polypoints,
                     ((View.QuickDraw and QuickDraw_Fills)=0) and IsClosed,
                     Style,
                     DisplayColor(GetColor), DisplayFillColor(fillcolor),
                     View,Fractal);

  If Style.Line = start_numeric_thickness_style Then ComputeExtent;

  FreeMem(poly);
end;

function PolyLinePrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
var
  i         : integer;
  n,tail    : LinePrimitive;
  seedstart : integer;

begin
  tail      := nil;
  seedstart := seed;
  for i := 0 to Count - 2 do
  begin
    n := LinePrimitive.Create(GetAdjustedX(points^[i].X),  GetAdjustedY(points^[i].Y),
                              GetAdjustedX(points^[i+1].X),GetAdjustedY(points^[i+1].Y),
                              seedstart,roughness,style,fractal);
    n.CopyCore(self);

    if tail = nil then NewChain := n else tail.Next := n;
    tail := n;
    inc(seedstart);
  end;
  Result := true;
end;

procedure PolyLinePrimitive.DrawHandles(const View:ViewPoint);
var i: integer;
begin
  for i := 0 to Count - 1 - ord(IsClosed) do
   DrawHandle(View,GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y));
end;

procedure PolyLinePrimitive.DrawOverlayHandles(const View:ViewPoint);
begin
  if Count <> 0 then DrawOverlayHandle(View,GetAdjustedX(points^[0].X),GetAdjustedY(points^[0].Y));
end;

function PolyLinePrimitive.SetFillColor(color:TColor):boolean;
begin
  SplitOff;
  fillcolor:=color;
  Result:=true;
end;

function PolyLinePrimitive.GetFillColor:TColor;
begin
  Result := fillcolor;
end;

function PolyLinePrimitive.FindHandle(const View:ViewPoint; x,y:Coord):boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to Count-1 do begin
    if TestHandle(View, GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y),x,y) then Exit;
    end;

  Result:=false;
end;

function PolyLinePrimitive.FindEndPoint(const View:ViewPoint; var x,y:Coord):boolean;
begin
  Result:=false;
  if (Count<2) or IsClosed then exit;

  if TestHandle(View, GetAdjustedX(points^[0].x),GetAdjustedY(points^[0].y),x,y) then
  begin
    x:=GetAdjustedX(points^[0].x);
    y:=GetAdjustedY(points^[0].y);
    Result:=true;
  end
  Else if TestHandle(View, GetAdjustedX(points^[Count-1].x),GetAdjustedY(points^[Count-1].y),x,y) then
  begin
    x:=GetAdjustedX(points^[Count-1].x);
    y:=GetAdjustedY(points^[Count-1].y);
    Result:=true;
  end;
end;

Function PolyLinePrimitive.FindPointOn(Const View: ViewPoint; Var X,Y,Angle: Coord): Boolean;
Var
  Found  : Boolean;
  X1,Y1  : Coord;
  X2,Y2  : Coord;
  X0,Y0  : Coord;
  XX,YY  : Coord;
  AA     : Coord;
  I      : Integer;
  D,Dist : Double;
  B      : Boolean;
  X3,Y3  : Coord;

Begin
  I     := 0;
  Found := False;
  View.CoordToScreen(X,Y,X0,Y0);
  X3    := X0;
  Y3    := Y0;
  Dist  := MaxDouble;
  While I < Count - 1 Do
  Begin
    View.CoordToScreen(GetAdjustedX(Points^[I].X),GetAdjustedY(Points^[I].Y),X1,Y1);
    View.CoordToScreen(GetAdjustedX(Points^[I + 1].X),GetAdjustedY(Points^[I + 1].Y),X2,Y2);
    XX := X0;
    YY := Y0;
    If Fractal
     Then B := FractalLineFindPointOn(XX,YY,AA,X1,Y1,X2,Y2,Seed + I,Roughness,View)
     Else B := NormalLineFindPointOn(XX,YY,AA,X1,Y1,X2,Y2);
    If (I = 0) Or Not Found Then
    Begin
      If B Then
      Begin
        Dist  := Distance(X0,Y0,XX,YY);
        X3    := XX;
        Y3    := YY;
        Angle := AA;
      End;
    End
    Else
    Begin
      If B Then
      Begin
        D := Distance(X0,Y0,XX,YY);
        If D < Dist Then
        Begin
          Dist  := D;
          X3    := XX;
          Y3    := YY;
          Angle := AA;
        End;
      End;
    End;
    Inc(I);
    Found := Found Or B;
  End; // While
  Result := Found;
  If Found Then View.ScreenToCoord(X3,Y3,X,Y);
End; // PolyLinePrimitive.FindPointOn

procedure PolyLinePrimitive.PointClosestTo(x,y:Coord; var px,py:Coord);
var n:integer;
begin
  n:=PointClosestInAdjustedArray(x,y,points,Count);
  px:=Points^[n].x;
  py:=Points^[n].y;
end;

function PolyLinePrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
var i,j,n:integer;
    lastx,lasty:Coord;
    resize:PCoordArray;
begin
  Result:=false;

  n:=0;
  j:=-1;
  for i:=0 to Count-1 do
  begin
    if TestHandle(View, GetAdjustedX(points^[i].X),GetAdjustedY(points^[i].Y),origx,origy) then
    begin
      if (mode=hmAll) or (n=0) then
      begin
        SplitOff;
        points^[i].X := origx+dx;
        points^[i].Y := origy+dy;
        j:=i;
      end;
      inc(n);
      Result:=true;
    end;
  end;

  case mode of
    hmAll: begin
          // If they've "deleted" any sections by moving one point on top of its
          // consecutive neighbor, remove that point.
          if (n>1) then begin
            n:=Count;
            lastx := points^[0].X;
            lasty := points^[0].Y;
            i:=1;
            while (i<=n-1) do begin
              if (points^[i].X = lastX) and (points^[i].Y = lastY) then begin
                for j:=i to n-2 do points^[j]:=points^[j+1];
                dec(n);
                end
              else begin
                lastx := points^[i].X;
                lasty := points^[i].Y;
                inc(i);
                end;
              end;
            Count:=n;
            end;
        end;
    hmOne: begin
          // If they are using Alt to drag out a handle, and there is only
          // one at this location, fabricate a handle that they can extract
          if (n=1) then begin
            GetMem(resize,(Count+1)*sizeof(CoordPoint));
            for i:=0 to j       do resize^[i]:=points^[i];
            for i:=j to Count-1 do resize^[i+1]:=points^[i];
            FreeMem(points);
            points:=resize;
            inc(Count);
            end;

          mode := hmFoundFirst;
        end;
    hmFoundFirst: begin
        end;
    end;

  // If we've moved a handle, recompute our extent.
  if Result then ComputeExtent;
end;

function PolyLinePrimitive.SetStyle(new_style:StyleAttrib):boolean;
begin
  Result:=false;

  if (style.Line <> new_style.Line) and (new_style.Line<>$FF) then begin
    SplitOff;
    style.Line:=new_style.Line;
    Result:=true;
    end;

  if (style.Fill <> new_style.Fill) and (new_style.Fill<>$FF) then begin
    If Not Result Then SplitOff;
    style.Fill:=new_style.Fill;
    Result:=true;
    end;

  if (style.First <> new_style.First) and (new_style.First<>$FF) then begin
    If Not Result Then SplitOff;
    style.First:=new_style.First;
    Result:=true;
    end;

  if (style.Last <> new_style.Last) and (new_style.Last<>$FF) then begin
    If Not Result Then SplitOff;
    style.Last:=new_style.Last;
    Result:=true;
    end;

  If (Style.FullStyle.Thickness <> New_Style.FullStyle.Thickness) And
     (New_Style.FullStyle.Thickness >= 0) Then
  Begin
    If Not Result Then SplitOff;
    Style.FullStyle.Thickness  := New_Style.FullStyle.Thickness;
    Style.FullStyle.SThickness := New_Style.FullStyle.SThickness;
    Result := True;
  End;
end;

function PolyLinePrimitive.GetStyle:StyleAttrib;
begin
  Result:=style;
end;

procedure PolyLinePrimitive.Move(dx,dy:Coord);
var i:integer;
begin
  If Base = Nil Then
   for i:=0 to Count-1 do
   begin
     points^[i].X := points^[i].X + dx;
     points^[i].Y := points^[i].Y + dy;
   end; // For i

  inherited Move(dx,dy);
end;

function PolyLinePrimitive.ApplyMatrix(var mat:Matrix):boolean;
var
  i     : integer;
  Scale : Coord;

begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;
    End;
  End;

  for i := 0 to Count - 1 do MultiplyPointByMatrix(points^[i].X,points^[i].Y,Mat);
  If Style.Line = start_numeric_thickness_style Then
  Begin
    Scale := (mat[1,1] + mat[2,2]) / 2;
    If Style.FullStyle.Thickness  >= 0 Then Style.FullStyle.Thickness  := Style.FullStyle.Thickness  * Scale;
    If Style.FullStyle.SThickness >= 0 Then Style.FullStyle.SThickness := Style.FullStyle.SThickness * Scale;
  End;
  Result:=true;
end;

Function PolyLinePrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var
  P : PolyLinePrimitive;
  B : Boolean;
  I : Integer;
  X1,Y1,X2,Y2: Coord;

Begin
  If D Is PolyLinePrimitive Then
  Begin
    P := PolyLinePrimitive(D);
    B := (Count = P.Count)                                         And
         (FillColor = P.FillColor)                                 And
         (Style.Bits = P.Style.Bits)                               And
         (Style.FullStyle.Thickness = P.Style.FullStyle.Thickness) And
         (Fractal = P.Fractal)                                     And
         ((Not Fractal) Or
          ((Seed = P.Seed) And
           (Roughness = P.Roughness))) And Inherited IsSimilarTo(D);
    If B And (Count > 0) Then
    Begin
      X1 := Points^[0].X;
      Y1 := Points^[0].Y;
      X2 := P.Points^[0].X;
      Y2 := P.Points^[0].Y;
      I  := 1;

      // Perhaps sometime in the future this check could take objects' relative
      // size into account, or at least scale VeryClose based on their size to
      // handle very small objects.  I'm not sure on this at the moment...

      While (I < Count) And B Do
      Begin
        B := B And (Abs((Points^[I].X - X1) - (P.Points^[I].X - X2)) < VeryClose) And
                   (Abs((Points^[I].Y - Y1) - (P.Points^[I].Y - Y2)) < VeryClose);
        Inc(I);
      End; // While
    End;
    Result := B;
  End
  Else Result := False;
End; // PolyLinePrimitive.IsSimilarTo

procedure PolyLinePrimitive.ComputeExtent;
var i:integer;
    b:CoordRect;
    X,Y: Coord;
    Pts,Pts1 : PCoordArray;
    NumCount : Integer;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    if (points=nil) or (Count=0) then exit;

    Pts      := Points;
    NumCount := Count;

    If Style.Line = start_numeric_thickness_style Then
    Begin
      Pts1 := GetLines(Nil,NumCount);
      Pts  := GetThickLines(Pts1,NumCount,Style,False,False,IsClosed);
      FreeMem(Pts1);
    End;

    fExtent.left   := pts^[0].X;
    fExtent.right  := pts^[0].X;
    fExtent.top    := pts^[0].Y;
    fExtent.bottom := pts^[0].Y;

    for i:=1 to NumCount-1 do begin
      if not fractal then begin
        Encompass(fExtent,pts^[i].X,pts^[i].Y);
        end
      else begin
        FractalSetSeed(seed + i - 1);
        GetFractalBox(pts^[i-1].X,pts^[i-1].Y,
                      pts^[i].X,pts^[i].Y,
                      (distance(pts^[i-1].X,pts^[i-1].Y,pts^[i].X,pts^[i].Y)*roughness)/1000,
                      b);

        Encompass(fExtent, b.left,b.top);
        Encompass(fExtent, b.right,b.bottom);
        end;
      end;
    If Pts <> Points Then FreeMem(Pts);
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

function PolyLinePrimitive.GetId:char;
begin
  if not fractal then
    Result:='P'
  else
    Result:='p';
end;

procedure PolyLinePrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
var i:integer;
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);

  stream.ReadBuffer(fillcolor,sizeof(fillcolor));
  stream.ReadBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.ReadBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.ReadBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;  
  stream.ReadBuffer(Count,sizeof(Count));

  If (Base = Nil) Or Not UseAliasInfo Then
  Begin
    If Points <> Nil Then FreeMem(Points);
    GetMem(points,Count*sizeof(CoordPoint));
    for i := 0 to Count - 1 do
    begin
      stream.ReadBuffer(points^[i].X,sizeof(points^[i].X));
      stream.ReadBuffer(points^[i].Y,sizeof(points^[i].Y));
    end; // For I
  End;

  if fractal then
  begin
    stream.ReadBuffer(seed,sizeof(seed));
    stream.ReadBuffer(roughness,sizeof(roughness));
  end;
end;

procedure PolyLinePrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
var
  i   : integer;
  X,Y : Coord; // Must be Coords!!!

begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  stream.WriteBuffer(fillcolor,sizeof(fillcolor));
  stream.WriteBuffer(style.Bits,sizeof(style.Bits));
  If Full Then
  Begin
    Stream.WriteBuffer(Style.FullStyle.Thickness,SizeOf(Style.FullStyle.Thickness));
    Stream.WriteBuffer(Style.FullStyle.SThickness,SizeOf(Style.FullStyle.SThickness));
  End;
  stream.WriteBuffer(Count,sizeof(Count));
  If (Base = Nil) Or Not UseAliasInfo Then
   for i := 0 to Count - 1 do
   begin
     X := GetAdjustedX(Points^[I].X) + AddX;
     Y := GetAdjustedY(Points^[I].Y) + AddY;
     stream.WriteBuffer(X,sizeof(points^[i].X));
     stream.WriteBuffer(Y,sizeof(points^[i].Y));
   end; // For I

  if fractal then
  begin
    stream.WriteBuffer(seed,sizeof(seed));
    stream.WriteBuffer(roughness,sizeof(roughness));
  end;
end;

Procedure PolyLinePrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    FillColor := TColor(GetCardinalProperty(E,'FILLCOLOR'));
    Count     := GetIntegerProperty(E,'COUNT');
    If Base = Nil Then
    Begin
      If Points <> Nil Then FreeMem(Points);
      GetMem(Points,Count * SizeOf(CoordPoint));
    End;
    GetCoordPointsProperty(E,'POINTS',Points,Count);
    Style.Bits                 := GetCardinalProperty(E,'STYLE');
    Style.FullStyle.Thickness  := GetCoordProperty(E,'THICKNESS');
    Fractal                    := GetBooleanProperty(E,'FRACTAL');
    Style.FullStyle.SThickness := GetCoordProperty(E,'STHICKNESS');
    If Fractal Then
    Begin
      Seed      := GetIntegerProperty(E,'SEED');
      Roughness := GetIntegerProperty(E,'ROUGHNESS');
    End;
    RefreshCopies;
  End;
End; // PolyLinePrimitive.ReadFromDOMElement

Function PolyLinePrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCardinalProperty(D,'FILLCOLOR',Cardinal(FillColor)));
    E.appendChild(NewIntegerProperty(D,'COUNT',Count));
    E.appendChild(NewCoordPointsProperty(D,'POINTS',Points,Count));
    E.appendChild(NewCardinalProperty(D,'STYLE',Style.Bits));
    E.appendChild(NewCoordProperty(D,'THICKNESS',Style.FullStyle.Thickness));
    E.appendChild(NewBooleanProperty(D,'FRACTAL',Fractal));
    E.appendChild(NewCoordProperty(D,'STHICKNESS',Style.FullStyle.SThickness));
    If Fractal Then
    Begin
      E.appendChild(NewIntegerProperty(D,'SEED',Seed));
      E.appendChild(NewIntegerProperty(D,'ROUGHNESS',Roughness));
    End;
  End;
  Result := E;
End; // PolyLinePrimitive.GetAsDOMElement

function PolylinePrimitive.SetSeed(new_seed:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetSeed(new_seed);
    end
  else begin
    if seed<>new_seed then begin
      SplitOff;
      seed:=new_seed;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function PolylinePrimitive.GetSeed:integer;
begin
  if not fractal then begin
    Result:=inherited GetSeed;
    end
  else begin
    Result:=seed;
    end;
end;

function PolylinePrimitive.SetRoughness(rough:integer):boolean;
begin
  if not fractal then begin
    Result:=inherited SetRoughness(rough);
    end
  else begin
    if roughness<>rough then begin
      SplitOff;
      roughness:=rough;
      ComputeExtent;
      Result:=true;
      end
    else
      Result:=false;
    end;
end;

function  PolylinePrimitive.GetRoughness:integer;
begin
  if not fractal then begin
    Result:=inherited GetRoughness;
    end
  else begin
    Result:=roughness;
    end;
end;

{------------------------------------------------------------------------------}

constructor GroupPrimitive.CreateBlank;
begin
  inherited Create;
end;

Procedure GroupPrimitive.SetMap(M: TObject);
Var D: DrawPrimitive;
Begin
  Inherited SetMap(M);
  D := Head;
  While D <> Nil Do
  Begin
    D.SetMap(M);
    D := D.Next;
  End; // While
End; // DrawPrimitive.SetMap

Procedure GroupPrimitive.CopyFromBase(AliasOnly: Boolean);

  Procedure MakeNewChain;
  Var P,NP,NewChainHead,NewChainTail: DrawPrimitive;
  Begin
    NewChainHead := Nil;
    NewChainTail := Nil;

    P := Head;
    While P <> Nil Do
    begin
      NP := P.Copy;
      NP.SplitOff;
      NP.ComputeExtent;
      NP.Move(Alias.X - Base.Extent.Left,Alias.Y - Base.Extent.Top);
      If NewChainTail = Nil
       Then NewChainHead      := NP
       Else NewChainTail.Next := NP;
      NewChainTail := NP;
      P            := P.Next;
    End; // While
    Head := NewChainHead;
  End; // MakeNewChain

Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    Head := (Base As GroupPrimitive).Head;
  End;
  If Not AliasOnly Then MakeNewChain;
End; // GroupPrimitive.CopyFromBase

function GroupPrimitive.Copy: DrawPrimitive;
//Var G: GroupPrimitive;
{
var newchainhead,newchaintail:DrawPrimitive;
    p,np:DrawPrimitive;
}
begin
  Result := GroupPrimitive.Create(Head);
  Result.MakeCopy(Self);
{
  If Base <> Nil Then
  Begin
    G.Base := Base;
    Base.Copies.AddObject('',G);
  End
  Else
  Begin
    Copies.AddObject('',G);
    G.Base := Self;
  End;
  Result := G;
}
{
  newchainhead:=nil;
  newchaintail:=nil;

  p:=head;
  while (p<>nil) do begin
    np:=p.Copy;
    if newchaintail=nil then
      newchainhead:=np
    else
      newchaintail.Next:=np;

    newchaintail:=np;
    p:=p.Next;
    end;

  if newchainhead=nil then
    Result:=nil
  else
    Result:=GroupPrimitive.Create(newchainhead);
}
end;

constructor GroupPrimitive.Create(starting_head:DrawPrimitive);
begin
  if (starting_head=nil) then begin
    // Don't let a group created without any items whatsoever
    // This exception should always be caught, so it doesn't need to be localized.
    raise Exception.Create('Cannot create empty group.  Please contact support@gryc.ws');
    end;

  inherited Create;
  head     := starting_head;
  fExtent  := head.ChainExtent(true);
  Alias.X  := fExtent.Left;
  Alias.Y  := fExtent.Top;
end;

function GroupPrimitive.SelectClick(const within:double; p:CoordPoint):boolean;
var t:DrawPrimitive;
begin
  If PtInCoordRect(fExtent, p) Then
  Begin
    Result:=true;

    If Base <> Nil Then
    Begin
      P.X := P.X - (Alias.X - Base.fExtent.Left);
      P.Y := P.Y - (Alias.Y - Base.fExtent.Top);
    End;

    t:=head;
    while (t<>nil) do begin
      if t.SelectClick(within,p) then exit;
      t:=t.Next;
      end;
  End;
  Result:=false;
end;

Procedure GroupPrimitive.ClearThis(Deallocate: Boolean);
Var P,F: DrawPrimitive;
Begin
  If Deallocate Then
  Begin
    P := Head;
    While P <> Nil Do
    Begin
      F := P;
      P := P.Next;
      F.Free;
    End; // While
  End;
  Head := Nil;
End; // GroupPrimitive.ClearThis

Procedure GroupPrimitive.Draw(View: ViewPoint);
Var
  P : DrawPrimitive;
  V : ViewPoint;
  
Begin
  V := MakeAliasView(View);
  P := Head;
  While P <> Nil Do
  Begin
    P.Draw(V);
    P := P.Next;
  End; // While
  If V <> View Then V.Free;
End; // GroupPrimitive.Draw

function GroupPrimitive.Decompose(const View:ViewPoint; var NewChain:DrawPrimitive; testinside:boolean):boolean;
begin
  SplitOff;
  NewChain := Head;
  Head     := Nil;
  Result   := True;
end;

procedure GroupPrimitive.ComputeExtent;
Var X,Y: Coord;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else fExtent := head.ChainExtent(true);
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

procedure GroupPrimitive.Move(dx,dy:Coord);
var p:DrawPrimitive;
begin
  If Base = Nil Then
  Begin
    p := head;
    while p <> nil do
    begin
      p.Move(dx,dy);
      p := p.Next;
    end; // While
  End
  Else Inherited Move(DX,DY);
  ComputeExtent;
end;

procedure GroupPrimitive.Reverse;
var p:DrawPrimitive;
begin
  SplitOff;
  p := head;

  while p <> nil do
  begin
    p.Reverse;
    p := p.Next;
  end; // While
end;

function GroupPrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
begin
  if FindHandle(View,origx,origy) then begin
    Move(dx,dy);
    Result:=true;
    end
  else
    Result:=false;
end;

function GroupPrimitive.SetFractal(state:FractalState):boolean;
begin
  SplitOff;
  Result := head.ChainSetFractal(state, true);
  if Result then ComputeExtent;
end;

function GroupPrimitive.ApplyMatrix(var mat:Matrix):boolean;
begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;
    End;
  End;

  If Base = Nil Then Result:=head.ChainApplyMatrix(mat,true) Else Result := True;
  if Result then ComputeExtent;
end;

Function GroupPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var
  P     : GroupPrimitive;
  P1,P2 : DrawPrimitive;
  B     : Boolean;

Begin
  If D Is GroupPrimitive Then
  Begin
    P := GroupPrimitive(D);

    // Make sure the chains are the same size and that they are equal

    If (Base = Nil) And (P.Base = Nil) Then
    Begin
      B  := True;
      P1 := Head;
      P2 := P.Head;
      While (P1 <> Nil) And (P2 <> Nil) And B Do
      Begin
        B  := B And P1.IsSimilarTo(P2);
        P1 := P1.Next;
        P2 := P2.Next;
      End; // While
      B := B And (P1 = Nil) And (P2 = Nil);
      Result := B;  // Don't check inherited here
    End
    Else Result := (Base = P);
  End
  Else Result := False;
End; // GroupPrimitive.IsSimilarTo

function GroupPrimitive.SetStyle(new_style:StyleAttrib):boolean;
begin
  SplitOff;
  Result:=head.ChainSetStyle(new_style,true);
end;

function GroupPrimitive.GetStyle:StyleAttrib;
begin
  Result:=head.ChainGetStyle(true);
end;

function GroupPrimitive.SetColor(color:TColor):boolean;
begin
  SplitOff;
  Result:=head.ChainSetColor(color,true);
end;

function GroupPrimitive.GetColor:TColor;
begin
  Result:=head.ChainGetColor(true);
end;

function GroupPrimitive.SetFillColor(color:TColor):boolean;
begin
  SplitOff;
  Result:=head.ChainSetFillColor(color,true);
end;

function GroupPrimitive.GetFillColor:TColor;
begin
  Result:=head.ChainGetFillColor(true);
end;

function GroupPrimitive.SetOverlay(overlay:byte):boolean;
begin
  SplitOff;
  fOverlay := overlay;
  Result:=head.ChainSetOverlay(overlay,true);
end;

function GroupPrimitive.GetOverlay:integer;
begin
  Result:=fOverlay;
//  Result:=head.ChainGetOverlay(true);
end;

function GroupPrimitive.SetSeed(new_seed:integer):boolean;
begin
  SplitOff;
  Result:=head.ChainSetSeed(new_seed,true);
  if Result then ComputeExtent;
end;

function GroupPrimitive.GetSeed:integer;
begin
  Result := head.ChainGetSeed(true);
end;

function GroupPrimitive.SetRoughness(rough:integer):boolean;
begin
  SplitOff;
  Result:=head.ChainSetRoughness(rough,true);
  if Result then ComputeExtent;
end;

function GroupPrimitive.GetRoughness:integer;
begin
  Result := head.ChainGetRoughness(true);
end;

function GroupPrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  SplitOff;
  Result:=head.ChainSetTextAttrib(view, attrib,true);
  if Result then ComputeExtent;
end;

function GroupPrimitive.GetTextAttrib:TextAttrib;
begin
  Result:=head.ChainGetTextAttrib(true);
end;

function GroupPrimitive.GetId:char;
begin
  Result:='G';
end;

Procedure GroupPrimitive.AddToBaseOrCopies(DoChain: Boolean);
Var D: DrawPrimitive;
Begin
  Inherited AddToBaseOrCopies(DoChain);

  // If the entire group has become an alias, then its primitive chain
  // is really the primitive chain of its base object, and therefore
  // shouldn't be checked.

  If (Base = Nil) And DoChain Then
  Begin
    D := Head;
    While D <> Nil Do
    Begin
      D.AddToBaseOrCopies(DoChain);
      D := D.Next;
    End; // While
  End;
End; // GroupPrimitive.AddToBaseOrCopies

procedure GroupPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  If (Base = Nil) Or Not Full
   Then head := ReadChain(stream, version, false, Full, UseAliasInfo, MapC);
end;

procedure GroupPrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  If (Base = Nil) Or Not UseAliasInfo Then
  Begin
    If Base <> Nil Then
    Begin
      AddX := AddX + (Alias.X - Base.fExtent.Left);
      AddY := AddY + (Alias.Y - Base.fExtent.Top);
    End;
    head.WriteChain(stream, true,Full,UseAliasInfo,AddX,AddY);
  End;
end;

Procedure GroupPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    Head := ReadChainFromDOMElement(E,Version,False, MapC);
    ComputeExtent;
    RefreshCopies;
  End;
End; // GroupPrimitive.ReadFromDOMElement

Function GroupPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then E.appendChild(Head.GetChainAsDOMElement(D,True,Undo));
  Result := E;
End; // GroupPrimitive.GetAsDOMElement

{------------------------------------------------------------------------------}

Procedure BitmapPrimitive.CopyFromBase(AliasOnly: Boolean);
Var BP: BitmapPrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    BP := Base As BitmapPrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    If AliasOnly Then Image := BP.Image
    Else
    Begin
      Image := TBitmap.Create;
      Image.Assign(BP.Image);
    End;
    Corners        := BP.Corners;
    Corners.Left   := Corners.Left  + (Alias.X - Base.fExtent.Left);
    Corners.Right  := Corners.Right + (Alias.X - Base.fExtent.Left);
    Corners.Top    := Corners.Right + (Alias.Y - Base.fExtent.Top);
    Corners.Bottom := Corners.Top   + (Alias.Y - Base.fExtent.Top);
  End;
End; // BitmapPrimitive.CopyFromBase

function BitmapPrimitive.Copy:DrawPrimitive;
//var newimage:TBitmap;
begin
{
  if image<>nil then begin
    newimage := TBitmap.Create;

    newimage.Assign(image);
    end
  else
    newimage := nil;
}
  Result:=BitmapPrimitive.Create(image, corners);
  Result.MakeCopy(Self);
//  Result.CopyCore(Self);
//  result.fExtent := fExtent;
end;

constructor BitmapPrimitive.CreateBlank;
begin
  inherited Create;
  image := nil;
end;

constructor BitmapPrimitive.Create(const start:TBitmap; InitialRect:CoordRect);
begin
  inherited Create;
  image := start;
  corners := InitialRect;
  ComputeExtent;
end;

Procedure BitmapPrimitive.ClearThis(Deallocate: Boolean);
Begin
  If (Image <> Nil) And Deallocate Then
  Begin
    Image.Free;
    Image := Nil;
  End;
End; // BitmapPrimitive.ClearThis

procedure BitmapPrimitive.Draw(View:ViewPoint);
var rect:TRect;
    ScreenCoord:CoordRect;
    V : ViewPoint;
    
begin
  V := MakeAliasView(View);
  V.GetCoordinateRect(ScreenCoord);

  if (image<>nil) and
     (V.OffScreenFullDetail or VisibleWithin(fExtent,ScreenCoord)) then begin
    rect := V.CoordToScreenRect(corners);
    V.Canvas.StretchDraw(rect,image);
    end;
  If V <> View Then V.Free;  
end;

function BitmapPrimitive.ApplyMatrix(var mat:Matrix):boolean;
begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;
    End;
  End;

  MultiplyPointByMatrix(corners.left,corners.top, mat);
  MultiplyPointByMatrix(corners.right,corners.bottom, mat);
  ComputeExtent;
  result:=true;
end;

Function BitmapPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
// Comparing bitmaps for equality would take a prohibitively long time if they
// were actually equal (or at least of equal dimensions), since we would have
// to compare all the pixels.  So forget it--they're all different, just like
// they always were.
Begin
  Result := False;
End; // BitmapPrimitive.IsSimilarTo

procedure BitmapPrimitive.ComputeExtent;
Var X,Y: Coord;
begin
  If Base <> Nil Then
  Begin
    X              := Alias.X - Base.fExtent.Left;
    Y              := Alias.Y - Base.fExtent.Top;
    fExtent        := Base.fExtent;
    fExtent.Left   := fExtent.Left   + X;
    fExtent.Top    := fExtent.Top    + Y;
    fExtent.Right  := fExtent.Right  + X;
    fExtent.Bottom := fExtent.Bottom + Y;
  End
  Else
  Begin
    fExtent := corners;
    CorrectCoordRect(fExtent);
  End;
  Alias.X := Extent.Left;
  Alias.Y := Extent.Top;
end;

function BitmapPrimitive.GetId:char;
begin
  Result:='B';
end;

procedure BitmapPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
var n:integer;
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(corners,sizeof(corners));
  If (Base = Nil) Or Not UseAliasInfo Then
  Begin
    stream.ReadBuffer(n,sizeof(n));            // Don't care about size: read and throw away
    image.Free;
    image := TBitmap.Create;
    image.LoadFromStream(stream);
  End;
  SetFillColor(fColor);
end;

procedure BitmapPrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
var sizeOfs,lastOfs:integer;
    n:integer;
begin
  fColor := GetFillColor;
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  stream.WriteBuffer(corners,sizeof(corners));

  If (Base = Nil) Or Not UseAliasInfo Then
  Begin
    // Write a placeholder so we can compute the size of the binary image.
    sizeOfs := stream.Position;
    n:=0;
    stream.WriteBuffer(n,sizeof(n));

    image.SaveToStream(stream);

    // Compute the size of the bitmap in the stream, go back, and patch in
    // the correct size.
    lastOfs := stream.Position;
    n:=lastOfs - sizeOfs;
    stream.Seek(sizeOfs,soFromBeginning);
    stream.WriteBuffer(n,sizeof(n));
    stream.Seek(lastOfs,soFromBeginning);
  End;
end;

Procedure BitmapPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Var
  M     : TMemoryStream;
  I,J   : Cardinal;
  St    : String;
  P1,P2 : PChar;

Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    Corners := GetCoordRectProperty(E,'CORNERS');
    St      := Trim(GetStringProperty(E,'IMAGE'));
    I       := Length(St) + 4; // Making sure it fits :)
    J       := MimeDecodedSize(Length(St));
    GetMem(P1,I);
    GetMem(P2,J);
    StrPCopy(P1,St);
    MimeDecode(P1^,Length(St),P2^);
    FreeMem(P1,I);
    M := TMemoryStream.Create;
    M.WriteBuffer(P2^,J);
    M.Position := 0;
    Image.Free;
    Image := TBitmap.Create;
    Image.LoadFromStream(M);
    SetFillColor(fColor);
    M.Free;
    FreeMem(P2,J);
    RefreshCopies;
  End;
End; // BitmapPrimitive.ReadFromDOMElement

Function BitmapPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var
  E      : TDOMElement;
  M      : TMemoryStream;
  I,J    : Cardinal;
  Buffer : Pointer;

Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordRectProperty(D,'CORNERS',Corners));
    M := TMemoryStream.Create;
    Image.SaveToStream(M);
    I := M.Position;
    M.Position := 0;
    J := MimeEncodedSize(I);
    GetMem(Buffer,J + 1);
    FillChar(Buffer^,J + 1,0);
    MimeEncodeNoCRLF(M.memory^,I,Buffer^);
    E.appendChild(NewStringProperty(D,'IMAGE',StrPas(Buffer)));
    FreeMem(Buffer,J);
    M.Free;
  End;
  Result := E;
End; // BitmapPrimitive.GetAsDOMElement

function BitmapPrimitive.MoveHandle(const View:ViewPoint; var mode:HandleMode; origx,origy,dx,dy:Coord):boolean;
begin
  Result:=false;

  if FindHandle(View,corners.left,corners.top) then begin
    corners.Left :=corners.left+dx;
    corners.top := corners.top+dy;
    Result:=true;
    end;
  if FindHandle(View,corners.Right,corners.top) then begin
    corners.right :=corners.right+dx;
    corners.top := corners.top+dy;
    Result:=true;
    end;
  if FindHandle(View,corners.left,corners.bottom) then begin
    corners.Left :=corners.left+dx;
    corners.bottom := corners.bottom+dy;
    Result:=true;
    end;
  if FindHandle(View,corners.right,corners.bottom) then begin
    corners.right :=corners.right+dx;
    corners.bottom := corners.bottom+dy;
    Result:=true;
    end;

  if Result=true then ComputeExtent;
end;

procedure BitmapPrimitive.Move(dx,dy:Coord);
begin
  inherited Move(dx,dy);
  corners.left  := corners.left + dx;
  corners.right := corners.right + dx;
  corners.top   := corners.top + dy;
  corners.bottom:= corners.bottom + dy;
end;

function BitmapPrimitive.SelectClick(const within:double; p:CoordPoint):boolean;
begin
  Result:=PtInCoordRect(fExtent, p);
end;

function BitmapPrimitive.SetFillColor(color:TColor):boolean;
begin
  SplitOff;
  if (color=clNone) then begin
    image.Transparent:=false;
    end
  else begin
    image.Transparent:=true;
    image.TransparentColor:=color;
  end;
  Result:=true;
end;

function BitmapPrimitive.GetFillColor:TColor;
begin
  if not image.Transparent then
     Result := clNone
  else
     Result := image.TransparentColor;
end;

{------------------------------------------------------------------------------}

Function HyperlinkPrimitive.GetAdjustedX1: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := Alias.X//(Base As HyperlinkPrimitive).X1 + (Alias.X - (Base As HyperlinkPrimitive).X1)
   Else Result := X1;
End; // HyperlinkPrimitive.GetAdjustedX1

Function HyperlinkPrimitive.GetAdjustedY1: Coord;
// This is needed because aliases actually have their own copy of discrete
// parameters and they aren't reliable.
Begin
  If Base <> Nil
   Then Result := Alias.Y//(Base As HyperlinkPrimitive).Y1 + (Alias.Y - (Base As HyperlinkPrimitive).Y1)
   Else Result := Y1;
End; // HyperlinkPrimitive.GetAdjustedY1

Procedure HyperlinkPrimitive.CopyFromBase(AliasOnly: Boolean);
Var HP: HyperlinkPrimitive;
Begin
  If Base <> Nil Then
  Begin
    Inherited CopyFromBase(AliasOnly);
    HP    := Base As HyperlinkPrimitive; // The "as" operator incurs a performance hit so let's minimize its use
    Flags := HP.Flags;
    Text  := HP.Text;
//    X1    := (Base As HyperlinkPrimitive).X1;
//    Y1    := (Base As HyperlinkPrimitive).Y1;
    X1    := Alias.X;//X1 + (Alias.X - Base.fExtent.Left);
    Y1    := Alias.Y;//Y1 + (Alias.Y - Base.fExtent.Top);
  End;
End; // HyperlinkPrimitive.CopyFromBase

function HyperlinkPrimitive.Copy: DrawPrimitive;
begin
  Result := HyperlinkPrimitive.Create(GetAdjustedX1, GetAdjustedY1, text, flags);
  Result.MakeCopy(Self);
end;

constructor HyperlinkPrimitive.CreateBlank;
begin
  inherited Create;
  flags := [];
  text  := '';
  X1    := 0;
  Y1    := 0;
end;

constructor HyperlinkPrimitive.Create(const x,y:Coord; str:string; flag:THyperlinkFlags);
begin
  Inherited Create;
  flags := flag;
  text  := str;
  x1    := x;
  y1    := y;
  ComputeExtent;
end;

procedure HyperlinkPrimitive.Draw(View:ViewPoint);
var sx1,sy1:integer;
    IsPrinter:boolean;
    CanDraw:boolean;

begin
  IsPrinter := GetDeviceCaps(View.Canvas.Handle,TECHNOLOGY) <> DT_RASDISPLAY;

  View.CoordToScreen(GetAdjustedX1,GetAdjustedY1,sx1,sy1);

  sx1 := sx1 - HyperlinkBullet.Width  div 2;
  sy1 := sy1 - HyperlinkBullet.Height div 2;

  // Do we draw the hyperlink?
  CanDraw := true;

  // Not if hidden
  if hyperHidden in flags then CanDraw := false;

  // Not if we don't want them printed out
  if IsPrinter and Settings.NoPrintedHyperlinks.Checked then CanDraw:=false;

  if CanDraw then View.Canvas.Draw(sx1, sy1, HyperlinkBullet);

  // Unlike any other "normal" object, this one is actually a point
  // object, but always has the same *screen* size.  Therefore, each
  // time we draw it, we recompute the extent.  This allows us to appropriately
  // draw our selection handles at the edges of the fixed size bullet,
  // regardless of how far in/out we're zoomed.
  View.ScreenToCoord(sx1,sy1, fExtent.Left,fExtent.Top);
  View.ScreenToCoord(sx1+HyperlinkBullet.Width, sy1+HyperlinkBullet.Height,
                     fExtent.Right,fExtent.Bottom);
end;

function HyperlinkPrimitive.ApplyMatrix(var mat:Matrix):boolean;
Var XX1,YY1: Coord;
begin
  If Not IsPureOffsetMatrix(Mat) Then SplitOff
  Else
  Begin
    If Base <> Nil Then
    Begin
      Alias.X := Alias.X + Mat[3,1];
      Alias.Y := Alias.Y + Mat[3,2];
      ComputeExtent;
      Result := True;
      Exit;      
    End;
  End;
  XX1 := GetAdjustedX1;
  YY1 := GetAdjustedY1;
  MultiplyPointByMatrix(XX1,YY1, mat);
  X1 := XX1;
  Y1 := YY1;
  ComputeExtent;
  result := true;
end;

Function HyperlinkPrimitive.IsSimilarTo(D: DrawPrimitive): Boolean;
Var P: HyperlinkPrimitive;
Begin
  If D Is HyperlinkPrimitive Then
  Begin
    P := HyperlinkPrimitive(D);
    Result := (Text = P.Text)   And
              (Flags = P.Flags) And Inherited IsSimilarTo(D);
  End
  Else Result := False;
End; // HyperlinkPrimitive.IsSimilarTo

procedure HyperlinkPrimitive.Move(dx,dy: Coord);
begin
  If Base = Nil Then
  Begin
    SplitOff; // Not 100% sure I need to do this--but it doesn't really hurt
    x1 := x1 + dx;
    y1 := y1 + dy;
  End;
  inherited Move(dx,dy);
end;

procedure HyperlinkPrimitive.ComputeExtent;
begin
  // This isn't totally a waste of time: if there are any calculations made on the
  // hyperlink's extent after it is modified, but before it has been drawn, this will
  // give us something to work with.  Note, however, that since the extent size is
  // dependent on the screen, we will typically overwrite this when we draw.
  Extent  := MakeCoordRect(GetAdjustedX1 - HyperlinkBullet.Width  Div 2,
                           GetAdjustedY1 - HyperlinkBullet.Height Div 2,
                           GetAdjustedX1 + HyperlinkBullet.Width  Div 2,
                           GetAdjustedY1 + HyperlinkBullet.Height Div 2);
  Alias.X := Extent.Left + HyperlinkBullet.Width  Div 2;
  Alias.Y := Extent.Top  + HyperlinkBullet.Height Div 2;
end;

function HyperlinkPrimitive.SelectClick(const within:double; p:CoordPoint):boolean;
begin
  Result := (Distance(GetAdjustedX1,GetAdjustedY1,p.x,p.y) < within);
end;

function HyperlinkPrimitive.GetId:char;
begin
  Result:='H';
end;

procedure HyperlinkPrimitive.DoRead(stream: TStream; version: integer; Full,UseAliasInfo: Boolean);
begin
  inherited DoRead(stream, version, Full,UseAliasInfo);
  stream.ReadBuffer(x1,sizeof(x1));
  stream.ReadBuffer(y1,sizeof(y1));
  Text := ReadStringFromStream(stream);
  stream.ReadBuffer(flags, sizeof(flags));
end;

procedure HyperlinkPrimitive.Write(stream:TStream; Full,UseAliasInfo: Boolean; AddX,AddY: Coord);
// See DrawPrimitive.WriteChain() for an explanation of Full and UseAliasInfo
Var X,Y: Coord; // Must be Coords!!!
begin
  inherited Write(stream,Full,UseAliasInfo,AddX,AddY);
  X := GetAdjustedX(X1) + AddX;
  Y := GetAdjustedY(Y1) + AddY;
  stream.WriteBuffer(X,sizeof(x1));
  stream.WriteBuffer(Y,sizeof(y1));
  WriteStringToStream(stream,Text);
  stream.WriteBuffer(flags, sizeof(flags));
end;

Procedure HyperlinkPrimitive.ReadFromDOMElement(E: TDOMElement; Version: Integer);
Begin
  If Not ReadBaseFromDOMElement(E) Then
  Begin
    Inherited ReadFromDOMElement(E,Version);
    X1    := GetCoordProperty(E,'X1');
    Y1    := GetCoordProperty(E,'Y1');
    Text  := MimeDecodeString(Trim(GetStringProperty(E,'TEXT')));
    Flags := [];
    If GetBooleanProperty(E,'EXECUTE') Then Flags := Flags + [hyperExecute];
    If GetBooleanProperty(E,'HIDDEN')  Then Flags := Flags + [hyperHidden];
    RefreshCopies;
  End;
End; // HyperlinkPrimitive.ReadFromDOMElement

Function HyperlinkPrimitive.GetAsDOMElement(D: TDOMDocument; Undo: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := Inherited GetAsDOMElement(D,Undo);
  If Not GetAsAliasDOMElement(D,E) Then
  Begin
    E.appendChild(NewCoordProperty(D,'X1',X1));
    E.appendChild(NewCoordProperty(D,'Y1',Y1));
    E.appendChild(NewStringProperty(D,'TEXT',MimeEncodeString(Text)));
    E.appendChild(NewBooleanProperty(D,'EXECUTE',hyperExecute In Flags));
    E.appendChild(NewBooleanProperty(D,'HIDDEN',hyperHidden In Flags));
  End;
  Result := E;
End; // HyperlinkPrimitive.GetAsDOMElement

function HyperlinkPrimitive.GetTextAttrib:TextAttrib;
begin
  Result.Valid := [tatHyperlinkText,tatHyperlinkFlags];
  Result.HyperlinkText := text;
  Result.HyperlinkFlags := flags;
end;

function HyperlinkPrimitive.SetTextAttrib(const View:ViewPoint; const attrib:TextAttrib):boolean;
begin
  Result:=false;
  if tatHyperlinkText in attrib.Valid then begin
    SplitOff;
    text:=attrib.HyperlinkText;
    Result:=true;
    end;
  if tatHyperlinkFlags in attrib.Valid then begin
    If Not Result Then SplitOff;
    flags := attrib.HyperlinkFlags;
    Result:=true;
    end;
end;                   

function HyperlinkPrimitive.FindHyperlink(const View:ViewPoint; x,y:Coord; var hypertext:string; var hyperflags:THyperlinkFlags):boolean;
begin
  // Assume we've drawn (and therefore set the extent) before this
  // function has been called.
  if PtInCoordRect(Extent, MakeCoordPoint(x,y)) then
  begin
    hypertext     := text;
    hyperflags    := flags;
    FindHyperlink := true;
  end
  else FindHyperlink := false;
end;

{------------------------------------------------------------------------------}

initialization
  FillPalette:=CreateFreshPalette;
  try
    HyperlinkBullet := TBitmap.Create;
    HyperlinkBullet.LoadFromFile(ParamStr(0) + '\..\HyperlinkBullet.bmp');
    HyperlinkBullet.TransparentMode := tmAuto;
    HyperlinkBullet.Transparent := true;
  except
    MessageBox(0,PChar(res_BulletNotFoundText),PChar(res_BulletNotFoundCaption),MB_OK);
  end;

finalization
  DeleteObject(FillPalette);
  HyperlinkBullet.Free;
end.
