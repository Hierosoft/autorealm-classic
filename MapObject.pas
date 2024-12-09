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
unit MapObject;

interface

uses SysUtils, imagechecklistbox, Classes, Graphics, DrawLines, Forms, Primitives, Clipbrd,
  MatrixMath, Geometry, StdCtrls, LocalizedStrings, DOM
  ;
{ ImageCheckListBox, jpeg, DIMime }

const // AutoREALM pre-release versions (prior to 1.0)
      // Oldest supported file format
      MAP_VERSION_3 = $00000003;

      // AutoREALM versions 1.0 through 1.03
      // * Addition of LA (Landscape) chunk
      MAP_VERSION_4 = $00000004;

      // AutoREALM versions 1.04 through 1.19
      //  * Addition of Bitmap object type
      //  * Addition of hyperlink object type
      MAP_VERSION_5 = $00000005;

      // AutoREALM versions 1.20 and beyond...
      //  * Addition of UUID to symbols in AuS file.
      //  * Creation of user symbol object (symbols were previously just grouped objects).
      MAP_VERSION_6 = $00000006;

      CURRENT_MAP_VERSION = MAP_VERSION_6;
      MapVersion = CURRENT_MAP_VERSION;

type

  ModType = (modGrid, modBkColor, modUnitType, modUnitScale, modOverlay,
    modOverlayState, modViewport, modViews,
    modComments, modSettings, modPrinter,
    modDeleted, modAdded, modChanged, modSymbols, modPushPins);

  ChunkType = (CO_CHUNK, // Color Chunk: background and grid colors
    CM_CHUNK, // Comment Chunk: Map/symbol comments
    OV_CHUNK, // Overlay Chunk: overlays in the map
    GR_CHUNK, // Grid Chunk: grid/snap settings
    VW_CHUNK, // View Chunk: Viewpoints
    PP_CHUNK, // Pushpin Chunk: Push pins
    OB_CHUNK, // Object chunk: Actual map objects
    EO_CHUNK, // End chunk: End of map file
    LA_CHUNK, // Landscape chunk: printer settings for landscape/portrait
    SE_CHUNK, // Selected chunk: Saved selection states
    PH_CHUNK, // Pushpin History
    UNDO_OPERATION // Undo : Not a real chunk, but saves any addition info used in creating undo records
    );

  AlignType = (NO_CHANGE, LEFT_TOP, CENTERS, RIGHT_BOTTOM,
    SLANTFORWARD, SLANTBACKWARD, SCATTER, JOG,
    SPACE_EQUALLY, STACK);

  OrderType = (Order_None, Order_LEFT_TOP, Order_RIGHT_TOP, Order_LEFT_BOTTOM, Order_RIGHT_BOTTOM,
               Order_CENTER, Order_EDGE, Order_RANDOM);

  // If any new enums are added, the XML routines MUST be changed as well!             
  PushPinFlagType = (PP_WaypointsVisible, PP_ShowNumber, PP_ShowNote);

  PushPinFlagSet = set of PushPinFlagType;

  ChunkSet = set of ChunkType;

  IdSet = set of char;

  UndoRecord = record
    Name: string;
    Stream: TMemoryStream;
//    Doc : TDOMDocument;
    modflags: set of ModType;
  end;

  PushPinPoint = record
    note:string;
    point:CoordPoint;
    end;

  PushPinInformation = record
    Name: string;
    HistoryCount:integer;
    History:array of PushPinPoint;
  end;

  MapCollection = class
  private
    head, tail: DrawPrimitive;
    Parent: TForm;
    modflags: set of ModType;
    ViewList: TList;
    fLandscape: boolean;
    supresscount: integer;
    MapComments: string;
    fReadOnly:boolean;

    { Speed enhancements to panning }
    UsePanRects: boolean;
    PanRect1, PanRect2: TRect;

    { Undo information }
    UndoMax: integer;
    CurrentUndo: integer;
    UndoArray: array of UndoRecord;

    { Pushpin information }
    pushpin:array of PushPinInformation;
    sPushpinflags:PushPinFlagSet;

    procedure SetUndoMax(n: integer);
    procedure RemovePreUndo;
    procedure RecreateMapState(number: integer);
    procedure SetLandscape(b: boolean);
    procedure WriteSelectStates(stream: TStream);
    procedure ReadSelectStates(stream: TStream; newhead: DrawPrimitive);

    procedure SupressRedraw;
    procedure RestoreRedraw;
    function GetSelectedObjectList:TList;

    function GetPushPinName(index:integer):string;
    procedure SetPushPinName(index:integer; s:string);
    function GetPushPinHistoryCount(index:integer):integer;
    function GetPushPinHistoryPoint(index:integer; history:integer):CoordPoint;
    procedure SetPushPinHistoryPoint(index:integer; history:integer; pt:CoordPoint);
    function GetPushPinPoint(index:integer):CoordPoint;
    procedure SetPushPinPoint(index:integer; pt:CoordPoint);
    function GetPushPinHistoryNote(index:integer; history:integer):string;
    procedure SetPushPinHistoryNote(index:integer; history:integer; s:string);
    function GetPushPinAnnotation(index:integer; history:integer):string;
    procedure SetPushPinFlags(ps:PushPinFlagSet);
    function GetPushPinText:string;
    procedure SetPushPinText(s:string);

    procedure SetReadOnly(b:boolean);
    function  ReadOnlyPrevents:boolean;

  protected
    Function  GetOverlaysAsDOMElement(D: TDOMDocument): TDOMElement;
    Procedure LoadOverlaysFromDOMElement(E: TDOMElement);
    procedure SaveOverlaysToStream(stream: TStream);
    procedure LoadOverlaysFromStream(stream: TStream);

    procedure SplitSelectedChain(var mainhead, maintail: DrawPrimitive;
      var chainhead, chaintail: DrawPrimitive);

    procedure GetFollowingSelection(var insertat:DrawPrimitive);
    procedure GetTwoPriorSelection(var insertat:DrawPrimitive);
    Procedure SetMapChain(D: DrawPrimitive);

  public
    CurrentView: ViewPoint;
    BasePrimitives : TStringList;

    function AnythingSelected: boolean;

    property ReadOnly:boolean read fReadOnly write SetReadOnly;
    property Landscape: boolean read fLandscape write SetLandscape;
    procedure SynchronizeOverlayList(list: TImageCheckListBox);
    function GetViewPoints: integer;
    function GetViewPoint(index: integer): ViewPoint;
    function GetCurrentViewPoint: ViewPoint;
    procedure SetCurrentViewPoint(const view: ViewPoint);
    function FindViewPoint(const name: string): integer;
    procedure DeleteViewPoint(index: integer);
    function SaveViewPoint(const view: ViewPoint; name: string): integer;
    procedure RestoreLastView;
    procedure RemoveAllViews;

    procedure Clear;
    function IsEmpty: boolean;

    function Modified: boolean;
    procedure ClearModified; overload;
    procedure ClearModified(mtype: ModType); overload;
    procedure SetModified(mtype: ModType);
    function ModifiedDescription: string;

    procedure Invalidate;
    procedure InvalidateRect(ext: TRect; erase: boolean);
    procedure InvalidateSelect(erase: boolean);
    procedure Grid(const View: Viewpoint);
    procedure Draw(const View: Viewpoint; showhandles: boolean);
    function OverlaysInUse: OverlaySet;
    procedure SelectAll;
    procedure ShowAll(var view: ViewPoint; zoomfactor: Coord);
    procedure ClearSelection(redraw: boolean = true);
    procedure CenterSelection(rescale: boolean = false);
    procedure StartAdding(name: string);
    procedure EndAdding;
    procedure Pan(dx, dy: integer);

    { function GetMetafile(all: boolean): TMetaFile; }
    function GetBitmap(all: boolean; viewonly:boolean; width,height:integer): TBitmap;

    function FindScalpelPoint(x, y: Coord; var p: DrawPrimitive; var index: integer): boolean;
    function FindClosestPoint(dx, dy: Coord; var point: CoordPoint; allowselected: boolean): boolean;
    function FindClosestPointOn(dx, dy: Coord; var point: CoordPoint; Var Angle: Coord; allowselected: boolean): boolean;
    Function FindClosestIntersection(dx, dy: Coord; var point: CoordPoint): Boolean;
    procedure SliceAlong(S1,S2: CoordPoint);
    Procedure SliceAtPoint(S: CoordPoint);
    procedure ReverseSelected(styleonly:boolean);
    function  SetFractalStateSelected(state:FractalState):boolean;    
    procedure SelectClosestObject(dx, dy: Coord);
    function SelectClickedObject(dx, dy: Coord): integer;
    function Select(rect: CoordRect): integer;
    procedure SelectFromPoint(x, y: Coord; AcceptableTypes: IdSet);
    procedure IterateSelect(rect: CoordRect);
    procedure MoveSelection(dx, dy: Coord);
    procedure ApplyMatrix(apply: Matrix);
    function CenterMatrix(mat: Matrix): Matrix;
    procedure RotateSelection(degrees: double);
    procedure ScaleSelection(xfactor, yfactor: double);
    procedure SkewSelection(sx, sy: double);
    procedure FlipSelection(xaxis, yaxis: boolean);
    procedure AlignSelection(h, v, wd, hg: AlignType);
    procedure OrderSelection(order: OrderType);
    procedure CreateArray(hc, vc: integer; hs, vs: Coord; hbetween, vbetween: boolean;
                          Ellipse,Rotate: Boolean; ex,ey: Coord);

    function MoveHandle(var mode: HandleMode; origx, origy, dx, dy: Coord): boolean;
    function FindHandle(x, y: Coord; all: boolean = false): DrawPrimitive;
    function FindHyperlink(x, y: Coord; var hypertext:string; var hyperflags:THyperlinkFlags): boolean;
    function FindEndPoint(var x, y: Coord; ignoreObject: DrawPrimitive; var p: DrawPrimitive): boolean;
    function FindPointOn(var x, y, Angle: Coord; ignoreObject: DrawPrimitive; var p: DrawPrimitive): boolean;
    procedure AddObject(obj: DrawPrimitive);
    function Extent(view: ViewPoint; all: boolean): TRect;
    function ExtentFloat(view: ViewPoint; all: boolean): CoordRect; 
    function CoordExtent(all:boolean): CoordRect;
    procedure RefreshExtent;

    function SetStyle(style: StyleAttrib): boolean;
    function GetStyle: StyleAttrib;
    function SetSeed(seed: integer): boolean;
    function GetSeed: integer;
    function SetColor(color: TColor): boolean;
    function GetColor: TColor;
    function SetFillColor(color: TColor): boolean;
    function GetFillColor: TColor;
    function SetOverlay(overlay: byte): boolean;
    function GetOverlay: integer;
    function SetRoughness(rough: integer): boolean;
    function GetRoughness: integer;
    function SetTextAttrib(const View: Viewpoint; const attrib: TextAttrib): boolean;
    function GetTextAttrib: TextAttrib;

    procedure RemoveOverlay(overlay, replacement: integer; delovl: boolean);

    procedure Delete;
    procedure Group;
    procedure UnGroup;
    procedure Decompose;

    procedure DeleteSelected;
    procedure GroupSelected;
    procedure UnGroupSelected;
    procedure DecomposeSelected;
    procedure CloseSelectedFigures;
    procedure DecomposeForPrintout(MapView: ViewPoint);

    procedure SendToBack;
    procedure BringToFront;
    procedure SendBackward;
    procedure BringForward;

    constructor Create(ParentForm: TForm); virtual;
    destructor Destroy; override;
    function GetSelectedObjects(ParentForm: TForm): MapCollection;

    Procedure ReadFromDOMDocument(D: TDOMDocument; Selected: Boolean; Version: Integer = CURRENT_MAP_VERSION; Insert: Boolean = False);
    Function GetAsDOMDocument(All: Boolean; SaveSet: ChunkSet = []): TDOMDocument;
    Procedure ReadFromDOMElement(E: TDOMElement; Selected: Boolean; Version: Integer = CURRENT_MAP_VERSION; Insert: Boolean = False);
    Function GetAsDOMElement(D: TDOMDocument; All: Boolean; SaveSet: ChunkSet = []): TDOMElement;
    procedure Write(stream: TStream; all,Full,UseAliasInfo: boolean; saveset: ChunkSet = []);
    procedure Read(stream: TStream; selected,Full,UseAliasInfo: boolean; version:integer=CURRENT_MAP_VERSION; insert:boolean = false);
    { function ReadMetafile(Meta: TMetaFile; selected: boolean): boolean; }
    function InsertMap(filename: string): boolean;
    { function InsertMetafile(filename: string): boolean; }
    function InsertBitmap(filename: string): boolean;
    function InsertJPEG(filename: string): boolean;

    function CopyContents(FullCopy: Boolean = False): DrawPrimitive;
    procedure RestoreContents(d:DrawPrimitive);

    procedure Cut;
    procedure Copy;
    procedure Paste;

    procedure SetUndoPoint(name: string; compress: boolean = false);
    function Undo: boolean;
    function Redo: boolean;
    function CurrentUndoName: string;
    function CurrentRedoName: string;
    property UndoLevels: integer read UndoMax write SetUndoMax;

    property Comments: string read MapComments write MapComments;

    property PushPinFlags:PushPinFlagSet read sPushpinflags write SetPushPinFlags;
    property PushPinText:string read GetPushPinText write SetPushPinText;
    property PushPinName[index:integer]:string read GetPushPinName write SetPushPinName;
    property PushPinPoint[index:integer]:CoordPoint read GetPushPinPoint write SetPushPinPoint;
    property PushPinHistoryCount[index:integer]:integer read GetPushPinHistoryCount;
    property PushPinHistoryPoint[index:integer; history:integer]:CoordPoint read GetPushPinHistoryPoint write SetPushPinHistoryPoint;
    property PushPinHistoryNote[index:integer; history:integer]:string read GetPushPinHistoryNote write SetPushPinHistoryNote;
    property PushPinAnnotation[index:integer; history:integer]:string read GetPushPinAnnotation;
    procedure PushPinHistoryPointAdd(index:integer; pt:CoordPoint; note:string);
    procedure PushPinHistoryClear(index:integer);
    function  PushPinPlaced(index:integer):boolean;
    function PushPinCount:integer;
    Procedure DisplaySelectedSize;
    Procedure CleanupBaseList;
    Property First: DrawPrimitive Read Head;
  end;

var
  Map: MapCollection;
  LastViewName: string = res_mapobj_view_sav;

Function  NewIntegerProperty(D: TDOMDocument; ElementName: String; I: Integer): TDOMElement;
Function  NewCardinalProperty(D: TDOMDocument; ElementName: String; I: Cardinal): TDOMElement;
Function  NewStringProperty(D: TDOMDocument; ElementName,St: String): TDOMElement;
Function  NewBooleanProperty(D: TDOMDocument; ElementName: String; B: Boolean): TDOMElement;
Function  NewDoubleProperty(D: TDOMDocument; ElementName: String; C: Double): TDOMElement;
Function  NewCoordProperty(D: TDOMDocument; ElementName: String; C: Coord): TDOMElement;
Function  NewCoordPointProperty(D: TDOMDocument; ElementName: String; C: CoordPoint): TDOMElement;
Function  NewCoordPointsProperty(D: TDOMDocument; ElementName: String; P: PCoordArray; Count: Integer): TDOMElement;
Function  NewCoordRectProperty(D: TDOMDocument; ElementName: String; C: CoordRect): TDOMElement;
Function  GetIntegerProperty(E: TDOMElement; ElementName: String): Integer;
Function  GetCardinalProperty(E: TDOMElement; ElementName: String): Cardinal;
Function  GetStringProperty(E: TDOMElement; ElementName: String): String;
Function  GetBooleanProperty(E: TDOMElement; ElementName: String): Boolean;
Function  GetDoubleProperty(E: TDOMElement; ElementName: String): Double;
Function  GetCoordProperty(E: TDOMElement; ElementName: String): Coord; Overload;
Function  GetCoordProperty(E: TDOMElement): Coord; Overload;
Function  GetCoordPointProperty(E: TDOMElement; ElementName: String): CoordPoint; Overload;
Function  GetCoordPointProperty(E: TDOMElement): CoordPoint; Overload;
Procedure GetCoordPointsProperty(E: TDOMElement; ElementName: String; P: PCoordArray; Count: Integer);
Function  GetCoordRectProperty(E: TDOMElement; ElementName: String): CoordRect;

implementation

uses Main, SettingsDialog, Math, GraphGrid, MapSettings, Dialogs, StreamUtil, Menus, XMLUnit;

const
  selborder = 3;

  // Use instead of TEnhMetaRecord to cast the pointers into so we don't
  // get warnings about access to invalid subrange (dParm[0..0]).
type
  ENHMETARECORD = packed record
    iType: DWORD; { Record type EMR_XXX}
    nSize: DWORD; { Record size in bytes}
    dParm: array[0..32767] of DWORD; { Parameters}
  end;

  MetaGDIObject = record
    Color: TColor;
    Brush: boolean;
    style: StyleAttrib;
  end;

  PMapCollection = ^MapCollection;
  MetaGDIObjectArray = array[0..32767] of MetaGDIObject;
  PMetaGDIObjectArray = ^MetaGDIObjectArray;

  MetaEnumExtra = record
    HandleCount: integer;
    HandleObj: PMetaGDIObjectArray;
    foreground, background: TColor;
    currx, curry: Coord;
    style: StyleAttrib;
    PMap: PMapCollection;
    TryBitmap: boolean;
  end;

  PMetaEnumExtra = ^MetaEnumExtra;

  HandleTable = packed array[0..32767] of HGDIOBJ;


var
  MapFormat: Word;

  {-----------------------------------------------------------------}

Function MyFloatToStr(E: Extended): String;
Begin
  Result := Format('%2.4f',[E]);
  // 2003-04-13 Messie: Format uses the regionalized windows variable
  // DecimalSeperator as the decimal seperator. We always want a . as
  // DecimalSeparator, so we need a workaround.
  if( DecimalSeparator <>  '.') then Result[Pos(DecimalSeparator,Result)] := '.';
End; // MyFloatToStr

Function NewIntegerProperty(D: TDOMDocument; ElementName: String; I: Integer): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(IntToStr(I)));
  Result := E;
End; // NewIntegerProperty

Function NewCardinalProperty(D: TDOMDocument; ElementName: String; I: Cardinal): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(IntToStr(I)));
  Result := E;
End; // NewCardinalProperty

Function NewStringProperty(D: TDOMDocument; ElementName,St: String): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(St));
  Result := E;
End; // NewStringProperty

Function NewBooleanProperty(D: TDOMDocument; ElementName: String; B: Boolean): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(BoolToStr(B,False)));
  Result := E;
End; // NewBooleanProperty

Function NewDoubleProperty(D: TDOMDocument; ElementName: String; C: Double): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(MyFloatToStr(C)));
  Result := E;
End; // NewDoubleProperty

Function NewCoordProperty(D: TDOMDocument; ElementName: String; C: Coord): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(MyFloatToStr(C)));
  Result := E;
End; // NewCoordProperty

Function NewCoordPointProperty(D: TDOMDocument; ElementName: String; C: CoordPoint): TDOMElement;
Var E: TDOMElement;
Begin
  E := D.createElement(ElementName);
  E.appendChild(D.createTextNode(MyFloatToStr(C.X) + ',' + MyFloatToStr(C.Y)));
  Result := E;
End; // NewCoordPointProperty

Function NewCoordPointsProperty(D: TDOMDocument; ElementName: String; P: PCoordArray; Count: Integer): TDOMElement;
Var
  E  : TDOMElement;
  St : String;
  I  : Integer;

Begin
  E  := D.createElement(ElementName);
  St := '';
  For I := 0 To Count - 1 Do
  Begin
    If St <> '' Then St := St + ':';
    St := St + MyFloatToStr(P^[I].X) + ',' + MyFloatToStr(P^[I].Y)
  End; // For I
  E.appendChild(D.createTextNode(St));
  Result := E;
End; // NewCoordPointsProperty

Function NewCoordRectProperty(D: TDOMDocument; ElementName: String; C: CoordRect): TDOMElement;
Var E: TDOMElement;
Begin
  E  := D.createElement(ElementName);
  E.appendChild(D.createTextNode(MyFloatToStr(C.Left) + ',' +
                                 MyFloatToStr(C.Top)  + ',' +
                                 MyFloatToStr(C.Right) + ',' +
                                 MyFloatToStr(C.Bottom)));
  Result := E;
End; // NewCoordRectProperty

Function GetIntegerProperty(E: TDOMElement; ElementName: String): Integer;
Var
  E1,E2 : TDOMNode;
  I,J   : Integer;

Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      Val(Trim((E2 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End
  Else
  Begin
    // 2003/05/21 - J.Friant
    // Elements with only one value (such as <ID>1</ID>)
    // fail on getFirstChildElement, apparently because you
    // can (should?) access the firstChild property directly.
    E1 := E.firstChild;
    If (E1 <> Nil) And (E1 Is TDOMText) Then
    Begin
      Val(Trim((E1 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End;
End; // GetIntegerProperty

Function GetCardinalProperty(E: TDOMElement; ElementName: String): Cardinal;
Var
  E1,E2 : TDOMNode;
  I     : Cardinal;
  J     : Integer;

Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      Val(Trim((E2 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End
  Else Result := 0;
End; // GetCardinalProperty

Function GetStringProperty(E: TDOMElement; ElementName: String): String;
Var E1,E2: TDOMNode;
Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText)
     Then Result := (E2 As TDOMText).data
     Else Result := '';
  End
  Else Result := '';
End; // GetStringProperty

Function GetBooleanProperty(E: TDOMElement; ElementName: String): Boolean;
Var E1,E2: TDOMNode;
Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText)
     Then Result := StrToBool(Trim((E2 As TDOMText).data))
     Else Result := False;
  End
  Else Result := False;
End; // GetBooleanProperty

Function GetDoubleProperty(E: TDOMElement; ElementName: String): Double;
Var
  E1,E2 : TDOMNode;
  I     : Double;
  J     : Integer;

Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      Val(Trim((E2 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End
  Else Result := 0;
End; // GetDoubleProperty

Function GetCoordProperty(E: TDOMElement; ElementName: String): Coord;
Var
  E1,E2 : TDOMNode;
  I     : Coord;
  J     : Integer;

Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      Val(Trim((E2 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End
  Else Result := 0;
End; // GetCoordProperty

Function GetCoordProperty(E: TDOMElement): Coord;
Var
  E2 : TDOMNode;
  I  : Coord;
  J  : Integer;

Begin
  If E <> Nil Then
  Begin
    E2 := E.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      Val(Trim((E2 As TDOMText).data),I,J);
      If J = 0 Then Result := I Else Result := 0;
    End
    Else Result := 0;
  End
  Else Result := 0;
End; // GetCoordProperty

Function GetCoordPointProperty(E: TDOMElement; ElementName: String): CoordPoint;
Var
  E1,E2 : TDOMNode;
  I     : Coord;
  J,K   : Integer;
  P     : CoordPoint;
  St    : String;

Begin
  P.X := 0;
  P.Y := 0;
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      St := Trim((E2 As TDOMText).data);
      K  := Pos(',',St);
      Val(Trim(Copy(St,1,K - 1)),I,J);
      If J = 0 Then P.X := I;
      Val(Trim(Copy(St,K + 1,Length(St))),I,J);
      If J = 0 Then P.Y := I;
    End;
  End;
  Result := P;
End; // GetCoordPointProperty

Function GetCoordPointProperty(E: TDOMElement): CoordPoint;
Var
  E2  : TDOMNode;
  I   : Coord;
  J,K : Integer;
  P   : CoordPoint;
  St  : String;

Begin
  P.X := 0;
  P.Y := 0;
  If E <> Nil Then
  Begin
    E2 := E.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      St := Trim((E2 As TDOMText).data);
      K  := Pos(',',St);
      Val(Trim(Copy(St,1,K - 1)),I,J);
      If J = 0 Then P.X := I;
      Val(Trim(Copy(St,K + 1,Length(St))),I,J);
      If J = 0 Then P.Y := I;
    End;
  End;
  Result := P;
End; // GetCoordPointProperty

Procedure GetCoordPointsProperty(E: TDOMElement; ElementName: String; P: PCoordArray; Count: Integer);
Var
  E1,E2   : TDOMNode;
  I       : Coord;
  J,K,L,M : Integer;
  St      : String;
  Start   : Integer;

  Function MyPos(Const Ch: Char; Const St: String; Start: Integer): Integer;
  Begin
    While (Start <= Length(St)) And (St[Start] <> Ch) Do Inc(Start);
    If Start > Length(St) Then Result := 0 Else Result := Start;
  End; // MyPos

Begin
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      St    := Trim((E2 As TDOMText).data);
      Start := 1;
      For L := 0 To Count - 1 Do
      Begin
        K := MyPos(',',St,Start);
        If K > 0 Then
        Begin
          Val(Trim(Copy(St,Start,K - Start)),I,J);
          If J = 0 Then P^[L].X := I;
          Start := K + 1;
          M := MyPos(':',St,Start);
          If M > 0
           Then Val(Trim(Copy(St,Start,M - Start)),I,J)
           Else Val(Trim(Copy(St,Start,Length(St))),I,J);
          If J = 0 Then P^[L].Y := I;
          If M > 0 Then Start := M + 1 Else Start := Length(St) + 1;
        End
        Else Start := Length(St) + 1;
      End; // For L
    End;
  End;
End; // GetCoordPointsProperty

Function GetCoordRectProperty(E: TDOMElement; ElementName: String): CoordRect;
Var
  E1,E2 : TDOMNode;
  I     : Coord;
  J,K   : Integer;
  P     : CoordRect;
  St    : String;

Begin
  P.Left   := 0;
  P.Top    := 0;
  P.Right  := 0;
  P.Bottom := 0;
  E1 := E.getFirstChildElement(ElementName);
  If E1 <> Nil Then
  Begin
    E2 := E1.firstChild;
    If (E2 <> Nil) And (E2 Is TDOMText) Then
    Begin
      St := Trim((E2 As TDOMText).data);
      K  := Pos(',',St);
      Val(Trim(Copy(St,1,K - 1)),I,J);
      If J = 0 Then P.Left := I;

      St := Trim(Copy(St,K + 1,Length(St)));
      K  := Pos(',',St);
      Val(Trim(Copy(St,1,K - 1)),I,J);
      If J = 0 Then P.Top := I;

      St := Trim(Copy(St,K + 1,Length(St)));
      K  := Pos(',',St);
      Val(Trim(Copy(St,1,K - 1)),I,J);
      If J = 0 Then P.Right := I;

      Val(Trim(Copy(St,K + 1,Length(St))),I,J);
      If J = 0 Then P.Bottom := I;
    End;
  End;
  Result := P;
End; // GetCoordRectProperty

function Compare(a, b: Coord): integer;
begin
  if (a < b) then
    Result := -1
  else
    if (a > b) then
      Result := 1
    else
      Result := 0;
end;

function LeftPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Extent.Left, DrawPrimitive(a2).Extent.Left);
end;

function RightPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Extent.Right, DrawPrimitive(a2).Extent.Right);
end;

function HCenterPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare((DrawPrimitive(a1).Extent.Left + DrawPrimitive(a1).Extent.Right) / 2,
    (DrawPrimitive(a2).Extent.Left + DrawPrimitive(a2).Extent.Right) / 2);
end;

function WidthPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Width, DrawPrimitive(a2).Width);
end;

function TopPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Extent.Top, DrawPrimitive(a2).Extent.Top);
end;

function BottomPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Extent.Bottom, DrawPrimitive(a2).Extent.Bottom);
end;

function VCenterPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare((DrawPrimitive(a1).Extent.Top + DrawPrimitive(a1).Extent.Bottom) / 2,
    (DrawPrimitive(a2).Extent.Top + DrawPrimitive(a2).Extent.Bottom) / 2);
end;

function HeightPredicate(a1, a2: Pointer): integer;
begin
  Result := Compare(DrawPrimitive(a1).Height, DrawPrimitive(a2).Height);
end;

function UpperLeftPredicate(a1, a2: Pointer): integer;
begin
  Result := -Compare(DrawPrimitive(a1).Extent.Bottom, DrawPrimitive(a2).Extent.Bottom);

  if (Result=0) then begin
     Result := -Compare(DrawPrimitive(a1).Extent.Left, DrawPrimitive(a2).Extent.Left);
     end;
end;

function UpperRightPredicate(a1, a2: Pointer): integer;
begin
  Result := -Compare(DrawPrimitive(a1).Extent.Bottom, DrawPrimitive(a2).Extent.Bottom);

  if (Result=0) then begin
     Result := Compare(DrawPrimitive(a1).Extent.Left, DrawPrimitive(a2).Extent.Left);
     end;
end;

function LowerLeftPredicate(a1, a2: Pointer): integer;
begin
  Result := -UpperRightPredicate(a1,a2);
end;

function LowerRightPredicate(a1, a2: Pointer): integer;
begin
  Result := -UpperLeftPredicate(a1,a2);
end;

function CenterPredicate(a1, a2: Pointer): integer;
var d1,d2:Coord;
begin
  d1:=Distance(DrawPrimitive(a1).Extent.Left, DrawPrimitive(a1).Extent.Bottom, 0, 0);
  d2:=Distance(DrawPrimitive(a2).Extent.Left, DrawPrimitive(a2).Extent.Bottom, 0, 0);
  Result := -Compare(d1,d2);
end;

function EdgePredicate(a1, a2: Pointer): integer;
begin
  Result := -CenterPredicate(a2,a2);
end;

function RandomPredicate(a1, a2: Pointer): integer;
const modulus = 5.0;
var d1,d2:Coord;
begin
  d1:= DrawPrimitive(a1).Extent.Bottom;
  d2:= DrawPrimitive(a2).Extent.Bottom;

  // Remove the higher significant portion of the number so we just use the
  // insignificant portion to sort by
  d1 := d1 - int(d1/modulus)*modulus;
  d2 := d2 - int(d2/modulus)*modulus;

  Result := Compare(d1, d2);
end;

{-----------------------------------------------------------------}

function  MapCollection.ReadOnlyPrevents:boolean;
begin
  if ReadOnly then begin
    MainForm.FlashReadOnlyProhibits;
    Result:=true;
    end
  else
    Result:=false;
end;

procedure MapCollection.RemovePreUndo;
var
  i: integer;
begin
  { If they've undone to a certain point, we need to remove
    the beginning of the undo array, since they can't redo
    after the first non-undo action. }
  if (CurrentUndo <> 0) then
    begin
      for i := 0 to CurrentUndo - 1 do
        begin
//          MainForm.DOMImpl.freeDocument(UndoArray[I].Doc);
//          MainForm.DOMImpl.freeUnusedASModels;
//          UndoArray[I].Doc := Nil;
          UndoArray[i].Stream.Free;
          UndoArray[i].Stream := nil;
          UndoArray[i].Name := '';
        end;

      { Now that we've freed the beginning, slide the rest back down }
      for i := CurrentUndo to High(UndoArray) do
        UndoArray[i - CurrentUndo] := UndoArray[i];
      { And clear out all the left-over slots }
      for i := High(UndoArray) - CurrentUndo to High(UndoArray) do
        begin
          UndoArray[i].Stream := nil;
//          UndoArray[I].Doc  := Nil;
          UndoArray[i].Name := '';
        end;

      CurrentUndo := 0;
    end;
end;

procedure MapCollection.SetUndoPoint(name: string; compress: boolean);
var
  i: integer;
begin
  // If we're compressing undo records together, and we are using at least 1 level
  // of undo, and it is an undo for the same item, nothing to do--quit.
  if compress and (UndoMax > 0) and (UndoArray[CurrentUndo].Name = name) then exit;

  { If undos aren't disabled }
  if (UndoMax > 0) then
    begin
      RemovePreUndo;

      { Now free the oldest undo record, and shuffle the rest of the array down }

      UndoArray[High(UndoArray)].Stream.Free;
      UndoArray[High(UndoArray)].Stream := nil;
      UndoArray[High(UndoArray)].Name := '';
      for i := High(UndoArray) downto Low(UndoArray) + 1 do
        UndoArray[i] := UndoArray[i - 1];

      { Create a memory stream to write into as the undo record}
      try
        UndoArray[0].Stream := TMemoryStream.Create;
        Write(UndoArray[0].Stream, true, True, True,
          [CO_CHUNK, CM_CHUNK, OV_CHUNK, VW_CHUNK, OB_CHUNK, UNDO_OPERATION]);
        UndoArray[0].Name := name;
        UndoArray[0].modflags := modflags;
      except
        { Not enough memory for an undo record !}
        UndoArray[0].Name := res_mapobj_undo_fail;
        UndoArray[0].modflags := [];
      end;
    end;
end;

procedure MapCollection.RecreateMapState(number: integer);
var
  i: integer;
begin
  Clear;
  UndoArray[number].Stream.Position := 0;
  Read(UndoArray[number].Stream, false, True, True);
  modflags := UndoArray[number].modflags;

  // Backwards synchronize; use the overlay checklist box to
  // restore the current view's active and visible overlays.
  with MainForm.OverlayList do
    begin
      CurrentView.ActiveOverlays  := [];
      CurrentView.VisibleOverlays := [];

      for i := 0 to Items.Count - 1 do
        begin
          case State[i] of
            cbChecked:
              begin
                CurrentView.ActiveOverlays  := CurrentView.ActiveOverlays + [i];
                CurrentView.VisibleOverlays := CurrentView.VisibleOverlays + [i];
              end;
            cbGrayed:
              begin
                CurrentView.VisibleOverlays := CurrentView.VisibleOverlays + [i];
              end;
            cbUnchecked:
              begin
              end;
          end;
        end;
    end;

end;

function MapCollection.Undo: boolean;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if (CurrentUndo < UndoMax) and Assigned(UndoArray[CurrentUndo].Stream{Doc}) then
    begin
      if (CurrentUndo = 0) then
        begin
          SetUndoPoint(res_mapobj_undo_restore);
          inc(CurrentUndo);
        end;

      Result := true;
      RecreateMapState(CurrentUndo);

      inc(CurrentUndo);
    end;
end;

function MapCollection.Redo: boolean;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if (CurrentUndo >= 2) then
    begin
      dec(CurrentUndo);
      if Assigned(UndoArray[CurrentUndo - 1].Stream{Doc}) then
        begin
          Result := true;
          RecreateMapState(CurrentUndo - 1);

          if (CurrentUndo = 1) then RemovePreUndo;
        end;
    end;
end;

function MapCollection.CurrentUndoName: string;
begin
  if (UndoMax = 0) or (CurrentUndo = UndoMax) then
    Result := ''
  else
    Result := UndoArray[CurrentUndo].Name;
end;

function MapCollection.CurrentRedoName: string;
begin
  if (UndoMax = 0) or (CurrentUndo = 0) then
    Result := ''
  else
    Result := UndoArray[CurrentUndo - 1].Name;
end;

procedure MapCollection.SetUndoMax(n: integer);
var
  i: integer;
begin
  if (n = UndoMax) then exit;

  { Don't let them be in the middle of a sequence of undo
    operations when they change the undo level, or we might
    find ourselves in the strange position of being at an
    undo level that we've already released. }
  RemovePreUndo;

  { If they're making it smaller, free any streams past the end
    that we no longer need }
  for i := n to High(UndoArray) do
    begin
//      MainForm.DOMImpl.freeDocument(UndoArray[I].Doc);
//      MainForm.DOMImpl.freeUnusedASModels;
//      UndoArray[I].Doc := Nil;
      UndoArray[i].Stream.Free;
      UndoArray[i].Stream := nil;
      UndoArray[i].Name := '';
    end;

  // Add 1 to account for the temp stream created by an initial Undo
  SetLength(UndoArray, n + 1);
  UndoMax := n;
end;

{ --------------------------------------------------------------- }

procedure MapCollection.SetLandscape(b: boolean);
begin
  if ReadOnlyPrevents then exit;

  if (b <> fLandscape) then
    begin
      fLandscape := b;
      modFlags := modFlags + [modPrinter];
    end;
end;

function MapCollection.GetViewPoints: integer;
begin
  Result := ViewList.Count;
end;

function MapCollection.GetViewPoint(index: integer): ViewPoint;
begin
  if (index < 0) or (index >= ViewList.Count) then
    Result := nil
  else
    Result := ViewPoint(ViewList.Items[index]);
end;

function MapCollection.FindViewPoint(const name: string): integer;
var
  i, index: integer;
  view: ViewPoint;
begin
  index := -1;
  for i := 0 to ViewList.Count - 1 do
    begin
      view := ViewPoint(ViewList.Items[i]);
      if (view.Name = name) then
        begin
          index := i;
          break;
        end;
    end;

  Result := index;
end;

procedure MapCollection.DeleteViewPoint(index: integer);
var
  view: ViewPoint;
begin
  if ReadOnlyPrevents then exit;

  view := ViewPoint(ViewList.Items[index]);
  view.Free;
  ViewList.Delete(index);
end;

procedure MapCollection.RemoveAllViews;
begin
  if ReadOnlyPrevents then exit;

  while (ViewList.Count > 0) do
    DeleteViewPoint(0);
end;

procedure MapCollection.RestoreLastView;
var
  index: integer;
begin
  index := FindViewPoint('');
  if (index >= 0) then
    SetCurrentViewPoint(GetViewPoint(index))
  else
    ShowAll(CurrentView, 0.5);
end;

procedure MapCollection.SynchronizeOverlayList(list: TImageCheckListBox);
var
  i: integer;
  active, visible: boolean;
begin
  for i := 0 to list.Items.Count - 1 do
    begin
      active := i in CurrentView.ActiveOverlays;
      visible := i in CurrentView.VisibleOverlays;

      if active and visible then
        list.State[i] := cbChecked
      else
        if visible then
          list.State[i] := cbGrayed
        else
          list.State[i] := cbUnchecked;

    end;
end;

function MapCollection.SaveViewPoint(const view: ViewPoint; name: string): integer;
var
  NewView: ViewPoint;
  index: integer;
begin
  Result:=0;

  if ReadOnlyPrevents then exit;

  index := FindViewPoint(name);
  if (index <> -1) then DeleteViewPoint(index);

  NewView := ViewPoint.Create(view);
  NewView.Name := Name;
  Result := ViewList.Add(Pointer(NewView));
end;

function MapCollection.GetCurrentViewPoint: ViewPoint;
begin
  Result := CurrentView;
end;

procedure MapCollection.SetCurrentViewPoint(const view: ViewPoint);
var
  NewView: ViewPoint;
begin
  NewView := ViewPoint.Create(view);
  NewView.Canvas := CurrentView.Canvas;
  CurrentView.Free;
  CurrentView := NewView;

  // Synchronize the screen controls...
  MainForm.BoldUnitCount.Value := NewView.Grid.GridBoldUnits;
  // Set the grid type we loaded
  case NewView.Grid.GridType of
    gtNone:
      begin
        MainForm.NoGraphBtn.Down := true;
      end;
    gtSquare:
      begin
        MainForm.SquareGraphBtn.Down := true;
      end;
    gtHex:
      begin
        MainForm.HexGraphBtn.Down := true;
      end;
    gtTriangle:
      begin
        MainForm.TriangleGraphBtn.Down := true;
      end;
  end;

  MainForm.GridSizeBar.Position := trunc(NewView.Grid.CurrentGridSize / UnitsPerGridTick);
  MainForm.UpdateGridLabel;
  SynchronizeOverlayList(MainForm.OverlayList);
end;

{ --------------------------------------------------------------- }


function MapCollection.Modified: boolean;
begin
  Result := (ModFlags <> []);
end;

procedure MapCollection.ClearModified;
begin
  ModFlags := [];
end;

procedure MapCollection.SetModified(mtype: ModType);
begin
  ModFlags := ModFlags + [mtype];
end;

procedure MapCollection.ClearModified(mtype: ModType);
begin
  ModFlags := ModFlags - [mtype];
end;

function MapCollection.ModifiedDescription: string;
begin
  Result := '';
  if modDeleted in ModFlags then Result := Result + res_mapobj_o_del + #13#10;
  if modAdded in ModFlags then Result := Result + res_mapobj_o_add + #13#10;
  if modChanged in ModFlags then Result := Result + res_mapobj_o_chd + #13#10;
  if modSymbols in ModFlags then Result := Result + res_mapobj_o_symbols + #13#10;
  if modOverlay in ModFlags then Result := Result + res_mapobj_o_overlays + #13#10;
  if modComments in ModFlags then Result := Result + res_mapobj_o_comments + #13#10;
  if modSettings in ModFlags then Result := Result + res_mapobj_o_map + #13#10;
  if modGrid in ModFlags then Result := Result + res_mapobj_o_grid + #13#10;
  if modBkColor in ModFlags then Result := Result + res_mapobj_o_backgd + #13#10;
  if modUnitType in ModFlags then Result := Result + res_mapobj_o_units + #13#10;
  if modUnitScale in ModFlags then Result := Result + res_mapobj_o_map2 + #13#10;
  if modViews in ModFlags then Result := Result + res_mapobj_o_view + #13#10;
  if modOverlayState in ModFlags then Result := Result + res_mapobj_o_overlays2 + #13#10;
  if modPrinter in ModFlags then Result := Result + res_mapobj_o_print + #13#10;
  if modViewport in ModFlags then Result := Result + res_mapobj_o_viewport + #13#10;
  if modPushPins in ModFlags then Result := Result + res_mapobj_o_pushpin + #13#10;
end;

function MapCollection.IsEmpty: boolean;
begin
  Result := (head = nil);
end;

procedure MapCollection.WriteSelectStates(stream: TStream);
var
  p: DrawPrimitive;
  b: boolean;
begin
  p := head;

  while (p <> nil) do
    begin
      b := p.IsSelected;
      stream.Write(b, sizeof(b));
      p := p.Next;
    end;
end;

procedure MapCollection.ReadSelectStates(stream: TStream; newhead: DrawPrimitive);
var
  p: DrawPrimitive;
  b: boolean;
begin
  p := newhead;

  while (p <> nil) do
    begin
      stream.Read(b, sizeof(b));
      p.Select(nil, b);
      p := p.Next;
    end;
  DisplaySelectedSize;
end;

function MapCollection.OverlaysInUse: OverlaySet;
var
  p: DrawPrimitive;
begin
  Result := [];

  p := head;

  while (p <> nil) do
    begin
      Result := Result + [p.GetOverlay];
      p := p.Next;
    end;
end;

procedure MapCollection.Grid(const View: Viewpoint);
begin
  View.Grid.DrawGrid(View);
end;

procedure MapCollection.Draw(const View: Viewpoint; showhandles: boolean);
var
  p: DrawPrimitive;
  clipextent, additionalclipextent: CoordRect;
  draw_grid: boolean;
  grid_on_top: boolean;
  grid_pos_cnt: cardinal;
begin
  // 2003/05/29 - J.Friant
  // The grid is now drawn based on the GridPosition order
  // as it compares to the list of Primitives in the
  // MapCollection.  Since each primitive is on it's own
  // "layer" we count the number of them that are drawn
  // to determine when to draw the grid.
  //
  // ** NOTE **
  // For now files saved under the old format only have 255
  // positions available.  So if you want to draw more
  // primitives than that below the grid then you'll need
  // to save your file in the XML format.
  //
  // The checkbox to draw the grid on top of all objects
  // still works like it used to.
  draw_grid := true;
  grid_on_top := (View.Grid.GridFlags and gfGridOnTop) = gfGridOnTop;
  grid_pos_cnt := 0;

  // If we're panning, then draw the grid underneath before we draw
  // the objects.  Otherwise, we lose visibility of objects that
  // used to have a fill, and the lines fall on the grid.
  // Dungeon layouts are especially prone to this.
  if (View.QuickDraw <> QuickDraw_None) then
  begin
    Grid(View);
    draw_grid := false;
    grid_on_top := false;
  end;

  p := head;

  { If panning, don't use the ClipRect, because it's the whole screen
    for a diagonal pan.  Instead, rely on the info set up by the
    panning routine }
  if UsePanRects then
  begin
    clipextent           := View.ScreenToCoordRect(PanRect1);
    additionalclipextent := View.ScreenToCoordRect(PanRect2);
  end
  else clipextent := View.ScreenToCoordRect(View.Canvas.ClipRect);

  StartNewHandleDraw;

  while p <> nil do
  begin
    if not grid_on_top then  // 2003/05/29 - J.Friant
    begin
      if ( grid_pos_cnt >= View.Grid.getGridPosition )
       and draw_grid then
      begin
        Grid(View);
        draw_grid := false;
      end
      else
        grid_pos_cnt := grid_pos_cnt + 1;
    end;

    if p.IsInOverlay(View.VisibleOverlays) then
    begin
      if p.OnScreen(clipextent) or
        (UsePanRects and p.OnScreen(additionalclipextent)) then
      begin
        p.Draw(View);

        if showhandles then
        begin
          if Settings.VisualOverlays.Checked and Settings.OverlayIcons.Checked then
          begin
            p.DrawOverlayHandles(View);
          end;
          if p.IsSelected then p.DrawHandles(View);
        end;

      end;
    end;
    p := p.Next;
  end; // While

  { Clear the one-shot PanRects }
  UsePanRects := false;

  // 2003/05/29 - J.Friant
  // If we haven't drawn the grid yet, then do it now
  if draw_grid then
    Grid(View);
end;

procedure MapCollection.SelectAll;
var
  p: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  p := head;

  while (p <> nil) do
    begin
      if p.IsInOverlay(CurrentView.ActiveOverlays) then
        begin
          p.Select(CurrentView, true);
        end;
      p := p.Next;
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.Invalidate;
begin
  if (supresscount <> 0) then exit;
  if Assigned(Parent) and (Parent is TMainForm) then
    (Parent as TMainForm).DoInvalidate;
end;

procedure MapCollection.Pan(dx, dy: integer);
var
  cx, cy: Coord;
  clientrect: TRect;
  ScreenCoord: CoordRect;
begin
  CurrentView.DeltaScreenToCoord(dx, dy, cx, cy);

  CurrentView.GetCoordinateRect(ScreenCoord);
  ScreenCoord.Left := ScreenCoord.Left - cx;
  ScreenCoord.Right := ScreenCoord.Right - cx;
  ScreenCoord.Top := ScreenCoord.Top - cy;
  ScreenCoord.Bottom := ScreenCoord.Bottom - cy;
  CurrentView.SetCoordinateRect(ScreenCoord);

  clientrect := MainForm.GetVisibleRect;
  inc(clientrect.Left, MainForm.Left);
  clientrect.Right := clientrect.Right + MainForm.Left - MainForm.VertScrollBar.Width + 3;

  inc(clientrect.Top, MainForm.Top);
  clientrect.Bottom := clientrect.Bottom + MainForm.Top - MainForm.HorzScrollBar.Height + 3;

  ScrollWindow(MainForm.Handle, dx, dy, @clientrect, @clientrect);

  { Enhancement to panning; if we are scrolling both horizontally and
    vertically, then don't use ClipRect, since it will contain the
    entire picture, and we'll end up drawing way too much.  Instead,
    compute the rects for the edges, and only draw within those. }
  if (dx <> 0) and (dy <> 0) then
    begin
      UsePanRects := true;
      PanRect1 := MainForm.GetVisibleRect;
      PanRect2 := PanRect1;
      if (dx < 0) then
        PanRect1.Left := PanRect1.Right + dx { Scrolling left }
      else
        PanRect1.Right := PanRect1.Left + dx; { Scrolling right }

      if (dy < 0) then
        PanRect2.Top := PanRect2.Bottom + dy { Scrolling up }
      else
        PanRect2.Bottom := PanRect2.Top + dy; { Scrolling down }
    end;

  UpdateWindow(MainForm.Handle);
  SetModified(modViewport);
end;

procedure MapCollection.InvalidateRect(ext: TRect; erase: boolean);
var
  border: integer;
begin
  if (supresscount <> 0) then exit;

  if (ext.Right - ext.Left = 0) or (ext.Bottom - ext.Top = 0) then exit;

  // Increase rectangle by 1 in each direction to make sure that the
  // edge specified is invalidated too (stupid Windows rectangle logic)
  //
  // The 15 is for the widest possible line style.
  // We need at least 5 to include the red select extent handles.
  border := 15 + 1;

  // Make sure to include possible overlay icons if necessary.
  if Settings.VisualOverlays.Checked and Settings.OverlayIcons.Checked then
    begin
      border := border + MainForm.OverlayImages.Width;
    end;

  dec(ext.Left, border);
  inc(ext.Right, border);
  dec(ext.Top, border);
  inc(ext.Bottom, border);

  if Assigned(Parent) then
    begin
      if (Parent is TMainForm) then (Parent as TMainForm).HideCrosshair;
      Windows.InvalidateRect(Parent.Handle, @ext, erase);
    end;
end;

procedure MapCollection.CloseSelectedFigures;
var
  p     : DrawPrimitive;
  style : StyleAttrib;

begin
  if ReadOnlyPrevents then exit;

  InvalidateRect(Extent(CurrentView, false), true);

  style.bits := $FFFFFFFF;
  style.Fill := MainForm.FillPattern.Pattern;
  Style.FullStyle.Thickness := -1;

  p := head;
  while p <> nil do
  begin
    if p.IsSelected and not p.IsClosed then
    begin
      p.CloseFigure;
      p.SetFillColor(MainForm.FillColor.color); // Update the fill color
      p.SetStyle(style);                        // and style.
      SetModified(modChanged);
    end;
    p := p.Next;
  end; // While

  // Silly to think, but closing the figure can expand the
  // selection extent if we're closing a fractal.
  
  InvalidateRect(Extent(CurrentView, false), true);
end; // MapCollection.CloseSelectedFigures

procedure MapCollection.ReverseSelected(styleonly:boolean);
var
  p: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  InvalidateRect(Extent(CurrentView, false), true);

  p := head;
  while (p <> nil) do
    begin
      if p.IsSelected then
      begin
        if styleonly
         then p.SetStyle(InvertLineStyle(p.GetStyle))
         else p.Reverse;
      end;
      p := p.Next;
    end;

  // For everything *except* fractals, the pre- and post-reverse
  // lines will have the same extent.  Fractals will not.
  InvalidateRect(Extent(CurrentView, false), true);
end;

procedure MapCollection.InvalidateSelect(erase: boolean);
var
  p: DrawPrimitive;
begin
  if (supresscount <> 0) then exit;

  InvalidateRect(Extent(CurrentView, false), erase);

  p := head;
  while (p <> nil) do
    begin
      if p.IsSelected then p.InvalidateHandles(Parent.Handle, CurrentView);
      p := p.Next;
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.ClearSelection(redraw: boolean);
var
  p: DrawPrimitive;
  ext: TRect;
begin
  if redraw then
    begin
      ext := Extent(CurrentView, false);
      inc(ext.Right);
      inc(ext.Bottom);
    end;

  p := head;

  while (p <> nil) do
    begin
 {{ 2003-01-07, uoirej

       // Passing not-null view to DrawPrimitive.Select causes redundant drawing
       // because we invalidate the rect at the end of this procedure.

      if redraw then
        begin
          if p.IsSelected then p.InvalidateHandles(Parent.Handle, CurrentView);
          p.Select(CurrentView, false);
        end
      else
        p.Select(nil, false);

      p := p.Next;

 }
         if redraw then
           begin
             if p.IsSelected then p.InvalidateHandles(Parent.Handle, CurrentView);
           end;

         p.Select(nil, false);
         p := p.Next;
   // }} 2003-01-07, uoirej
    end;

  if redraw then
    begin
      InvalidateRect(ext, true);
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.Delete;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
  begin
    SetUndoPoint(res_mapobj_undo_del);
    DeleteSelected;
  end;
  DisplaySelectedSize;
end;

procedure MapCollection.DeleteSelected;
var
  f, p: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);

  p := head;
  tail := nil;

  while (p <> nil) do
    begin
      if p.IsSelected then
        begin
          SetModified(modDeleted);
          if (tail = nil) then
            head := p.Next
          else
            tail.Next := p.Next;

          f := p;
          p := p.Next;
          f.Free;
        end
      else
        begin
          tail := p;
          p := p.Next;
        end;
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.RemoveOverlay(overlay, replacement: integer; delovl: boolean);
var
  p: DrawPrimitive;
  view: ViewPoint;
  i: integer;

  procedure SlideOverlays(var view: ViewPoint);
  var
    os: OverlaySet;
    j: integer;
  begin
    os := [];
    for j := 0 to overlay - 1 do
      if j in view.ActiveOverlays then os := os + [j];
    for j := overlay to 255 do
      if j in view.ActiveOverlays then os := os + [j - 1];
    view.ActiveOverlays := os;

    os := [];
    for j := 0 to overlay - 1 do
      if j in view.VisibleOverlays then os := os + [j];
    for j := overlay to 255 do
      if j in view.VisibleOverlays then os := os + [j - 1];
    view.VisibleOverlays := os;
  end;

begin
  if ReadOnlyPrevents then exit;

  SetUndoPoint(res_mapobj_undo_del_overlay);

  InvalidateSelect(true);
  ClearSelection;
  p := head;

  { Perform the replacement and delete if necessary }
  while (p <> nil) do
    begin
      if (p.GetOverlay = overlay) then
        begin
          if delovl then
            p.Select(CurrentView, true)
          else
            begin
              SetModified(modChanged);
              p.SetOverlay(replacement);
            end;
        end;

      p := p.Next;
    end;

  DeleteSelected;

  { Since the overlay in question is going to be removed, slide
    the rest of the overlays down for all the objects}
  p := head;

  while (p <> nil) do
    begin
      if (p.GetOverlay >= overlay) then p.SetOverlay(p.GetOverlay - 1);
      p := p.Next;
    end;

  { Make sure we slide down the active and visible overlays for all
    the views too }
  for i := 0 to GetViewPoints - 1 do
    begin
      view := GetViewPoint(i);
      SlideOverlays(view);
    end;

  SlideOverlays(CurrentView);
end;

function MapCollection.FindClosestPoint(dx, dy: Coord; var point: CoordPoint; allowselected: boolean): boolean;
var
  p: DrawPrimitive;
  d, temp: double;
  tempx, tempy: Coord;
begin
  Result := false;
  d := MaxDouble;
  p := head;
  point.x := dx;
  point.y := dy;

  while (p <> nil) do
    begin
      if (allowselected or not p.IsSelected) and
        p.IsInOverlay(CurrentView.VisibleOverlays) then
        begin

          p.PointClosestTo(dx, dy, tempx, tempy);
          temp := distance(dx, dy, tempx, tempy);
          if (temp < d) then
            begin
              d := temp;
              point.x := tempx;
              point.y := tempy;
              Result := true;
            end;
        end;
      p := p.Next;
    end;
end;

function MapCollection.FindClosestPointOn(dx, dy: Coord; var point: CoordPoint; Var Angle: Coord; allowselected: boolean): boolean;
var
  P : DrawPrimitive;
  D : Double;

  Function CheckPrimitive(P: DrawPrimitive; Var D: Double; DX,DY: Coord; Var Point: CoordPoint; AllowSelected: Boolean): Boolean;
  Const MinDist = 15;
  Var
    CX,CY : Coord;
    Temp  : Double;
    P1    : DrawPrimitive;
    G     : GroupPrimitive;
    A     : Coord;

  Begin
    CX     := DX;
    CY     := DY;
    Result := False;

    // JD 10-10-02: Don't check an object if we are a certain distance away, or this
    // will take FOREVER on really large maps

    If (CX >= P.Extent.Left   - MinDist) And
       (CY >= P.Extent.Top    - MinDist) And
       (CX <= P.Extent.Right  + MinDist) And
       (CY <= P.Extent.Bottom + MinDist) Then
    Begin
      If P.FindPointOn(CurrentView,CX,CY,A) Then
      Begin
        Temp := Distance(DX, DY, CX, CY);
        If Temp < D Then
        Begin
          D       := Temp;
          Point.X := CX;
          Point.Y := CY;
          Angle   := A;
          Result  := True;
        End;
      End;
      If P Is GroupPrimitive Then
      Begin
        G  := P As GroupPrimitive;
        P1 := G.First;
        While P1 <> Nil Do
        Begin

          // Delphi 6's dumb optimization makes this necessary :(

          If Not Result
           Then Result := CheckPrimitive(P1,D,DX,DY,Point,AllowSelected)
           Else CheckPrimitive(P1,D,DX,DY,Point,AllowSelected);
          P1     := P1.Next;
        End; // While
      End;
    End;
  End; // CheckPrimitive

begin
  Result  := false;
  d       := MaxDouble;
  p       := head;
  point.x := dx;
  point.y := dy;
  while (p <> nil) do
  begin
    if (allowselected or not p.IsSelected) and
       p.IsInOverlay(CurrentView.VisibleOverlays) then
    begin

      // Delphi 6's dumb optimization makes this necessary :(

      If Not Result
       Then Result := CheckPrimitive(P,D,DX,DY,Point,AllowSelected)
       Else CheckPrimitive(P,D,DX,DY,Point,AllowSelected);
    end;
    p := p.Next;
  End; // While
End; // MapCollection.FindClosestPointOn

Function MapCollection.FindClosestIntersection(dx, dy: Coord; var point: CoordPoint): Boolean;
Var
  P           : DrawPrimitive;
  Points      : Array Of CoordPoint;
  NumSegments : Array Of Integer;
  ISect       : CoordPoint;
  B           : Boolean;

  Function CheckPrimitive(P: DrawPrimitive): Boolean;
  Const MinDist = 15;
  Var
    P1        : DrawPrimitive;
    G         : GroupPrimitive;
    Points1   : PCoordArray;
    NumPoints : Integer;
    I,J,K     : Integer;
    Pt1,Pt2   : CoordPoint;

  Begin
    Result := False;

    // JD 10-10-02: Don't check an object if we are a certain distance away, or this
    // will take FOREVER on really large maps

    If (DX >= P.Extent.Left   - MinDist) And
       (DY >= P.Extent.Top    - MinDist) And
       (DX <= P.Extent.Right  + MinDist) And
       (DY <= P.Extent.Bottom + MinDist) Then
    Begin
      Points1 := P.GetLines(CurrentView,NumPoints);
      If NumPoints > 0 Then
      Begin
        J := High(Points) + 1;
        SetLength(Points,J + NumPoints * 2); // Grow to max size
        K := 0;
        For I := 1 To NumPoints - 1 Do
        Begin
          CurrentView.ScreenToCoord(Points1^[I - 1].X,Points1^[I - 1].Y,Pt1.X,Pt1.Y);
          CurrentView.ScreenToCoord(Points1^[I].X,Points1^[I].Y,Pt2.X,Pt2.Y);
          If DistanceToSegment(MakeCoordPoint(DX,DY),Pt1,Pt2) < MinDist Then
          Begin
            Points[J + K]     := Pt1;
            Points[J + K + 1] := Pt2;
            Inc(K,2);
          End;
        End;
        SetLength(Points,J + K);           // Shrink to what we actually used
        If K > 0 Then
        Begin
          SetLength(NumSegments,High(NumSegments) + 2);
          NumSegments[High(NumSegments)] := K;
        End;
        FreeMem(Points1,NumPoints * SizeOf(CoordPoint));
      End;

      If P Is GroupPrimitive Then
      Begin
        G  := P As GroupPrimitive;
        P1 := G.First;
        While P1 <> Nil Do
        Begin

          // Delphi 6's dumb optimization makes this necessary :(

          If Not Result
           Then Result := CheckPrimitive(P1)
           Else CheckPrimitive(P1);
          P1 := P1.Next;
        End; // While
      End;
    End;
  End; // CheckPrimitive

begin
  Result  := False;
  p       := head;
  point.x := dx;
  point.y := dy;
  B       := False;
  SetLength(Points,0);
  SetLength(NumSegments,0);
  while (p <> nil) do
  begin
    if p.IsSelected and p.IsInOverlay(CurrentView.VisibleOverlays) then
    begin

      // Delphi 6's dumb optimization makes this necessary :(

      If Not B
       Then B := CheckPrimitive(P)
       Else CheckPrimitive(P)
    end;
    p := p.Next;
  End; // While

  // Look for the intersection

  If High(Points) > -1 Then
  Begin
    ISect.X := DX;
    ISect.Y := DY;
    Result  := NearestIntersection(Points,NumSegments,ISect);
    If Result Then Point := ISect;
  End;

  SetLength(Points,0);
  SetLength(NumSegments,0);
End; // MapCollection.FindClosestIntersection

function MapCollection.SelectClickedObject(dx, dy: Coord): integer;
var
  p, closest: DrawPrimitive;
  pixel_dist: double;
  cx, cy: Coord;
  d: CoordPoint;
begin
  // We're returning the number of the last frozen overlay that would have
  // been selected.  If we found some real stuff, don't report it; the
  // return value is used as a "dummy" reminder that they may be trying
  // to select items in a frozen overlay.
  Result := -1;

  if ReadOnlyPrevents then exit;

  CurrentView.DeltaScreenToCoord(6, 6, cx, cy);
  pixel_dist := Distance(cx, cy, 0, 0);

  // Allow for the possibility that the select region will be shrinking
  // (i.e., they're using Ctrl to remove an item from the select region)
  if AnythingSelected then InvalidateSelect(true);

  closest := nil;
  p       := head;
  d       := MakeCoordPoint(dx, dy);
  while (p <> nil) do
    begin
      // Keep resetting so that later objects (in front) have precedence
      // We also want to keep visible objects in a higher precedence than
      // non-visible ones!
      if p.SelectClick(pixel_dist, d) and
        p.IsInOverlay(CurrentView.VisibleOverlays) then closest := p;

      p := p.Next;
    end;

  if (closest = nil) then exit;

  if closest.IsInOverlay(CurrentView.ActiveOverlays) then
    begin
  {{ 2003-01-07, uoirej

       // As I can see, the only effect of not-null view passed to DrawPrimitive.Select
       // is that the object is being redrawn. It seems redundant here because we
       // invalidate more-than-ever-needed with two InvalidateSelect calls.

      closest.Select(CurrentView, not closest.IsSelected);
  }
         closest.Select(nil, not closest.IsSelected);
   // }} 2003-01-07, uoirej
    end
  else
    Result := closest.GetOverlay;

  InvalidateSelect(true);
  DisplaySelectedSize;
end;

procedure MapCollection.SelectClosestObject(dx, dy: Coord);
var
  p, closest: DrawPrimitive;
  d, temp: double;
  tempx, tempy: Coord;
begin
  if ReadOnlyPrevents then exit;

  // Allow for the possibility that the select region will be shrinking
  // (i.e., they're using Ctrl to remove an item from the select region)
  if AnythingSelected then InvalidateSelect(true);

  d := MaxDouble;
  p := head;
  closest := head;

  while (p <> nil) do
    begin
      if p.IsInOverlay(CurrentView.ActiveOverlays) then
        begin
          p.PointClosestTo(dx, dy, tempx, tempy);
          temp := distance(dx, dy, tempx, tempy);
          if (temp < d) then
            begin
              d := temp;
              closest := p;
            end;
        end;
      p := p.Next;
    end;

  if (closest <> nil) then
    begin
      closest.Select(CurrentView, not closest.IsSelected);
      InvalidateSelect(true);
    end;

  DisplaySelectedSize;
end;

function MapCollection.Select(rect: CoordRect): integer;
var
  p: DrawPrimitive;
  foundone: boolean;
begin
  Result := -1;
  if ReadOnlyPrevents then exit;

  // Allow for the possibility that the select region will be shrinking
  // (i.e., they're using Ctrl to remove an item from the select region)
  if AnythingSelected then InvalidateSelect(true);

  foundone := false;

  p := head;
  CorrectCoordRect(rect);

  while (p <> nil) do
    begin
      if p.IsWithin(rect) then
        begin
          if p.IsInOverlay(CurrentView.ActiveOverlays) then
            begin
              p.Select(CurrentView, not p.IsSelected);
              foundone := true;
            end
          else
            if p.IsInOverlay(CurrentView.VisibleOverlays) then
              begin
                Result := p.GetOverlay;
              end;
        end;
      p := p.Next;
    end;

  InvalidateSelect(true);

  // We're returning the number of the last frozen overlay that would have
  // been selected.  If we found some real stuff, don't report it; the
  // return value is used as a "dummy" reminder that they may be trying
  // to select items in a frozen overlay.
  if (foundone) then Result := -1;
  DisplaySelectedSize;
end;

procedure MapCollection.SelectFromPoint(x, y: Coord; AcceptableTypes: IdSet);
var
  p: DrawPrimitive;
  rect: CoordRect;
  shouldselect: boolean;
begin
  if ReadOnlyPrevents then exit;

  rect.left := x;
  rect.right := x;
  rect.top := y;
  rect.bottom := y;

  StartNewHandleDraw;

  p := head;
  while (p <> nil) do
    begin
      shouldselect := (p.GetId in AcceptableTypes) and
        p.IsInOverlay(CurrentView.ActiveOverlays) and
        p.IsTouching(rect);

      if p.IsSelected = shouldselect then
        begin
          // Even though we're not drawing it, keep lastHandle updated
          // so "large" handle works right
          p.DrawHandles(nil);
        end
      else
        begin
          p.DrawHandles(CurrentView);
          p.Select(nil, shouldselect);
        end;

      p := p.Next;
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.SliceAlong(s1, s2: CoordPoint);
var
  p    : DrawPrimitive;
  newp : DrawPrimitive;
  r    : CoordRect;
  
begin
  if ReadOnlyPrevents then exit;

  p    := head;
  tail := nil;

  r.TopLeft     := s1;
  r.BottomRight := s2;
  CorrectCoordRect(r);

  while p <> nil do
  begin
    if p.IsInOverlay(CurrentView.ActiveOverlays) and
      VisibleWithin(p.Extent, r) and
      p.SliceAlong(s1, s2, newp) then
    begin
      SetMapChain(newp);
      p.Invalidate(Parent.Handle, CurrentView);
      newp.Next := p.Next;
      if tail = nil
       then head := newp
       else tail.Next := newp;
      newp.Invalidate(Parent.Handle, CurrentView);

      p.Free;
      tail := newp;

      // Standard procedure now; we always have to determine base/alias status
      // (an object must NEVER EVER be neither!)

      newp.AddToBaseOrCopies;
    end
    else tail := p;

    p := tail.Next;
  end; // While
end; // MapCollection.SliceAlong

// The following procedure depends on short-circuit evaluation (it's the default anyway)
{$B-}
Procedure MapCollection.SliceAtPoint(S: CoordPoint);
Const SliceLength = 0.5;
Var
  P    : DrawPrimitive;
  NewP : DrawPrimitive;
  R1   : CoordRect;
  R2   : CoordRect;
  R3   : CoordRect;
  R4   : CoordRect;

Begin
  If ReadOnlyPrevents Then Exit;

  P    := Head;
  Tail := Nil;

  // If these four can't slice it, I don't know what will

  R1.TopLeft.X     := S.X;
  R1.TopLeft.Y     := S.Y - SliceLength;
  R1.BottomRight.X := S.X;
  R1.BottomRight.Y := S.Y + SliceLength;

  R2.TopLeft.X     := S.X - SliceLength;
  R2.TopLeft.Y     := S.Y;
  R2.BottomRight.X := S.X + SliceLength;
  R2.BottomRight.Y := S.Y;

  R3.TopLeft.X     := S.X - SliceLength;
  R3.TopLeft.Y     := S.Y - SliceLength;
  R3.BottomRight.X := S.X + SliceLength;
  R3.BottomRight.Y := S.Y + SliceLength;

  R4.TopLeft.X     := S.X - SliceLength;
  R4.TopLeft.Y     := S.Y + SliceLength;
  R4.BottomRight.X := S.X + SliceLength;
  R4.BottomRight.Y := S.Y - SliceLength;

  CorrectCoordRect(R1);
  CorrectCoordRect(R2);
  CorrectCoordRect(R3);
  CorrectCoordRect(R4);

  While P <> Nil Do
  Begin
    If P.IsInOverlay(CurrentView.ActiveOverlays) And
       VisibleWithin(P.Extent, R4) And // This will do just fine
       (P.SliceAlong(R1.TopLeft, R1.BottomRight, NewP) Or // The compiler should do only one
        P.SliceAlong(R2.TopLeft, R2.BottomRight, NewP) Or
        P.SliceAlong(R3.TopLeft, R3.BottomRight, NewP) Or
        P.SliceAlong(R4.TopLeft, R4.BottomRight, NewP)) Then
    Begin
      SetMapChain(newp);
      P.Invalidate(Parent.Handle, CurrentView);
      NewP.Next := P.Next;
      If Tail = Nil Then Head := NewP Else Tail.Next := NewP;
      NewP.Invalidate(Parent.Handle, CurrentView);
      P.Free;
      Tail := NewP;

      // Standard procedure now; we always have to determine base/alias status
      // (an object must NEVER EVER be neither!)

      NewP.AddToBaseOrCopies;
    End
    Else Tail := P;
    P := Tail.Next;
  End; // While
End;

procedure MapCollection.IterateSelect(rect: CoordRect);
var
  p: DrawPrimitive;
  FirstInSelectRegion: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);

  p := head;
  FirstInSelectRegion := nil;
  CorrectCoordRect(rect);

  while (p <> nil) do
    begin
      if p.IsInOverlay(CurrentView.ActiveOverlays) and p.IsTouching(rect) then
        begin
          if (FirstInSelectRegion = nil) then FirstInSelectRegion := p;

          p.Select(CurrentView, not p.IsSelected);

          if p.IsSelected then
            begin
              FirstInSelectRegion := nil;
              break;
            end;
        end;
      p := p.Next;
    end;

  if (FirstInSelectRegion <> nil) then
    begin
      FirstInSelectRegion.Select(CurrentView, not FirstInSelectRegion.IsSelected);
    end;

  InvalidateSelect(true);
  DisplaySelectedSize;
end;

procedure MapCollection.CreateArray(hc, vc: integer; hs, vs: Coord; hbetween, vbetween: boolean;
                                    Ellipse,Rotate: Boolean; ex,ey: Coord);
var
  r: CoordRect;
  p, newchain, np: DrawPrimitive;
  i, j: integer;
  Degrees : Double;
  X,Y     : Double;

begin
  if ReadOnlyPrevents then exit;

  if (hc = 1) and (vc = 1) then exit;

  r := head.ChainExtent(false);
  if hbetween then hs := hs + (r.Right - r.Left);
  if vbetween then vs := vs + (r.Bottom - r.Top);

  Map.SetUndoPoint(Format(res_mapobj_array_create, [hc, vc]));

  p := head;
  while (p <> nil) do
    begin
      if p.IsSelected then
        begin
          newchain := p.Next;
          for j := vc - 1 downto 0 do
            begin
              for i := hc - 1 downto 0 do
                begin

                  // JD 7-30-02

                  if (i <> 0) or (j <> 0) Or Ellipse then
                    begin
                      np := p.Copy;

                      // Standard procedure now; we always have to determine base/alias status
                      // (an object must NEVER EVER be neither!)

                      np.AddToBaseOrCopies(true);

                      Assert(np <> nil);

                      If Ellipse Then
                      Begin
                        X := (ex + J * VS) * Cos(2 * Pi * I / HC);
                        Y := (ey + J * VS) * Sin(2 * Pi * I / HC);
                        If Rotate And (ey <> 0) Then
                        Begin
                          Degrees := 270 - (ArcTan2(Y * (ex / ey),X) * 180 / Pi);
                          NP.ChainApplyMatrix(CenterMatrix(RotationMatrix(Degrees)),true);
                        End;
                        np.Move(X,Y);
                      End
                      Else
                      Begin
                        np.Move(p.Extent.Left + hs * i - np.Extent.Left,
                          p.Extent.Top + vs * j - np.Extent.Top);
                      End;
                      np.Invalidate(Parent.Handle, CurrentView);
                      np.Next := newchain;
                      { Update tail if the item being duplicated is the
                        last one }
                      if (newchain = nil) then tail := np;

                      newchain := np;
                    end;
                end;
            end;
          np := p.Next;
          p.Next := newchain;
          p := np;
        end
      else
        p := p.Next;
    end;
  InvalidateSelect(true);
end;

procedure MapCollection.AlignSelection(h, v, wd, hg: AlignType);
var
  p: DrawPrimitive;
  list: TList;
  i: integer;
  x, w: Coord;
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);

  // Create a list of all the objects to be aligned
  list := GetSelectedObjectList;

  if list.Count <= 1 then
  begin
    list.Free;
    exit;
  end;

  // Now that we have the list, do the requested actions
  // ------------ HORIZONTAL -------------
  case h of
    LEFT_TOP:
      begin
        list.Sort(LeftPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Left;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - p.Extent.Left, 0);
          end;
      end;
    CENTERS:
      begin
        x := 0;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            x := x + (p.Extent.Left + p.Extent.Right) / 2;
          end;
        x := x / list.Count;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - (p.Extent.Left + p.Extent.Right) / 2, 0);
          end;
      end;
    RIGHT_BOTTOM:
      begin
        list.Sort(RightPredicate);
        x := DrawPrimitive(list.Items[list.Count - 1]).Extent.Right;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - p.Extent.Right, 0);
          end;
      end;
    SLANTFORWARD,
      SLANTBACKWARD:
      begin
        list.Sort(LeftPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Left;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Left - x) / list.Count;
        list.Sort(TopPredicate);
        if (h = SLANTBACKWARD) then
          begin
            x := x + w * list.Count;
            w := -w;
          end;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - p.Extent.Left, 0);
            x := x + w;
          end;
      end;
    SCATTER:
      begin
        list.Sort(LeftPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Left;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Left - x);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x + Random * w - p.Extent.Left, 0);
          end;
      end;
    JOG:
      begin
        list.Sort(LeftPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Left;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Left - x);
        list.Sort(TopPredicate);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x + (i and 1) * w - p.Extent.Left, 0);
          end;
      end;
    SPACE_EQUALLY:
      begin
        list.Sort(HCenterPredicate);
        p := DrawPrimitive(list.Items[0]);
        x := (p.Extent.Left + p.Extent.Right) / 2;
        p := DrawPrimitive(list.Items[list.Count - 1]);
        w := (p.Extent.Left + p.Extent.Right) / 2 - x;

        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - (p.Extent.Left + p.Extent.Right) / 2, 0);
            x := x + w / (list.Count - 1);
          end;
      end;
    STACK:
      begin
        list.Sort(LeftPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Right;
        for i := 1 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(x - p.Extent.Left, 0);
            x := p.Extent.Right;
          end;
      end;
  end;

  // ------------ VERTICAL --------------
  case v of
    LEFT_TOP:
      begin
        list.Sort(TopPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Top;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - p.Extent.Top);
          end;
      end;
    CENTERS:
      begin
        x := 0;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            x := x + (p.Extent.Top + p.Extent.Bottom) / 2;
          end;
        x := x / list.Count;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - (p.Extent.Top + p.Extent.Bottom) / 2);
          end;
      end;
    RIGHT_BOTTOM:
      begin
        list.Sort(BottomPredicate);
        x := DrawPrimitive(list.Items[list.Count - 1]).Extent.Bottom;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - p.Extent.Bottom);
          end;
      end;
    SLANTFORWARD,
      SLANTBACKWARD:
      begin
        list.Sort(TopPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Top;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Top - x) / list.Count;
        list.Sort(LeftPredicate);
        if (v = SLANTBACKWARD) then
          begin
            x := x + w * list.Count;
            w := -w;
          end;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - p.Extent.Top);
            x := x + w;
          end;
      end;
    SCATTER:
      begin
        list.Sort(TopPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Top;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Top - x);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x + Random * w - p.Extent.Top);
          end;
      end;
    JOG:
      begin
        list.Sort(TopPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Top;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Extent.Top - x);
        list.Sort(LeftPredicate);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x + (i and 1) * w - p.Extent.top);
          end;
      end;
    SPACE_EQUALLY:
      begin
        list.Sort(VCenterPredicate);
        p := DrawPrimitive(list.Items[0]);
        x := (p.Extent.Top + p.Extent.Bottom) / 2;
        p := DrawPrimitive(list.Items[list.Count - 1]);
        w := (p.Extent.Top + p.Extent.Bottom) / 2 - x;

        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - (p.Extent.Top + p.Extent.Bottom) / 2);
            x := x + w / (list.Count - 1);
          end;
      end;
    STACK:
      begin
        list.Sort(TopPredicate);
        x := DrawPrimitive(list.Items[0]).Extent.Bottom;
        for i := 1 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.Move(0, x - p.Extent.Top);
            x := p.Extent.Bottom;
          end;
      end;
  end;

  // ------------ WIDTH -------------
  case wd of
    LEFT_TOP:
      begin // NARROWEST
        list.Sort(WidthPredicate);
        x := DrawPrimitive(list.Items[0]).Width;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x, 0);
          end;
      end;
    CENTERS:
      begin // AVERAGE WIDTH
        x := 0;
        for i := 0 to list.Count - 1 do
          begin
            x := x + DrawPrimitive(list.Items[i]).Width;
          end;
        x := x / list.Count;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x, 0);
          end;
      end;
    RIGHT_BOTTOM:
      begin // WIDEST
        list.Sort(WidthPredicate);
        x := DrawPrimitive(list.Items[list.Count - 1]).Width;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x, 0);
          end;
      end;
    SLANTFORWARD,
      SLANTBACKWARD:
      begin
        list.Sort(WidthPredicate);
        x := DrawPrimitive(list.Items[0]).Width;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Width - x) / list.Count;
        list.Sort(TopPredicate);
        if (wd = SLANTBACKWARD) then
          begin
            x := x + w * list.Count;
            w := -w;
          end;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x, 0);
            x := x + w;
          end;
      end;
    SCATTER:
      begin
        list.Sort(WidthPredicate);
        x := DrawPrimitive(list.Items[0]).Width;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Width - x);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x + Random * w, 0);
          end;
      end;
    JOG:
      begin
        list.Sort(WidthPredicate);
        x := DrawPrimitive(list.Items[0]).Width;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Width - x);
        list.Sort(TopPredicate);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(x + (i and 1) * w, 0);
          end;
      end;
  end;

  // ------------ HEIGHT -------------
  case hg of
    LEFT_TOP:
      begin // SHORTEST
        list.Sort(HeightPredicate);
        x := DrawPrimitive(list.Items[0]).Height;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x);
          end;
      end;
    CENTERS:
      begin // AVERAGE HEIGHT
        x := 0;
        for i := 0 to list.Count - 1 do
          begin
            x := x + DrawPrimitive(list.Items[i]).Height;
          end;
        x := x / list.Count;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x);
          end;
      end;
    RIGHT_BOTTOM:
      begin // TALLEST
        list.Sort(HeightPredicate);
        x := DrawPrimitive(list.Items[list.Count - 1]).Height;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x);
          end;
      end;
    SLANTFORWARD,
      SLANTBACKWARD:
      begin
        list.Sort(HeightPredicate);
        x := DrawPrimitive(list.Items[0]).Height;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Height - x) / list.Count;
        list.Sort(LeftPredicate);
        if (hg = SLANTBACKWARD) then
          begin
            x := x + w * list.Count;
            w := -w;
          end;
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x);
            x := x + w;
          end;
      end;
    SCATTER:
      begin
        list.Sort(HeightPredicate);
        x := DrawPrimitive(list.Items[0]).Height;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Height - x);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x + Random * w);
          end;
      end;
    JOG:
      begin
        list.Sort(HeightPredicate);
        x := DrawPrimitive(list.Items[0]).Height;
        w := (DrawPrimitive(list.Items[list.Count - 1]).Height - x);
        list.Sort(LeftPredicate);
        for i := 0 to list.Count - 1 do
          begin
            p := DrawPrimitive(list.Items[i]);
            p.SetSize(0, x + (i and 1) * w);
          end;
      end;
  end;

  list.Free;

  InvalidateSelect(true);
  DisplaySelectedSize;
end;


procedure MapCollection.MoveSelection(dx, dy: Coord);
var
  p: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  p := head;
  InvalidateSelect(true);

  while (p <> nil) do
    begin
      if p.IsSelected then
        begin
          SetModified(modChanged);
          p.Move(dx, dy);
        end;

      p := p.Next;
    end;

  InvalidateSelect(true);
  DisplaySelectedSize;
end;

procedure MapCollection.ApplyMatrix(apply: Matrix);
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);
  if head.ChainApplyMatrix(apply, false) then InvalidateSelect(true);
  DisplaySelectedSize;
end;

function MapCollection.CenterMatrix(mat: Matrix): Matrix;
var
  r    : CoordRect;
  x, y : Coord;

begin
  r := head.ChainExtent(false);
  x := (r.Right + r.Left) / 2;
  y := (r.Bottom + r.Top) / 2;
  Result := OffsetMatrix(-x, -y);
  MatrixMultiplyBy(Result, mat);
  MatrixMultiplyBy(Result, OffsetMatrix(x, y));
end;

procedure MapCollection.RotateSelection(degrees: double);
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);
  if head.ChainApplyMatrix(CenterMatrix(RotationMatrix(degrees)), false) then
    begin
      InvalidateSelect(true);
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.ScaleSelection(xfactor, yfactor: double);
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);
  if head.ChainApplyMatrix(CenterMatrix(ScaleMatrix(xfactor, yfactor)), false) then
    begin
      InvalidateSelect(true);
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.SkewSelection(sx, sy: double);
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);
  if head.ChainApplyMatrix(CenterMatrix(SkewMatrix(-sx, -sy)), false) then
    begin
      InvalidateSelect(true);
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.FlipSelection(xaxis, yaxis: boolean);
begin
  if ReadOnlyPrevents then exit;

  InvalidateSelect(true);
  if head.ChainApplyMatrix(CenterMatrix(FlipMatrix(xaxis, yaxis)), false) then
    begin
      InvalidateSelect(true);
    end;
  DisplaySelectedSize;
end;

function MapCollection.MoveHandle(var mode: HandleMode; origx, origy, dx, dy: Coord): boolean;
var
  p: DrawPrimitive;
  oldext: CoordRect;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  p := head;

  while (p <> nil) do
    begin
      if p.IsSelected then
        begin
          oldext := p.Extent;
          if p.MoveHandle(CurrentView, mode, origx, origy, dx, dy) then
            begin
              InvalidateRect(CurrentView.CoordToScreenRect(oldext), true);
              InvalidateRect(CurrentView.CoordToScreenRect(p.Extent), true);
              Result := true;
              if (mode <> hmAll) then break;
            end;
        end;

      p := p.Next;
    end;

  if Result then SetModified(modChanged);

  DisplaySelectedSize;
  //  Invalidate;
end;

function MapCollection.FindHandle(x, y: Coord; all: boolean): DrawPrimitive;
var
  p: DrawPrimitive;
begin
  Result := nil;

  p := head;

  while (p <> nil) do
    begin
      if (all or p.IsSelected) and p.FindHandle(CurrentView, x, y) then
        begin
          Result := p;
          exit;
        end;

      p := p.Next;
    end;
end;

function MapCollection.FindHyperlink(x, y: Coord; var hypertext:string; var hyperflags:THyperlinkFlags): boolean;
var p: DrawPrimitive;
begin
  Result := false;

  p := head;

  while (p <> nil) do
    begin
      if p.FindHyperlink(CurrentView, x,y, hypertext, hyperflags) then
        begin
          Result := true;
          exit;
        end;

      p := p.Next;
    end;
end;

function MapCollection.FindScalpelPoint(x, y: Coord; var p: DrawPrimitive; var index: integer): boolean;
begin
  Result := false;

  p := head;

  while (p <> nil) do
    begin
      if p.FindScalpelPoint(CurrentView, x, y, index) then
        begin
          Result := true;
          exit;
        end;

      p := p.Next;
    end;
end;

function MapCollection.FindEndPoint(var x, y: Coord; ignoreObject: DrawPrimitive; var p: DrawPrimitive): boolean;
begin
  Result := false;

  p := head;

  while (p <> nil) do
    begin
      if p.FindEndPoint(CurrentView, x, y) and (p <> ignoreObject) then
        begin
          Result := true;
          exit;
        end;

      p := p.Next;
    end;
end;

function MapCollection.FindPointOn(var x, y, Angle: Coord; ignoreObject: DrawPrimitive; var p: DrawPrimitive): boolean;
begin
  Result := false;
  p      := head;
  while p <> nil do
  begin
    if p.FindPointOn(CurrentView, x, y, Angle) and (p <> ignoreObject) then
    begin
      Result := true;
      exit;
    end;
    p := p.Next;
  end; // While
end;

procedure MapCollection.RefreshExtent;
var p:DrawPrimitive;
begin
  p := head;

  while (p <> nil) do begin
    p.ComputeExtent;
    p := p.Next;
    end;
end;


function MapCollection.Extent(View: ViewPoint; all: boolean): TRect;
var
  r: TRect;
begin
  r := Rect(0, 0, 0, 0);

  r := View.CoordToScreenRect(head.ChainExtent(all));

  if (r.Left <> r.Right) or (r.Top <> r.Bottom) then
    begin
      dec(r.Left, selborder);
      dec(r.Top, selborder);
      inc(r.Right, selborder);
      inc(r.Bottom, selborder);
    end;

  Result := r;
end;

function MapCollection.ExtentFloat(View: ViewPoint; all: boolean): CoordRect;
Var R1,R2: CoordRect;
begin
  R1 := CoordExtent(All);
  View.CoordToScreen(R1.Left,R1.Top,R2.Left,R2.Top);
  View.CoordToScreen(R1.Right,R1.Bottom,R2.Right,R2.Bottom);

  if (R2.Left <> R2.Right) Or (R2.Top <> R2.Bottom) then
  begin
    R2.Left   := R2.Left   - SelBorder;
    R2.Top    := R2.Top    - SelBorder;
    R2.Right  := R2.Right  + SelBorder;
    R2.Bottom := R2.Bottom + SelBorder;
  end;

  Result := r2;
end;

function MapCollection.CoordExtent(all:boolean): CoordRect;
begin
  Result := head.ChainExtent(all);
end;

procedure MapCollection.ShowAll(var view: ViewPoint; zoomfactor: Coord);
var
  r: CoordRect;
  ext: TRect;
  width, height: Coord;
begin
  ext := Extent(view, true);
  if (ext.Right - ext.Left <> 0) and (ext.Bottom - ext.Top <> 0) then
  begin
    r := View.ScreenToCoordRect(ext);

    if zoomfactor <> 0.0 then
    begin
      width    := (r.right - r.left) * zoomfactor;
      r.left   := r.left - width;
      r.right  := r.right + width;

      height   := (r.bottom - r.top) * zoomfactor;
      r.top    := r.top - height;
      r.bottom := r.bottom + height;
    end;

    View.SetCoordinateRect(r);
  end;
end;

procedure MapCollection.StartAdding(name: string);
begin
  if ReadOnlyPrevents then exit;

  ClearSelection;
  DisplaySelectedSize;
  if (name <> '') then SetUndoPoint(name);
  SetModified(modAdded);
end;

procedure MapCollection.EndAdding;
begin
  if not Settings.SelectLast.Checked then ClearSelection;
  DisplaySelectedSize;
end;

Procedure MapCollection.SetMapChain(D: DrawPrimitive);
Begin
  While D <> Nil Do
  Begin
    D.SetMap(Self);
    D := D.Next;
  End; // While
End; // MapCollection.SetMapChain

procedure MapCollection.AddObject(obj: DrawPrimitive);
var
  ext : TRect;
  ovr : integer;

begin
  // Don't allow adding empty objects.  Check for this because empty symbols
  // will eventually end up as empty objects here.
  if (obj=nil) then exit;

  if ReadOnlyPrevents then begin
    // Note: This case should never happen, since we're preventing all access to tools that
    // would do the actual adding.  Don't bother localizing this message...
    raise Exception.Create('MapCollection.AddObject called while ReadOnly.  Please contact support@gryc.ws');
    end;

  ext := CurrentView.CoordToScreenRect(obj.Extent);
  dec(ext.Left, selborder);
  dec(ext.Top, selborder);
  inc(ext.Right, selborder);
  inc(ext.Bottom, selborder);
  SetMapChain(Obj);
  if (tail = nil) then
    begin
      head := obj;
      tail := obj;
    end
  else
    begin
      tail.Next := obj;
      obj.Next := nil;
      tail := obj;
    end;

 // {{ 2003-01-07, uoirej
    {
       // As I can see, the only effect of not-null view passed to DrawPrimitive.Select
       // is that the object is being redrawn. It seems redundant here because we
       // invalidate object rect at the end of this procedure.

  if (supresscount = 0) then
    obj.Select(CurrentView, true)
  else
    obj.Select(nil, true);
}

       obj.Select(nil, true);
   // }} 2003-01-07, uoirej

  ovr := obj.GetOverlay;
  if not (ovr in CurrentView.VisibleOverlays) then
    begin
      MessageBox(MainForm.Handle,
        pchar(res_mapobj_overlay_inv1),
        pchar(res_mapobj_overlay_inv2),
        MB_OK);
      CurrentView.VisibleOverlays := CurrentView.VisibleOverlays + [ovr];
      CurrentView.ActiveOverlays := CurrentView.ActiveOverlays + [ovr];
      SynchronizeOverlayList(MainForm.OverlayList);
    end;

  InvalidateRect(ext, false);
  DisplaySelectedSize;

  // Standard procedure now; we always have to determine base/alias status
  // (an object must NEVER EVER be neither!)

  Obj.AddToBaseOrCopies(True);
end;

constructor MapCollection.Create(ParentForm: TForm);
var
  cr: CoordRect;
  i:integer;
begin
  BasePrimitives := TStringList.Create;
  Parent := ParentForm;
  head := nil;
  tail := nil;
  ClearModified;
  UndoMax := 0;
  UndoArray := nil;
  CurrentUndo := 0;
  UsePanRects := false;
  Landscape := false;
  supresscount := 0;

  cr := MakeCoordRect(0, 0, Screen.Width, Screen.Height);
  CurrentView := ViewPoint.Create(cr, Screen.Width, Screen.Height);
  ViewList := TList.Create;

  // Now create the push pin information; use dynamic arrays
  SetLength(pushpin, PushPinCount);

  pushpinflags:=[PP_WaypointsVisible, PP_ShowNumber, PP_ShowNote];
  
  for i:=0 to PushPinCount-1 do begin
    pushpin[i].Name := '';
    pushpin[i].HistoryCount := 0;
    pushpin[i].History := nil;
  end;
end;

destructor MapCollection.Destroy;
var i:integer;
begin
  RemoveAllViews;
  Clear;
  CurrentView.Free;
  SetUndoMax(0);
  ViewList.Free;

  // Set the array lengths to 0 to deallocate their memory

  for i := 0 to PushPinCount - 1 do SetLength(pushpin[i].History, 0);

  SetLength(pushpin, 0);
  BasePrimitives.Free;
end;

procedure MapCollection.Clear;
var
  p, next: DrawPrimitive;
begin
  SetModified(modDeleted);
  p := head;

  while (p <> nil) do
    begin
      next := p.Next;
      p.Free;
      p := next;
    end;

  head := nil;
  tail := nil;
  BasePrimitives.Clear;

  Invalidate;
  DisplaySelectedSize;
end;

function MapCollection.AnythingSelected: boolean;
var
  p: DrawPrimitive;
begin
  Result := true;
  p := head;
  while (p <> nil) do
    begin
      if (p.IsSelected) then exit;
      p := p.Next;
    end;
  Result := false;
end;

function MapCollection.SetStyle(style: StyleAttrib): boolean;
begin
  Result := false;
  if ReadOnlyPrevents then exit;

  if AnythingSelected and
     ((style.bits <> GetStyle.bits) Or
      ((style.Line = start_numeric_thickness_style) And
       (style.FullStyle.Thickness <> GetStyle.FullStyle.Thickness))) then SetUndoPoint(res_mapobj_undo_style);

  if head.ChainSetStyle(style, false) then
    begin
      SetModified(modChanged);
      InvalidateSelect(true);
      Result := true;
    end;
end;

function MapCollection.GetStyle: StyleAttrib;
begin
  Result := head.ChainGetStyle(false);
end;

function MapCollection.SetColor(color: TColor): boolean;
begin
  Result := false;
  if ReadOnlyPrevents then exit;

  if AnythingSelected and (color <> GetColor) then SetUndoPoint(res_mapobj_undo_color);

  if head.ChainSetColor(color, false) then
    begin
      SetModified(modChanged);
      InvalidateSelect(false);
      Result := true;
    end;
end;

function MapCollection.GetColor: TColor;
begin
  Result := head.ChainGetColor(false);
end;

function MapCollection.SetFillColor(color: TColor): boolean;
begin
  Result := false;
  if ReadOnlyPrevents then exit;

  if AnythingSelected and (color <> GetFillColor) then SetUndoPoint(res_mapobj_undo_colorf);

  if head.ChainSetFillColor(color, false) then
    begin
      SetModified(modChanged);
      InvalidateSelect(false);
      Result := true;
    end;
end;

function MapCollection.GetFillColor: TColor;
begin
  Result := head.ChainGetFillColor(false);
end;

function MapCollection.SetSeed(seed: integer): boolean;
var
  oldext: TRect;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if AnythingSelected and (Seed <> GetSeed) then SetUndoPoint(res_mapobj_undo_seed, true);

  oldext := Extent(CurrentView, false);
  if head.ChainSetSeed(seed, false) then
    begin
      SetModified(modChanged);
      InvalidateRect(oldext, true);
      InvalidateSelect(true);
      Result := true;
    end;
end;

function MapCollection.GetSeed: integer;
begin
  Result := head.ChainGetSeed(false);
end;

function MapCollection.SetRoughness(rough: integer): boolean;
var
  oldext: TRect;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if AnythingSelected and (rough <> GetRoughness) then SetUndoPoint(res_mapobj_undo_rough, true);

  oldext := Extent(CurrentView, false);
  if head.ChainSetRoughness(rough, false) then
    begin
      SetModified(modChanged);
      InvalidateRect(oldext, true);
      InvalidateSelect(true);
      Result := true;
    end;
end;

function MapCollection.GetRoughness: integer;
begin
  Result := head.ChainGetRoughness(false);
end;

function MapCollection.SetFractalStateSelected(state:FractalState):boolean;
var oldext:TRect;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if AnythingSelected then
  begin
    case state of
      fsSetNormal:   Map.SetUndoPoint(res_fractalstate_normal);
      fsSetFractal:  Map.SetUndoPoint(res_fractalstate_fractal);
      fsFlipFractal: Map.SetUndoPoint(res_fractalstate_toggle);
    end; // Case
  end;

  oldext := Extent(CurrentView, false);
  if head.ChainSetFractal(state, false) then
  begin
    SetModified(modChanged);
    InvalidateRect(oldext, true);
    InvalidateSelect(true);
    Result := true;
  end;
end;

function MapCollection.SetOverlay(overlay: byte): boolean;
begin
  Result := false;

  if ReadOnlyPrevents then exit;

  if AnythingSelected and (overlay <> GetOverlay) then SetUndoPoint(res_mapobj_undo_overlay);

  if head.ChainSetOverlay(overlay, false) then
    begin
      SetModified(modChanged);
      InvalidateSelect(true);
      Result := true;
    end;
end;

function MapCollection.GetOverlay: integer;
begin
  Result := head.ChainGetOverlay(false);
end;

function MapCollection.SetTextAttrib(const View: Viewpoint; const attrib: TextAttrib): boolean;
var
  oldext: TRect;
begin
  Result := false;

  if ReadOnlyPrevents then exit;
  
  oldext := Extent(View, false);
  if head.ChainSetTextAttrib(View, attrib, false) then
    begin
      SetModified(modChanged);
      InvalidateRect(oldext, true);
      InvalidateSelect(true);
      Result := true;
    end;
end;

function MapCollection.GetTextAttrib: TextAttrib;
begin
  Result := head.ChainGetTextAttrib(false);
end;

procedure MapCollection.Group;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
    begin
      SetUndoPoint(res_mapobj_undo_group);
      GroupSelected;
    end;
end;

procedure MapCollection.GroupSelected;
var
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

  head := mainhead;
  tail := maintail;

  if grouphead <> nil then
  begin
    StartAdding('');
    try
      AddObject(GroupPrimitive.Create(grouphead));
    except
      // Attempt to add null group
    end;
    EndAdding;
    InvalidateSelect(true);

    // Move all selected items to the same group
    while grouphead <> nil do
    begin
      grouphead.SetOverlay(MainForm.ActiveOverlay.ItemIndex);
      grouphead := grouphead.Next;
    end;
  end;
end;

procedure MapCollection.SendToBack;
var
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
    begin
      SetUndoPoint(res_mapobj_undo_back);
      SetModified(modChanged);
      SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

      if (mainhead = nil) then begin
         head := grouphead;
         tail := grouptail;
         end
      else if (grouphead <> nil) then
        begin
          head := grouphead;
          grouptail.Next := mainhead;
          tail := maintail;
          InvalidateSelect(true);
        end;
    end;
end;

procedure MapCollection.BringToFront;
var
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
    begin
      SetUndoPoint(res_mapobj_undo_front);
      SetModified(modChanged);
      SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

      if (mainhead = nil) then begin
         head := grouphead;
         tail := grouptail;
         end
      else if (grouphead <> nil) then
        begin
          head := mainhead;
          maintail.Next := grouphead;
          tail := grouptail;
          InvalidateSelect(true);
        end;
    end;
end;

procedure MapCollection.BringForward;
var
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
  insertpoint:DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then begin
      // Bringing "forward" actually moves items later in the list, since
      // we draw from first to last.
      GetFollowingSelection(insertpoint);

      // If we can't get something after selection, it must already be the last,
      // or there is no selection.
      if (insertpoint=nil) then exit;

      SetUndoPoint(res_mapobj_undo_backward_one);
      SetModified(modChanged);

      SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

      // Insert selected items at our insertion point
      grouptail.Next := InsertPoint.Next;
      InsertPoint.Next := grouphead;

      // Set new head and tail
      head:=mainhead;

      if (grouptail.Next=nil) then
        tail:=grouptail
      else
        tail:=maintail;

      InvalidateSelect(true);
    end;
end;

procedure MapCollection.SendBackward;
var
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
  insertpoint:DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then begin
      GetTwoPriorSelection(insertpoint);

      // If we can't get something two prior, we are either too close to the beginning,
      // or there is no selection.  We can test for "already at back" by looking
      // at the head (i.e. last in Z-Order) item's selected state--if so, we bail.
      if (head<>nil) and (head.IsSelected) then exit;

      SetUndoPoint(res_mapobj_undo_forward_one);
      SetModified(modChanged);

      SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

      // Insert selected items at our insertion point
      // If insertPoint=nil, we've moved all the way to the front,
      // so reset the head

      if (InsertPoint=nil) then begin
        grouptail.Next := mainhead;
        head := grouphead;
        end
      else begin
        grouptail.Next := InsertPoint.Next;
        InsertPoint.Next := grouphead;
        head := mainhead;
        end;

      tail := maintail;

      InvalidateSelect(true);
    end;
end;

procedure MapCollection.GetFollowingSelection(var insertat:DrawPrimitive);
var p:DrawPrimitive;
begin
  insertat:=nil;
  p:=head;

  // Keep chasing down the chain until we run out--we want to find
  // the item following the *last* selected item.

  while (p<>nil) do begin
    // Found a selected item?  Point to next as where to insert
    if p.IsSelected and (p.Next<>nil) then insertat:=p.Next;

    p:=p.Next;
    end;
end;

procedure MapCollection.GetTwoPriorSelection(var insertat:DrawPrimitive);
var p:DrawPrimitive;
begin
  insertat:=nil;
  p:=head;

  // Run until we find the *first* selected item.
  // Return the item two before that first selected item.
  // Why two?  Since we have a linked list, we need to alter
  // the prior object to move the link, and we need this function
  // to move our objects up one object in the Z-Order.
  while (p<>nil) do begin
    // Found a selected item?  Point to next as where to insert
    if (p.Next<>nil) and (p.Next.Next<>nil) and (p.Next.Next.IsSelected) then begin
      insertat:=p;
      exit;
      end;

    p:=p.Next;
    end;
end;

procedure MapCollection.SplitSelectedChain(var mainhead, maintail: DrawPrimitive;
  var chainhead, chaintail: DrawPrimitive);
var
  p, n: DrawPrimitive;

begin
  mainhead  := nil;
  maintail  := nil;
  chainhead := nil;
  chaintail := nil;

  p := head;

  while p <> nil do
  begin
    // If selected, add to the group list, else add to the main list.
    if p.IsSelected then
    begin
      if chaintail = nil then
      begin
        chainhead := p;
        chaintail := p;
      end
      else
      begin
        chaintail.Next := p;
        chaintail      := p;
      end;
    end
    else
    begin
      if maintail = nil then
      begin
        mainhead := p;
        maintail := p;
      end
      else
      begin
        maintail.Next := p;
        maintail      := p;
      end;
    end;

    n      := p.Next;
    p.Next := nil;
    p      := n;
  end; // While
  DisplaySelectedSize;
end;

procedure MapCollection.Ungroup;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
    begin
      SetUndoPoint(res_mapobj_undo_group_no);
      UngroupSelected;
    end;
end;

procedure MapCollection.UnGroupSelected;
var
  p, cn   : DrawPrimitive;
  g       : GroupPrimitive;
  oldtail : DrawPrimitive;

begin
  SetModified(modDeleted);
  p       := head;
  tail    := nil;
  oldtail := nil;

  while p <> nil do
  begin
    if p.IsSelected and (p is GroupPrimitive) then
    begin
      g := p as GroupPrimitive;

      // Did we accidentally add this group (which we're about to free)
      // as the tail of the object list?  If so, back up to the previous object.
      if g = tail then tail := oldtail;

      // If the group is an alias, make a copy and insert it in the object
      // list.

      G.SplitOff;

      // Do processing on the group as a whole: turn on selection.
      cn := g.First;
      while cn <> nil do
      begin
        cn.Select(CurrentView, true);
        cn := cn.Next;
      end; // While

      // Connect the contents of the group (if any) to the main chain
      if g.First <> nil then
      begin
        // Connect the string inside the group to the tail.
        if tail = nil
         then head      := g.First
         else tail.Next := g.First;

        // Now, skip the tail to the end of that chain
        tail := g.First;
        while tail.Next <> nil do tail := tail.Next;
      end;

      // connect the next object (if any) after the group to the tail
      if p.Next <> nil then
      begin
        // Save the tail before this operation in case we have to back
        // it out.  We back it out if the object we're adding to the tail
        // happens to be a selected group that we're just about to break up.
        oldtail := tail;

        if tail = nil then
        begin
          head := p.Next;
          tail := p.Next;
        end
        else
        begin
          tail.Next := p.Next;
          tail      := p.Next;
        end;
      end;

      // Don't leave anything attached to the group and delete the group
      p       := p.Next;
      g.First := nil;
      g.Free;
    end
    else
    begin
      tail := p;
      p    := p.Next;
    end;
  end; // While

  InvalidateSelect(true);
end;

procedure MapCollection.Decompose;
begin
  if ReadOnlyPrevents then exit;

  if AnythingSelected then
    begin
      SetUndoPoint(res_mapobj_undo_decompose);
      DecomposeSelected;
    end;
end;

procedure MapCollection.DecomposeSelected;
var p, newchain: DrawPrimitive;
begin
  // The initial invalidate allows us to shrink our extent displayed on
  // screen in the case of decomposing text.  Decomposing text to polylines
  // will always shrink the extent.
  
  InvalidateSelect(true);

  SetModified(modChanged);
  p    := head;
  tail := nil;

  while p <> nil do
  begin
    if p.IsSelected then
    begin
      if p.Decompose(CurrentView, newchain) then
      begin
        SetMapChain(newchain);

        // Hook decomposed chain into place of object

        if tail = nil
         then head      := newchain
         else tail.Next := newchain;

        // Scoot to the end of the new chain, and
        // link the trailing stuff into place

        while newchain.Next <> nil do
        Begin
          // Standard procedure now; we always have to determine base/alias status
          // (an object must NEVER EVER be neither!)

          NewChain.AddToBaseOrCopies(True);
          newchain := newchain.Next;
        End; // While

        // Standard procedure now; we always have to determine base/alias status
        // (an object must NEVER EVER be neither!)        

        NewChain.AddToBaseOrCopies(True);
        newchain.Next := p.next;

        // Remove the old object and point to the end of the new one

        p.Free;
        p := newchain;
      end;
    end;

    tail := p;
    p    := p.Next;
  end; // While

  InvalidateSelect(true);
  DisplaySelectedSize;
end; // MapCollection.DecomposeSelected

procedure MapCollection.DecomposeForPrintout(MapView: ViewPoint);

  function DecomposeChain(var head, tail: DrawPrimitive): boolean;
  var p, newchain: DrawPrimitive;
  begin
    p      := head;
    tail   := nil;
    result := false;

    while p <> nil do
    begin
      case p.GetId of
        'G',
        'S':
        begin
          // Return true if there were any groups so we can
          // keep repeating the application of decompose until
          // there's no symbols at all.  Otherwise, symbols in
          // a group will escape decomposition.
          if p.GetId = 'G' then Result := true;

          if p.Decompose(MapView, newchain) then
          begin
            SetMapChain(newchain);
            // Hook decomposed chain into place of object

            if tail = nil
             then head      := newchain
             else tail.Next := newchain;

            // Scoot to the end of the new chain, and
            // link the trailing stuff into place

            while newchain.Next <> nil do
            Begin
              // Standard procedure now; we always have to determine base/alias status
              // (an object must NEVER EVER be neither!)

              NewChain.AddToBaseOrCopies;
              newchain := newchain.Next;
            End; // While

            // Standard procedure now; we always have to determine base/alias status
            // (an object must NEVER EVER be neither!)

            NewChain.AddToBaseOrCopies;
            newchain.Next := p.next;

            // Remove the old object and point to the end of the new one

            p.Free;
            p := newchain;
          end;
        end;
      end; // Case

      tail := p;
      p    := p.Next;
    end; // While
  end; // DecomposeChain

begin
  while DecomposeChain(head, tail) do
  begin
  end; // While
end; // MapCollection.DecomposeForPrintout

Function MapCollection.GetOverlaysAsDOMElement(D: TDOMDocument): TDOMElement;
Var
  I,N : Integer;
  S   : String;
  E   : TDOMElement;
  E1  : TDOMElement;

Begin
  E := D.createElement('OVERLAYS');
  With MainForm Do
  Begin
    N := OverlayList.Items.Count;
    E.appendChild(NewIntegerProperty(D,'COUNT',N));
    For I := 0 To N - 1 Do
    Begin
      S := OverlayList.Items[I];
      E1 := D.createElement('OVERLAY_' + IntToStr(I));
      E1.appendChild(NewStringProperty(D,'NAME',MimeEncodeString(S)));
      E.appendChild(E1);
    End; // For I
  End;
  Result := E;
End; // MapCollection.GetOverlaysAsDOMElement

Procedure MapCollection.LoadOverlaysFromDOMElement(E: TDOMElement);
Var
  I,J,N      : Integer;
  LastActive : Integer;
  SA         : Array Of String;
  E1         : TDOMElement;
  St         : String;

Begin
  With MainForm Do
  Begin
    LastActive := ActiveOverlay.ItemIndex;

    N := GetIntegerProperty(E,'COUNT');
    If N > 0 Then
    Begin
      SetLength(SA,N);
      For I := 0 To N - 1 Do SA[I] := '';
      E1 := E.findFirstChildElement;
      While E1 <> Nil Do
      Begin
        If System.Copy(E1.tagName,1,8) = 'OVERLAY_' Then
        Begin
          St := Trim(E1.tagName);
          Val(System.Copy(St,9,Length(St)),I,J);
          If (I >= 0) And (I < N) Then
           SA[I] := MimeDecodeString(Trim(GetStringProperty(E1,'NAME')));
        End;
        E1 := E1.findNextSiblingElement;
      End; // While

      For I := 0 To N - 1 Do
      Begin
        If (I >= OverlayList.Items.Count) Or (SA[I] <> OverlayList.Items[I]) Then
        Begin
          ActiveOverlay.Items.Insert(I, SA[I]);
          OverlayList.Items.Insert(I, SA[I]);
          OverlayList.Checked[I] := True;
        End;
      End; // For I
      SetLength(SA,0);
    End;
    While N < OverlayList.Items.Count Do DeleteOverlay(N);

    ActiveOverlay.ItemIndex := LastActive;
    OverlayList.ItemIndex   := LastActive;

    UpdateOverlayImages(OverlaysInUse);
  End;
End; // MapCollection.LoadOverlaysFromDOMElement

procedure MapCollection.SaveOverlaysToStream(stream: TStream);
var
  i, n: integer;
  s: string;
begin
  with MainForm do
    begin
      n := OverlayList.Items.Count;
      stream.WriteBuffer(n, sizeof(n));
      for i := 0 to n - 1 do
        begin
          s := OverlayList.Items[i];
          WriteStringToStream(stream, s);
        end;
    end;
end;

procedure MapCollection.LoadOverlaysFromStream(stream: TStream);
var
  i, n: integer;
  s: string;
  LastActive: integer;
begin
  with MainForm do
    begin
      LastActive := ActiveOverlay.ItemIndex;

      stream.ReadBuffer(n, sizeof(n));
      for i := 0 to n - 1 do
        begin
          s := ReadStringFromStream(stream);
          if (i >= OverlayList.Items.Count) or (s <> OverlayList.Items[i]) then
            begin
              ActiveOverlay.Items.Insert(i, s);
              OverlayList.Items.Insert(i, s);
              OverlayList.Checked[i] := true;
            end;
        end;
      while (n < OverlayList.Items.Count) do
        DeleteOverlay(n);

      ActiveOverlay.ItemIndex := LastActive;
      OverlayList.ItemIndex := LastActive;

      UpdateOverlayImages(OverlaysInUse);
    end;
end;

Procedure MapCollection.CleanupBaseList;
// As DrawPrimitives are added and deleted, the BasePrimitives list can wind up
// containing lots of empty (Nil) entries.  It is good to occasionally clean up
// this list so as to take up as little space as possible, especially before
// saving a map file.  This routine cleans up the list by removing empty slots.
// Since all DrawPrimitives contain actual pointers to their base objects, it
// isn't all that hard to do.
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To BasePrimitives.Count - 1 Do
  Begin

    // Skip any empty entries

    If BasePrimitives.Objects[I] <> Nil Then
    Begin

      // Find the first empty entry we can find without passing our current
      // position

      While (BasePrimitives.Objects[J] <> Nil) And (J < I) Do Inc(J);

      // If we found an empty entry that is before this one, swap the two

      If I <> J Then
      Begin
        BasePrimitives.Exchange(I,J);

        // Move up one entry since we filled this one

        Inc(J);
      End;
    End;
  End; // For I

  // Shorten the base list now that everything has been moved to the front

  I := BasePrimitives.Count - 1;
  While (I >= 0) And (BasePrimitives.Objects[I] = Nil) Do Dec(I);
  J := BasePrimitives.Count - 1;
  While J > I Do
  Begin
    BasePrimitives.Delete(J);
    Dec(J);
  End; // While
End; // MapCollection.CleanupBaseList

Procedure MapCollection.ReadFromDOMElement(E: TDOMElement; Selected: Boolean; Version: Integer = CURRENT_MAP_VERSION; Insert: Boolean = False);
Var
  NewHead : DrawPrimitive;
  Chunk   : TDOMElement;
  Root    : TDOMElement;

  Procedure LoadGlobals;
  Var
    CurGrid, BkColor : TColor;
    OldGrid, OldBack : TNotifyEvent;

  Begin
    // Load globals associated with the map

    CurGrid := TColor(GetCardinalProperty(Chunk,'CURRENT_GRID_COLOR'));
    BkColor := TColor(GetCardinalProperty(Chunk,'BACKGROUND_COLOR'));

    If Not Insert Then
    Begin
      CurrentGridColor := CurGrid;
      With MainForm Do
      Begin
        OldGrid                  := GridColor.OnChange;
        OldBack                  := BackgroundColor.OnChange;
        GridColor.OnChange       := Nil;
        BackgroundColor.OnChange := Nil;

        BackgroundColor.Color    := BkColor;
        Color                    := BkColor;
        GridColor.Color          := CurrentGridColor;

        BackgroundColor.OnChange := OldBack;
        GridColor.OnChange       := OldGrid;
      End;
    End;
  End; // LoadGlobals

  Procedure LoadGrid;
  Begin
    // Load Grid settings

    If Not Insert Then
    Begin
      MapSettingsDialog.SnapToGrid.Checked    := GetBooleanProperty(Chunk,'SNAPTOGRID');
      MapSettingsDialog.SnapToPoint.Checked   := GetBooleanProperty(Chunk,'SNAPTOPOINT');
      MapSettingsDialog.cbSnapTo.Checked      := GetBooleanProperty(Chunk,'SNAPALONG');
      MapSettingsDialog.cbRotateSnap.Checked  := GetBooleanProperty(Chunk,'ROTATESNAP');
      MapSettingsDialog.DisplayGrid.Checked   := GetBooleanProperty(Chunk,'DISPLAYGRID');
      MapSettingsDialog.DesignGridUnits.Value := GetIntegerProperty(Chunk,'DESIGNGRIDUNITS');
    End;
  End; // LoadGrid

  Procedure LoadViewpoints;
  Var
    I     : Integer;
    Views : Integer;
    View  : ViewPoint;
    E     : TDOMElement;
    VA    : Array Of ViewPoint;

  Begin
    // Load the viewpoints

    Views := GetIntegerProperty(Chunk,'COUNT');

    // If inserting a map, throw the views away

    If Not Insert Then
    Begin
      RemoveAllViews;
      If Views > 0 Then
      Begin
        E    := Chunk.findFirstChildElement;
        SetLength(VA,Views); // Do this to guarantee preservation of the order
        For I := 0 To Views - 1 Do VA[I] := Nil;
        I := -1; // reinitialize for below
        While E <> Nil Do
        Begin
          // 2003/05/21 - J.Friant
          // We were trying to grab the ID value from the
          // VIEWPOINT node (which doesn't work) so I moved
          // it so we could see the right values.
          If E.tagName = 'ID' Then
          Begin
            I := GetIntegerProperty(E,'ID');
          End;
          If E.tagName = 'VIEWPOINT' Then
          Begin
            View := ViewPoint.Create;
            View.LoadFromDOMElement(E);
            View.SetCoordinateSize(Screen.Width, Screen.Height, False);
            If (I >= 0) And (I < Views) Then VA[I] := View;
          End;
          E := E.findNextSiblingElement;
        End; // While
        For I := 0 To Views - 1 Do
         If VA[I] <> Nil Then
         Begin
           SaveViewPoint(VA[I],VA[I].Name);
           VA[I].Free;
         End;
        SetLength(VA,0);
      End;
    End;
  End; // LoadViewpoints

  Procedure ReadPushPinCount(Var PushPins: Integer; Var IgnorePins: Integer);
  Begin
    // Start by assuming we'll read all pushpins
    PushPins   := GetIntegerProperty(Chunk,'COUNT');
    IgnorePins := 0;

    // If inserting map, throw all pushpins away.
    If Insert Then
    Begin
      IgnorePins := PushPins;
      PushPins   := 0;
    End
    Else
    Begin
      // Otherwise, if there are more pushpins than we have
      // available, ignore the rest of them.
      If PushPins > PushPinCount Then
      Begin
        IgnorePins := PushPins - PushPinCount;
        PushPins   := PushPinCount;
      End;
    End;
  End; // ReadPushPinCount

  Procedure LoadPushPins;
  Var
    I,J        : Integer;
    PushPins   : Integer;
    IgnorePins : Integer;
    E          : TDOMElement;
    St         : String;

  Begin
    ReadPushPinCount(PushPins,IgnorePins);

    // Read the pushpins we can handle
    E := Chunk.findFirstChildElement;
    While E <> Nil Do
    Begin
      If System.Copy(E.tagName,1,4) = 'PIN_' Then
      Begin
        St := Trim(E.tagName);
        Val(System.Copy(St,5,Length(St)),I,J);
        If (I >= 0) And (I < PushPins) Then
        Begin
          MainForm.PushPinList.Checked[I] := GetBooleanProperty(E,'CHECKED');
          If GetBooleanProperty(E,'PLACED') Then
           PushPinHistoryPointAdd(I,GetCoordPointProperty(E,'POINT'),'');
        End;
      End;
      E := E.findNextSiblingElement;
    End; // While
  End; // LoadPushPins

  Procedure LoadPushPinHistory;
  Var
    PushPins   : Integer;
    IgnorePins : Integer;
    I,J,K,V    : Integer;
    E,E1       : TDOMElement;
    St         : String;

  Begin
    For I := 0 To PushPinCount - 1 Do PushPinHistoryClear(I);

    ReadPushPinCount(PushPins,IgnorePins);
    If Not Insert Then
    Begin
      PushPinFlags := [];
      If GetBooleanProperty(Chunk,'WAYPOINTS_VISIBLE') Then PushPinFlags := PushPinFlags + [PP_WaypointsVisible];
      If GetBooleanProperty(Chunk,'SHOW_NUMBER')       Then PushPinFlags := PushPinFlags + [PP_ShowNumber];
      If GetBooleanProperty(Chunk,'SHOW_NOTE')         Then PushPinFlags := PushPinFlags + [PP_ShowNote];
    End;

    // Read push pin histories and names
    E := Chunk.findFirstChildElement;
    While E <> Nil Do
    Begin
      If System.Copy(E.tagName,1,12) = 'PIN_HISTORY_' Then
      Begin
        St := Trim(E.tagName);
        Val(System.Copy(St,13,Length(St)),I,J);
        If (I >= 0) And (I < PushPins) Then
        Begin
          PushPinName[I] := MimeDecodeString(Trim(GetStringProperty(E,'NAME')));
          V              := GetIntegerProperty(E,'COUNT');
          E1             := E.findFirstChildElement;
          While E1 <> Nil Do
          Begin
            If System.Copy(E1.tagName,1,5) = 'HIST_' Then
            Begin
              St := Trim(E.tagName);
              Val(System.Copy(St,6,Length(St)),J,K);
              If (J >= 1) And (J < V) Then
               PushPinHistoryPointAdd(I,GetCoordPointProperty(E1,'POINT'),
                                      MimeDecodeString(Trim(GetStringProperty(E1,'NOTE'))));
            End;
            E1 := E1.findNextSiblingElement;
          End; // While
        End;
      End;
      E := E.findNextSiblingElement;
    End; // While
  End; // LoadPushPinHistory

  Procedure LoadLandscape;
  Begin
    If Not Insert Then fLandscape := GetBooleanProperty(Chunk,'LANDSCAPE');
  End; // LoadLandscape

  Procedure LoadComments;
  Begin
    If Not Insert Then MapComments := MimeDecodeString(Trim(GetStringProperty(Chunk,'COMMENTS')));
  End; // LoadComments

  Procedure LoadOverlays;
  Begin
    // If inserting a map, throw away all overlays...

    If Not Insert Then LoadOverlaysFromDOMElement(Chunk);
  End; // LoadOverlays

Begin
  NewHead := Nil;

  Try
    Root := E.getFirstChildElement('MAP');
    If Root <> Nil Then
    Begin
      Chunk := Root.findFirstChildElement;
      While Chunk <> Nil Do
      Begin
             If Chunk.tagName = 'GLOBALS'         Then LoadGlobals
        Else If Chunk.tagName = 'COMMENTSET'      Then LoadComments
        Else If Chunk.tagName = 'OVERLAYS'        Then LoadOverlays
        Else If Chunk.tagName = 'GRID'            Then LoadGrid
        Else If Chunk.tagName = 'VIEWPOINTS'      Then LoadViewpoints
        Else If Chunk.tagName = 'PUSHPINS'        Then LoadPushPins
        Else If Chunk.tagName = 'PUSHPIN_HISTORY' Then LoadPushPinHistory
        Else If Chunk.tagName = 'LANDSCAPESET'    Then LoadLandscape
        Else If Chunk.tagName = 'MAP_CONTENTS'    Then
          NewHead := DrawPrimitive.ReadChainFromDOMElement(Chunk,Version,Selected,Self)
        Else
        Begin
          ShowMessage(res_mapobj_file_chunk_un);
          Exit;
        End;
        Chunk := Chunk.findNextSiblingElement;
      End; // While
    End;
  Except
    ShowMessage(res_mapobj_file_end);
  End;

  // Fixup pointers
  If Tail = Nil Then
  begin
    Head := NewHead;
    Tail := NewHead;
  End
  Else Tail.Next := NewHead;

  // Chase to the new tail (if there is one)
  If Tail <> Nil Then
  Begin
    While Tail.Next <> Nil Do Tail := Tail.Next;
  End;
End; // MapCollection.ReadFromDOMElement

Procedure MapCollection.ReadFromDOMDocument(D: TDOMDocument; Selected: Boolean; Version: Integer = CURRENT_MAP_VERSION; Insert: Boolean = False);
Begin
  ReadFromDOMElement(D.documentElement,Selected,Version,Insert);
End; // MapCollection.ReadFromDOMDocument

procedure MapCollection.Read(stream: TStream; selected,Full,UseAliasInfo: boolean; version:integer; insert: boolean);
var
  newhead: DrawPrimitive;
  chunkid: smallint;
  done: boolean;
  chunktag: array[0..3] of char;

  procedure LoadColors;
  var
    curgrid, bkcolor: TColor;
    oldgrid, oldback: TNotifyEvent;
  begin
    { Load globals associated with the map }
    stream.ReadBuffer(curgrid, sizeof(curgrid));
    stream.ReadBuffer(bkcolor, sizeof(bkcolor));

    if not insert then
      begin
        CurrentGridColor := curgrid;
        with MainForm do
          begin
            oldgrid := GridColor.OnChange;
            oldback := BackgroundColor.OnChange;
            GridColor.OnChange := nil;
            BackgroundColor.OnChange := nil;

            BackgroundColor.Color := bkcolor;
            Color := bkcolor;
            GridColor.Color := CurrentGridColor;

            BackgroundColor.OnChange := oldback;
            GridColor.OnChange := oldgrid;
          end;
      end;
  end;

  procedure LoadGrid;
  var
    b1, b2, b3: boolean;
    v: integer;
  begin
    { Load Grid settings }
    stream.ReadBuffer(b1, sizeof(b1));
    stream.ReadBuffer(b2, sizeof(b2));
    stream.ReadBuffer(b3, sizeof(b3));
    stream.ReadBuffer(v, sizeof(v));
    if not insert then
    begin
      MapSettingsDialog.SnapToGrid.Checked    := b1;
      MapSettingsDialog.SnapToPoint.Checked   := b2;
      MapSettingsDialog.cbSnapTo.Checked      := b2; // JD 8-6-02
      MapSettingsDialog.DisplayGrid.Checked   := b3;
      MapSettingsDialog.DesignGridUnits.Value := v;
    end;
  end;

  procedure LoadViewpoints;
  var
    i: integer;
    views: integer;
    view: ViewPoint;
  begin
    { Load the viewpoints }
    stream.ReadBuffer(views, sizeof(views));

    if insert then
      begin // If inserting a map, throw the views away
        view := ViewPoint.Create;
        for i := 0 to views - 1 do
          view.LoadFromStream(stream);
        view.Free;
        exit;
      end;

    RemoveAllViews;
    view := ViewPoint.Create;
    for i := 0 to views - 1 do
      begin
        view.LoadFromStream(stream);
        view.SetCoordinateSize(Screen.Width, Screen.Height, false);
        SaveViewPoint(view, view.Name);
      end;
    view.Free;
  end;

  procedure ReadPushPinCount(var pushpins:integer; var ignorepins:integer);
  begin
    // Start by assuming we'll read all pushpins
    stream.ReadBuffer(pushpins, sizeof(pushpins));
    ignorepins:=0;

    // If inserting map, throw all pushpins away.
    if insert then begin
      ignorepins := pushpins;
      pushpins := 0;
      end
    else begin
      // Otherwise, if there are more pushpins than we have
      // available, ignore the rest of them.
      if pushpins > PushPinCount then begin
        ignorepins := pushpins - PushPinCount;
        pushpins := PushPinCount;
      end;
    end;
  end;

  procedure LoadPushPins;
  var
    i: integer;
    b: boolean;
    pushpins, ignorepins: integer;
    pt: CoordPoint;
  begin
    ReadPushPinCount(pushpins,ignorepins);

    // Read the pushpins we can handle
    for i := 0 to pushpins - 1 do begin
        stream.ReadBuffer(b, sizeof(b));
        MainForm.PushPinList.Checked[i] := b;
        stream.ReadBuffer(b, sizeof(b));
        if (b) then
          begin
            stream.ReadBuffer(pt, sizeof(CoordPoint));
            PushPinHistoryPointAdd(i, pt, '');
          end;
      end;

    // Ignore the rest of the pushpins in the file, but consume them
    // out of the stream.
    for i := 1 to ignorepins do
      begin
        stream.ReadBuffer(b, sizeof(b));
        stream.ReadBuffer(b, sizeof(b));
        if b then stream.ReadBuffer(pt, sizeof(pt));
      end;
  end;

  procedure LoadPushPinHistory;
  var
    pushpins, ignorepins: integer;
    cp: CoordPoint;
    i, j, v:integer;
    s:string;
    ps:PushPinFlagSet;
  begin
    for i := 0 to PushPinCount - 1 do begin
      PushPinHistoryClear(i);
      end;

    ReadPushPinCount(pushpins,ignorepins);
    stream.ReadBuffer(ps, sizeof(ps));
    if (not insert) then pushpinflags := ps;

    // Read push pin histories and names
    for i:=0 to pushpins-1 do begin
       s := ReadStringFromStream(stream);
       PushPinName[i] := s;
       stream.ReadBuffer(v, sizeof(v));
       for j:=v-1 downto 1 do begin
         stream.ReadBuffer(cp, sizeof(cp));
         s := ReadStringFromStream(stream);
         PushPinHistoryPointAdd(i, cp, s);
         end;
    end;

    // Read and throw away remainder of unused pins
    for i:=1 to ignorepins do begin
       s := ReadStringFromStream(stream);
       stream.ReadBuffer(v, sizeof(v));
       for j:=v-1 downto 1 do begin
         stream.ReadBuffer(cp, sizeof(cp));
         s := ReadStringFromStream(stream);
         end;
    end;
  end;

  procedure LoadLandscape;
  var
    b: boolean;
  begin
    stream.ReadBuffer(b, sizeof(b));
    if not insert then fLandscape := b;
  end;

  procedure LoadComments;
  var
    s: string;
  begin
    s := ReadStringFromStream(stream);
    if not insert then MapComments := s;
  end;

  procedure LoadOverlays;
  var
    i, n: integer;
    s: string;
  begin
    if insert then
      begin // If inserting a map, throw away all overlays...
        stream.ReadBuffer(n, sizeof(n));
        for i := 1 to n do
          s := ReadStringFromStream(stream);
      end
    else
      LoadOverlaysFromStream(stream);
  end;

begin
  newhead := nil;
  done := false;

  try
    repeat
      stream.ReadBuffer(chunktag, sizeof(chunktag));
      if (chunktag <> '<CH>') then
        begin
          ShowMessage(res_mapobj_file_chunk_no);
          exit;
        end;

      stream.ReadBuffer(chunkid, sizeof(chunkid));

      case chunkid of
        ord('C') + 256 * ord('O'): LoadColors;
        ord('C') + 256 * ord('M'): LoadComments;
        ord('O') + 256 * ord('V'): LoadOverlays;
        ord('G') + 256 * ord('R'): LoadGrid;
        ord('V') + 256 * ord('W'): LoadViewpoints;
        ord('P') + 256 * ord('P'): LoadPushPins;
        ord('P') + 256 * ord('H'): LoadPushPinHistory;
        ord('L') + 256 * ord('A'): LoadLandscape;
        ord('O') + 256 * ord('B'):
            newhead := DrawPrimitive.ReadChain(stream, version, selected, Full, UseAliasInfo, Self);
        ord('S') + 256 * ord('E'): ReadSelectStates(stream, newhead);
        ord('E') + 256 * ord('O'): done := true;
        else
          begin
            ShowMessage(res_mapobj_file_chunk_un);
            exit;
          end;
      end;
    until done;
  except
    ShowMessage(res_mapobj_file_end);
  end;

  { Fixup pointers }
  if (tail = nil) then
    begin
      head := newhead;
      tail := newhead;
    end
  else
    tail.Next := newhead;

  // Chase to the new tail (if there is one)
  if (tail <> nil) then
    begin
      while (tail.Next <> nil) do
        tail := tail.Next;
    end;
end; // MapCollection.Read

Function MapCollection.GetAsDOMElement(D: TDOMDocument; All: Boolean; SaveSet: ChunkSet): TDOMElement;
Var
  Root                  : TDOMElement;
  Chunk                 : TDOMElement;
  E,E1                  : TDOMElement;

  I, J, Views, PushPins : integer;
  View                  : ViewPoint;
  B                     : boolean;
  V                     : integer;
  BackgroundColor       : TColor;
  CP                    : CoordPoint;

Begin
  CleanupBaseList;
  Root := D.createElement('MAP');
  If SaveSet = [] Then
   SaveSet := [CO_CHUNK, CM_CHUNK, OV_CHUNK, GR_CHUNK, VW_CHUNK,
               PP_CHUNK, OB_CHUNK, EO_CHUNK, LA_CHUNK, PH_CHUNK];

  // Save globals associated with the map
  If CO_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('GLOBALS');
    // Note: need to use MainForm.Color here, not Background.Color, because
    // the button's color has already been changed, and we need to preserve
    // the old (still current until we return from this function) color
    // for correct operation of undo.  Under any other normal circumstance,
    // MainForm.Color and Background.Color are equal.
    Backgroundcolor := MainForm.Color;
    Chunk.appendChild(NewCardinalProperty(D,'VERSION',CURRENT_MAP_VERSION));
    Chunk.appendChild(NewCardinalProperty(D,'CURRENT_GRID_COLOR',Cardinal(CurrentGridColor)));
    Chunk.appendChild(NewCardinalProperty(D,'BACKGROUND_COLOR',Cardinal(BackgroundColor)));
    Root.appendChild(Chunk);
  End;

  If CM_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('COMMENTSET');
    Chunk.appendChild(NewStringProperty(D,'COMMENTS',MimeEncodeString(MapComments)));
    Root.AppendChild(Chunk);
  End;

  If OV_CHUNK In SaveSet Then Root.AppendChild(GetOverlaysAsDOMElement(D));

  If LA_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('LANDSCAPESET');
    Chunk.appendChild(NewBooleanProperty(D,'LANDSCAPE',LandScape));
    Root.AppendChild(Chunk);
  End;

  // Save Grid settings
  If GR_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('GRID');
    Chunk.appendChild(NewBooleanProperty(D,'SNAPTOGRID',MapSettingsDialog.SnapToGrid.Checked));
    Chunk.appendChild(NewBooleanProperty(D,'SNAPTOPOINT',MapSettingsDialog.SnapToPoint.Checked));
    Chunk.appendChild(NewBooleanProperty(D,'SNAPALONG',MapSettingsDialog.cbSnapTo.Checked));
    Chunk.appendChild(NewBooleanProperty(D,'ROTATESNAP',MapSettingsDialog.cbRotateSnap.Checked));
    Chunk.appendChild(NewBooleanProperty(D,'DISPLAYGRID',MapSettingsDialog.DisplayGrid.Checked));
    Chunk.appendChild(NewIntegerProperty(D,'DESIGNGRIDUNITS',MapSettingsDialog.DesignGridUnits.Value));
    Root.AppendChild(Chunk);
  End;

  // Save the viewpoints
  If VW_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('VIEWPOINTS');
    If Not (UNDO_OPERATION In SaveSet) Then
    Begin
      View := GetCurrentViewPoint;
      SaveViewPoint(View, '');
    End;

    Views := ViewList.Count;
    Chunk.appendChild(NewIntegerProperty(D,'COUNT',Views));
    For I := 0 to Views - 1 Do
    Begin
      View := GetViewPoint(I);
      Chunk.appendChild(NewIntegerProperty(D,'ID',I));
      Chunk.appendChild(View.GetAsDOMElement(D));
    End; // For I
    Root.AppendChild(Chunk);
  End;

  // Save pushpin history:
  // NOTE: this chunk must come before the pushpin chunk
  // if it exists in the stream. 
  If PH_CHUNK In SaveSet Then
  Begin
    Chunk    := D.createElement('PUSHPIN_HISTORY');
    PushPins := PushPinCount;
    Chunk.appendChild(NewIntegerProperty(D,'COUNT',PushPins));
    Chunk.appendChild(NewBooleanProperty(D,'WAYPOINTS_VISIBLE',PP_WaypointsVisible In SPushPinFlags));
    Chunk.appendChild(NewBooleanProperty(D,'SHOW_NUMBER',      PP_ShowNumber       In SPushPinFlags));
    Chunk.appendChild(NewBooleanProperty(D,'SHOW_NOTE',        PP_ShowNote         In SPushPinFlags));
    For I := 0 To PushPins - 1 Do
    Begin
      E := D.createElement('PIN_HISTORY_' + IntToStr(I));
      E.appendChild(NewStringProperty(D,'NAME',MimeEncodeString(PushPin[I].Name)));
      V := PushPinHistoryCount[I];
      E.appendChild(NewIntegerProperty(D,'COUNT',V));
      Chunk.appendChild(E);
      For J := V - 1 DownTo 1 Do
      Begin
        E1 := D.createElement('HIST_' + IntToStr(J));
        CP := PushPinHistoryPoint[I,J];
        E1.appendChild(NewCoordPointProperty(D,'POINT',CP));
        E1.appendChild(NewStringProperty(D,'NOTE',MimeEncodeString(PushPinHistoryNote[I,J])));
        E.appendChild(E1);
      End; // For J
    End; // For I
    Root.AppendChild(Chunk);
  End;

  // Save the pushpins
  If PP_CHUNK In SaveSet Then
  Begin
    Chunk    := D.createElement('PUSHPINS');
    PushPins := PushPinCount;
    Chunk.appendChild(NewIntegerProperty(D,'COUNT',PushPins));
    For I := 0 to PushPins - 1 Do
    Begin
      E := D.createElement('PIN_' + IntToStr(I));
      Chunk.appendChild(E);
      B := MainForm.PushPinList.Checked[I];
      E.appendChild(NewBooleanProperty(D,'CHECKED',B));
      If PushPinPlaced(I) Then
      Begin
        E.appendChild(NewBooleanProperty(D,'PLACED',True));
        E.appendChild(NewCoordPointProperty(D,'POINT',PushPinHistoryPoint[I,0]));
      End
      Else E.appendChild(NewBooleanProperty(D,'PLACED',False));
    End; // For I
    Root.AppendChild(Chunk);
  End;

  // Save the map contents (and selection states if need be)
  If OB_CHUNK In SaveSet Then
  Begin
    Chunk := D.createElement('MAP_CONTENTS');
    Chunk.appendChild(Head.GetChainAsDOMElement(D,All,UNDO_OPERATION In SaveSet));
    Root.AppendChild(Chunk);
  End;
  Result := Root;
End; // MapCollection.GetAsDOMElement

Function MapCollection.GetAsDOMDocument(All: Boolean; SaveSet: ChunkSet): TDOMDocument;
Var
  D    : TDOMDocument;
  Root : TDOMElement;

Begin
  D    := MainForm.DOMImpl.createDocument('DOCUMENT',Nil);
  Root := GetAsDOMElement(D,All,SaveSet);
  D.DocumentElement.appendChild(Root);
  Result := D;
End; // MapCollection.GetAsDOMDocument

procedure MapCollection.Write(stream: TStream; all,Full,UseAliasInfo: boolean; saveset: ChunkSet);
var
  i, j, views, pushpins: integer;
  view: ViewPoint;
  b: boolean;
  v: integer;
  backgroundcolor: TColor;
  cp: CoordPoint;

  procedure LabelChunk(s: string);
  const
    tag: array[0..3] of char = '<CH>';
  var
    sw: Smallint;
  begin
    stream.WriteBuffer(tag, sizeof(tag));
    sw := ord(s[1]) + (ord(s[2]) shl 8);
    stream.WriteBuffer(sw, sizeof(sw));
  end;

begin
  CleanupBaseList;
  if saveset = [] then
    saveset := [CO_CHUNK, CM_CHUNK, OV_CHUNK, GR_CHUNK, VW_CHUNK,
      PP_CHUNK, OB_CHUNK, EO_CHUNK, LA_CHUNK, PH_CHUNK];

  { Save globals associated with the map }
  if (CO_CHUNK in saveset) then
    begin
      LabelChunk('CO');
      // Note: need to use MainForm.Color here, not Background.Color, because
      // the button's color has already been changed, and we need to preserve
      // the old (still current until we return from this function) color
      // for correct operation of undo.  Under any other normal circumstance,
      // MainForm.Color and Background.Color are equal.
      backgroundcolor := MainForm.Color;
      stream.WriteBuffer(CurrentGridColor, sizeof(CurrentGridColor));
      stream.WriteBuffer(backgroundcolor, sizeof(backgroundcolor));
    end;

  if (CM_CHUNK in saveset) then
    begin
      LabelChunk('CM');
      WriteStringToStream(stream, MapComments);
    end;

  if (OV_CHUNK in saveset) then
    begin
      LabelChunk('OV');
      SaveOverlaysToStream(stream);
    end;

  if (LA_CHUNK in saveset) then
    begin
      LabelChunk('LA');
      stream.WriteBuffer(Landscape, sizeof(Landscape));
    end;

  { Save Grid settings }
  if (GR_CHUNK in saveset) then
    begin
      LabelChunk('GR');
      b := MapSettingsDialog.SnapToGrid.Checked;
      stream.WriteBuffer(b, sizeof(b));
      b := MapSettingsDialog.SnapToPoint.Checked;
      stream.WriteBuffer(b, sizeof(b));
      b := MapSettingsDialog.DisplayGrid.Checked;
      stream.WriteBuffer(b, sizeof(b));
      v := MapSettingsDialog.DesignGridUnits.Value;
      stream.WriteBuffer(v, sizeof(v));
    end;

  { Save the viewpoints }
  if (VW_CHUNK in saveset) then
    begin
      LabelChunk('VW');
      if not (UNDO_OPERATION in saveset) then
        begin
          view := GetCurrentViewPoint;
          SaveViewPoint(view, '');
        end;

      views := ViewList.Count;
      stream.WriteBuffer(views, sizeof(views));
      for i := 0 to views - 1 do
        begin
          view := GetViewPoint(i);
          view.SaveToStream(stream);
        end;
    end;

  { Save pushpin history:
    NOTE: this chunk must come before the pushpin chunk
    if it exists in the stream. }
  if (PH_CHUNK in saveset) then begin
     LabelChunk('PH');
     pushpins := PushPinCount;
     stream.WriteBuffer(pushpins, sizeof(pushpins));
     stream.WriteBuffer(spushpinflags, sizeof(spushpinflags));
     
     for i := 0 to pushpins - 1 do begin
        WriteStringToStream(stream, pushpin[i].Name);

        v := PushPinHistoryCount[i];
        stream.WriteBuffer(v, sizeof(v));

        for j:=v - 1 downto 1 do begin
          cp:=PushPinHistoryPoint[i,j];
          stream.WriteBuffer(cp, sizeof(CoordPoint));
          WriteStringToStream(stream, PushPinHistoryNote[i,j]);
        end;
      end;
    end;

  { Save the pushpins }
  if (PP_CHUNK in saveset) then
    begin
      LabelChunk('PP');
      pushpins := PushPinCount;
      stream.WriteBuffer(pushpins, sizeof(pushpins));
      for i := 0 to pushpins - 1 do
        begin
          b := MainForm.PushPinList.Checked[i];
          stream.WriteBuffer(b, sizeof(b));
          if (PushPinPlaced(i)) then
            begin
              b := true;
              stream.WriteBuffer(b, sizeof(b));
              cp := PushPinHistoryPoint[i,0];
              stream.WriteBuffer(cp, sizeof(cp));
            end
          else
            begin
              b := false;
              stream.WriteBuffer(b, sizeof(b));
            end;
        end;
    end;

  { Save the map contents }
  if (OB_CHUNK in saveset) then
    begin
      LabelChunk('OB');
      head.WriteChain(stream, all, Full, UseAliasInfo);
    end;

  { Save the selection states }
  if (UNDO_OPERATION in saveset) then
    begin
      LabelChunk('SE');
      WriteSelectStates(stream);
    end;

  LabelChunk('EO');
end; // MapCollection.Write

procedure MapCollection.Cut;
begin
  if ReadOnlyPrevents then exit;

  SetUndoPoint('Cut');
  Copy;
  DeleteSelected;
end;

function MapCollection.GetBitmap(all: boolean; viewonly:boolean; width,height:integer): TBitmap;
var
  MapView: ViewPoint;
  crView: CoordRect;
  crExtent: CoordRect;
  offx, offy: Coord;
begin
  Result := TBitmap.Create;

  if not AnythingSelected then all := true;

  Result.Width := width;
  Result.Height := height;
  Result.Canvas.Brush.Color := MainForm.BackgroundColor.Color;
  Result.Canvas.Rectangle(0, 0, Result.Width, Result.Height);

  MapView := ViewPoint.Create(CurrentView);
  MapView.OffScreenFullDetail := true;

  crExtent := head.ChainExtent(all);

  if viewonly then begin
    { If getting a bitmap of a selected region at current zoom level,
      we need to line up the viewport with the beginning of the bitmap }
    if not all then begin
      MapView.GetCoordinateRect(crView);
      offx := crExtent.Left - crView.Left;
      offy := crExtent.Top - crView.Top;
      crView.Left := crView.Left + offx;
      crView.Top := crView.Top + offy;
      crView.Right := crView.Right + offx;
      crView.Bottom := crView.Bottom + offy;
      MapView.SetCoordinateRect(crView);
      end;
    end
  else begin
    {Get entire map fit to the bitmap }
    MapView.SetCoordinateSize(Width,Height,true);
    Map.ShowAll(MapView, 0.0);
    end;

  MapView.Canvas := Result.Canvas;
  Draw(MapView, false);
  MapView.Free;
end;

function EnhMetaEnumProc(hdc: HDC; const lpHandleTable: HandleTable; var rec: ENHMETARECORD;
  nObj: integer; extra: PMetaEnumExtra): DWORD; stdcall;

  procedure SelectObject;
  var
    n: integer;
  begin
    n := rec.dParm[0] and $7FFFFFFF; // Clear high bit

    if (rec.dParm[0] and $80000000) <> 0 then
      begin
        extra^.style.Fill := 0;
        extra^.style.Line := 1;
        extra^.style.FullStyle.Thickness  := 0;
        extra^.style.FullStyle.SThickness := 0;
        case n of
          WHITE_BRUSH: CurrentFillColor := clWhite;
          LTGRAY_BRUSH: CurrentFillColor := clSilver;
          GRAY_BRUSH: CurrentFillColor := clGray;
          DKGRAY_BRUSH: CurrentFillColor := clDkGray;
          BLACK_BRUSH: CurrentFillColor := clBlack;
          NULL_BRUSH: CurrentFillColor := clNone;
          WHITE_PEN: CurrentColor := clWhite;
          BLACK_PEN: CurrentColor := clBlack;
          NULL_PEN: extra^.style.Line := 0;
        end;
      end
    else
      begin
        if extra^.HandleObj^[n].Brush then
          begin
            extra^.style.Fill := extra^.HandleObj^[n].Style.Fill;
            CurrentFillColor := extra^.HandleObj^[n].Color;
          end
        else
          begin
            extra^.style.Line := extra^.HandleObj^[n].Style.Line;
            extra^.style.FullStyle.Thickness  := extra^.HandleObj^[n].Style.FullStyle.Thickness;
            extra^.style.FullStyle.SThickness := extra^.HandleObj^[n].Style.FullStyle.SThickness;
            CurrentColor := extra^.HandleObj^[n].Color;
          end;
      end;
  end;

  procedure DoRectangle;
  var
    cx1, cx2: Coord;
    cy1, cy2: Coord;
    list: PCoordArray;
    emrrectangle: PEMRRectangle;
  begin
    emrrectangle := PEMRRectangle(@rec);
    GetMem(list, sizeof(CoordPoint) * 5);

    cx1 := emrrectangle^.rclBox.Left;
    cy1 := emrrectangle^.rclBox.Top;
    cx2 := emrrectangle^.rclBox.Right;
    cy2 := emrrectangle^.rclBox.Bottom;

    list^[0].X := cx1;
    list^[0].y := cy1;
    list^[1].X := cx1;
    list^[1].y := cy2;
    list^[2].X := cx2;
    list^[2].y := cy2;
    list^[3].X := cx2;
    list^[3].y := cy1;
    list^[4].X := cx1;
    list^[4].y := cy1;

    extra^.PMap^.AddObject(PolylinePrimitive.Create(list, 5, extra^.style));
  end;

  procedure DoPolyLine;
  var
    i, j, sz: integer;
    list: PCoordArray;
    emrpolyline: PEMRPolyLine;
    fill: TColor;
  begin
    fill := CurrentFillColor;
    emrpolyline := PEMRPolyLine(@rec);
    sz := emrpolyline^.cptl;
    if (rec.iType = EMR_POLYGON) or (rec.iType = EMR_POLYLINETO) then inc(sz);

    GetMem(list, sizeof(CoordPoint) * sz);

    j := 0;
    if (rec.iType = EMR_POLYLINETO) then
      begin
        inc(j);
        list^[0].x := extra^.currx;
        list^[0].y := extra^.curry;
      end;

    for i := 0 to emrpolyline^.cptl - 1 do
      begin
        list^[j].X := emrpolyline^.aptl[i].x;
        list^[j].y := emrpolyline^.aptl[i].y;
        inc(j);
      end;

    if (rec.iType = EMR_POLYGON) then list^[sz - 1] := list^[0];

    if (rec.iType <> EMR_POLYGON) then CurrentFillColor := clNone;

    extra^.PMap^.AddObject(PolylinePrimitive.Create(list, sz, extra^.style));

    if (rec.iType <> EMR_POLYGON) then CurrentFillColor := fill;
  end;

  procedure DoPolyline16;
  var
    i, j, sz: integer;
    list: PCoordArray;
    emrpolyline16: PEMRPolyLine16;
    fill: TColor;
  begin
    fill := CurrentFillColor;
    emrpolyline16 := PEMRPolyLine16(@rec);
    sz := emrpolyline16^.cpts;
    if (rec.iType = EMR_POLYGON16) or (rec.iType = EMR_POLYLINETO16) then inc(sz);

    GetMem(list, sizeof(CoordPoint) * sz);

    j := 0;

    if rec.iType = EMR_POLYLINETO16 then
      begin
        inc(j);
        list^[0].x := extra^.currx;
        list^[0].y := extra^.curry;
      end;

    for i := 0 to emrpolyline16^.cpts - 1 do
      begin
        list^[j].X := emrpolyline16^.apts[i].x;
        list^[j].y := emrpolyline16^.apts[i].y;
        inc(j);
      end;

    if (rec.iType = EMR_POLYGON16) then list^[sz - 1] := list^[0];

    if (rec.iType <> EMR_POLYGON16) then CurrentFillColor := clNone;

    extra^.PMap^.AddObject(PolylinePrimitive.Create(list, sz, extra^.style));

    if (rec.iType <> EMR_POLYGON) then CurrentFillColor := fill;
  end;

  procedure DoPolyPolygon;
  var
    z, i, j, ct, sz, next: integer;
    list: PCoordArray;
    emrpolypolygon: PEMRPolyPolygon;
    fill: TColor;
  begin
    fill := CurrentFillColor;
    emrpolypolygon := PEMRPolyPolygon(@rec);
    next := emrpolypolygon^.nPolys - 1;

    for z := 1 to emrpolypolygon^.nPolys do
      begin
        sz := emrpolypolygon.aPolyCounts[z - 1];
        ct := sz;
        if (rec.iType = EMR_POLYPOLYGON) then inc(sz);

        GetMem(list, sizeof(CoordPoint) * sz);

        j := 0;

        for i := 0 to ct - 1 do
          begin
            list^[j].X := emrpolypolygon^.aptl[next + i].x;
            list^[j].y := emrpolypolygon^.aptl[next + i].y;
            inc(j);
          end;

        if (rec.iType = EMR_POLYPOLYGON) then list^[sz - 1] := list^[0];

        if (rec.iType <> EMR_POLYPOLYGON) then CurrentFillColor := clNone;

        extra^.PMap^.AddObject(PolylinePrimitive.Create(list, sz, extra^.style));

        if (rec.iType <> EMR_POLYPOLYGON) then CurrentFillColor := fill;
        inc(next, ct);
      end;
  end;

  procedure DoPolyPolygon16;
  var
    z, i, j, ct, sz, next: integer;
    list: PCoordArray;
    emrpolypolygon16: PEMRPolyPolygon16;
    fill: TColor;
  begin
    emrpolypolygon16 := PEMRPolyPolygon16(@rec);
    next := emrpolypolygon16^.nPolys - 1;
    fill := CurrentFillColor;

    for z := 1 to emrpolypolygon16^.nPolys do
      begin
        sz := emrpolypolygon16.aPolyCounts[z - 1];
        ct := sz;
        if (rec.iType = EMR_POLYPOLYGON16) then inc(sz);

        GetMem(list, sizeof(CoordPoint) * sz);

        j := 0;

        for i := 0 to ct - 1 do
          begin
            list^[j].X := emrpolypolygon16^.apts[next + i].x;
            list^[j].y := emrpolypolygon16^.apts[next + i].y;
            inc(j);
          end;

        if (rec.iType = EMR_POLYPOLYGON16) then list^[sz - 1] := list^[0];

        if (rec.iType <> EMR_POLYPOLYGON16) then CurrentFillColor := clNone;

        extra^.PMap^.AddObject(PolylinePrimitive.Create(list, sz, extra^.style));

        if (rec.iType <> EMR_POLYPOLYGON16) then CurrentFillColor := fill;
        inc(next, ct);
      end;
  end;

  procedure CreatePen;
  var
    emrpen: PEMRCreatePen;
  begin
    emrpen := PEMRCreatePen(@rec);
    extra^.HandleObj^[emrpen^.ihPen].Color := emrpen^.lopn.lopnColor;
    extra^.HandleObj^[emrpen^.ihPen].Brush := false;
    //      case emrpen^.lopn.lopnWidth.X of
    //        0,1: begin
    //          case emrpen^.lopn.lopnStyle of
    //            PS_DASH:         sz:=numberthicknesses+3;
    //            PS_DOT:          sz:=numberthicknesses+0;
    //            PS_DASHDOT:      sz:=numberthicknesses+1;
    //            PS_DASHDOTDOT:   sz:=numberthicknesses+2;
    //            PS_NULL:         sz:=0;
    //            else
    //              sz:=1;
    //            end;
    //          end;
    //        2..numberthicknesses: begin
    //          sz := emrpen^.lopn.lopnWidth.X;
    //          end;
    //        else
    //          sz:= numberthicknesses;
    //        end;
    if (emrpen^.lopn.lopnStyle = PS_NULL) then
      extra^.HandleObj^[emrpen^.ihPen].Style.Line := 0
    else
      extra^.HandleObj^[emrpen^.ihPen].Style.Line := 1;
    extra^.HandleObj^[emrpen^.ihPen].Style.FullStyle.Thickness  := 0;
    extra^.HandleObj^[emrpen^.ihPen].Style.FullStyle.SThickness := 0;
  end;

  procedure CreateExtPen;
  var
    emrextpen: PEMRExtCreatePen;
  begin
    emrextpen := PEMRExtCreatePen(@rec);
    extra^.HandleObj^[emrextpen^.ihPen].Color := emrextpen^.elp.elpColor;
    extra^.HandleObj^[emrextpen^.ihPen].Brush := false;
    extra^.HandleObj^[emrextpen^.ihPen].Style.Line := 1;
    extra^.HandleObj^[emrextpen^.ihPen].Style.FullStyle.Thickness  := 0;
    extra^.HandleObj^[emrextpen^.ihPen].Style.FullStyle.SThickness := 0;
  end;

  procedure CreateBrush;
  var
    emrbrush: PEMRCreateBrushIndirect;
  begin
    emrbrush := PEMRCreateBrushIndirect(@rec);
    extra^.HandleObj^[emrbrush^.ihBrush].Color := emrbrush^.lb.lbColor;
    extra^.HandleObj^[emrbrush^.ihBrush].Brush := true;
    extra^.HandleObj^[emrbrush^.ihBrush].Style.Fill := 0;
  end;

  procedure CreateMonoBrush;
  var
    emrmonobrush: PEMRCreateMonoBrush;
  begin
    emrmonobrush := PEMRCreateMonoBrush(@rec);
    extra^.HandleObj^[emrmonobrush^.ihBrush].Color := extra^.background;
    extra^.HandleObj^[emrmonobrush^.ihBrush].Brush := true;
    extra^.HandleObj^[emrmonobrush^.ihBrush].Style.Fill := 0;
  end;

  procedure DoEllipse;
  var
    center, edge: CoordPoint;
    emrellipse: PEMREllipse;
  begin
    emrellipse := PEMREllipse(@rec);
    center.x := (emrellipse^.rclBox.Left + emrellipse^.rclBox.Right) / 2;
    center.y := (emrellipse^.rclBox.Top + emrellipse^.rclBox.Bottom) / 2;
    edge.x := emrellipse^.rclBox.Left;
    edge.y := emrellipse^.rclBox.Top;
    extra^.PMap^.AddObject(PolyCurvePrimitive.Ellipse(center, edge, extra^.style));
  end;

  procedure DoLineTo;
  var
    emrlineto: PEMRLineTo;
    cx2, cy2: Coord;
  begin
    emrlineto := PEMRLineTo(@rec);
    cx2 := emrlineto^.ptl.x;
    cy2 := emrlineto^.ptl.y;
    extra^.PMap^.AddObject(LinePrimitive.Create(extra^.currx, extra^.curry, cx2, cy2, extra^.style));
    extra^.currx := cx2;
    extra^.curry := cy2;
  end;

  procedure DoMoveTo;
  var
    emrmoveto: PEMRMoveToEx;
  begin
    emrmoveto := PEMRMoveToEx(@rec);
    extra^.currx := emrmoveto^.ptl.x;
    extra^.curry := emrmoveto^.ptl.y;
  end;

  procedure DoSetPixel;
  var
    emrsetpixel: PEMRSetPixelV;
    x, y: Coord;
    SaveCurrent: TColor;
  begin
    emrsetpixel := PEMRSetPixelV(@rec);
    x := emrsetpixel^.ptlPixel.x;
    y := emrsetpixel^.ptlPixel.y;
    SaveCurrent := CurrentColor;
    CurrentColor := emrsetpixel^.crColor;
    extra^.PMap^.AddObject(LinePrimitive.Create(x, y, x, y, extra^.style));
    CurrentColor := SaveCurrent;
  end;

  procedure Missing(const s: string);
  begin
    OutputDebugString(PChar(s));
  end;

begin
  if extra^.HandleCount <> nObj then
    begin
      extra^.HandleCount := nObj;
      GetMem(extra^.HandleObj, nObj * sizeof(MetaGDIObject));
    end;

  case rec.iType of
    EMR_SELECTOBJECT: SelectObject;
    EMR_RECTANGLE: DoRectangle;
    EMR_POLYLINETO,
      EMR_POLYLINE,
      EMR_POLYGON: DoPolyline;
    EMR_POLYLINETO16,
      EMR_POLYGON16,
      EMR_POLYLINE16: DoPolyline16;
    EMR_SETTEXTCOLOR: extra^.foreground := rec.dParm[0];
    EMR_SETBKCOLOR: extra^.background := rec.dParm[0];
    EMR_CREATEPEN: CreatePen;
    EMR_EXTCREATEPEN: CreateExtPen;
    EMR_CREATEBRUSHINDIRECT: CreateBrush;

    EMR_CREATEDIBPATTERNBRUSHPT,
      EMR_CREATEMONOBRUSH: CreateMonoBrush;
    EMR_ELLIPSE: DoEllipse;
    EMR_MOVETOEX: DoMoveTo;
    EMR_LINETO: DoLineTo;
    EMR_SETPIXELV: DoSetPixel;
    EMR_POLYPOLYLINE16,
      EMR_POLYPOLYGON16: DoPolyPolygon16;
    EMR_POLYPOLYLINE,
      EMR_POLYPOLYGON: DoPolyPolygon;

    EMR_BITBLT,
      EMR_MASKBLT,
      EMR_PLGBLT,
      EMR_SETDIBITSTODEVICE,
      EMR_STRETCHDIBITS,
      EMR_STRETCHBLT:
      begin
        extra^.TryBitmap := true;
      end;

    EMR_POLYBEZIER:
      begin Missing('PolyBezier');
      end;
    EMR_POLYBEZIERTO:
      begin Missing('PolyBezierTo');
      end;
    EMR_POLYDRAW:
      begin Missing('PolyDraw');
      end;

    EMR_SETTEXTALIGN:
      begin Missing('SetTextAlign');
      end;
    EMR_EXTCREATEFONTINDIRECTW:
      begin Missing('ExtCreateFontIndirectW');
      end;
    EMR_EXTTEXTOUTA:
      begin Missing('ExtTextOutA');
      end;
    EMR_EXTTEXTOUTW:
      begin Missing('ExtTextOutW');
      end;
    EMR_POLYTEXTOUTA:
      begin Missing('PolyTextOutA');
      end;
    EMR_POLYTEXTOUTW:
      begin Missing('PolyTextOutW');
      end;

    EMR_ARCTO:
      begin Missing('ArcTo');
      end;
    EMR_ROUNDRECT:
      begin Missing('RoundRect');
      end;
    EMR_ANGLEARC:
      begin Missing('AngleArc');
      end;
    EMR_ARC:
      begin Missing('Arc');
      end;
    EMR_CHORD:
      begin Missing('Chord');
      end;
    EMR_PIE:
      begin Missing('Pie');
      end;

    EMR_FILLRGN:
      begin Missing('FillRgn');
      end;
    EMR_FRAMERGN:
      begin Missing('FrameRgn');
      end;
    EMR_POLYBEZIER16:
      begin Missing('PolyBezier16');
      end;
    EMR_POLYBEZIERTO16:
      begin Missing('PolyBezierTo16');
      end;
    EMR_POLYDRAW16:
      begin Missing('PolyDraw16');
      end;

    EMR_HEADER:
      begin
        Missing('Header');
      end;

    EMR_EOF:
      begin
        if (extra^.HandleObj <> nil) then FreeMem(extra^.HandleObj);
      end;
    EMR_CREATEPALETTE,
      EMR_SETWINDOWEXTEX,
      EMR_SETWINDOWORGEX,
      EMR_SETVIEWPORTEXTEX,
      EMR_SETVIEWPORTORGEX,
      EMR_SETBRUSHORGEX,
      EMR_SETMAPPERFLAGS,
      EMR_SETMAPMODE,
      EMR_SETBKMODE,
      EMR_SETPOLYFILLMODE,
      EMR_SETROP2,
      EMR_SETSTRETCHBLTMODE,
      EMR_SETCOLORADJUSTMENT,
      EMR_OFFSETCLIPRGN,
      EMR_SETMETARGN,
      EMR_EXCLUDECLIPRECT,
      EMR_INTERSECTCLIPRECT,
      EMR_SCALEVIEWPORTEXTEX,
      EMR_SCALEWINDOWEXTEX,
      EMR_SAVEDC,
      EMR_RESTOREDC,
      EMR_SETWORLDTRANSFORM,
      EMR_MODIFYWORLDTRANSFORM,
      EMR_DELETEOBJECT,
      EMR_SELECTPALETTE,
      EMR_SETPALETTEENTRIES,
      EMR_RESIZEPALETTE,
      EMR_REALIZEPALETTE,
      EMR_EXTFLOODFILL,
      EMR_SETARCDIRECTION,
      EMR_SETMITERLIMIT,
      EMR_BEGINPATH,
      EMR_ENDPATH,
      EMR_CLOSEFIGURE,
      EMR_FILLPATH,
      EMR_STROKEANDFILLPATH,
      EMR_STROKEPATH,
      EMR_FLATTENPATH,
      EMR_WIDENPATH,
      EMR_SELECTCLIPPATH,
      EMR_ABORTPATH,

    EMR_GDICOMMENT,
      EMR_INVERTRGN,
      EMR_PAINTRGN,
      EMR_EXTSELECTCLIPRGN,
      //    EMR_BITBLT,
    //    EMR_STRETCHBLT,
    //    EMR_MASKBLT,
    //    EMR_PLGBLT,
    //    EMR_SETDIBITSTODEVICE,
    //    EMR_STRETCHDIBITS,

    EMR_SETICMMODE,
      EMR_CREATECOLORSPACE,
      EMR_SETCOLORSPACE,
      EMR_DELETECOLORSPACE,
      EMR_GLSRECORD,
      EMR_GLSBOUNDEDRECORD,
      EMR_PIXELFORMAT:
      begin
      end;
  end;

  Result := 1;
end;
{
function MapCollection.ReadMetafile(Meta: TMetaFile; selected: boolean): boolean;
var
  r: TRect;
  e: MetaEnumExtra;
  SaveCurrent, SaveFillColor: TColor;
  last_tail: DrawPrimitive;
begin
  // Keep track of the tail so we can see if any objects got inserted
  last_tail := tail;
  e.PMap := @Self;
  e.HandleCount := 0;
  e.HandleObj := nil;
  e.foreground := clBlack;
  e.background := clWhite;
  e.style.Line := 1;
  e.style.Fill := 0;
  e.style.First := 0;
  e.style.Last := 0;
  e.style.FullStyle.Thickness := 0;
  e.style.FullStyle.SThickness := 0;
  e.currx := 0;
  e.curry := 0;
  e.TryBitmap := false;

  SaveCurrent := CurrentColor;
  SaveFillColor := CurrentFillColor;

  EnumEnhMetaFile(0, Meta.Handle, @EnhMetaEnumProc, @e, r);

  CurrentColor := SaveCurrent;
  CurrentFillColor := SaveFillColor;

  // If no objects were inserted, but a bitmap was among the objects, then return false
  // so we will reattempt this paste as a bitmap paste.
  Result := not ((e.TryBitmap) and (tail = last_tail));
end;
}

{
function MapCollection.GetMetafile(all: boolean): TMetaFile;
var
  MapView: ViewPoint;
  rect: TRect;
  crView: CoordRect;
  crExtent: CoordRect;
  offx, offy: Coord;
  metafilecanvas: TMetafileCanvas;
begin
  { -- Now Create a bitmap and a metafile to go along with the native data -- }
  Result := TMetafile.Create;

  rect := Extent(CurrentView, all);
  Result.Width := rect.right - rect.left + 1;
  Result.Height := rect.bottom - rect.top + 1;

  MetafileCanvas := TMetafileCanvas.CreateWithComment(Result, 0,
    res_mapobj_metafile_comment,
    Format(res_mapobj_metafile_title, [Result.Width, Result.Height]));

  // Experiment: do these lines of code affect the resolution of the output?
  // Comment from someone using the Metafile output to go to a print bureau
  // and the fonts were at 72 DPI instead of higher.  Not sure what affects
  // that...
  //  Result.MMWidth := Result.MMWidth*10;
  //  Result.MMHeight := Result.MMHeight*10;

  MapView := ViewPoint.Create(CurrentView);

  { Line up the display with the beginning of the bitmap }
  MapView.GetCoordinateRect(crView);
  crExtent := head.ChainExtent(all);
  offx := crExtent.Left - crView.Left;
  offy := crExtent.Top - crView.Top;
  crView.Left := crView.Left + offx;
  crView.Top := crView.Top + offy;
  crView.Right := crView.Right + offx;
  crView.Bottom := crView.Bottom + offy;
  MapView.SetCoordinateRect(crView);

  MapView.Canvas := MetafileCanvas;
  Draw(MapView, false);

  MapView.Free;

  MetafileCanvas.Free;
end;
}

function MapCollection.GetSelectedObjects(ParentForm: TForm): MapCollection;
var
  memory: TMemoryStream;
  sel: MapCollection;
begin
  sel := MapCollection.Create(ParentForm);
  { -- Create a memory stream, and write the selected objects into it --}
  memory := TMemoryStream.Create;
  Write(memory, false, True, False, [OB_CHUNK]);

  // reset the pointer, and read into the new object
  memory.Position := 0;
  Sel.Read(memory, true, True,False);

  memory.Free;
  GetSelectedObjects := Sel;
end;

procedure MapCollection.Copy;
var
  Data: THandle;
  DataPtr: Pointer;
  memory: TMemoryStream;
//  XML   : String;
//  D     : TDOMDocument;
  
begin
  { -- Create a memory stream, and write the selected objects into it --}
  memory := TMemoryStream.Create;
{
  D := Map.GetAsDOMDocument(False,[OB_CHUNK]);
  MainForm.DOMToXMLParser1.writeToString(D,'Latin1',XML);
  Memory.WriteBuffer(PChar(XML)^, Length(XML));
  MainForm.DOMImpl.freeDocument(D);
  MainForm.DOMImpl.freeUnusedASModels;
}
  Write(memory, false, True, False, [OB_CHUNK]);

  // Note that we cannot use the Delphi Clipboard object because
  // it "owns" the window handle--that prevents us from using
  // delayed rendering, and so we must use the Windows API directly.
  OpenClipboard(Parent.Handle);
  EmptyClipboard;

  try
    Data := GlobalAlloc(GMEM_MOVEABLE, memory.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        memory.Position := 0;
        Move(memory.Memory^, DataPtr^, memory.Size);
        SetClipboardData(MapFormat, Data);

        // We support delayed rendering for metafiles and bitmaps;
        // use NULL to indicate to Windows that we should recieve
        // a WM_RENDERFORMAT message to actually generate
        // the clipped object.
        //
        // Specifically, this fixes problems with people doing
        // copy operations with a high level of zoom--we were
        // attempting to generate a potentially huge bitmap
        // object that we didn't have enough memory to finish.
        // The user didn't even want the bitmap representation,
        // we were just generating it as a side-effect of the
        // copy. Now, we only generate the non-native formats
        // when the user attempts to paste them into another
        // document.
        //
        // CAVEAT:  Note that we do not do anything special to
        // attempt to preserve the exact state of the map when
        // this copy happens.  This means that the delayed render
        // will not be a faithful representation if the user
        // has modified the map or changed zoom levels.  I feel that
        // this is an acceptable compromise, since attempting to
        // preserve that state would introduce a large amount of
        // unneeded complexity.  Most times when people are copying
        // things back and forth between two different applications,
        // they immediately use the copied object, so this should
        // not be a problem.
        SetClipboardData(CF_METAFILEPICT, 0);
        SetClipboardData(CF_BITMAP, 0);

      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipboard;
  end;

  memory.Free;
end;

procedure MapCollection.CenterSelection(rescale: boolean);
var
  pasterect: CoordRect;
  cx, cy: Coord;
  ScreenCoord: CoordRect;
  w1, w2, h1, h2: Coord;
  factor: double;
begin
  CurrentView.GetCoordinateRect(ScreenCoord);
  pasterect := head.ChainExtent(false);
  cx := (ScreenCoord.Left + ScreenCoord.Right) / 2;
  cy := (ScreenCoord.Top + ScreenCoord.Bottom) / 2;
  MoveSelection(cx - ((pasterect.left + pasterect.Right) / 2),
    cy - ((pasterect.top + pasterect.bottom) / 2));
  if rescale then
    begin
      h1 := ScreenCoord.Bottom - ScreenCoord.Top;
      h2 := pasterect.bottom - pasterect.top;
      w1 := ScreenCoord.right - ScreenCoord.left;
      w2 := pasterect.right - pasterect.left;
      factor := 0.5;
      if (h2 <> 0) and (w2 <> 0) then
        begin
          if (h1 / h2 < w1 / w2) then
            factor := factor * (h1 / h2)
          else
            factor := factor * (w1 / w2);

          ScaleSelection(factor, factor);
        end;
    end;
  DisplaySelectedSize;
end;

procedure MapCollection.Paste;
var
  hData: THandle;
  pData: Pointer;
  pBytes: PChar;
  memory: TMemoryStream;
  Meta: TMetaFile;
  img: TBitmap;
  r: TRect;
  TryBitmap: boolean;
//  D : TDOMDocument;
begin
  if ReadOnlyPrevents then exit;

  SetUndoPoint(res_mapobj_undo_paste);
  ClearSelection;
  Clipboard.Open;
  hData := Clipboard.GetAsHandle(MapFormat);
  if hData <> 0 then
    begin
      pData := GlobalLock(hData);
      try
        memory := TMemoryStream.Create;
        pBytes := PChar(pData);
        memory.Write(pBytes^, GlobalSize(hData));
        memory.Position := 0;
{
        D := MainForm.XMLToDOMParser1.streamToDom(Memory);
        ReadFromDOMDocument(D,True);
        MainForm.DOMImpl.freeDocument(D);
        MainForm.DOMImpl.freeUnusedASModels;
}
        Read(memory, true, True, False);
        SetModified(modAdded);
        memory.Free;
      finally
        GlobalUnlock(hData);
      end;
    end
  else
    begin
      TryBitmap := true;
      {
      if Clipboard.HasFormat(CF_METAFILEPICT) then
        begin
          Meta := nil;
          try
            Meta := TMetaFile.Create;
            Meta.Assign(Clipboard);
            SupressRedraw;
            if ReadMetafile(Meta, true) then
              begin
                GroupSelected;
                CenterSelection(true);
                SetModified(modAdded);
                TryBitmap := false;
              end;
          finally
            RestoreRedraw;
            Meta.Destroy;
          end;
        end;
      }
      // If there is only a bitmap on the clipboard, or if there's a metafile that
      // only contains a bitmap, insert it.
      if Clipboard.HasFormat(CF_BITMAP) and TryBitmap then
        begin
          img := TBitmap.Create;
          img.Assign(Clipboard);
          r.Left := 0;
          r.Top := 0;
          r.Right := img.Width;
          r.Bottom := img.Height;
          SupressRedraw;
          AddObject(BitmapPrimitive.Create(img, CurrentView.ScreenToCoordRect(r)));
          CenterSelection;
          RestoreRedraw;
          SetModified(modAdded);
          // NOTE: we don't destroy the bitmap because the primitve has
          // taken it over...
        end;
    end;

  Clipboard.Close;

  CenterSelection(false);
  DisplaySelectedSize;
end;

function MapCollection.InsertMap(filename: string): boolean;
var
  f: TFileStream;
  id: integer;
  version: integer;
begin
  InsertMap := false;

  if ReadOnlyPrevents then exit;

  f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  f.ReadBuffer(id, sizeof(id));
  f.ReadBuffer(version, sizeof(version));
  if (id <> MapFileId) then
    begin
      ShowMessage(Format(res_mapobj_file_name_in, [filename]));
      f.Free;
      exit;
    end;

  if (version > CURRENT_MAP_VERSION) or (version < MAP_VERSION_3) then
    begin
      ShowMessage(Format(res_mapobj_file_ver_in, [filename]));
      f.Free;
      exit;
    end;

  ClearSelection;
  SetUndoPoint(Format(res_mapobj_undo_file_insert, [ExtractFilename(filename)]));
  Read(f, true, False, True, version, true);
  CenterSelection(false);

  // Not only does grouping keep the inserted file together, but it
  // moves everything to the current overlay, which since the inserted
  // file might have a totally different overlay scheme is a very good thing.
  GroupSelected;

  SetModified(modAdded);

  f.free;

  InsertMap := true;
end;

procedure MapCollection.SupressRedraw;
begin
  inc(supresscount);
end;

procedure MapCollection.RestoreRedraw;
begin
  if (supresscount <> 0) then dec(supresscount);
  if (supresscount = 0) then Invalidate;
end;
{
function MapCollection.InsertMetafile(filename: string): boolean;
var
  f: TFileStream;
  Meta: TMetaFile;
begin
  InsertMetafile := false;

  if ReadOnlyPrevents then exit;
  
  f := nil;
  Meta := nil;
  try
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

    Meta := TMetaFile.Create;
    Meta.LoadFromStream(f);

    ClearSelection;
    SetUndoPoint(Format(res_mapobj_undo_file_insert, [ExtractFilename(filename)]));

    SupressRedraw;
    ReadMetafile(Meta, true);
    CenterSelection(true);
    GroupSelected;
    RestoreRedraw;
    SetModified(modAdded);

    Meta.Free;
    f.Free;

    InsertMetafile := true;
  except
    Meta.Free;
    f.free;
    InsertMetafile := false;
  end;
  DisplaySelectedSize;
end;
}
function MapCollection.InsertBitmap(filename: string): boolean;
var
  f: TFileStream;
  img: TBitmap;
  r: TRect;
begin
  InsertBitmap := false;

  if ReadOnlyPrevents then exit;

  f := nil;
  img := nil;
  try
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

    img := TBitmap.Create;
    img.LoadFromStream(f);

    ClearSelection;
    SetUndoPoint(Format(res_mapobj_undo_file_insert, [ExtractFilename(filename)]));
    r.Left := 0;
    r.Top := 0;
    r.Right := img.Width;
    r.Bottom := img.Height;
    SupressRedraw;
    AddObject(BitmapPrimitive.Create(img, CurrentView.ScreenToCoordRect(r)));
    CenterSelection;
    RestoreRedraw;
    SetModified(modAdded);
    // NOTE: we don't destroy the bitmap because the primitve has
    // taken it over...
    f.free;
    InsertBitmap := true;
  except
    f.free;
    img.free;
    InsertBitmap := false;
  end;
  DisplaySelectedSize;
end;


(******************************************************************************)
//NAME: InsertJPEG
//DESCRIPTION: A copy of Insertbitmap, slightly altered to handle JPEG images
{ TODO : Streamline all raster graphics importing to minimise code replication. }
(******************************************************************************)
function MapCollection.InsertJPEG(filename: string): boolean;
var
  f: TFileStream;
  img: TBitmap;
  jpgimage : TJPegImage;
  r: TRect;
begin
  result := false;

  if ReadOnlyPrevents then exit;

  f := nil;
  img := nil;
  try
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

    img := TBitmap.Create;

    jpgimage := TJPegImage.Create;
    jpgimage.LoadFromFile(filename);
    img.assign(jpgimage);
    jpgimage.free;

    ClearSelection;
    SetUndoPoint(Format(res_mapobj_undo_file_insert, [ExtractFilename(filename)]));
    r.Left := 0;
    r.Top := 0;
    r.Right := img.Width;
    r.Bottom := img.Height;
    SupressRedraw;
    AddObject(BitmapPrimitive.Create(img, CurrentView.ScreenToCoordRect(r)));
    CenterSelection;
    RestoreRedraw;
    SetModified(modAdded);
    // NOTE: we don't destroy the bitmap because the primitve has
    // taken it over...
    f.free;
    result := true;
  except
    f.free;
    img.free;
    result := false;
  end;
  DisplaySelectedSize;
end;

function MapCollection.CopyContents(FullCopy: Boolean): DrawPrimitive;
var
  p, next: DrawPrimitive;
begin
  // If our object has no "guts", then return a similarly empty result.
  if (head=nil) then begin
    CopyContents := nil;
    exit;
    end;

  p := head.Copy;
  If FullCopy Then P.SplitOff;
  next := head.Next;
  CopyContents := p;

  while (next <> nil) do
    begin
      p.Next := next.Copy;
      If FullCopy Then P.Next.SplitOff;
      next := Next.Next;
      p := p.next;
    end;
end;

procedure MapCollection.RestoreContents(d:DrawPrimitive);
begin
  // Remove existing contents
  Clear;

  // Restore to selected chain
  head := d;
  tail := d;

  // Move tail to end of chain
  while (tail<>nil) and (tail.Next<>nil) do begin
    tail := tail.Next;
    end;
end;

function MapCollection.GetSelectedObjectList:TList;
var p: DrawPrimitive;
begin
  // Create a list of all the objects to be aligned
  result := TList.Create;
  p := head;
  while (p <> nil) do
    begin
      if p.IsSelected then
        begin
          result.Add(p);
          SetModified(modChanged);
        end;
      p := p.Next;
    end;
end;

procedure MapCollection.OrderSelection(order:OrderType);
var
  p: DrawPrimitive;
  list: TList;
  r: CoordRect;
  x, y: Coord;
  i:integer;
  mainhead, maintail, grouphead, grouptail: DrawPrimitive;
begin
  if ReadOnlyPrevents then exit;

  // If no change, then exit without doing any work
  if order=Order_None then exit;

  InvalidateSelect(true);

  // Separate the selection from the main map
  SplitSelectedChain(mainhead, maintail, grouphead, grouptail);

  // Create a list of the selected objects
  list := TList.Create;
  p := grouphead;
  while (p <> nil) do begin
    list.Add(p);
    SetModified(modChanged);
    p := p.Next;
    end;

  // Trivial case: if the list is either empty, or a single object, we're done
  if (list.Count <= 1) then begin
    list.Free;

    // If it was a single item, add that item back to the list
    if (mainhead = nil) then begin
       head := grouphead;
       tail := grouptail;
       end
    else if (grouphead<>nil) then begin
      head := mainhead;
      maintail.Next := grouphead;
      tail := grouptail;
      end;
    exit;
    end;

  // Now that we have the list, do the requested actions to the list
  case order of
    Order_LEFT_TOP: begin
        list.Sort(UpperLeftPredicate);
        end;
    Order_RIGHT_TOP: begin
        list.Sort(UpperRightPredicate);
        end;
    Order_LEFT_BOTTOM: begin
        list.Sort(LowerLeftPredicate);
        end;
    Order_RIGHT_BOTTOM: begin
        list.Sort(LowerLeftPredicate);
        end;
    Order_RANDOM: begin
        list.Sort(RandomPredicate);
        end;
    Order_CENTER, Order_EDGE: begin
        // Need to center objects around the orgin: the distance predicates
        // need to have a reference point, which is the center.
        r := head.ChainExtent(false);
        x := (r.Right + r.Left) / 2;
        y := (r.Bottom + r.Top) / 2;
        head.ChainApplyMatrix(OffsetMatrix(-x, -y), false);

        if (order=Order_CENTER) then
          list.Sort(CenterPredicate)
        else
          list.Sort(EdgePredicate);

        // Now that we're done sorting, move them back.
        head.ChainApplyMatrix(OffsetMatrix(x, y), false);
        end;
  end;

  // Re-add the sorted objects back to the main object list
  head := mainhead;
  tail := maintail;

  for i:=0 to list.Count-1 do begin
    p := DrawPrimitive(list.Items[i]);

    if (tail=nil) then head:=p
                  else tail.Next := p;

    tail := p;
    end;

  tail.Next := nil;

  list.Free;
  DisplaySelectedSize;
end;

function  MapCollection.GetPushPinName(index:integer):string;
begin
  Result := pushpin[index].Name;

  // If no name was actually assigned, use the color name.
  if (Result='') then Result := MainForm.PushPinList.Items[index];
end;

procedure MapCollection.SetPushPinName(index:integer; s:string);
begin
  if ReadOnlyPrevents then exit;

  if (s<>pushpin[index].Name) then begin
    SetModified(modPushPins);
    pushpin[index].Name := s;
    end;
end;

function  MapCollection.GetPushPinHistoryCount(index:integer):integer;
begin
  Result := pushpin[index].HistoryCount;
end;

function  MapCollection.GetPushPinHistoryPoint(index:integer; history:integer):CoordPoint;
begin
  if (index>=PushPinCount) or (history>=pushpin[index].HistoryCount) then begin
    raise ERangeError.Create(res_pushpin_out_of_range);
  end;

  Result := pushpin[index].History[history].point;
end;

procedure MapCollection.SetPushPinHistoryPoint(index:integer; history:integer; pt:CoordPoint);
begin
  if ReadOnlyPrevents then exit;

  if (index>=PushPinCount) or (history>=pushpin[index].HistoryCount) then begin
    raise ERangeError.Create(res_pushpin_out_of_range);
  end;

  pushpin[index].History[history].point := pt;
end;

function  MapCollection.GetPushPinHistoryNote(index:integer; history:integer):string;
begin
  if (index>=PushPinCount) or (history>=pushpin[index].HistoryCount) then begin
    raise ERangeError.Create(res_pushpin_out_of_range);
  end;

  Result := pushpin[index].History[history].note;
end;

procedure MapCollection.SetPushPinHistoryNote(index:integer; history:integer; s:string);
begin
  if ReadOnlyPrevents then exit;

  if (index>=PushPinCount) or (history>=pushpin[index].HistoryCount) then begin
    raise ERangeError.Create(res_pushpin_out_of_range);
  end;

  if (s <> pushpin[index].History[history].note) then begin
    SetModified(modPushPins);
    pushpin[index].History[history].note := s;
    end;
end;

procedure MapCollection.PushPinHistoryPointAdd(index:integer; pt:CoordPoint; note:string);
var i:integer;
    hn:integer;
begin
  SetModified(modPushPins);
  // Increment count of items in the list
  hn := pushpin[index].HistoryCount;
  inc(pushpin[index].HistoryCount);

  // Increase size of existing list
  SetLength(pushpin[index].History, hn+1);

  // Ripple the old list elements down; leave the 0th spot open.
  for i:=0 to hn-1 do begin
    pushpin[index].History[hn-i].point := pushpin[index].History[hn-i-1].point;
    pushpin[index].History[hn-i].note  := pushpin[index].History[hn-i-1].note;
    end;

  // Set the 0th point to the new info
  pushpin[index].History[0].point := pt;
  pushpin[index].History[0].note  := note;
end;

function MapCollection.GetPushPinPoint(index:integer):CoordPoint;
begin
  if (pushpin[index].HistoryCount=0) then begin
    PushPinHistoryPointAdd(index, MakeCoordPoint(0,0), '');
    end;

  Result := GetPushPinHistoryPoint(index, 0)
end;

procedure MapCollection.SetPushPinPoint(index:integer; pt:CoordPoint);
begin
  if ReadOnlyPrevents then exit;

  if (pushpin[index].HistoryCount>=1) then
    SetPushPinHistoryPoint(index, 0, pt)
  else begin
    PushPinHistoryPointAdd(index, pt, '');
  end;
end;

procedure MapCollection.PushPinHistoryClear(index:integer);
begin
  SetModified(modPushPins);
  SetLength(pushpin[index].History, 0);
  pushpin[index].HistoryCount := 0;
end;

function  MapCollection.PushPinPlaced(index:integer):boolean;
begin
  PushPinPlaced := (pushpin[index].HistoryCount <> 0);
end;

function  MapCollection.PushPinCount:integer;
begin
  if MainForm.PushPinList<>nil then
    PushPinCount := Mainform.PushPinList.Items.Count
  else
    PushPinCount := 0;
end;

function MapCollection.GetPushPinAnnotation(index:integer; history:integer):string;
var n:integer;
begin
  Result := '';

  if (PP_WaypointsVisible in PushPinFlags) and (history<>0) then begin
    if PP_ShowNumber in PushPinFlags then begin
       // Show the numbers starting with 1 for the oldest (last) in the list
       n := PushPinHistoryCount[index] - history;
       Result := Result + ' ' + IntToStr(n);
       end;
    if PP_ShowNote in PushPinFlags then begin
       Result := Result + ' ' + PushPinHistoryNote[index,history];
       end;

    if (Result<>'') then Result:=Result+' ';
    end;
end;

procedure MapCollection.SetPushPinFlags(ps:PushPinFlagSet);
begin
  sPushPinFlags := ps;
  MainForm.MakeWaypointsVisibleMenu.Checked := (PP_WaypointsVisible in ps);
  MainForm.PushPinShowNumberMenu.Checked    := (PP_ShowNumber in ps);
  MainForm.PushPinShowNoteMenu.Checked      := (PP_ShowNote in ps);
end;

function MapCollection.GetPushPinText:string;
var i,j:integer;
    x:XML;
begin
  x:=XML.Create;

  x.Comment('AutoREALM Push Pin List');
  x.OpenTag('PUSHPINLIST');

  for i:=0 to PushPinCount-1 do begin
    if (PushPinHistoryCount[i]>1) and Mainform.PushPinList.Checked[i] then begin
      x.OpenTag('PIN');
      x.Field('NAME', StripHotKey(PushPinName[i]));

      for j:=PushPinHistoryCount[i]-1 downto 1 do begin
        x.OpenTag('WAYPOINT');
        x.Field('X', FloatToStr(PushPinHistoryPoint[i,j].X));
        x.Field('Y', FloatToStr(PushPinHistoryPoint[i,j].Y));
        x.Field('NOTE', PushPinHistoryNote[i,j]);
        x.CloseTag('WAYPOINT');
        end;
      x.CloseTag('PIN');
      end;
    end;
  x.CloseTag('PUSHPINLIST');

  Result := x.AsText;

  x.Free;
end;

procedure MapCollection.SetPushPinText(s:string);
var x:XML;
    pinname:string;
    i, idx:integer;
    temp:string;
    pt:CoordPoint;
    note:string;
    done:boolean;
begin
  x:=XML.Create;

  x.AsText := s;

  if not x.MatchOpenTag('PUSHPINLIST') then x.BadTag;

  while x.MatchOpenTag('PIN') do begin
    if x.MatchOpenTag('NAME') then begin
      pinname := x.GetValue;
      if not x.MatchCloseTag('NAME') then x.BadTag;
      end;

    idx := -1;
    for i:=0 to PushPinCount-1 do begin
      if pinname=StripHotKey(PushPinName[i]) then begin
        idx:=i;
        break;
        end;
      end;

    if idx = -1 then begin
      raise EInOutError.CreateFmt(res_XMLBadPinName, [pinname]);
      end;

    PushPinHistoryClear(idx);
    while x.MatchOpenTag('WAYPOINT') do begin
      pt.X := 0;
      pt.Y := 0;
      note := '';
      done := false;

      while not done do begin
        temp := x.CurrentTag;
        if (temp = 'X') then begin
          x.GetTag;
          pt.x := StrToFloat(x.Getvalue);
          if not x.MatchCloseTag('X') then x.BadTag;
          end
        else if (temp = 'Y') then begin
          x.GetTag;
          pt.y := StrToFloat(x.Getvalue);
          if not x.MatchCloseTag('Y') then x.BadTag;
          end
        else if (temp = 'NOTE') then begin
          x.GetTag;
          note := x.GetValue;
          if not x.MatchCloseTag('NOTE') then x.BadTag;
          end
        else if (temp = '\WAYPOINT') then begin
          x.GetTag;
          // End the waypoint definition by creating the waypoint
          PushPinHistoryPointAdd(idx, pt, note);
          done := true;
          end
        else begin
          // Consume unknown fields until the matching end tag.
          while not x.MatchCloseTag(temp) do x.GetTag;
          end;
        end;
      end;

    if not x.MatchCloseTag('PIN') then x.BadTag;

    // If we're done with this push pin, add the last point one more time.
    // Why?  Because the 0th "waypoint" is the actual last position of the
    // pushpin.  The real waypoints are from 1 to n-1.  We need to put the
    // "real" pushpin somewhere, and so we put it over the last waypoint
    // (the exact same thing that happens when you create a waypoint from
    // the map).
    PushPinHistoryPointAdd(idx, pt, '');

    // Check any pushpins we import.
    Mainform.PushPinList.Checked[idx] := true;
    end;

  if not x.MatchCloseTag('PUSHPINLIST') then x.BadTag;

  x.Free;
end;

procedure MapCollection.SetReadOnly(b:boolean);
begin
  fReadOnly := b;
  if b then ClearSelection(true);
end;

// JD 8-2-02

Procedure MapCollection.DisplaySelectedSize;
Var
  C     : CoordRect;
  CX,CY : Coord;

Begin
  If AnythingSelected Then
  Begin
    MainForm.edtXSize.Enabled := True;
    MainForm.edtYSize.Enabled := True;
    C  := Map.CoordExtent(false);
    CX := C.Right - C.Left;
    CY := C.Bottom - C.Top;
    CX := CX * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    CY := CY * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    MainForm.edtXSize.Text := Format('%3.3f',[CX]);
    MainForm.edtYSize.Text := Format('%3.3f',[CY]);
  End
  Else
  Begin
    MainForm.edtXSize.Enabled := False;
    MainForm.edtYSize.Enabled := False;
    MainForm.edtXSize.Text    := '';
    MainForm.edtYSize.Text    := '';
  End;
End; // MapCollection.DisplaySelectedSize

begin
  MapFormat := RegisterClipboardFormat('AutoREALM Map Format');
end.
