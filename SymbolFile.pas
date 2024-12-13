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
unit SymbolFile;

{$MODE Delphi}

{+------------------------------------------------------------------------+
  SymbolFile: Contains the following classes:
    Symbol:            A single symbol in the symbol library
    SymbolGroup:       A symbol group (all the symbols in a single .AuS file)
    SymbolGroupList:   All the symbols (all groups in the AutoREALM program dir)

  The user interface code is not in this file: see SymbolLib.pas.
 +------------------------------------------------------------------------+}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, MapObject, SettingsDialog, LocalizedStrings{,
  XDOM_2_3};

  // =====================================================================================

type Symbol = class
     private
       function GetComments:string;
       procedure SetComments(s:string);

     public
       Name    : string;          // Name of the symbol
       Objects : MapCollection;   // Objects in the symbol
       Next    : Symbol;          // Next symbol in the group list (if any, nil if not)
       Modified: boolean;         // Has this symbol been modified?
       Favorite: boolean;         // Is this symbol in the favorites list?
       UniqueID: integer;         // Uniquely identifies this symbol
       nImageIndex:integer;        // Index of the cached image for the symbol;

       constructor Create;
       destructor Destroy;  override;

       procedure Read(f:TStream; Full,UseAliasInfo: Boolean);
       procedure Write(f:TStream; Full,UseAliasInfo: Boolean);

       Procedure ReadFromDOMElement(E: TDOMElement);
       Function  GetAsDOMElement(D: TDOMDocument): TDOMElement;

       procedure DrawImage(Canvas:TCanvas; width,height:integer; zoom:single);
       function  CreateBitmap(width,height:integer; AntiAliased: Boolean = False):TBitmap; // Modified JD 9-2-02

       property  Comments:string read GetComments write SetComments;
       property  ImageIndex:integer read nImageIndex write nImageIndex;

       function  CreateTempFile:string;
       procedure LoadTempFile(filename:string);
     end;

// =====================================================================================

     SymbolGroup = class
       FileName : String;
       SymbolHead:Symbol;
       Next     : SymbolGroup;
       fModified : boolean;
       fLoaded   : boolean;

       procedure SetModified(b:boolean);

     public
       constructor Create;
       destructor Destroy;  override;

       procedure Clear;
       function Prepare(fname:string):boolean;
       procedure Ready;
       function Load(fname:string):boolean;
       function Save(fname:string):boolean;

       function  GroupName:string;
       function  Count:integer;
       property HasBeenLoaded:boolean read fLoaded;

       procedure AddSymbol(sym:Symbol);
       procedure DeleteSymbol(which:Symbol; destroy:boolean=true);
       procedure RemoveCachedImage(index:integer);

       function  GetSymbol(index:integer):Symbol;
       function  FirstSymbol:Symbol;
       function  NextSymbol(p:Symbol):Symbol;

       function FindSymbol(uid:integer):Symbol;

       property Modified:boolean read fModified write SetModified;
     end;

// =====================================================================================

     SymbolGroupList = class
       GroupHead : SymbolGroup;
       symboldirectory : String;

     public
       constructor Create;
       destructor Destroy;  override;

       procedure Clear;

       procedure Save(symdir:string);
       procedure Load(symdir:string);

       function  Count:integer;

       procedure NewGroup(name:string);
       procedure OpenGroup(filename:string);
       procedure DeleteGroup(which:SymbolGroup);

       function FirstGroup:SymbolGroup;
       function NextGroup(p:SymbolGroup):SymbolGroup;

       function FindGroup(name:string):SymbolGroup;

       function CreateReference(grp:SymbolGroup; sym:Symbol):string;
       function GetReference(ref:string):Symbol;

       procedure RemoveCachedImage(index:integer);
     end;


implementation

uses Primitives, SysUtils, SymbolLib, StreamUtil, MAIN, Geometry;

const SymbolFileId  = $53747541;      // AutS (in little-endian format)
      SymbolVersion = $00000001;      // Symbol file version

const SymbolChunks = [CM_CHUNK,       // Save Comments, Objects, and EOF,
                      OB_CHUNK,       // but that's it.
                      EO_CHUNK];

var
  NextUniqueID:integer;
  AntiAliasLookup : Array[0..256 * 16 - 1] Of LongWord; // 16.16 fixed-point lookup table
  AntiAliasBitmap : TBitmap;

// =====================================================================================

//--------------------------------------------------------------------------
// Name: Symbol.Create
//
// Purpose: Creates and initializes a single symbol.
//
// Notes:
//--------------------------------------------------------------------------
constructor Symbol.Create;
begin
  Name     := '';
  Modified := false;
  Next     := nil;
  Objects  := MapCollection.Create(MainForm);
  Favorite := false;
  UniqueID := NextUniqueID;
  inc(NextUniqueID);
  nImageIndex := -1;
end;

//--------------------------------------------------------------------------
// Name: Symbol.Destroy
//
// Purpose: Destroys a single symbol.
//
// Notes:
//--------------------------------------------------------------------------
destructor Symbol.Destroy;
begin
  Objects.Free;
end;

//--------------------------------------------------------------------------
// Name: Symbol.Destroy
//
// Purpose: Destroys a single symbol.
//
// Notes:
//--------------------------------------------------------------------------
procedure Symbol.Read(f:TStream; Full,UseAliasInfo: Boolean);
begin
  Name:=ReadStringFromStream(f);
  Objects.Read(f,false,Full,UseAliasInfo);

  f.Read(Favorite,sizeof(Favorite));

  Modified := false;
end;

Procedure Symbol.ReadFromDOMElement(E: TDOMElement);
Var Sym: TDOMElement;
Begin
  Sym := E.getFirstChildElement('SYMBOL');
  If Sym <> Nil Then
  Begin
    Name     := GetStringProperty(Sym,'NAME');
    Favorite := GetBooleanProperty(Sym,'FAVORITE');
    // 2003/12/08 - J.Friant
    // Symbols should not change the map settings, so we
    // should set the "Insert" flag to throw away all the
    // extra settings.  So argument 4 becomes True.
    Objects.ReadFromDOMElement(Sym,False,CURRENT_MAP_VERSION,True);
  End;
  Modified := False;
End; // Symbol.ReadFromDOMElement

//--------------------------------------------------------------------------
// Name: Symbol.Destroy
//
// Purpose: Destroys a single symbol.
//
// Notes:
//--------------------------------------------------------------------------
procedure Symbol.Write(f:TStream; Full,UseAliasInfo: Boolean);
begin
  WriteStringToStream(f,Name);
  Objects.Write(f,true,Full,UseAliasInfo,SymbolChunks);
  f.Write(Favorite,sizeof(Favorite));
  Modified := false;
end;

Function Symbol.GetAsDOMElement(D: TDOMDocument): TDOMElement;
Var
  Map : TDOMElement;
  Sym : TDOMElement;

Begin
  Map := Objects.GetAsDOMElement(D,True);
  Sym := D.createElement('SYMBOL');
  Sym.appendChild(NewStringProperty(D,'NAME',Name));
  Sym.appendChild(NewBooleanProperty(D,'FAVORITE',Favorite));
  Sym.appendChild(Map);
  Result := Sym;
End; // Symbol.GetAsDOMElement

//--------------------------------------------------------------------------
// Name: Symbol.GetComments
//
// Purpose: Gets comments for a symbol
//
// Notes: Uses the comment field in the map object.
//--------------------------------------------------------------------------
function Symbol.GetComments:string;
begin
  GetComments := Objects.Comments;
end;

//--------------------------------------------------------------------------
// Name: Symbol.SetComments
//
// Purpose: Sets comments for a symbol
//
// Notes: Uses the comment field in the map object.
//--------------------------------------------------------------------------
procedure Symbol.SetComments(s:string);
begin
  Objects.Comments := s;
end;

//--------------------------------------------------------------------------
// Name: Symbol.DrawImage
//
// Purpose: Draws symbol on the given canvas
//--------------------------------------------------------------------------
procedure Symbol.DrawImage(Canvas:TCanvas; width,height:integer; zoom:single);
var  MapView:ViewPoint;
     cr:CoordRect;
begin
  MapView := ViewPoint.Create;
  cr      := Objects.CoordExtent(true);
  MapView.SetCoordinateSize(Width,Height,false);
  MapView.SetCoordinateRect(cr);
  MapView.VisibleOverlays := [0..255];
  MapView.Canvas          := Canvas;
  MapView.QuickDraw       := QuickDraw_Lines;
  Objects.ShowAll(MapView, zoom);
  Objects.Draw(MapView,false);
  MapView.Free;
end;

//--------------------------------------------------------------------------
// Name: Symbol.CreateBitmap
//
// Purpose: Creates a bitmap of the symbol for the given width and height
//--------------------------------------------------------------------------
function Symbol.CreateBitmap(width,height:integer; AntiAliased: Boolean):TBitmap;
Type
  TColorArray = Array[0..8] Of TColor;
  PLongWord = ^LongWord;
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;

Var
  X,Y         : Integer;
  CA          : TColorArray;
  S           : Integer;
  Max0        : Single;
  Min0        : Single;
  Max1        : Single;
  Min1        : Single;
  P0,P1,P2    : PLongWord;
  P0A,P1A,P2A : PLongWord;
  P0B,P1B,P2B : PLongWord;
  Q           : PLongWord;
  I           : Integer;

  Procedure FindMinMax(BMP: TBitmap; Var BMin,BMax: Single);
  // Assumes that the bitmap's PixelFormat is pf32Bit!!!
  Var
    X,Y   : Integer;
    C     : TColor;
    R,G,B : Integer;
    S     : Single;
    P     : PLongWord;

  Begin
    BMin := 3;
    BMax := 0;
    For Y := 0 To BMP.Height - 1 Do
    Begin
      P := BMP.ScanLine[Y];
      For X := 0 To BMP.Width - 1 Do
      Begin
        C    := P^;
        R    := TRGBA(C).R;
        G    := TRGBA(C).G;
        B    := TRGBA(C).B;
        S    := Sqr(R / 255) + Sqr(G / 255) + Sqr(B / 255);
        BMin := Min(BMin,S);
        BMax := Max(BMax,S);
        Inc(LongWord(P),4);
      End; // For X
    End; // For Y
    BMin := Sqrt(BMin / 3);
    BMax := Sqrt(BMax / 3);
  End; // FindMinMax

  Procedure Equalize(BMP: TBitmap; Min0,Max0,Min1,Max1: Single);
  // Assumes that the bitmap's PixelFormat is pf32Bit!!!
  Var
    X,Y   : Integer;
    C     : TColor;
    CA    : Array[0..255] Of Byte;
    P     : PLongWord;

  Begin
    // Make the lookup table for speed

    For X := 0 To 255 Do
    Begin
      If (Max1 <> Min1)
       Then Y := Round((Min0 + ((X / 255) - Min1) * (Max0 - Min0) / (Max1 - Min1)) * 255)
       Else Y := Round(Min0 * 255);
      If Y < 0 Then Y := 0;
      If Y > 255 Then Y := 255;
      CA[X] := Y;
    End; // For X

    // Process the image

    For Y := 0 To BMP.Height - 1 Do
    Begin
      P := BMP.ScanLine[Y];
      For X := 0 To BMP.Width - 1 Do
      Begin
        C := P^;
        TRGBA(C).R := CA[TRGBA(C).R];
        TRGBA(C).G := CA[TRGBA(C).G];
        TRGBA(C).B := CA[TRGBA(C).B];
        P^ := C;
        Inc(LongWord(P),4);
      End; // For X
    End; // For Y
  End; // Equalize

  Function Weight(Const CA: TColorArray): TColor;
  Var
    R,G,B : LongWord;
    C     : TColor;
    I     : Integer;

  Begin
    // Start with a value representing 0.5 so we don't have to do any rounding logic
    
    R := 32768;
    G := 32768;
    B := 32768;
    For I := 0 To 8 Do
    Begin
      R := R + AntiAliasLookup[(I Shl 8) + TRGBA(CA[I]).R];
      G := G + AntiAliasLookup[(I Shl 8) + TRGBA(CA[I]).G];
      B := B + AntiAliasLookup[(I Shl 8) + TRGBA(CA[I]).B];
    End; // For I

    LongWord(C) := 0;

    I := R Shr 16;
    If I > 255 Then I := 255;
    TRGBA(C).R := I;

    I := G Shr 16;
    If I > 255 Then I := 255;
    TRGBA(C).G := I;

    I := B Shr 16;
    If I > 255 Then I := 255;
    TRGBA(C).B := I;

    Result := C;
  End; // Weight

begin
  Result             := TBitmap.Create;
  Result.Width       := Width;
  Result.Height      := Height;
  Result.PixelFormat := pf32Bit; // Convert to RGBA so we can use the ScanLine property to speed things up
  If AntiAliased Then
  Begin
    AntiAliasBitmap.Width       := Width  * 2 + 1;
    AntiAliasBitmap.Height      := Height * 2 + 1;
    AntiAliasBitmap.PixelFormat := pf32Bit; // Convert to RGBA so we can use the ScanLine property to speed things up

    // Quickly fill the bitmap with 100% white

    For Y := 0 To AntiAliasBitmap.Height - 1 Do
    Begin
      P0 := AntiAliasBitmap.ScanLine[Y];
      For X := 0 To AntiAliasBitmap.Width - 1 Do
      Begin
        P0^ := $00FFFFFF;
        Inc(LongWord(P0),4);
      End; // For X
    End; // For Y

    // Don't ask me why the scaling factor has to be different; running two instances and comparing showed them to be so

    DrawImage(AntiAliasBitmap.Canvas,AntiAliasBitmap.Width,AntiAliasBitmap.Height,-0.105);

    S := Height * 2;
    I := Height - 1;
    For Y := 0 To Height - 1 Do
    Begin
      P0  := AntiAliasBitmap.ScanLine[S];
      P1  := AntiAliasBitmap.ScanLine[S - 1];
      P2  := AntiAliasBitmap.ScanLine[S - 2];
      Dec(S,2);
      Q   := Result.ScanLine[I];
      P0A := P0;
      P1A := P1;
      P2A := P2;
      Dec(I);
      For X := 0 To Width - 1 Do
      Begin
        P0B := P0A;
        P1B := P1A;
        P2B := P2A;

        CA[0] := P0B^;
        CA[3] := P1B^;
        CA[6] := P2B^;
        Inc(LongWord(P0B),4);
        Inc(LongWord(P1B),4);
        Inc(LongWord(P2B),4);
        CA[1] := P0B^;
        CA[4] := P1B^;
        CA[7] := P2B^;
        Inc(LongWord(P0B),4);
        Inc(LongWord(P1B),4);
        Inc(LongWord(P2B),4);
        CA[2] := P0B^;
        CA[5] := P1B^;
        CA[8] := P2B^;

        Q^ := Weight(CA);
        Inc(LongWord(Q),4);
        Inc(LongWord(P0A),8);
        Inc(LongWord(P1A),8);
        Inc(LongWord(P2A),8);
      End; // For X
    End; // For Y

    FindMinMax(AntiAliasBitmap,Min0,Max0);
    FindMinMax(Result,Min1,Max1);

    Equalize(Result,Min0,Max0,Min1,Max1);
  End
  Else DrawImage(Result.Canvas,Width,Height, -0.15);
end;

//--------------------------------------------------------------------------
// Name: Symbol.CreateTempfile
//
// Purpose: Writes the symbol into a temporary file, and returns the name
//          of the file.  Used for passing a symbol to a spawned copy of
//          AutoREALM for symbol editing.
//--------------------------------------------------------------------------
function Symbol.CreateTempFile:string;
var f:TFileStream;
    tempfile:array[0..255] of char;
    temppath:array[0..255] of char;
    id:integer;
    version:integer;
begin
    Result:='';
    GetTempPath(sizeof(temppath),temppath);
    GetTempfileName(temppath, 'AUR', 0, tempfile);

    f:=nil;
    try
      f:=TFileStream.Create(tempfile, fmCreate or fmShareExclusive);
      id:=Main.MapFileId;
      version:=MapObject.MapVersion;
      f.WriteBuffer(id, sizeof(id));
      f.WriteBuffer(version, sizeof(version));

      Objects.Write(f,true,False,False);
      Result:=tempfile;
    finally
      f.Free;
    end;
end;

//--------------------------------------------------------------------------
// Name: Symbol.LoadTempFile
//
// Purpose: Reads the symbol back from a temporary file that has been
//          potentially edited by the spawned copy of AutoREALM.
//--------------------------------------------------------------------------
procedure Symbol.LoadTempFile(filename:string);
var f:TFileStream;
    junk:integer;
begin

    f:=nil;
    try
      f:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
      // Throw away the version & Map ID: we just spawned it, so it
      // had better be the same version of the program!
      f.ReadBuffer(junk, sizeof(junk));
      f.ReadBuffer(junk, sizeof(junk));

      Objects.Clear;                     // Delete our old objects
      Objects.Read(f,false,False,True);  // Read new data out of the stream.
    finally
      f.Free;
    end;
end;

// =====================================================================================

//--------------------------------------------------------------------------
// Name: SymbolGroup.Create
//
// Purpose: Creates and initializes symbol group.  A symbol group is
//          a set of symbols in the same file, typically grouped by
//          similarity.
//
// Notes:
//--------------------------------------------------------------------------
constructor SymbolGroup.Create;
begin
  SymbolHead := nil;
  Modified := false;
  fLoaded := false;
  Next := nil;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Destroy
//
// Purpose: Destructor: clears out symbol list
//
// Notes:
//--------------------------------------------------------------------------
destructor SymbolGroup.Destroy;
begin
  Clear;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.SetModified
//
// Purpose: Sets the modified flag
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroup.SetModified(b:boolean);
begin
  fModified := b;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Clear
//
// Purpose: Removes and frees all symbols in the group.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroup.Clear;
var p,next:Symbol;
begin
  p:=SymbolHead;

  while (p<>nil) do begin
    next:=p.Next;
    p.Free;
    p:=next;
    end;

  SymbolHead := nil;
  fLoaded := false;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Prepare
//
// Purpose: Sets the group filename, but does not load the group.
//          Group will be auto-loaded as needed.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.Prepare(fname:string):boolean;
begin
  Filename := fname;
  fLoaded := false;
  Prepare := true;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Ready
//
// Purpose: Auto-loads the group if needed
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroup.Ready;
begin
  if fLoaded or (Filename='') then exit;

  Load(Filename);
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Load
//
// Purpose: Loads a file into a symbol group
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.Load(fname:string):boolean;
var
  f       : TFileStream;
  id      : integer;
  version : integer;
  sym     : Symbol;
  P       : PChar;
  IsXML   : Boolean;
  D       : TDOMDocument;
  I,J,K   : Integer;
  E       : Array Of TDOMElement;
  E1      : TDOMElement;
  St      : String;

begin
  Result := False;
  f      := nil;
  try
    f := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);

    // Check to see if it's an XML file

    GetMem(P,6);
    StrPCopy(P,'12345');
    F.ReadBuffer(P^,5);
    IsXML := (UpperCase(Trim(StrPas(P))) = '<?XML');
    FreeMem(P,6);
    F.Position := 0;

    If IsXML Then
    Begin
      D := MainForm.XMLToDOMParser1.streamToDom(F);
      I := GetIntegerProperty(D.documentElement,'COUNT');
      If I > 0 Then
      Begin
        Clear;
        Filename := fname;
        SetLength(E,I);
        E1 := D.documentElement.findFirstChildElement;
        While E1 <> Nil Do
        Begin
          If System.Copy(E1.tagName,1,7) = 'SYMBOL_' Then
          Begin
            St := Trim(E1.tagName);
            Val(System.Copy(St,8,Length(St)),J,K);
            If (J >= 0) And (J < I) And (K = 0) Then E[J] := E1;
          End;
          E1 := E1.findNextSiblingElement;
        End; // While
        For J := I - 1 DownTo 0 Do
        Begin
          sym := Symbol.Create;
          sym.ReadFromDOMElement(E[J]);

          // Actively delete symbols are owned by ProFantasy, and were
          // accidentally released in versions 1.14 through 1.18.
          // (Sorry, Chad--this isn't meant to make you look bad; the comment
          // is the only way I had to identify them!)
          if (sym.GetComments = 'Created by Chad Burnett')
           then sym.Free
           else AddSymbol(sym);
        End; // For J
        SetLength(E,0);
      End;
      MainForm.DOMImpl.freeDocument(D);
      MainForm.DOMImpl.freeUnusedASModels;
    End
    Else
    Begin
      f.ReadBuffer(id, sizeof(id));
      f.ReadBuffer(version, sizeof(version));
      if (id <> SymbolFileId) then begin
        ShowMessage(Format(res_symbolfile_file_notvalid,[filename]));
        f.Free;
        exit;
        end;

      if (version > SymbolVersion) then begin
        ShowMessage(Format(res_mapobj_file_ver_in,[filename]));
        f.Free;
        exit;
        end;

      Clear;
      Filename := fname;

      while (f.Position < f.Size) do begin
        sym := Symbol.Create;
        sym.Read(f,False,False);

        // Actively delete symbols are owned by ProFantasy, and were
        // accidentally released in versions 1.14 through 1.18.
        // (Sorry, Chad--this isn't meant to make you look bad; the comment
        // is the only way I had to identify them!)
        if (sym.GetComments = 'Created by Chad Burnett')
         then sym.Free
         else AddSymbol(sym);
        end;
    End;
    f.Free;
    Modified := false;
    Result   := true;
    fLoaded  := true;

  except
    else begin
      ShowMessage(Format (res_main_file_noopen,[fname]));
      f.Free;
      exit;
      end;
  end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Save
//
// Purpose: Saves symbol group into a file.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.Save(fname:string):boolean;
var f          : TFileStream;
    id         : integer;
    version    : integer;
    backupname : string;
    Ext        : String;
    p          : Symbol;
    D          : TDOMDocument;
    E          : TDOMElement;
    XML        : String;
    I          : Integer;

begin
  Ready;     // Prepare group by loading it if necessary

  Result:=false;

  if Settings.MakeBackups.Checked then begin
    backupname:=filename + '.bak';
    DeleteFile(PChar(backupname));
    RenameFile(filename,backupname);
    end;

  Filename := fname;
  Ext      := Uppercase(ExtractFileExt(filename));

  try
    f := TFileStream.Create(filename, fmCreate or fmShareExclusive);
    If Ext = '.AUSX' Then
    Begin
      D := MainForm.DOMImpl.createDocument('DOCUMENT',Nil);

      p := SymbolHead;
      I := 0;
      while p <> nil do
      begin
        E := D.createElement('SYMBOL_' + IntToStr(I));
        D.documentElement.appendChild(E);
        E.appendChild(P.GetAsDOMElement(D));
        Inc(I);
        p := p.Next;
      end; // While
      D.documentElement.appendChild(NewIntegerProperty(D,'COUNT',I));

      MainForm.DOMToXMLParser1.writeToString(D,'Latin1',XML);
      f.WriteBuffer(PChar(XML)^, Length(XML));
      MainForm.DOMImpl.freeDocument(D);
      MainForm.DOMImpl.freeUnusedASModels;
    End
    Else
    Begin
      id      := SymbolFileId;
      version := SymbolVersion;
      f.WriteBuffer(id, sizeof(id));
      f.WriteBuffer(version, sizeof(version));

      p := SymbolHead;
      while p <> nil do
      begin
        p.Write(f,False,False);
        p := p.Next;
      end; // While
    End;
    f.Free;

    Modified := false;
    Result:=true;
  except
    else ShowMessage(Format(res_main_file_nosave, [filename]));
  end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.GroupName
//
// Purpose: Gets name of a group, which is the base of the filename.
//          If the group file is C:\AutoREALM\MyGroup.aus, Groupname='MyGroup'
//
// Notes:
//--------------------------------------------------------------------------
function  SymbolGroup.GroupName:string;
var s:string;
    extension_len:integer;
begin
  extension_len := length(ExtractFileExt(Filename));
  s := ExtractFilename(Filename);
  GroupName := copy(s,1,length(s)-extension_len);
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.Count
//
// Purpose: Returns number symbols in the group.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.Count:integer;
var n:integer;
    p:Symbol;
begin
  Ready;     // Prepare group by loading it if necessary

  n:=0;
  p:=SymbolHead;
  while (p<>nil) do begin
    inc(n);
    p := p.Next;
    end;
  Count := n;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.AddSymbol
//
// Purpose: Adds symbol to beginning of symbol list.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroup.AddSymbol(sym:Symbol);
begin
  sym.Next   := SymbolHead;
  SymbolHead := sym;
  Modified   := true;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.DeleteSymbol
//
// Purpose: Removes symbol from group.  If destroy is false, the symbol
//          is not destroyed, just removed from the list (someone else
//          owns the symbol).
//
// Notes: Deleting an item not in the list does nothing.
//--------------------------------------------------------------------------
procedure SymbolGroup.DeleteSymbol(which:Symbol; destroy:boolean);
var last,p:Symbol;
begin
  last:=nil;
  p:=SymbolHead;

  while (p<>nil) and (p<>which) do begin
    last := p;
    p := p.Next;
  end;

  if (p<>nil) then begin
    if (last=nil) then
      SymbolHead := p.Next
    else
      last.Next := p.Next;

    p.Next := nil;        // Not absolutely necessary, but isolate this object in its own chain.

    if destroy then begin
      p.Free;               // Delete the object.
      end;

    Modified := true;
  end;

end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.GetSymbol
//
// Purpose: Gets ptr to the indexed symbol.  Index is 0-based.
//
// Notes: Using an index larger than the number of symbols in the group
//        will return a nil pointer.
//--------------------------------------------------------------------------
function SymbolGroup.GetSymbol(index:integer):Symbol;
var p:Symbol;
begin
  Ready;     // Prepare group by loading it if necessary

  p:=SymbolHead;

  while (p<>nil) and (index<>0) do begin
    dec(index);
    p:=p.Next;
  end;

  GetSymbol:=p;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.FirstSymbol
//
// Purpose: Returns ptr to first symbol.
//
// Notes:
//--------------------------------------------------------------------------
function  SymbolGroup.FirstSymbol:Symbol;
begin
  Ready;     // Prepare group by loading it if necessary

  FirstSymbol:=SymbolHead;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.NextSymbol
//
// Purpose: Returns next symbol in the list, or nil if there are no more.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.NextSymbol(p:Symbol):Symbol;
begin
  Ready;     // Prepare group by loading it if necessary

  NextSymbol := p.Next;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.FindSymbol
//
// Purpose: Gets the symbol with the matching unique Id, or nil if not found.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroup.FindSymbol(uid:integer):Symbol;
begin
  Ready;     // Prepare group by loading it if necessary

  Result := FirstSymbol;
  while (Result<>nil) do begin
    if (Result.UniqueID = uid) then exit;
    Result := Result.Next;
    end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroup.RemoveCachedImage
//
// Purpose: Reindexes remainder of image indexes to fill the hole.
//--------------------------------------------------------------------------
procedure SymbolGroup.RemoveCachedImage(index:integer);
var p:Symbol;
begin
  p:=SymbolHead;

  while (p<>nil) do begin
    if (p.ImageIndex = index) then
      p.ImageIndex := -1
    else if (p.ImageIndex > index) then
      p.ImageIndex := p.ImageIndex - 1;

    p:=p.Next;
    end;
end;

// =====================================================================================



//--------------------------------------------------------------------------
// Name: SymbolGroupList.Create
//
// Purpose: Creates a list of groups.  One instances of this class is used
//          for the entire symbol tree view.
//
// Notes:
//--------------------------------------------------------------------------
constructor SymbolGroupList.Create;
begin
  GroupHead := nil;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.Destroy
//
// Purpose: Destroys a group list.
//
// Notes:
//--------------------------------------------------------------------------
destructor SymbolGroupList.Destroy;
begin
  Clear;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.Clear
//
// Purpose: Deletes all groups in the group list.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroupList.Clear;
var p,next:SymbolGroup;
begin
  p:=GroupHead;

  while (p<>nil) do begin
    next:=p.Next;
    p.Free;
    p:=next;
    end;

  GroupHead := nil;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.Save
//
// Purpose: Saves all *modified* group files to the given directory.
//
// Notes: Directory name must end with a backslash!
//--------------------------------------------------------------------------
procedure SymbolGroupList.Save(symdir:string);
var p:SymbolGroup;
begin
  symboldirectory:= symdir;
  p:=GroupHead;

  while (p<>nil) do begin
    if (p.Modified) then begin
      p.Save(symboldirectory + p.GroupName + '.AuSX');
    end;
    p := p.Next;
  end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.Load
//
// Purpose: Loads all group (.AuS) files in the given directory
//
// Notes: Directory name must end with a backslash!
//--------------------------------------------------------------------------
procedure SymbolGroupList.Load(symdir:string);
var
  SearchRec: TSearchRec;
  i,err : integer;
  symbol_list : TStringList;
  Ext : String;

begin
  symbol_list := TStringList.Create;

  Clear;
  SymbolDirectory := symdir;

  // Look for both standard binary and new XML symbol files

  err := FindFirst(symboldirectory + '*.AuS*', 0, SearchRec);
  while (err = 0) do
  begin
    Ext := UpperCase(ExtractFileExt(SearchRec.Name));
    If (Ext = '.AUS') Or (Ext = '.AUSX') Then
     symbol_list.Add(symboldirectory + SearchRec.Name);
    err:=FindNext(SearchRec);
  end;
  FindClose(SearchRec);

  symbol_list.Sort;

  // We create the groups in opposite order because each new
  // group is prepended to the head of the list
  for i:=symbol_list.Count-1 downto 0 do begin
      OpenGroup(symbol_list.Strings[i]);
  end;

  symbol_list.Destroy;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.DeleteGroup
//
// Purpose: Deletes the indexed group.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroupList.DeleteGroup(which:SymbolGroup);
var last,p:SymbolGroup;
begin
  last:=nil;
  p:=GroupHead;

  while (p<>nil) and (which<>p) do begin
    last := p;
    p := p.Next;
  end;

  if (p<>nil) then begin
    if (last=nil) then
      GroupHead := p.Next
    else
      last.Next := p.Next;

    DeleteFile(p.Filename);     // Remove the file from disk so we don't load it again
    p.Next := nil;        // Not absolutely necessary, but isolate this object in its own chain.
    p.Free;               // Delete the object.
  end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.OpenGroup
//
// Purpose: Adds a group to the list and loads the associated group file.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroupList.OpenGroup(filename:string);
var grp:SymbolGroup;
begin
   grp := SymbolGroup.Create;

   // Don't fully load the group: wait until we need it.
   // The loading of all the symbol groups on program startup is 1) annoying
   // because it takes a long time, and 2) wastes memory.  This way, we only
   // consume memory and time if we need to.
   grp.Prepare(filename);

   grp.Next := GroupHead;
   GroupHead := grp;
end;


//--------------------------------------------------------------------------
// Name: SymbolGroupList.NewGroup
//
// Purpose: Creates a new group not yet associated with a disk file.
//
// Notes:
//--------------------------------------------------------------------------
procedure SymbolGroupList.NewGroup(name:string);
var grp:SymbolGroup;
begin
   grp :=SymbolGroup.Create;
   grp.FileName :=symboldirectory + Name + '.AuSX';
   grp.Next := GroupHead;
   grp.Modified := true;
   GroupHead := grp;

   // Create group file so we can load it when we need to.
   // Mark as "loaded" so we don't try to load it before the file
   // has been created.
   grp.fLoaded := true;
   Save(symboldirectory);
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.Count
//
// Purpose: Returns number of group files.
//
// Notes:
//--------------------------------------------------------------------------
function  SymbolGroupList.Count:integer;
var n:integer;
    p:SymbolGroup;
begin
  n:=0;
  p:=GroupHead;
  while (p<>nil) do begin
    inc(n);
    p := p.Next;
    end;
  Count := n;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.FirstGroup
//
// Purpose: returns ptr to first group file.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroupList.FirstGroup:SymbolGroup;
begin
  FirstGroup := GroupHead;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.NextGroup
//
// Purpose: Gets ptr to next group file, or nil if no more.
//
// Notes:
//--------------------------------------------------------------------------
function SymbolGroupList.NextGroup(p:SymbolGroup):SymbolGroup;
begin
  NextGroup := p.Next;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.FindGroup
//
// Purpose: Returns the group with the given name or nil if not found.
//--------------------------------------------------------------------------
function SymbolGroupList.FindGroup(name:string):SymbolGroup;
begin
  Result := FirstGroup;
  while (Result<>nil) do begin
    if (Result.GroupName = name) then exit;
    Result := Result.Next;
    end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.CreateReference
//
// Purpose: Creates a text name that uniquely refers to a symbol in a group.
//          Used to link buttons with the symbols they represent.
//--------------------------------------------------------------------------
function SymbolGroupList.CreateReference(grp:SymbolGroup; sym:Symbol):string;
begin
  Result:='$' + IntToHex(sym.UniqueID,8) + '.' + grp.GroupName;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.GetReference
//
// Purpose: Converts a symbol reference retrieved from CreateReference
//          into a symbol.
//--------------------------------------------------------------------------
function SymbolGroupList.GetReference(ref:string):Symbol;
var uid:integer;
    gname:string;
    grp:SymbolGroup;
begin
  GetReference := nil;

  uid := StrToInt(copy(ref,1,9));
  gname := copy(ref,11,length(ref));

  grp:=FindGroup(gname);
  if (grp<>nil) then begin
    GetReference:=grp.FindSymbol(uid);
  end;
end;

//--------------------------------------------------------------------------
// Name: SymbolGroupList.RemoveCachedImage
//
// Purpose: Removes the referenced image, and reindexes remainder of images
//          to fill the hole.
//--------------------------------------------------------------------------
procedure SymbolGroupList.RemoveCachedImage(index:integer);
var grp:SymbolGroup;
begin
  // If image isn't cached, nothing to do, so exit.
  if (index=-1) then exit;

  // Delete the image from the cache
  MainForm.FavoritesImageList.Delete(index);

  // Re-index rest of icons, since removing this one has
  // shifted down any icons after it.  If we're deleting the image
  // from the actual icon, set it to -1 (not cached).

  grp := FirstGroup;
  while (grp<>nil) do begin
    grp.RemoveCachedImage(index);
    grp := grp.Next;
    end;
end;

Procedure BuildAntiAliasLookup;
Const
  W1 = 1;                    // Weights based on Exp(-r^2)
  W2 = {0.3679;}0.6412;
  W3 = {0.2431;}0.4111;
  N  = W1 + 4 * W2 + 4 * W3; // Normalization factor

Var
  M   : Array[0..8] Of Single;
  I,J : Integer;

Begin
  // Put the 1/255 part in here so we don't have to do a divide when the matrix is used

  M[0] := W3 / N / 255;
  M[1] := W2 / N / 255;
  M[2] := W3 / N / 255;
  M[3] := W2 / N / 255;
  M[4] := W1 / N / 255;
  M[5] := W2 / N / 255;
  M[6] := W3 / N / 255;
  M[7] := W2 / N / 255;
  M[8] := W1 / N / 255;
  For I := 0 To 8 Do
  Begin
    For J := 0 To 255 Do AntiAliasLookup[(I Shl 8) + J] := Round(M[I] * J * $01000000); // Fixed-point
  End; // For I
End; // BuildAntiAliasLookup

Initialization
  BuildAntiAliasLookup;
  AntiAliasBitmap             := TBitmap.Create;
  AntiAliasBitmap.Width       := 17;
  AntiAliasBitmap.Height      := 17;
  AntiAliasBitmap.PixelFormat := pf32Bit; // Convert to RGBA so we can use the ScanLine property to speed things up
Finalization
  AntiAliasBitmap.Free;
end.
