unit CTManager;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Grids, Primitives;

type
  TCTColorRef = Class
    Old,New: TColor;
  End;
  TCTProfile = Class
  Protected
    FName     : String;
    FList     : TStringList;
    FFileName : String;
    Function  GetNumColors: Integer;
    Function  GetOldColor(Index: Integer): TColor;
    Function  GetNewColor(Index: Integer): TColor;
    Procedure SetNewColor(Index: Integer; C: TColor);
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   LoadFromFile(FileName: String);
    Procedure   SaveToFile(FileName: String); Overload;
    Procedure   SaveToFile;                   Overload;
    Procedure   Add(Old,New: TColor);
    Procedure   Delete(Index: Integer);
    Function    IndexOfOld(Old: TColor): Integer;
    Function    IndexOfNew(New: TColor): Integer;
    Procedure   CopyFrom(P: TCTProfile);
    Procedure   Clear;
    Property    Name      : String           Read FName     Write FName;
    Property    FileName  : String           Read FFileName Write FFileName;
    Property    NumColors : Integer          Read GetNumColors;
    Property    OldColor[I: Integer]: TColor Read GetOldColor;
    Property    NewColor[I: Integer]: TColor Read GetNewColor Write SetNewColor;
  End;
  TfrmCTManager = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    imgDrawArea: TImage;
    lbSelectionItems: TListBox;
    ColorButton1: TColorButton;
    dgSelectionColors: TDrawGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbProfiles: TComboBox;
    Bevel1: TBevel;
    btnNew: TButton;
    btnSave: TButton;
    btnRename: TButton;
    btnRemoveTranslation: TButton;
    sgProfile: TStringGrid;
    btnOk: TBitBtn;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Label4: TLabel;
    ColorButton2: TColorButton;
    btnAdd: TButton;
    btnDelete: TButton;
    lblModified: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbSelectionItemsClick(Sender: TObject);
    procedure dgSelectionColorsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure cbProfilesChange(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure sgProfileDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure btnAddClick(Sender: TObject);
    procedure sgProfileClick(Sender: TObject);
    procedure ColorButton2Change(Sender: TObject);
    procedure btnRemoveTranslationClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var FormCloseAction: TCloseAction);
    procedure dgSelectionColorsClick(Sender: TObject);
  private
    { Private declarations }
    View                    : ViewPoint;
    SelectedList            : TStringList;
    PrimitiveColors         : TStringList;
    PrimitiveColorListValid : Boolean;
    CTProfiles              : TStringList;
    WorkProfile             : TCTProfile;
    Modified                : Boolean;
    Procedure PopulateColorList(D: DrawPrimitive);
    Procedure AddColor(List: TStringList; C: TColor);
    Procedure SetupColorList;
    Procedure SetupProfileColorList;
    Procedure ClearCTProfiles;
    Procedure LoadCTProfiles;
    Procedure SetModified(B: Boolean);
    Procedure Translate(D: DrawPrimitive);
    Procedure InverseTranslate(D: DrawPrimitive);
  public
    { Public declarations }
    Procedure TranslateSelectionColors;
    Procedure InverseTranslateSelectionColors;
  end;

var
  frmCTManager: TfrmCTManager;

implementation

{$R *.dfm}

Uses MapObject,Geometry;

// TCTProfile

Constructor TCTProfile.Create;
Begin
  FName     := '';
  FFileName := '';
  FList     := TStringList.Create;
End; // TCTProfile.Create

Destructor TCTProfile.Destroy;
Begin
  Clear;
  FList.Free;
End; // TCTProfile.Destroy

Procedure TCTProfile.LoadFromFile(FileName: String);
Var
  F     : System.Text;
  St    : String;
  St1   : String;
  I,J   : Integer;
  C1,C2 : LongWord;
  TCR   : TCTColorRef;

Begin
  If FileExists(FileName) Then
  Begin
    Clear;
    FFileName := FileName;
    AssignFile(F,FileName);
    Reset(F);
    If Not Eof(F) Then
    Begin
      ReadLn(F,FName);
      FName := Trim(FName);
      While Not Eof(F) Do
      Begin
        ReadLn(F,St);
        I := Pos(',',St);
        If I <> 0 Then
        Begin
          St1 := Trim(Copy(St,1,I - 1));
          St  := Trim(Copy(St,I + 1,Length(St)));
          Val('$' + St1,C1,I);
          Val('$' + St,C2,J);
          If (I = 0) And (J = 0) Then
          Begin
            TCR := TCTColorRef.Create;
            TCR.Old := TColor(C1);
            TCR.New := TColor(C2);
            FList.AddObject(St1,TCR);
          End;
        End;
      End; // While
    End;
    CloseFile(F);
  End;
End; // TCTProfile.LoadFromFile

Procedure TCTProfile.SaveToFile(FileName: String);
Var
  F   : System.Text;
  I   : Integer;
  TCR : TCTColorRef;

Begin
  AssignFile(F,FileName);
  ReWrite(F);
  WriteLn(F,FName);
  For I := 0 To FList.Count - 1 Do
  Begin
    TCR := FList.Objects[I] As TCTColorRef;
    WriteLn(F,IntToHex(LongWord(TCR.Old),8),', ',IntToHex(LongWord(TCR.New),8));
  End; // For I
  CloseFile(F);
End; // TCTProfile.SaveToFile

Procedure TCTProfile.SaveToFile;
Begin
  If FFileName <> '' Then SaveToFile(FFileName);
End; // TCTProfile.SaveToFile

Procedure TCTProfile.Add(Old,New: TColor);
Var
  I   : Integer;
  TCR : TCTColorRef;

Begin
  I := FList.IndexOf(IntToHex(LongWord(Old),8));
  If I < 0 Then
  Begin
    TCR     := TCTColorRef.Create;
    TCR.Old := Old;
    TCR.New := New;
    FList.AddObject(IntToHex(LongWord(Old),8),TCR);
  End
  Else (FList.Objects[I] As TCTColorRef).New := New;
End; // TCTProfile.Add

Procedure TCTProfile.Delete(Index: Integer);
Begin
  If (Index >= 0) And (Index < FList.Count) Then
  Begin
    FList.Objects[Index].Free;
    FList.Delete(Index);
  End;
End; // TCTProfile.Delete

Function TCTProfile.IndexOfOld(Old: TColor): Integer;
Begin
  Result := FList.IndexOf(IntToHex(LongWord(Old),8));
End; // TCTProfile.IndexOfOld

Function TCTProfile.IndexOfNew(New: TColor): Integer;
Var
  I     : Integer;
  Found : Boolean;
  TCR   : TCTColorRef;

Begin
  // Have to do it the hard way :(
  // There is NO guarantee that this will get the "right" one, if multiple
  // colors map to the same color.  All we can do is get the first one.

  I     := 0;
  Found := False;
  While (I < FList.Count) And Not Found Do
  Begin
    TCR := FList.Objects[I] As TCTColorRef;
    If TCR.New = New Then Found := True Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TCTProfile.IndexOfNew

Procedure TCTProfile.Clear;
Var I: Integer;
Begin
  Name      := '(none)';
  FFileName := '';
  For I := 0 To FList.Count - 1 Do FList.Objects[I].Free;
  FList.Clear;
End; // TCTProfile.Clear

Function TCTProfile.GetNumColors: Integer;
Begin
  Result := FList.Count;
End; // TCTProfile.GetNumColors

Function TCTProfile.GetOldColor(Index: Integer): TColor;
Begin
  If (Index >= 0) And (Index < FList.Count)
   Then Result := (FList.Objects[Index] As TCTColorRef).Old
   Else Result := TColor(0);
End; // TCTProfile.GetOldColor

Function TCTProfile.GetNewColor(Index: Integer): TColor;
Begin
  If (Index >= 0) And (Index < FList.Count)
   Then Result := (FList.Objects[Index] As TCTColorRef).New
   Else Result := TColor(0);
End; // TCTProfile.GetNewColor

Procedure TCTProfile.SetNewColor(Index: Integer; C: TColor);
Begin
  If (Index >= 0) And (Index < FList.Count) Then
   (FList.Objects[Index] As TCTColorRef).New := C;
End; // TCTProfile.SetNewColor

Procedure TCTProfile.CopyFrom(P: TCTProfile);
Var
  I    : Integer;
  TCR1 : TCTColorRef;
  TCR2 : TCTColorRef;

Begin
  Clear;
  FName     := P.FName;
  FFileName := P.FFileName;
  For I := 0 To P.FList.Count - 1 Do
  Begin
    TCR1 := TCTColorRef.Create;
    TCR2 := P.FList.Objects[I] As TCTColorRef;
    TCR1.Old := TCR2.Old;
    TCR1.New := TCR2.New;
    FList.AddObject(P.FList.Strings[I],TCR1);
  End; // For I
End; // TCTProfile.CopyFrom

// TfrmCTManager

procedure TfrmCTManager.FormShow(Sender: TObject);
Var
  D  : DrawPrimitive;
  St : String;

begin
  SetModified(False);
  PrimitiveColorListValid := False;
  sgProFile.ColWidths[0]  := Canvas.TextWidth('From') + 16;
  sgProfile.ColCount      := 1;
  btnRename.Enabled       := False;
  btnDelete.Enabled       := False;
  lbSelectionItems.Clear;
  SelectedList.Clear;
  D := Map.First;
  While D <> Nil Do
  Begin
    If D.IsSelected Then
    Begin
           If D Is BitmapPrimitive    Then St := 'Bitmap'
      Else If D Is CurvePrimitive     Then St := 'Curve'
      Else If D Is GroupPrimitive     Then St := 'Group'
      Else If D Is HyperlinkPrimitive Then St := 'Hyperlink'
      Else If D Is LinePrimitive      Then St := 'Line'
      Else If D Is PolyCurvePrimitive Then St := 'Polycurve'
      Else If D Is PolyLinePrimitive  Then St := 'Polyline'
      Else If D Is SymbolPrimitive    Then St := 'Symbol'
      Else If D Is TextCurvePrimitive Then St := 'Text curve'
      Else If D Is TextPrimitive      Then St := 'Text'
      Else St := 'Draw object';
      SelectedList.AddObject(St,D);
      lbSelectionItems.Items.Add(St);
    End;
    D := D.Next;
  End; // While
  SetupProfileColorList;
end;

procedure TfrmCTManager.FormResize(Sender: TObject);
Var I: Integer;
begin
  I := (dgSelectionColors.Width - 6) Div (dgSelectionColors.DefaultColWidth + 1);
  If I < 1 Then I := 1;
  dgSelectionColors.ColCount := I;
end;

procedure TfrmCTManager.FormDestroy(Sender: TObject);
begin
  WorkProfile.Free;
  ClearCTProfiles;
  CTProfiles.Free;
  SelectedList.Free;
  PrimitiveColors.Free;
  View.Canvas := Nil;
  View.Free;
end;

procedure TfrmCTManager.FormCreate(Sender: TObject);
Var Rect: CoordRect;
begin
  WorkProfile                := TCTProfile.Create;
  PrimitiveColorListValid    := False;
  SelectedList               := TStringList.Create;
  PrimitiveColors            := TStringList.Create;
  CTProfiles                 := TStringList.Create;
  PrimitiveColors.Sorted     := True;
  PrimitiveColors.Duplicates := dupIgnore;
  Rect.Left    := 0;
  Rect.Top     := 0;
  Rect.Right   := 1;
  Rect.Bottom  := 1;
  View         := ViewPoint.Create(Rect,imgDrawArea.Width,imgDrawArea.Height);
  View.Canvas  := imgDrawArea.Canvas;
  LoadCTProfiles;
end;

procedure TfrmCTManager.lbSelectionItemsClick(Sender: TObject);
Var
  I : Integer;
  D : DrawPrimitive;
  R : CoordRect;
  C : Coord;

begin
  I := lbSelectionItems.ItemIndex;
  If (I >= 0) And (I < SelectedList.Count) Then
  Begin
    // Get the object's extent and back off from it a little bit

    D := SelectedList.Objects[I] As DrawPrimitive;
    D.ComputeExtent;
    R := D.Extent;
    C := R.Bottom - R.Top;
    R.Top    := R.Top    - C * 0.05;
    R.Bottom := R.Bottom + C * 0.05;
    C := R.Right - R.Left;
    R.Left   := R.Left   - C * 0.05;
    R.Right  := R.Right  + C * 0.05;
    View.SetCoordinateRect(R);
    View.Canvas.Brush.Color := clWhite;
    View.Canvas.Brush.Style := bsSolid;
    View.Canvas.Rectangle(0,0,imgDrawArea.Width,imgDrawArea.Height);
    D.Draw(View);
    PrimitiveColorListValid := False;
    PrimitiveColors.Clear;
    PopulateColorList(D);
    PrimitiveColorListValid := True;
    SetupColorList;
    dgSelectionColors.Invalidate;
  End;
end;

Procedure TfrmCTManager.AddColor(List: TStringList; C: TColor);
Begin
  List.Add(IntToHex(LongWord(C),8));
End; // TfrmCTManager.AddColor

Procedure TfrmCTManager.PopulateColorList(D: DrawPrimitive);
Var D1: DrawPrimitive;
Begin
  If D Is GroupPrimitive Then
  Begin
    D1 := (D As GroupPrimitive).First;
    While D1 <> Nil Do
    Begin
      PopulateColorList(D1);
      D1 := D1.Next;
    End; // While
  End
  Else
  Begin
    AddColor(PrimitiveColors,D.GetColor);
    AddColor(PrimitiveColors,D.GetFillColor);
  End;
End; // TfrmCTManager.PopulateColorList

Procedure TfrmCTManager.SetupColorList;
Var I: Integer;
Begin
  If dgSelectionColors.ColCount > 0 Then
  Begin
    I := PrimitiveColors.Count Div dgSelectionColors.ColCount;
    If I * dgSelectionColors.ColCount < PrimitiveColors.Count Then Inc(I);
    If I < 1 Then I := 1;
    dgSelectionColors.RowCount := I;
  End;
End; // TfrmCTManager.SetupColorList

Procedure TfrmCTManager.SetupProfileColorList;
Begin
  sgProfile.ColCount := WorkProfile.NumColors + 1;
  If (sgProfile.ColCount > 1) And (sgProfile.Col < 1) Then sgProfile.Col := 1;
  sgProfileClick(Self);
  sgProfile.Invalidate;
End; // TfrmCTManager.SetupProfileColorList

procedure TfrmCTManager.dgSelectionColorsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  I,J : Integer;
  L   : LongWord;
  C   : TColor;

begin
  I := ARow * dgSelectionColors.ColCount + ACol;
  If (I >= 0) And (I < PrimitiveColors.Count) Then
  Begin
    Val('$' + PrimitiveColors.Strings[I],L,J);
    If J = 0 Then
    Begin
      C := TColor(L);
      dgSelectionColors.Canvas.Pen.Color   := C;
      dgSelectionColors.Canvas.Brush.Color := C;
      dgSelectionColors.Canvas.Brush.Style := bsSolid;
      dgSelectionColors.Canvas.Rectangle(Rect);
    End;
  End
  Else
  Begin
    dgSelectionColors.Canvas.Pen.Color   := dgSelectionColors.Color;
    dgSelectionColors.Canvas.Brush.Color := dgSelectionColors.Color;
    dgSelectionColors.Canvas.Brush.Style := bsSolid;
    dgSelectionColors.Canvas.Rectangle(Rect);
    dgSelectionColors.Canvas.Pen.Color   := clBlack;
    dgSelectionColors.Canvas.MoveTo(Rect.Left,Rect.Top);
    dgSelectionColors.Canvas.LineTo(Rect.Right,Rect.Bottom);
    dgSelectionColors.Canvas.MoveTo(Rect.Left,Rect.Bottom - 1);
    dgSelectionColors.Canvas.LineTo(Rect.Right,Rect.Top - 1);
  End;
end;

Procedure TfrmCTManager.LoadCTProfiles;
Var
  S  : TSearchRec;
  CP : TCTProfile;

Begin
  ClearCTProfiles;
  If FindFirst(ExtractFilePath(Application.ExeName) + '*.AuP',faAnyFile,S) = 0 Then
  Begin
    Repeat
      CP := TCTProfile.Create;
      CP.LoadFromFile(ExtractFilePath(Application.ExeName) + S.Name);
      CTProfiles.AddObject('',CP);
      cbProfiles.Items.Add(CP.Name);
    Until FindNext(S) <> 0;
  End;
End; // TfrmCTManager.LoadCTProfiles

Procedure TfrmCTManager.ClearCTProfiles;
Var I: Integer;
Begin
  For I := 0 To CTProfiles.Count - 1 Do CTProfiles.Objects[I].Free;
  CTProfiles.Clear;
  cbProfiles.Clear;
End; // TfrmCTManager.ClearCTProfiles

procedure TfrmCTManager.cbProfilesChange(Sender: TObject);
var
  B: Boolean;
  I: Integer;
begin
  if Modified then
  begin
    B := (MessageDlg('Warning',
                     'You have changed this profile!'#13#10'Discard changes?',
                     mtWarning, [mbYes, mbNo], 0) = mrYes);
  end
  else
    B := True;

  if B then
  begin
    I := cbProfiles.ItemIndex;
    if (I >= 0) and (I < CTProfiles.Count) then
      WorkProfile.CopyFrom(TCTProfile(CTProfiles.Objects[I]));

    btnRename.Enabled := True;
    btnDelete.Enabled := True;
    SetupProfileColorList;
    SetModified(False);
  end;
end;


procedure TfrmCTManager.btnNewClick(Sender: TObject);
var
  B: Boolean;
begin
  if Modified then
  begin
    B := (MessageDlg('Warning',
                     'You have changed this profile!'#13#10'Discard changes?',
                     mtWarning, [mbYes, mbNo], 0) = mrYes);
  end
  else
    B := True;

  if B then
  begin
    WorkProfile.Clear;
    SetModified(False);
    cbProfiles.ItemIndex := -1;
    btnRename.Enabled := False;
    btnDelete.Enabled := False;
    SetupProfileColorList;
  end;
end;

procedure TfrmCTManager.btnSaveClick(Sender: TObject);
Var
  B  : Boolean;
  St : String;
  I  : Integer;
  P  : TCTProfile;

begin
  If WorkProfile.FileName = '' Then
  Begin
    St := Trim(InputBox('Enter name of new profile','Profile name:',''));
    B := (St <> '');
    If B Then
    Begin
      // Resolve name conflicts

      While cbProfiles.Items.IndexOf(St) >= 0 Do St := St + 'a';
      WorkProfile.Name := St;
      For I := 1 To Length(St) Do
      Begin
        If St[I] In ['*','/','\','+','?',':','^','|','''','"','<','>'] Then St[I] := '_';
      End; // For I

      // Resolve filename conflicts

      While FileExists(ExtractFilePath(Application.ExeName) + St + '.AuP') Do St := St + 'a';
      WorkProfile.FileName := ExtractFilePath(Application.ExeName) + St + '.AuP';

      // Add the new profile

      P := TCTProfile.Create;
      P.CopyFrom(WorkProfile);
      CTProfiles.AddObject('',P);
      cbProfiles.Items.Add(P.Name);
      cbProfiles.ItemIndex := cbProfiles.Items.Count - 1;
    End;
  End
  Else B := True;
  If B Then
  Begin
    WorkProfile.SaveToFile;
    SetModified(False);
    btnRename.Enabled := True;
    btnDelete.Enabled := True;
    (CTProfiles.Objects[cbProfiles.ItemIndex] As TCTProfile).CopyFrom(WorkProfile);
  End;
end;

procedure TfrmCTManager.btnRenameClick(Sender: TObject);
Var St: String;
begin
  If (cbProfiles.ItemIndex >= 0) And (cbProfiles.ItemIndex < CTProfiles.Count) Then
  Begin
    St := Trim(InputBox('Enter name of new profile','Profile name:',''));
    If St <> '' Then
    Begin
      // Resolve name conflicts, but keep the same filename

      While cbProfiles.Items.IndexOf(St) >= 0 Do St := St + 'a';
      WorkProfile.Name := St;
      (CTProfiles.Objects[cbProfiles.ItemIndex] As TCTProfile).Name := St;
      cbProfiles.Items.Strings[cbProfiles.ItemIndex] := St;
    End;
  End;
end;

procedure TfrmCTManager.sgProfileDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  C     : TColor;
  St    : String;
  XC,YC : Integer;
  TW,TH : Integer;

begin
  If ACol = 0 Then
  Begin
    C := sgProfile.FixedColor;
    sgProfile.Canvas.Pen.Color   := C;
    sgProfile.Canvas.Brush.Color := C;
    sgProfile.Canvas.Brush.Style := bsSolid;
    sgProfile.Canvas.Rectangle(Rect);
    sgProfile.Canvas.Pen.Color   := clBtnHighlight;
    sgProfile.Canvas.MoveTo(Rect.Left,Rect.Bottom);
    sgProfile.Canvas.LineTo(Rect.Left,Rect.Top);
    sgProfile.Canvas.LineTo(Rect.Right,Rect.Top);
    sgProfile.Canvas.Pen.Color   := clBlack;
    sgProfile.Canvas.MoveTo(Rect.Right - 1,Rect.Top);
    sgProfile.Canvas.LineTo(Rect.Right - 1,Rect.Bottom - 1);
    sgProfile.Canvas.LineTo(Rect.Left,Rect.Bottom - 1);
    sgProfile.Canvas.Pen.Color   := clBtnShadow;
    sgProfile.Canvas.MoveTo(Rect.Right - 2,Rect.Top);
    sgProfile.Canvas.LineTo(Rect.Right - 2,Rect.Bottom - 2);
    sgProfile.Canvas.LineTo(Rect.Left,Rect.Bottom - 2);
    sgProfile.Canvas.Font.Color := clBtnText;
    XC := (Rect.Left + Rect.Right)  Div 2;
    YC := (Rect.Top  + Rect.Bottom) Div 2;
    St := 'From';
    TW := sgProfile.Canvas.TextWidth(St);
    TH := sgProfile.Canvas.TextHeight(St);
    sgProfile.Canvas.TextOut(XC - (TW Div 2) - 1,YC - (TH Div 2) - (sgProfile.DefaultRowHeight Div 4) - 1,St);
    St := 'To';
    TW := sgProfile.Canvas.TextWidth(St);
    TH := sgProfile.Canvas.TextHeight(St);
    sgProfile.Canvas.TextOut(XC - (TW Div 2) - 1,YC - (TH Div 2) + (sgProfile.DefaultRowHeight Div 4) - 1,St);
  End
  Else If ACol <= WorkProfile.NumColors Then
  Begin
    C := WorkProfile.OldColor[ACol - 1];
    sgProfile.Canvas.Pen.Color   := C;
    sgProfile.Canvas.Brush.Color := C;
    sgProfile.Canvas.Brush.Style := bsSolid;
    sgProfile.Canvas.Rectangle(Rect.Left,Rect.Top,Rect.Right,sgProfile.DefaultRowHeight Div 2);

    C := WorkProfile.NewColor[ACol - 1];
    sgProfile.Canvas.Pen.Color   := C;
    sgProfile.Canvas.Brush.Color := C;
    sgProfile.Canvas.Brush.Style := bsSolid;
    sgProfile.Canvas.Rectangle(Rect.Left,sgProfile.DefaultRowHeight Div 2,Rect.Right,Rect.Bottom);
  End
  Else
  Begin
    C := sgProfile.Color;
    sgProfile.Canvas.Pen.Color   := C;
    sgProfile.Canvas.Brush.Color := C;
    sgProfile.Canvas.Brush.Style := bsSolid;
    sgProfile.Canvas.Rectangle(Rect);
  End;
  If (gdSelected In State) And (ACol > 0) Then
  Begin
    sgProfile.Canvas.Brush.Style := bsClear;
    sgProfile.Canvas.Pen.Color := clBlack;
    sgProfile.Canvas.Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
    sgProfile.Canvas.Pen.Color := clWhite;
    sgProfile.Canvas.Rectangle(Rect.Left + 1,Rect.Top + 1,Rect.Right - 1,Rect.Bottom - 1);
    sgProfile.Canvas.Brush.Style := bsSolid;
  End;
end;

procedure TfrmCTManager.btnAddClick(Sender: TObject);
Var
  I,J : Integer;
  L   : LongWord;
  C   : TColor;

begin
  If (dgSelectionColors.Col >= 0) And
     (dgSelectionColors.Row >= 0) Then
  Begin
    I := dgSelectionColors.Row * dgSelectionColors.ColCount + dgSelectionColors.Col;
    If (I >= 0) And (I < PrimitiveColors.Count) Then
    Begin
      Val('$' + PrimitiveColors.Strings[I],L,J);
      If J = 0 Then
      Begin
        C := TColor(L);
        WorkProfile.Add(C,ColorButton1.Color);
        SetModified(True);
        SetupProfileColorList;
      End;
    End;
  End;
end;

procedure TfrmCTManager.sgProfileClick(Sender: TObject);
Var B: Boolean;
begin
  B := Modified;
  If (sgProfile.Col > 0) And (sgProfile.Col <= WorkProfile.NumColors) Then
  Begin
    ColorButton2.Color   := WorkProfile.NewColor[sgProfile.Col - 1];
    ColorButton2.Enabled := True;
  End
  Else ColorButton2.Enabled := False;
  btnRemoveTranslation.Enabled := ColorButton2.Enabled;
  SetModified(B);
end;

procedure TfrmCTManager.ColorButton2Change(Sender: TObject);
begin
  If (sgProfile.Col > 0) And (sgProfile.Col <= WorkProfile.NumColors) Then
  Begin
    WorkProfile.NewColor[sgProfile.Col - 1] := ColorButton2.Color;
    SetModified(True);
  End;
end;

procedure TfrmCTManager.btnRemoveTranslationClick(Sender: TObject);
begin
  if (sgProfile.Col > 0) and (sgProfile.Col <= WorkProfile.NumColors) then
  begin
    if MessageDlg('Warning',
                  'Remove this color association?'#13#10'This cannot be undone!',
                  mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      WorkProfile.Delete(sgProfile.Col - 1);
      SetModified(True);
      SetupProfileColorList;
    end;
  end;
end;

procedure TfrmCTManager.btnDeleteClick(Sender: TObject);
var
  I: Integer;
  St: String;
begin
  if (WorkProfile.FileName <> '') and
     (cbProfiles.ItemIndex >= 0) and
     (cbProfiles.ItemIndex < CTProfiles.Count) then
  begin
    St := 'Permanently DELETE profile "' + WorkProfile.Name + '"?'#13#10'This cannot be undone!';
    if MessageDlg('Warning', St, mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      if FileExists(WorkProfile.FileName) then
        DeleteFile(WorkProfile.FileName);
      WorkProfile.Clear;
      I := cbProfiles.ItemIndex;
      cbProfiles.ItemIndex := -1;
      cbProfiles.Items.Delete(I);
      CTProfiles.Objects[I].Free;
      CTProfiles.Delete(I);
      SetModified(False);
      btnRename.Enabled := False;
      btnDelete.Enabled := False;
      SetupProfileColorList;
    end;
  end;
end;

Procedure TfrmCTManager.SetModified(B: Boolean);
Begin
  Modified            := B;
  lblModified.Visible := B;
End; // TfrmCTManager.SetModified

Procedure TfrmCTManager.TranslateSelectionColors;
Var D: DrawPrimitive;
Begin
  D := Map.First;
  While D <> Nil Do
  Begin
    If D.IsSelected Then Translate(D);
    D := D.Next;
  End; // While
End; // TfrmCTManager.TranslateSelectionColors

Procedure TfrmCTManager.InverseTranslateSelectionColors;
Var D: DrawPrimitive;
Begin
  D := Map.First;
  While D <> Nil Do
  Begin
    If D.IsSelected Then InverseTranslate(D);
    D := D.Next;
  End; // While
End; // TfrmCTManager.InverseTranslateSelectionColors

Procedure TfrmCTManager.Translate(D: DrawPrimitive);
Var
  D1 : DrawPrimitive;
  I  : Integer;

Begin

  // The cleanest way to do this is to make the whole thing "unique".

  D.SplitOff;
  If D Is GroupPrimitive Then
  Begin
    D1 := (D As GroupPrimitive).First;
    While D1 <> Nil Do
    Begin
      Translate(D1);
      D1 := D1.Next;
    End; // While
  End
  Else
  Begin
    I := WorkProfile.IndexOfOld(D.GetColor);
    If I >= 0 Then D.SetColor(WorkProfile.NewColor[I]);
    I := WorkProfile.IndexOfOld(D.GetFillColor);
    If I >= 0 Then D.SetFillColor(WorkProfile.NewColor[I]);
  End;

  // Now see if this object in fact needs to be an alias.  Since this routine is
  // recursive anyway, we don't have to do the whole chain.

  D.AddToBaseOrCopies(False);
End; // TfrmCTManager.Translate

Procedure TfrmCTManager.InverseTranslate(D: DrawPrimitive);
Var
  D1 : DrawPrimitive;
  I  : Integer;

Begin

  // The cleanest way to do this is to make the whole thing "unique"

  D.SplitOff;
  If D Is GroupPrimitive Then
  Begin
    D1 := (D As GroupPrimitive).First;
    While D1 <> Nil Do
    Begin
      InverseTranslate(D1);
      D1 := D1.Next;
    End; // While
  End
  Else
  Begin
    I := WorkProfile.IndexOfNew(D.GetColor);
    If I >= 0 Then D.SetColor(WorkProfile.OldColor[I]);
    I := WorkProfile.IndexOfNew(D.GetFillColor);
    If I >= 0 Then D.SetFillColor(WorkProfile.OldColor[I]);
  End;

  // Now see if this object in fact needs to be an alias.  Since this routine is
  // recursive anyway, we don't have to do the whole chain.

  D.AddToBaseOrCopies(False);
End; // TfrmCTManager.InverseTranslate

procedure TfrmCTManager.FormClose(Sender: TObject;
  var FormCloseAction: TCloseAction);
begin
  lbSelectionItems.Clear;
end;

procedure TfrmCTManager.dgSelectionColorsClick(Sender: TObject);
Var
  I,J : Integer;
  L   : LongWord;

begin
  I := dgSelectionColors.Row * dgSelectionColors.ColCount + dgSelectionColors.Col;
  If (I >= 0) And (I < PrimitiveColors.Count) Then
  Begin
    Val('$' + PrimitiveColors.Strings[I],L,J);
    If J = 0 Then ColorButton1.Color := TColor(L);
  End;
end;

end.
