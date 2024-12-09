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
unit CustomPrint;
interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Spin, Printers, Printer4Lazarus, Geometry, Primitives,
  Dialogs, PrintersDlgs;

type
  TCustomPrintDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    PrinterGroup: TGroupBox;
    Label1: TLabel;
    PrinterList: TComboBox;
    PropertiesBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CommentField: TLabel;
    WhereField: TLabel;
    TypeField: TLabel;
    StatusField: TLabel;
    CopiesGroup: TGroupBox;
    Label6: TLabel;
    NumberCopies: TSpinEdit;
    Collate: TCheckBox;
    CollatedImage: TImage;
    UncollatedImage: TImage;
    PrintRangeGroup: TGroupBox;
    RegionAll: TRadioButton;
    RegionView: TRadioButton;
    ViewList: TComboBox;
    RegionCurrent: TRadioButton;
    PaintBox: TPaintBox;
    Bevel1: TBevel;
    TileGroup: TGroupBox;
    TileCB: TCheckBox;
    HorizontalTiles: TSpinEdit;
    Horizontal: TLabel;
    Vertical: TLabel;
    VerticalTiles: TSpinEdit;
    AreaTimer: TTimer;
    RegionSelected: TRadioButton;
    RegionScale: TRadioButton;
    PrintScale: TLabel;
    procedure CollateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TileClick(Sender: TObject);
    procedure PrinterListChange(Sender: TObject);
    procedure PropertiesBtnClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure RegionClick(Sender: TObject);
    procedure ViewListChange(Sender: TObject);
    procedure AreaTimerTimer(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    ShowRect:TRect;
    IsTiling:boolean;
    ShowingArea:boolean;
    Alternate:boolean;
    StartX,StartY:integer;
  public
    { Public declarations }
    OldSetting:TPrinterOrientation;
    TileWidth,TileHeight:integer;
    ViewRect:CoordRect;
    VisibleOverlays:OverlaySet;

    procedure UpdateArea;
  end;

var
  CustomPrintDialog: TCustomPrintDialog;

implementation

uses MapObject, Main, LocalizedStrings;
// PrintersDlgs, Dialogs;

// WinSpool
{$R *.dfm}

var SmallMap:TBitmap;
    MapView:ViewPoint;

{ TPrinterDevice -- public implementation of private class used
  in Printers unit.  Used to get access to PrinterDevices that
  are the associated Objects with the Printers TStringList of
  the Printer object}
type
  TPrinterDevice = class
    Driver, Device, Port: String;
    constructor Create(ADriver, ADevice, APort: PChar);
    function IsEqual(ADriver, ADevice, APort: PChar): Boolean;
  end;

constructor TPrinterDevice.Create(ADriver, ADevice, APort: PChar);
begin
  inherited Create;
  Driver := ADriver;
  Device := ADevice;
  Port := APort;
end;

function TPrinterDevice.IsEqual(ADriver, ADevice, APort: PChar): Boolean;
begin
  Result := (Device = ADevice) and ((Port = '') or (Port = APort));
end;

// These functions return values in printer units.
//   Before accessing the Printer.Handle, ensure the printer is initialized
//   by setting PrinterIndex or calling BeginDoc.
//   Without initialization, Printer.Handle might be invalid.

function GetPageWidth: Integer;
begin
  // Returns the total width of the paper in printer units
  Result := Printer.PageWidth;
end;

function GetPageHeight: Integer;
begin
  // Returns the total height of the paper in printer units
  Result := Printer.PageHeight;
end;

function GetPageOffsetLeft: Integer;
begin
  // Estimate the physical offset from the left edge of the paper
  Result := Printer.XDPI div 4; // Adjust based on typical offsets, if necessary
end;

function GetPageOffsetRight: Integer;
begin
  // Estimate the physical offset from the right edge of the paper
  Result := GetPageWidth - GetPageOffsetLeft - Printer.PageWidth;
end;

function GetPageOffsetTop: Integer;
begin
  // Estimate the physical offset from the top edge of the paper
  Result := Printer.YDPI div 4; // Adjust based on typical offsets, if necessary
end;

function GetPageOffsetBottom: Integer;
begin
  // Estimate the physical offset from the bottom edge of the paper
  Result := GetPageHeight - GetPageOffsetTop - Printer.PageHeight;
end;

function GetPixelsPerInchX: Integer;
begin
  // Returns the horizontal DPI (dots per inch)
  Result := Printer.XDPI;
end;

function GetPixelsPerInchY: Integer;
begin
  // Returns the vertical DPI (dots per inch)
  Result := Printer.YDPI;
end;



{ ------------------------------------------------------------ }

procedure TCustomPrintDialog.CollateClick(Sender: TObject);
begin
  if Collate.Checked then begin
    CollatedImage.Visible:=true;
    UnCollatedImage.Visible:=false;
    end
  else begin
    UnCollatedImage.Visible:=true;
    CollatedImage.Visible:=false;
    end;
end;

procedure TCustomPrintDialog.TileClick(Sender: TObject);
begin
  Horizontal.Enabled := TileCB.Checked;
  Vertical.Enabled := TileCB.Checked;
  HorizontalTiles.Enabled := TileCB.Checked;
  VerticalTiles.Enabled := TileCB.Checked;
end;

procedure TCustomPrintDialog.FormShow(Sender: TObject);
var i:integer;
    view:ViewPoint;
    viewcount:integer;
    cr:CoordRect;
begin
  OldSetting:=Printer.Orientation;
  if Map.Landscape then Printer.Orientation := poLandscape
                   else Printer.Orientation := poPortrait;

  PrintScale.Caption := MainForm.GridLabel.Caption;

  PrinterList.Items.Clear;
  for i:=0 to Printer.Printers.Count-1 do
    PrinterList.Items.Add(Printer.Printers[i]);

  PrinterList.ItemIndex:=Printer.PrinterIndex;
  PrinterListChange(Sender);

  ViewList.Items.Clear;
  viewcount:=Map.GetViewpoints;
  for i:=0 to viewcount-1 do begin
    view:=Map.GetViewPoint(i);

    if (view.Name='') then
      ViewList.Items.Add(LastViewName)
    else
      ViewList.Items.Add(view.Name);
    end;

  if (viewcount<>0) then ViewList.ItemIndex:=0;

  ViewList.Enabled:=(viewcount<>0);
  RegionView.Enabled:=(viewcount<>0);

  Screen.Cursor:=crHourglass;
  SmallMap:=TBitmap.Create;
  SmallMap.Width:=PaintBox.Width-1;
  SmallMap.Height:=PaintBox.Height-1;
  // Fill the background in the same color as the real map
  SmallMap.Canvas.Brush.Color:=MainForm.Color;
  SmallMap.Canvas.FillRect(Rect(0,0,SmallMap.Width,SmallMap.Height));

  MapView := ViewPoint.Create(Map.CurrentView);
  MapView.VisibleOverlays:=[0..255];
  MapView.Canvas:=SmallMap.Canvas;
  MapView.QuickDraw:=QuickDraw_Lines;
  MapView.SetCoordinateSize(SmallMap.Width,SmallMap.Height,false);
  Map.ShowAll(MapView, 0.0);
  MapView.GetCoordinateRect(cr);
  MapView.SetCoordinateRect(AdjustToPrinterPage(cr));
  Map.Draw(MapView,false);
  ShowingArea:=false;
  Alternate:=false;
  AreaTimer.Enabled:=true;
  Screen.Cursor:=crDefault;
end;

procedure TCustomPrintDialog.UpdateArea;
var OldMode:TPenMode;
    i,w,h,x,y:integer;
begin
  if (ViewRect.Left=ViewRect.Right) or (ViewRect.Top=ViewRect.Bottom) then exit;

  if not ShowingArea then begin
    ShowRect:=MapView.CoordToScreenRect(AdjustToPrinterPage(ViewRect));
    IsTiling:=TileCB.Checked;
    TileWidth:=1;
    TileHeight:=1;
    if IsTiling then begin
      try
        TileWidth:=HorizontalTiles.Value;
        TileHeight:=VerticalTiles.Value;
      except
        TileWidth:=1;
        TileHeight:=1;
        end;
      end;
  end;

  OldMode:=PaintBox.Canvas.Pen.Mode;
  PaintBox.Canvas.Pen.Mode:=pmNotXor;
  if Alternate then
    PaintBox.Canvas.Pen.Style:=psDot
  else
    Paintbox.Canvas.Pen.Style:=psDash;

  if IsTiling then begin
    PaintBox.Canvas.Pen.Color:=clBlue;

    w:=(ShowRect.Right-ShowRect.Left);
    h:=(ShowRect.Bottom-ShowRect.Top);

    y:=ShowRect.Top+h*TileHeight;
    for i:=0 to TileWidth do begin
      x:=ShowRect.Left+w*i;
      PaintBox.Canvas.MoveTo(x,ShowRect.Top+ord(i<=1)*h);
      PaintBox.Canvas.LineTo(x,y);
      end;

    x:=ShowRect.Left+w*TileWidth;
    for i:=0 to TileHeight do begin
      y:=ShowRect.Top+h*i;
      PaintBox.Canvas.MoveTo(ShowRect.Left+ord(i<=1)*w,y);
      PaintBox.Canvas.LineTo(x,y);
      end;
    end;

  PaintBox.Canvas.Pen.Color:=clRed;
  PaintBox.Canvas.MoveTo(ShowRect.Left,ShowRect.Top);
  PaintBox.Canvas.LineTo(ShowRect.Left,ShowRect.Bottom);
  PaintBox.Canvas.LineTo(ShowRect.Right,ShowRect.Bottom);
  PaintBox.Canvas.LineTo(ShowRect.Right,ShowRect.Top);
  PaintBox.Canvas.LineTo(ShowRect.Left,ShowRect.Top);
  PaintBox.Canvas.Pen.Mode:=OldMode;

  ShowingArea:=not ShowingArea;
  if not ShowingArea then Alternate:=not Alternate;
end;

procedure TCustomPrintDialog.PaintBoxPaint(Sender: TObject);
var view:ViewPoint;
PW,PH : Double; // Printer width, height in inches
begin
  VisibleOverlays:=Map.CurrentView.VisibleOverlays;
  if RegionAll.Checked then begin
    MapView.GetCoordinateRect(ViewRect);
    end
  else if RegionCurrent.Checked then begin
    Map.CurrentView.GetCoordinateRect(ViewRect);
    end
  else if RegionView.Checked then begin
    view:=Map.GetViewPoint(ViewList.ItemIndex);
    if view<>nil then begin
      View.GetCoordinateRect(ViewRect);
      VisibleOverlays:=View.VisibleOverlays;
      end;
    end
  else if RegionScale.Checked then begin
    Map.CurrentView.GetCoordinateRect(ViewRect);
    PW := Printer.PageWidth  / GetPixelsPerInchX;
    PH := Printer.PageHeight / GetPixelsPerInchY;
    ViewRect.Right  := ViewRect.Left + PW * (30 * 45 / 7);
    ViewRect.Bottom := ViewRect.Top  + PH * (30 * 45 / 7);

    // Fill in with code to set the print size so that the graph
    // is the correct number of squares per inch.  This requires
    // some rework in the grid settings too...
    end;


  PaintBox.Canvas.Draw(0,0,SmallMap);
  ShowingArea:=false;

  UpdateArea;
end;

procedure TCustomPrintDialog.FormHide(Sender: TObject);
begin
  Printer.Orientation:=OldSetting;
  AreaTimer.Enabled:=false;
  SmallMap.Free;
  MapView.Free;
end;

procedure TCustomPrintDialog.PrinterListChange(Sender: TObject);
var
  PrintDev: String; // Device name
  PortName: String; // Port name
  status: String;
begin
  { Ensure a printer is selected in the list }
  if PrinterList.ItemIndex < 0 then
    Exit;

  { Get the printer's name from the list }
  PrintDev := Printer.Printers[PrinterList.ItemIndex];

  { Determine if this is the default printer }
  if PrinterList.ItemIndex = Printer.PrinterIndex then
    StatusField.Caption := res_cprint_default
  else
    StatusField.Caption := '';

  { Lazarus does not provide direct port info, provide a generic fallback }
  TypeField.Caption := PrintDev; // Typically, the printer name
  WhereField.Caption := 'Unknown'; // Port information is unavailable in Lazarus

  { Add a placeholder comment and status }
  CommentField.Caption := ''; // No comment available cross-platform
  status := res_cprint_ready;

  { Determine printer status - Cross-platform abstraction }
  if Printer.Printers.Count > 0 then
  begin
    if Printer.PrinterIndex = PrinterList.ItemIndex then
      status := res_cprint_ready
    else
      status := res_cprint_not_available; // Generic fallback status
  end
  else
    status := res_cprint_error; // No printers detected

  StatusField.Caption := StatusField.Caption + status;
end;


procedure TCustomPrintDialog.PropertiesBtnClick(Sender: TObject);
var
  PrintDialog: TPrintDialog;
  deviceName: String;
begin
  deviceName := TypeField.Caption;

  PrintDialog := TPrintDialog.Create(nil);
  try
    {$IFDEF Windows}
    Printer.SetPrinter(deviceName, '', '', 0); // Ensure correct printer is selected
    if not PrintDialog.Execute then
      Exit;
    {$ELSE}
    // On non-Windows platforms, limited properties adjustment
    // TODO: print dialog
    ShowMessage('Please select printer settings manually. Custom dialogs can be implemented here.');
    // if not PrintDialog.Execute then
      Exit;
    {$ENDIF}

    { Update printer settings, such as orientation }
    if Printer.Orientation = poLandscape then
      Map.Landscape := True
    else
      Map.Landscape := False;
  finally
    {$IFDEF Windows}
    PrintDialog.Free;
    // TODO: {$ELSE}
    {$ENDIF}
  end;
end;



procedure TCustomPrintDialog.RegionClick(Sender: TObject);
begin
  RegionSelected.Enabled:=false;
  PaintBox.Repaint;
end;

procedure TCustomPrintDialog.ViewListChange(Sender: TObject);
begin
  RegionView.Checked:=true;
  PaintBox.Repaint;
end;

procedure TCustomPrintDialog.AreaTimerTimer(Sender: TObject);
begin
  UpdateArea;
  UpdateArea;
end;

procedure TCustomPrintDialog.OKBtnClick(Sender: TObject);
var
  PrinterDevice: string;
begin
  { Switch the printer if a different one is selected }
  if PrinterList.ItemIndex <> Printer.PrinterIndex then
  begin
    Printer.PrinterIndex := PrinterList.ItemIndex;

    // Get the printer name (cross-platform way)
    PrinterDevice := Printer.Printers[Printer.PrinterIndex];
    Printer.Title := PrinterDevice; // Optional: Set the document title

    // Lazarus automatically handles the underlying device name, driver, and port.
  end;

  try
    // Set the number of copies, fallback to 1 on error
    Printer.Copies := NumberCopies.Value;
  except
    Printer.Copies := 1;
  end;

  // Adjust the rectangle to fit the printer's page
  ViewRect := AdjustToPrinterPage(ViewRect);
end;

procedure TCustomPrintDialog.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var cx,cy:Coord;
begin
  RegionSelected.Enabled:=true;
  RegionSelected.Checked:=true;

  if ShowingArea then UpdateArea;

  StartX:=x;
  StartY:=y;
  MapView.ScreenToCoord(X,Y,cx,cy);
  ViewRect.Top:=cy;
  ViewRect.Bottom:=cy;
  ViewRect.Left:=cx;
  ViewRect.Right:=cx;

  UpdateArea;
end;

procedure TCustomPrintDialog.PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var cx,cy:Coord;
begin
  if RegionSelected.Checked and RegionSelected.Enabled then begin
    if ShowingArea then UpdateArea;

    MapView.ScreenToCoord(X,Y,cx,cy);

    if (abs(X-StartX)>abs(Y-StartY)) then begin
      ViewRect.Right:=cx;
      ViewRect.Bottom:=ViewRect.Top+(cx-ViewRect.Left)*Printer.PageHeight/Printer.PageWidth;
      end
    else begin
      ViewRect.Bottom:=cy;
      ViewRect.Right:=ViewRect.Left+(cy-ViewRect.Top)*Printer.PageWidth/Printer.PageHeight;
      end;

    UpdateArea;
    end;
end;

procedure TCustomPrintDialog.PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RegionSelected.Enabled := false;
end;

end.
