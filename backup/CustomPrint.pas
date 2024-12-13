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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Spin, Printers, Geometry, Primitives;

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
    Tile: TCheckBox;
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

uses WinSpool, MapObject, Main, LocalizedStrings;

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

// These functions return values in printer units

function GetPageWidth: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH)
end;

function GetPageHeight: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT)
end;

function GetPageOffsetLeft: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX)
end;

function GetPageOffsetRight: Integer;
begin
  Result := GetPageWidth - GetPageOffsetLeft - GetDeviceCaps(Printer.Handle, HORZRES)
end;

function GetPageOffsetTop: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY)
end;

function GetPageOffsetBottom: Integer;
begin
  Result := GetPageHeight - GetPageOffsetTop - GetDeviceCaps(Printer.Handle, VERTRES)
end;

function GetPixelsPerInchX: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSX)
end;

function GetPixelsPerInchY: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSY)
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
  Horizontal.Enabled := Tile.Checked;
  Vertical.Enabled := Tile.Checked;
  HorizontalTiles.Enabled := Tile.Checked;
  VerticalTiles.Enabled := Tile.Checked;
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
    IsTiling:=Tile.Checked;
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
var { Variables for querying the printer info }
    PrintDev:TPrinterDevice;
    PrinterHandle:Cardinal;
    PrinterInfo:PPrinterInfo2;
    PrinterInfoBuffer:array[0..1024] of char;
    InfoSize:DWORD;
    status:string;

begin
  { Figure out all the info that goes in the Printer group box }
  PrintDev:=TPrinterDevice(Printer.Printers.Objects[PrinterList.ItemIndex]);

  if PrinterList.ItemIndex=Printer.PrinterIndex then
    StatusField.Caption:=res_cprint_default
  else
    StatusField.Caption:='';

  TypeField.Caption:=PrintDev.Device;
  WhereField.Caption:=PrintDev.Port;

  CommentField.Caption:='';
  status:=res_cprint_ready;

  if OpenPrinter(PChar(PrintDev.Device), PrinterHandle, nil) then begin
    InfoSize:=sizeof(PrinterInfoBuffer);
    PrinterInfo:=PPrinterInfo2(@PrinterInfoBuffer);
    if GetPrinter(PrinterHandle,2,@PrinterInfoBuffer,sizeof(PrinterInfoBuffer),@InfoSize) then begin
      CommentField.Caption:=PrinterInfo.pComment;
      case PrinterInfo.Status of
        PRINTER_STATUS_PAUSED            : status:=res_cprint_paused;
        PRINTER_STATUS_ERROR             : status:=res_cprint_error;
        PRINTER_STATUS_PAPER_JAM         : status:=res_cprint_paper_jam;
        PRINTER_STATUS_PAPER_OUT         : status:=res_cprint_out_of_paper;
        PRINTER_STATUS_PAPER_PROBLEM     : status:=res_cprint_paper_problem;
        PRINTER_STATUS_OFFLINE           : status:=res_cprint_offline;
        PRINTER_STATUS_OUTPUT_BIN_FULL   : status:=res_cprint_output_bin_full;
        PRINTER_STATUS_NOT_AVAILABLE     : status:=res_cprint_not_available;
        PRINTER_STATUS_TONER_LOW         : status:=res_cprint_toner_low;
        PRINTER_STATUS_NO_TONER          : status:=res_cprint_no_toner;
        PRINTER_STATUS_OUT_OF_MEMORY     : status:=res_cprint_out_of_memory;
        PRINTER_STATUS_DOOR_OPEN         : status:=res_cprint_door_open;
        PRINTER_STATUS_SERVER_UNKNOWN    : status:=res_cprint_unknown_server;
        PRINTER_STATUS_POWER_SAVE        : status:=res_cprint_power_save;
        end;
      end;
    ClosePrinter(PrinterHandle);
    end;
  StatusField.Caption:=StatusField.Caption+status;
end;

procedure TCustomPrintDialog.PropertiesBtnClick(Sender: TObject);
var PrinterHandle:Cardinal;
    { Variables for printer switching }
    deviceName,driverName,portName:array[0..255] of char;
    Mode:THandle;
begin
  StrPCopy(deviceName, TypeField.Caption);

  if OpenPrinter(DeviceName, PrinterHandle, nil) then begin
    PrinterProperties(Handle, PrinterHandle);
    ClosePrinter(PrinterHandle);
    // Need to Get/Set the Printer to cause the Printer object to
    // pick up the new settings (most importantly, Portrait/Landscape).
    Printer.GetPrinter(deviceName, driverName, portName, mode);
    Printer.SetPrinter(deviceName, driverName, portName, 0);
    Map.Landscape := (Printer.Orientation = poLandscape);
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
var { Variables for printer switching }
    deviceName,driverName,portName:array[0..255] of char;
    Mode:THandle;
begin
  { Switch the printer that Delphi uses }
  if PrinterList.ItemIndex<>Printer.PrinterIndex then begin
    Printer.PrinterIndex:=PrinterList.ItemIndex;
    Printer.GetPrinter(deviceName, driverName, portName, mode);
    Printer.SetPrinter(deviceName, driverName, portName, 0);
    end;

  try
    Printer.Copies:=NumberCopies.Value;
  except
    Printer.Copies:=1;
  end;

  ViewRect:=AdjustToPrinterPage(ViewRect);
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
