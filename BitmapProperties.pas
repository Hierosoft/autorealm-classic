unit BitmapProperties;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TBitmapPropertyDlg = class(TForm)
    cmdOK: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    radio320x200: TRadioButton;
    radio640x480: TRadioButton;
    radio800x600: TRadioButton;
    radio1024x768: TRadioButton;
    radioCustom: TRadioButton;
    customWidth: TEdit;
    customHeight: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    AreaView: TRadioButton;
    AreaWholeMap: TRadioButton;
    radioWindowSize: TRadioButton;
    lblWindowSize: TLabel;
    PaintBox: TPaintBox;
    lblBitmapSize: TLabel;
    radio1152x864: TRadioButton;
    radio1280x1024: TRadioButton;
    radio1600x1200: TRadioButton;
    JPEGSettings: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    JPEGQuality: TTrackBar;
    Label4: TLabel;
    JPEGImageSmoothing: TCheckBox;
    JPEGProgressiveEncoding: TCheckBox;
    procedure cmdOKClick(Sender: TObject);
    procedure radioCustomClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdatePreview(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    procedure AssignProperties;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BitmapPropertyDlg: TBitmapPropertyDlg;

  BitmapWidth,BitmapHeight:integer;
  UseAreaCurrent,UseAreaFull:boolean;

implementation

uses MAIN, MapObject, LocalizedStrings;

{$R *.lfm}

var viewWidth,viewHeight:integer;


procedure TBitmapPropertyDlg.radioCustomClick(Sender: TObject);
begin
  CustomWidth.SetFocus;
end;

procedure TBitmapPropertyDlg.AssignProperties;
begin
  if radio320x200.Checked then begin
    BitmapWidth := 320;
    BitmapHeight := 200;
    end
  else if radio640x480.Checked then begin
    BitmapWidth := 640;
    BitmapHeight := 480;
    end
  else if radio800x600.Checked then begin
    BitmapWidth := 800;
    BitmapHeight := 600;
    end
  else if radio1024x768.Checked then begin
    BitmapWidth := 1024;
    BitmapHeight := 768;
    end
  else if radio1152x864.Checked then begin
    BitmapWidth := 1152;
    BitmapHeight := 864;
    end
  else if radio1280x1024.Checked then begin
    BitmapWidth := 1280;
    BitmapHeight := 1024;
    end
  else if radio1600x1200.Checked then begin
    BitmapWidth := 1600;
    BitmapHeight := 1200;
    end
  else if radioWindowSize.Checked then begin
    BitmapWidth := viewWidth;
    BitmapHeight := viewHeight;
    end
  else begin
    BitmapWidth := StrToIntDef(CustomWidth.Text,100);
    BitmapHeight := StrToIntDef(CustomHeight.Text,100);
    end;

  UseAreaCurrent := false;
  UseAreaFull := false;

  if AreaView.Checked then UseAreaCurrent:=true;
  if AreaWholeMap.Checked then UseAreaFull:=true;
end;

procedure TBitmapPropertyDlg.cmdOKClick(Sender: TObject);
begin
  AssignProperties;
end;

procedure TBitmapPropertyDlg.FormShow(Sender: TObject);
var r:TRect;
begin
  r := MainForm.GetVisibleRect;

  viewWidth := r.Right - r.Left;
  viewHeight := r.Bottom - r.Top;

  lblWindowSize.Caption := IntToStr(viewWidth) + ' x ' + IntToStr(viewHeight);
end;

procedure TBitmapPropertyDlg.UpdatePreview(Sender: TObject);
var bitmap:TBitmap;
    r:TRect;
begin
  AssignProperties;
  r.Left := 0;
  r.Top := 0;
  r.Right := PaintBox.Width;
  r.Bottom := PaintBox.Height;

  PaintBox.Canvas.Pen.Color := clBlack;
  PaintBox.Canvas.Font.Color  := clBlack;

  Screen.Cursor:=crHourglass;

  try
    // Draw the bitmap
    bitmap := Map.GetBitmap(true, UseAreaCurrent, BitmapWidth, BitmapHeight);
    PaintBox.Canvas.StretchDraw(r,Bitmap);
    bitmap.Free;

    // Draw a frame
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Rectangle(0,0,PaintBox.Width,PaintBox.Height);

    lblBitmapSize.Caption := IntToStr(BitmapWidth) + ' x ' + IntToStr(BitmapHeight);

  except
    PaintBox.Canvas.Brush.Color := clWhite;
    PaintBox.Canvas.Rectangle(0,0,PaintBox.Width,PaintBox.Height);
    PaintBox.Canvas.TextRect(r,(PaintBox.Width - PaintBox.Canvas.TextWidth(res_bitmapproperties_badbitmap)) div 2,
                             PaintBox.Height div 2,res_bitmapproperties_badbitmap);
  end;

  Screen.Cursor:=crDefault;
end;

procedure TBitmapPropertyDlg.PaintBoxPaint(Sender: TObject);
begin
  UpdatePreview(Sender);
end;

end.
