unit CustomHint;

interface

uses Graphics;

  const
    clParchment = $E0FFFF;

  type
     TCustomHint = class
     private
        Underneath:TBitmap;
        UnderneathX,UnderneathY:integer;
        HintIsVisible:boolean;
        Canvas:TCanvas;
        sText:string;
        mStartX,mStartY:integer;
        mEndX,mEndY:integer;
        mOffsetX,mOffsetY:integer;

        procedure SetVisible(b:boolean);
        procedure SetText(s:string);
        procedure SetEndX(a:integer);
        procedure SetEndY(a:integer);
     public
        constructor Create(cv:TCanvas);
        destructor Destroy; override;
        property Visible:boolean read HintIsVisible write Setvisible;
        property Text:string     read sText         write SetText;
        property StartX:integer  read mStartX       write mStartX;
        property StartY:integer  read mStartY       write mStartY;
        property OffsetX:integer read mOffsetX      write mOffsetX;
        property OffsetY:integer read mOffsetY      write mOffsetY;
        property X:integer       read mEndX         write SetEndX;
        property Y:integer       read mEndY         write SetEndY;
     end;

     procedure ShowPopupBox(canvas:TCanvas; x,y:integer; s:string);
     procedure GetPopupSize(canvas:TCanvas; sText:string; var w,h:integer);

implementation


uses Classes, SysUtils, StrUtils;

procedure GetPopupSize(canvas: TCanvas; sText: string; var w, h: integer);
var
  lines: TStringList;
  i, lineWidth: integer;
begin
  // Split the text into lines to handle multi-line text
  lines := TStringList.Create;
  try
    // Normalize line endings to Unix style (#10), then back to Windows style (#13#10)
    sText := ReplaceStr(sText, #13#10, #10);
    sText := ReplaceStr(sText, #10, #13#10);
    lines.Text := sText;

    w := 0; // Start with zero width
    h := 0; // Start with zero height

    // Calculate the width and height of each line
    for i := 0 to lines.Count - 1 do
    begin
      lineWidth := Canvas.TextWidth(lines[i]);
      if lineWidth > w then
        w := lineWidth; // Update max width if current line is wider
      Inc(h, Canvas.TextHeight(lines[i]));
    end;

    // Add padding: 4 pixels space on either side of the width,
    // and 2 pixels space on top and bottom of the height
    w := w + 8;
    h := h + 4;
  finally
    lines.Free;
  end;
end;


procedure ShowPopupBox(canvas: TCanvas; x, y: integer; s: string);
var
  r: TRect;
  w, h: integer;
begin
  r.Left := x;
  r.Top := y;
  GetPopupSize(Canvas, s, w, h);
  r.Right := r.Left + w;
  r.Bottom := r.Top + h;

  with Canvas do
  begin
    Brush.Color := clParchment;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Font.Color := clBlack;
    FillRect(r);

    Brush.Color := clBlack;
    FrameRect(r);

    Brush.Color := clParchment;

    // Account for margins inside box: move text down and left a little.
    r.Left := r.Left + 4;
    r.Top := r.Top + 2;

    // Use TextRect instead of DrawText.
    TextRect(r, r.Left, r.Top, s);
  end;
end;


procedure TCustomHint.SetVisible(b:boolean);
var hintX,hintY:integer;
    w,h:integer;
begin
  if (b = HintIsVisible) then exit;

  if HintIsVisible then begin
    { Restore what's underneath our hint if we're showing it }
    Canvas.CopyRect(Rect(UnderneathX,UnderneathY,UnderneathX+Underneath.Width,UnderneathY+underneath.Height),
                    underneath.canvas,
                    Rect(0,0,underneath.Width,underneath.Height));
    end;

  if b then begin
    { Compute our hint message }
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 10;

    GetPopupSize(Canvas, sText, w,h);

    { Compute our starting location }
    if (mStartX > mEndX) then
      hintX := MEndX - w - 8 - mOffsetX
    else
      hintX := mEndX + 16 + mOffsetX;       {StartX < EndX }

    if (mStartY > mEndY) then
      hintY := mEndY - mOffsetY
    else
      hintY := MEndY + mOffsetY;

    { Save what's underneath the hint }
    UnderneathX := hintX;
    UnderneathY := hintY;
    underneath.Width := w;
    underneath.Height:= h;
    underneath.Canvas.CopyRect(Rect(0,0,w,h),Canvas,Rect(hintX,hintY,hintX+w,hintY+h));

    { Draw it }
    ShowPopupBox(Canvas,hintX,hintY,sText);
    end;

  HintIsVisible:=b;
end;

constructor TCustomHint.Create(cv:TCanvas);
begin
  Canvas:=cv;
  Underneath := TBitmap.Create;
  HintIsVisible:=false;
  mOffsetX:=0;
  mOffsetY:=0;
  mStartX:=0;
  mStartY:=0;
  mEndX:=0;
  mEndY:=0;
end;

destructor TCustomHint.Destroy;
begin
  Underneath.Free;
  inherited Destroy;
end;

procedure TCustomHint.SetText(s:string);
var oldvisible:boolean;
begin
  if s<>sText then begin
    oldvisible:=HintIsVisible;

    SetVisible(false);
    sText := s;
    SetVisible(oldvisible);
    end;
end;

procedure TCustomHint.SetEndX(a:integer);
var oldvisible:boolean;
begin
  if a<>mEndX then begin
    oldvisible:=HintIsVisible;

    SetVisible(false);
    mEndX := a;
    SetVisible(oldvisible);
    end;
end;

procedure TCustomHint.SetEndY(a:integer);
var oldvisible:boolean;
begin
  if a<>mEndY then begin
    oldvisible:=HintIsVisible;

    SetVisible(false);
    mEndY := a;
    SetVisible(oldvisible);
    end;
end;



end.
