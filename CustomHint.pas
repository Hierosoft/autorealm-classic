unit CustomHint;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Graphics;

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

uses Classes;

procedure GetPopupSize(canvas:TCanvas; sText:string; var w,h:integer);
var r:TRect;
begin
  // The DrawText function inflates the given rectangle's width/height.
  // Start with all zeros so we can just use the bottom right corner.
  r.Top := 0;
  r.Left:= 0;
  r.Bottom:=0;
  r.Right :=0;

  // Note that we must use DrawText instead of Delphi's conveinent TextWidth/TextHeight
  // because those functions are not sensitive to multiple lines.
  DrawText(Canvas.Handle, PChar(sText), -1, r, DT_LEFT + DT_NOPREFIX + DT_CALCRECT);

  // Width = Text + 4 pixels space on either side
  // Height = Text + 2 pixels spacing on either side
  w:=r.Right + 8;
  h:=r.Bottom + 4;
end;

procedure ShowPopupBox(canvas:TCanvas; x,y:integer; s:string);
var r:TRect;
    w,h:integer;
begin
  r.Left := x;
  r.Top  := y;
  GetPopupSize(Canvas, s, w, h);
  r.Right := r.Left + w;
  r.Bottom := r.Top + h;

  with Canvas do begin
    Brush.Color := clParchment;
    Brush.Style := bsSolid;
    Pen.Color   := clBlack;
    Font.Color  := clBlack;
    FillRect(r);
    Brush.Color := clBlack;
    FrameRect(r);
    Brush.Color := clParchment;
    // Account for margins inside box: move text down and left a little.
    r.Left := r.Left + 4;
    r.Top := r.Top + 2;
    // Use DrawText so we can handle multiple lines.
    DrawText(Canvas.Handle, PChar(s), -1, r, DT_LEFT + DT_NOPREFIX);
    end;
end;

procedure TCustomHint.SetVisible(b:boolean);
var x,y:integer;
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
      x := MEndX - w - 8 - mOffsetX
    else
      x := mEndX + 16 + mOffsetX;       {StartX < EndX }

    if (mStartY > mEndY) then
      y := mEndY - mOffsetY
    else
      y := MEndY + mOffsetY;

    { Save what's underneath the hint }
    UnderneathX := x;
    UnderneathY := y;
    underneath.Width := w;
    underneath.Height:= h;
    underneath.Canvas.CopyRect(Rect(0,0,w,h),Canvas,Rect(x,y,x+w,y+h));

    { Draw it }
    ShowPopupBox(Canvas,x,y,sText);
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
