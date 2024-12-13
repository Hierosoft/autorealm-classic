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
unit ToolObject;

interface

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Geometry;


type TToolState = (tsOff, tsStarted);

     TToolHandler = class
     protected
        StartX,StartY: Coord;//integer;
        EndX,EndY: Coord;//integer;
        state:TToolState;
        Canvas:TCanvas;
        AutoPan:boolean;
        CustomCursor:integer;

     public
        function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean; virtual;
        procedure MouseMove(Shift: TShiftState; X,Y: Integer);                        virtual;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);   virtual;
        procedure Cancel; virtual;
        procedure Done;   virtual;
        procedure Draw(erase:boolean);   virtual;
        procedure Paint;  virtual;
        procedure Refresh;virtual;
        procedure RefreshCursor;
        procedure CreateClosedFigure; virtual;
        procedure CreateOpenFigure; virtual;
        procedure Move(dx,dy:integer); virtual;
        function Pan(var x,y:integer):boolean;
        procedure Escape;
        procedure Backspace; virtual;
        procedure ShowCrosshair(X,Y:integer); virtual;
        procedure HideCrosshair; virtual;

        constructor Create(cv:TCanvas);
        destructor Destroy; override;
     end;

implementation

uses Main, MapObject, Snap, SettingsDialog;

const PanMarginStart = 20;
      PanMarginStop  = 50;

      noCustomCursor = -999;

var   CrossHair:TPoint;
      CrossHairEnabled:boolean;


procedure TToolHandler.Escape;
begin
  if (state=tsStarted) then
    Cancel
  else begin
    if Map.AnythingSelected then begin
      Map.InvalidateSelect(true);
      Map.ClearSelection;
      Refresh;
      end;
    end;
end;

procedure TToolHandler.Cancel;
var OldMode:TPenMode;
begin
  if (state=tsStarted) then begin
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;
    Draw(true);
    Canvas.Pen.Mode := OldMode;
    end;

  state:=tsOff;
  EndX:=-1;
  SnapLEDsOff;
end;

procedure TToolHandler.Done;
begin
  state:=tsOff;
  EndX:=-1;
  SnapLEDsOff;
end;

procedure TToolHandler.CreateClosedFigure;
begin
end;

procedure TToolHandler.CreateOpenFigure;
begin
end;

procedure TToolHandler.Move(dx,dy:integer);
begin
  StartX := StartX+dx;
  StartY := StartY+dy;
end;

procedure TToolHandler.Refresh;
begin
end;

procedure TToolHandler.Draw(erase:boolean);
begin
  RefreshCursor;
end;

procedure TToolHandler.RefreshCursor;
begin
  if CustomCursor <> noCustomCursor then begin
    PaintBox.Cursor := CustomCursor;
    end;
end;

procedure TToolHandler.Paint;
begin
end;

procedure TToolHandler.Backspace;
begin
end;

function TToolHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer):boolean;
begin
  if (Button=mbLeft) then begin
    StartX := X;
    StartY := Y;
    state  := tsStarted;
    Result := true;
    end
  else
    Result:=false;
end;

function TToolHandler.Pan(var x,y:integer):boolean;
var r:TRect;
    dx,dy:integer;
    OutsidePan:boolean;
    OldPen:TPenMode;
begin
   Result:=false;
   r:=MainForm.GetVisibleRect;

   if (MainForm.PanTimer.Enabled) or
      (X <= r.left+PanMarginStart) or (Y <= r.top+PanMarginStart) or
      (X >= r.right-PanMarginStart) or (Y >= r.bottom-PanMarginStart) then begin

      dx:=0;
      dy:=0;
      if (X<=r.left+PanMarginStop)   then dx:= (r.left+PanMarginStop-x+1);
      if (X>=r.right-PanMarginStop)  then dx:=-(PanMarginStop-(r.right-x)+1);
      if (Y<=r.top+PanMarginStop)    then dy:= (r.top+PanMarginStop-y+1);
      if (Y>=r.bottom-PanMarginStop) then dy:=-(PanMarginStop-(r.bottom-y)+1);

      if (dx<0) then
         dx:=-2*trunc(sqrt(-dx))
      else
         dx:=2*trunc(sqrt(dx));

      if (dy<0) then
         dy:=-2*trunc(sqrt(-dy))
      else
         dy:=2*trunc(sqrt(dy));

      OldPen:=Canvas.Pen.Mode;
      Canvas.Pen.Mode := pmCopy;

      Map.Pan(dx,dy);
      Move(dx,dy);
      Mouse.CursorPos := Point(Mouse.CursorPos.X+dx,Mouse.CursorPos.Y+dy);
      X:=X+dx;
      Y:=Y+dy;

      Canvas.Pen.Mode:=OldPen;

      OutsidePan := (x>=r.left+PanMarginStop) and (x<=r.right-PanMarginStop) and
                    (y>=r.top+PanMarginStop) and (y<=r.bottom-PanMarginStop);

      MainForm.PanTimer.Enabled := not OutsidePan;
      Result:=true;
    end;
end;

procedure TToolHandler.HideCrosshair;
var OldMode:TPenMode;
    OldColor:TColor;
begin
  if CrossHairEnabled and (CrossHair.X<>-1) and (CrossHair.Y<>-1) then begin
    OldMode := Canvas.Pen.Mode;
    OldColor:= Canvas.Pen.Color;
    Canvas.Pen.Color := clLime;
    Canvas.Pen.Mode := pmNotXor;
    Canvas.MoveTo(0,CrossHair.Y);
    Canvas.LineTo(GetSystemMetrics(SM_CXSCREEN),CrossHair.Y);
    Canvas.MoveTo(CrossHair.X,0);
    Canvas.LineTo(CrossHair.X,GetSystemMetrics(SM_CYSCREEN));
    Canvas.Pen.Mode := OldMode;
    canvas.Pen.Color:= OldColor;
    CrossHair.X := -1;
    CrossHair.Y := -1;
    end;
end;

procedure TToolHandler.ShowCrosshair(X,Y:integer);
var OldMode:TPenMode;
    OldColor:TColor;
begin
  CrossHairEnabled := ((GetKeyState(VK_SCROLL) and 1)<>0) xor Settings.CrosshairOn.Checked;

  if CrossHairEnabled then begin
    PaintBox.Update;
    CrossHair.X := X;
    CrossHair.Y := Y;
    OldMode := Canvas.Pen.Mode;
    OldColor:= Canvas.Pen.Color;
    Canvas.Pen.Color := clLime;
    Canvas.Pen.Mode := pmNotXor;
    Canvas.MoveTo(0,CrossHair.Y);
    Canvas.LineTo(GetSystemMetrics(SM_CXSCREEN),CrossHair.Y);
    Canvas.MoveTo(CrossHair.X,0);
    Canvas.LineTo(CrossHair.X,GetSystemMetrics(SM_CYSCREEN));
    Canvas.Pen.Mode := OldMode;
    canvas.Pen.Color:= OldColor;
    end;
end;

procedure TToolHandler.MouseMove(Shift: TShiftState; X,Y: Integer);
var OldMode:TPenMode;
begin
  HideCrosshair;
  if CustomCursor <> noCustomCursor then begin
    PaintBox.Cursor := CustomCursor;
    end;

  if (state=tsStarted) then begin
    OldMode := Canvas.Pen.Mode;
    Canvas.Pen.Mode := pmNotXor;

    if (EndX <> -1) then Draw(true);

    if AutoPan then Pan(X,Y);

    EndX := X;
    EndY := Y;
    Draw(false);
    Canvas.Pen.Mode := OldMode;
    end;

  ShowCrossHair(X,Y);
end;

procedure TToolHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (state=tsStarted) then begin
    EndX:=X;
    EndY:=Y;
    Done;
    end;
end;

constructor TToolHandler.Create(cv:TCanvas);
begin
  EndX:=-1;
  state :=tsOff;
  Canvas:=cv;
  AutoPan:=false;
  CustomCursor:=noCustomCursor;
end;

destructor TToolHandler.Destroy;
begin
  inherited Destroy;
end;

end.
