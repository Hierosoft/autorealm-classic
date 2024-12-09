unit TBDock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, Graphics;

type

  { TTBDock }

  TTBDock = class(TPanel)
  private
    FIsDocked: Boolean;
    FDockArea: TPanel; // This would be the docking area if you need it
  protected
    procedure SetDocked(Value: Boolean); virtual;
    procedure ResizeDocked; virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockTo(DockArea: TPanel);
    procedure Undock;
    property IsDocked: Boolean read FIsDocked;
  end;

implementation

{ TTBDock }

constructor TTBDock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsDocked := False;
  // Set default style for the panel, e.g., no border or custom docking indicator
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TTBDock.SetDocked(Value: Boolean);
begin
  if FIsDocked <> Value then
  begin
    FIsDocked := Value;
    if FIsDocked then
    begin
      ResizeDocked;  // Resize the component when docked
    end
    else
    begin
      // Handle undocking if needed (e.g., make it float again)
    end;
  end;
end;

procedure TTBDock.ResizeDocked;
begin
  // Handle resizing logic, if needed (resize within the docking area)
  if FIsDocked then
    Align := alClient; // Dock to the client area of the parent control (DockArea)
end;

procedure TTBDock.DockTo(DockArea: TPanel);
begin
  // Dock the panel to the specified area
  FDockArea := DockArea;
  Parent := DockArea;  // Make the docking area the parent
  SetDocked(True);
end;

procedure TTBDock.Undock;
begin
  // Move the panel back to its original floating position
  Parent := nil;  // Remove the docking area as the parent
  SetDocked(False);
end;

procedure TTBDock.Paint;
begin
  inherited Paint;
  // Optional: Add custom paint code to indicate docking state, etc.
  if FIsDocked then
    Canvas.Brush.Color := clBtnFace // Example color when docked
  else
    Canvas.Brush.Color := clWhite; // Example color when undocked
  Canvas.FillRect(ClientRect);
end;

end.
