unit ImageCheckListBox;

interface

uses
  SysUtils, Classes, Controls, Graphics, ComCtrls, ImgList, LCLType, LMessages;

type
  { TImageCheckListBox }
  TImageCheckListBox = class(TCustomListView)
  private
    FCheckList: array of Boolean;
    FImageList: TImageList;
    procedure SetCheck(Index: Integer; Value: Boolean);
    function GetCheck(Index: Integer): Boolean;
    procedure SetImageList(Value: TImageList);
    procedure DrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  // protected
  //   procedure DoOnCustomDrawItem(Item: TListItem; Rect: TRect); virtual;
  // ^ Forward Declaration not solved
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddItem(const ACaption: string; AImageIndex: Integer; AChecked: Boolean);
    property Check[Index: Integer]: Boolean read GetCheck write SetCheck;
    property ImageList: TImageList read FImageList write SetImageList;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TImageCheckListBox]);
end;

{ TImageCheckListBox }

constructor TImageCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  Checkboxes := True;  // Enables checkboxes in TListView
  FCheckList := [];
  // TODO: OnCustomDrawItem := DrawItem;  // Assign the custom draw event
end;

procedure TImageCheckListBox.SetCheck(Index: Integer; Value: Boolean);
begin
  if (Index >= 0) and (Index < Length(FCheckList)) then
  begin
    FCheckList[Index] := Value;
    Invalidate;  // Redraw the ListView to update the checkboxes
  end;
end;

function TImageCheckListBox.GetCheck(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < Length(FCheckList)) then
    Result := FCheckList[Index]
  else
    Result := False;
end;

procedure TImageCheckListBox.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
    FImageList := Value;
    LargeImages := FImageList;  // You can set SmallImages if needed
  end;
end;

procedure TImageCheckListBox.AddItem(const ACaption: string; AImageIndex: Integer; AChecked: Boolean);
begin
  with Items.Add do
  begin
    Caption := ACaption;
    ImageIndex := AImageIndex;
    SetLength(FCheckList, Length(FCheckList) + 1);
    FCheckList[High(FCheckList)] := AChecked;
  end;
end;

procedure TImageCheckListBox.DrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  CheckRect: TRect;
  CheckSize: Integer;
  ItemTextRect: TRect;
begin
  // Allow the default drawing to occur
  DefaultDraw := True;

  // Get the size of the checkbox
  CheckSize := 16;  // Size of the checkbox, modify if needed.
  CheckRect := Item.DisplayRect(drBounds);
  CheckRect.Right := CheckRect.Left + CheckSize;

  // Draw the checkbox based on its state (checked or unchecked)
  if FCheckList[Item.Index] then
  begin
    Canvas.FillRect(CheckRect);  // Background for checkbox
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(CheckRect.Left, CheckRect.Top, CheckRect.Right, CheckRect.Bottom);  // Draw the checkbox border
    Canvas.Pen.Color := clBlack;
    // Draw checkmark (can be an image or custom drawing)
    Canvas.MoveTo(CheckRect.Left + 3, CheckRect.Top + 3);
    Canvas.LineTo(CheckRect.Right - 3, CheckRect.Bottom - 3);
    Canvas.MoveTo(CheckRect.Right - 3, CheckRect.Top + 3);
    Canvas.LineTo(CheckRect.Left + 3, CheckRect.Bottom - 3);
  end
  else
  begin
    Canvas.FillRect(CheckRect);  // Fill with background
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(CheckRect.Left, CheckRect.Top, CheckRect.Right, CheckRect.Bottom);  // Draw the checkbox border
  end;

  // Draw the image (if any)
  if Assigned(FImageList) then
  begin
    FImageList.Draw(Canvas, CheckRect.Right + 4, Item.DisplayRect(drBounds).Top + (Item.DisplayRect(drBounds).Bottom - Item.DisplayRect(drBounds).Top - FImageList.Height) div 2, Item.ImageIndex);
  end;

  // Draw the text next to the image
  ItemTextRect := Item.DisplayRect(drBounds);
  ItemTextRect.Left := ItemTextRect.Left + CheckSize + 4 + FImageList.Width;  // Move text to the right of the image
  Canvas.TextOut(ItemTextRect.Left, ItemTextRect.Top + (ItemTextRect.Bottom - ItemTextRect.Top - Canvas.TextHeight(Item.Caption)) div 2, Item.Caption);
end;

end.

