unit PoliteSpinEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Spin, StdCtrls;

type
  { TPoliteSpinEdit }
  TPoliteSpinEdit = class(TFloatSpinEdit)
  // private
    // FMinValue: Double;
    // FMaxValue: Double;
    // FStep: Double;
    // procedure SetMinValue(Value: Double);
    // procedure SetMaxValue(Value: Double);
    // procedure SetStep(Value: Double);
  // public
    // constructor Create(AOwner: TComponent); override;
  // published
    // property MinValue: Double read FMinValue write SetMinValue;
    // property MaxValue: Double read FMaxValue write SetMaxValue;
    // property Step: Double read FStep write SetStep;
  end;

procedure Register;

implementation

{ TPoliteSpinEdit }


procedure Register;
begin
  RegisterComponents('Samples', [TPoliteSpinEdit]);
end;

end.

