unit frmMakeFrom3DUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Geometry;

type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;
  TVector = Record
    X,Y,Z: Single;
  End;
  TBezier = Record
    V1,V2,V3,V4: TVector;
  End;
  TPatch = Record
    B1,B2,B3,B4: TBezier;
  End;
  TSurface = Record
    P : Array Of TPatch;
  End;
  TMatrix = Array[0..3,0..3] Of Single;
  TPolygon = Array Of CoordPoint;
  TfrmMakeFrom3D = class(TForm)
    rbCone: TRadioButton;
    edtPolygonSides: TEdit;
    grpSymmetry: TGroupBox;
    lblSides: TLabel;
    udPolygonSides: TUpDown;
    grpShape: TGroupBox;
    Label2: TLabel;
    edtElasticity: TEdit;
    udElasticity: TUpDown;
    tbElasticity: TTrackBar;
    cbTop: TCheckBox;
    cbSides: TCheckBox;
    Label1: TLabel;
    edtCenterHeight: TEdit;
    udCenterHeight: TUpDown;
    imgMain: TImage;
    tbCenterHeight: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtXRot: TEdit;
    edtYRot: TEdit;
    edtZRot: TEdit;
    udXRot: TUpDown;
    udYRot: TUpDown;
    udZRot: TUpDown;
    tbXRot: TTrackBar;
    tbYRot: TTrackBar;
    tbZRot: TTrackBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtMR: TEdit;
    edtMG: TEdit;
    edtMB: TEdit;
    edtMH: TEdit;
    edtMS: TEdit;
    edtMV: TEdit;
    udMR: TUpDown;
    udMG: TUpDown;
    udMB: TUpDown;
    udMH: TUpDown;
    udMS: TUpDown;
    udMV: TUpDown;
    tbMR: TTrackBar;
    tbMG: TTrackBar;
    tbMB: TTrackBar;
    tbMH: TTrackBar;
    tbMS: TTrackBar;
    tbMV: TTrackBar;
    pnlModelColor: TPanel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    edtLR: TEdit;
    edtLG: TEdit;
    edtLB: TEdit;
    edtLH: TEdit;
    edtLS: TEdit;
    edtLV: TEdit;
    udLR: TUpDown;
    udLG: TUpDown;
    udLB: TUpDown;
    udLH: TUpDown;
    udLS: TUpDown;
    udLV: TUpDown;
    tbLR: TTrackBar;
    tbLG: TTrackBar;
    tbLB: TTrackBar;
    tbLH: TTrackBar;
    tbLS: TTrackBar;
    tbLV: TTrackBar;
    pnlLightColor: TPanel;
    edtAR: TEdit;
    edtAG: TEdit;
    edtAB: TEdit;
    edtAH: TEdit;
    edtAS: TEdit;
    edtAV: TEdit;
    udAR: TUpDown;
    udAG: TUpDown;
    udAB: TUpDown;
    udAH: TUpDown;
    udAS: TUpDown;
    udAV: TUpDown;
    tbAR: TTrackBar;
    tbAG: TTrackBar;
    tbAB: TTrackBar;
    tbAH: TTrackBar;
    tbAS: TTrackBar;
    tbAV: TTrackBar;
    pnlAmbientColor: TPanel;
    Label24: TLabel;
    edtInnerRadius: TEdit;
    udInnerRadius: TUpDown;
    tbInnerRadius: TTrackBar;
    Bevel1: TBevel;
    Bevel2: TBevel;
    rbCylindrical: TRadioButton;
    Label25: TLabel;
    edtOuterRadius: TEdit;
    udOuterRadius: TUpDown;
    tbOuterRadius: TTrackBar;
    cbPolygonal: TCheckBox;
    TabSheet4: TTabSheet;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    edtOR: TEdit;
    edtOG: TEdit;
    edtOB: TEdit;
    edtOH: TEdit;
    edtOS: TEdit;
    edtOV: TEdit;
    udOR: TUpDown;
    udOG: TUpDown;
    udOB: TUpDown;
    udOH: TUpDown;
    udOS: TUpDown;
    udOV: TUpDown;
    tbOR: TTrackBar;
    tbOG: TTrackBar;
    tbOB: TTrackBar;
    tbOH: TTrackBar;
    tbOS: TTrackBar;
    tbOV: TTrackBar;
    pnlOutlineColor: TPanel;
    cbUseOutlineColor: TCheckBox;
    rbSpherical: TRadioButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbRemoveHiddenSurfaces: TCheckBox;
    Label32: TLabel;
    edtYTrans: TEdit;
    udYTrans: TUpDown;
    tbYTrans: TTrackBar;
    PageControl2: TPageControl;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label33: TLabel;
    Label34: TLabel;
    edtXTrans: TEdit;
    edtZTrans: TEdit;
    udXTrans: TUpDown;
    udZTrans: TUpDown;
    tbXTrans: TTrackBar;
    tbZTrans: TTrackBar;
    rbRectangular: TRadioButton;
    Label35: TLabel;
    edtThickness: TEdit;
    udThickness: TUpDown;
    tbThickness: TTrackBar;
    procedure rbConeClick(Sender: TObject);
    procedure edtElasticityChange(Sender: TObject);
    procedure udElasticityClick(Sender: TObject; Button: TUDBtnType);
    procedure tbElasticityChange(Sender: TObject);
    procedure edtPolygonSidesChange(Sender: TObject);
    procedure edtCenterHeightChange(Sender: TObject);
    procedure udCenterHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure tbCenterHeightChange(Sender: TObject);
    procedure edtXRotChange(Sender: TObject);
    procedure edtYRotChange(Sender: TObject);
    procedure edtZRotChange(Sender: TObject);
    procedure udXRotClick(Sender: TObject; Button: TUDBtnType);
    procedure udYRotClick(Sender: TObject; Button: TUDBtnType);
    procedure udZRotClick(Sender: TObject; Button: TUDBtnType);
    procedure tbXRotChange(Sender: TObject);
    procedure tbYRotChange(Sender: TObject);
    procedure tbZRotChange(Sender: TObject);
    procedure cbTopClick(Sender: TObject);
    procedure cbSidesClick(Sender: TObject);
    procedure edtMRChange(Sender: TObject);
    procedure udMRClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMRChange(Sender: TObject);
    procedure edtMGChange(Sender: TObject);
    procedure udMGClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMGChange(Sender: TObject);
    procedure edtMBChange(Sender: TObject);
    procedure udMBClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMBChange(Sender: TObject);
    procedure edtMHChange(Sender: TObject);
    procedure udMHClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMHChange(Sender: TObject);
    procedure edtMSChange(Sender: TObject);
    procedure udMSClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMSChange(Sender: TObject);
    procedure edtMVChange(Sender: TObject);
    procedure udMVClick(Sender: TObject; Button: TUDBtnType);
    procedure tbMVChange(Sender: TObject);
    procedure edtLRChange(Sender: TObject);
    procedure udLRClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLRChange(Sender: TObject);
    procedure edtLGChange(Sender: TObject);
    procedure udLGClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLGChange(Sender: TObject);
    procedure edtLBChange(Sender: TObject);
    procedure udLBClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLBChange(Sender: TObject);
    procedure edtLHChange(Sender: TObject);
    procedure udLHClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLHChange(Sender: TObject);
    procedure edtLSChange(Sender: TObject);
    procedure udLSClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLSChange(Sender: TObject);
    procedure edtLVChange(Sender: TObject);
    procedure udLVClick(Sender: TObject; Button: TUDBtnType);
    procedure tbLVChange(Sender: TObject);
    procedure edtARChange(Sender: TObject);
    procedure udARClick(Sender: TObject; Button: TUDBtnType);
    procedure tbARChange(Sender: TObject);
    procedure edtAGChange(Sender: TObject);
    procedure udAGClick(Sender: TObject; Button: TUDBtnType);
    procedure tbAGChange(Sender: TObject);
    procedure edtABChange(Sender: TObject);
    procedure udABClick(Sender: TObject; Button: TUDBtnType);
    procedure tbABChange(Sender: TObject);
    procedure edtAHChange(Sender: TObject);
    procedure udAHClick(Sender: TObject; Button: TUDBtnType);
    procedure tbAHChange(Sender: TObject);
    procedure edtASChange(Sender: TObject);
    procedure udASClick(Sender: TObject; Button: TUDBtnType);
    procedure tbASChange(Sender: TObject);
    procedure edtAVChange(Sender: TObject);
    procedure udAVClick(Sender: TObject; Button: TUDBtnType);
    procedure tbAVChange(Sender: TObject);
    procedure edtInnerRadiusChange(Sender: TObject);
    procedure udInnerRadiusClick(Sender: TObject; Button: TUDBtnType);
    procedure tbInnerRadiusChange(Sender: TObject);
    procedure rbCylindricalClick(Sender: TObject);
    procedure edtOuterRadiusChange(Sender: TObject);
    procedure udOuterRadiusClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOuterRadiusChange(Sender: TObject);
    procedure cbPolygonalClick(Sender: TObject);
    procedure edtORChange(Sender: TObject);
    procedure udORClick(Sender: TObject; Button: TUDBtnType);
    procedure tbORChange(Sender: TObject);
    procedure edtOGChange(Sender: TObject);
    procedure udOGClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOGChange(Sender: TObject);
    procedure edtOBChange(Sender: TObject);
    procedure udOBClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOBChange(Sender: TObject);
    procedure edtOHChange(Sender: TObject);
    procedure udOHClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOHChange(Sender: TObject);
    procedure edtOSChange(Sender: TObject);
    procedure udOSClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOSChange(Sender: TObject);
    procedure edtOVChange(Sender: TObject);
    procedure udOVClick(Sender: TObject; Button: TUDBtnType);
    procedure tbOVChange(Sender: TObject);
    procedure cbUseOutlineColorClick(Sender: TObject);
    procedure rbSphericalClick(Sender: TObject);
    procedure cbSmoothClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbRemoveHiddenSurfacesClick(Sender: TObject);
    procedure edtYTransChange(Sender: TObject);
    procedure udYTransClick(Sender: TObject; Button: TUDBtnType);
    procedure tbYTransChange(Sender: TObject);
    procedure edtXTransChange(Sender: TObject);
    procedure udXTransClick(Sender: TObject; Button: TUDBtnType);
    procedure tbXTransChange(Sender: TObject);
    procedure edtZTransChange(Sender: TObject);
    procedure udZTransClick(Sender: TObject; Button: TUDBtnType);
    procedure tbZTransChange(Sender: TObject);
    procedure rbRectangularClick(Sender: TObject);
    procedure udThicknessClick(Sender: TObject; Button: TUDBtnType);
    procedure tbThicknessChange(Sender: TObject);
    procedure edtThicknessChange(Sender: TObject);
  private
    { Private declarations }
    ModelColor        : TColor;
    LightColor        : TColor;
    AmbientColor      : TColor;
    OutlineColor      : TColor;
    SettingModelRGB   : Boolean;
    SettingModelHSV   : Boolean;
    SettingLightRGB   : Boolean;
    SettingLightHSV   : Boolean;
    SettingAmbientRGB : Boolean;
    SettingAmbientHSV : Boolean;
    SettingOutlineRGB : Boolean;
    SettingOutlineHSV : Boolean;
    Procedure PaintModel;
    Procedure SetModelColorFromRGB;
    Procedure SetModelColorFromHSV;
    Procedure SetLightColorFromRGB;
    Procedure SetLightColorFromHSV;
    Procedure SetAmbientColorFromRGB;
    Procedure SetAmbientColorFromHSV;
    Procedure SetOutlineColorFromRGB;
    Procedure SetOutlineColorFromHSV;
    Procedure RGBtoHSV(R,G,B: Single; Var H,S,V: Single);
    Procedure HSVtoRGB(H,S,V: Single; Var R,G,B: Single);
    Function  GetSurface: TSurface;
    Function  GetColor(Normal: TVector): TColor;
    Function  GetOutlineColor(Col: TColor): TColor;
  public
    { Public declarations }
    Procedure ExportSurface;
  end;

var
  frmMakeFrom3D: TfrmMakeFrom3D;

implementation

Uses MapObject,Primitives,DrawLines,Math,Bezier;

Const FOCAL_DISTANCE = 200;

{$R *.dfm}

Function Project(V: TVector): TPoint;
Begin
  If V.Z = 0 Then V.Z := 1;
  Result.X := Round( FOCAL_DISTANCE * V.X / V.Z);
  Result.Y := Round(-FOCAL_DISTANCE * V.Y / V.Z);
End; // Project

Function RProject(V: TVector): TVector;
Begin
  If V.Z = 0 Then V.Z := 1;
  Result.X :=  FOCAL_DISTANCE * V.X / V.Z;
  Result.Y := -FOCAL_DISTANCE * V.Y / V.Z;
  Result.Z := 0;
End; // RProject

Function BProject(B: TBezier): TBezier;
Begin
  Result.V1 := RProject(B.V1);
  Result.V2 := RProject(B.V2);
  Result.V3 := RProject(B.V3);
  Result.V4 := RProject(B.V4);
End; // BProject

Function POINT_Distance2(P1,P2: TPoint): Single;
Begin
  Result := Sqr(P1.X - P2.X) + Sqr(P1.Y - P2.Y);
End; // POINT_Distance2

Function VEC(X,Y,Z: Single): TVector;
Begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
End; // VEC

Function VEC_Length(V: TVector): Single;
Begin
  Result := Sqrt(Sqr(V.X) + Sqr(V.Y) + Sqr(V.Z));
End; // VEC_Length

Function VEC_Mult(V: TVector; S: Single): TVector;
Begin
  Result.X := V.X * S;
  Result.Y := V.Y * S;
  Result.Z := V.Z * S;
End; // VEC_Mult

Function VEC_Normalize(V: TVector): TVector;
Var L: Single;
Begin
  L := VEC_Length(V);
  If L <> 0 Then Result := VEC_Mult(V,1 / L) Else Result := V;
End; // VEC_Normalize

Function VEC_Add(V1,V2: TVector): TVector;
Begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
  Result.Z := V1.Z + V2.Z;
End; // VEC_Add

Function VEC_Sub(V1,V2: TVector): TVector;
Begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
  Result.Z := V1.Z - V2.Z;
End; // VEC_Sub

Function VEC_Dot(V1,V2: TVector): Single;
Begin
  Result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
End; // VEC_Dot

Function VEC_Cross(V1,V2: TVector): TVector;
Begin
  Result.X := V1.Y * V2.Z - V1.Z * V2.Y;
  Result.Y := V1.Z * V2.X - V1.X * V2.Z;
  Result.Z := V1.X * V2.Y - V1.Y * V2.X;
End; // VEC_Cross

Function VEC_Average(V1,V2: TVector): TVector;
Begin
  Result := VEC_Mult(VEC_Add(V1,V2),0.5);
End; // VEC_Average

Function VEC_MultMatrix(Source: TVector; Mat: TMatrix): TVector;
Begin
  Result.X := Source.X * Mat[0,0] +
              Source.Y * Mat[1,0] +
              Source.Z * Mat[2,0] +
                         Mat[3,0];
  Result.Y := Source.X * Mat[0,1] +
              Source.Y * Mat[1,1] +
              Source.Z * Mat[2,1] +
                         Mat[3,1];
  Result.Z := Source.X * Mat[0,2] +
              Source.Y * Mat[1,2] +
              Source.Z * Mat[2,2] +
                         Mat[3,2];
End; // VEC_MultMatrix

Function VEC_Distance2(V1,V2: TVector): Single;
Begin
  Result := Sqr(V1.X - V2.X) + Sqr(V1.Y - V2.Y) + Sqr(V1.Z - V2.Z);
End; // VEC_Distance2

Function MAT_Mult(Mat1,Mat2: TMatrix): TMatrix;
Var I,J: Integer;
Begin
  For I := 0 To 3 Do
   For J := 0 To 3 Do
    Result[I,J] := Mat1[I,0] * Mat2[0,J] +
                   Mat1[I,1] * Mat2[1,J] +
                   Mat1[I,2] * Mat2[2,J] +
                   Mat1[I,3] * Mat2[3,J];
End; // MAT_Mult

Function MAT_Identity: TMatrix;
Begin
  Result[0,0] := 1; Result[0,1] := 0; Result[0,2] := 0; Result[0,3] := 0;
  Result[1,0] := 0; Result[1,1] := 1; Result[1,2] := 0; Result[1,3] := 0;
  Result[2,0] := 0; Result[2,1] := 0; Result[2,2] := 1; Result[2,3] := 0;
  Result[3,0] := 0; Result[3,1] := 0; Result[3,2] := 0; Result[3,3] := 1;
End; // MAT_Identity

Function TR_Translate(Matrix: TMatrix; TX,TY,TZ: Single): TMatrix;
Var TMat: TMatrix;
Begin
  TMat[0,0] := 1;  TMat[0,1] := 0;  TMat[0,2] := 0;  TMat[0,3] := 0;
  TMat[1,0] := 0;  TMat[1,1] := 1;  TMat[1,2] := 0;  TMat[1,3] := 0;
  TMat[2,0] := 0;  TMat[2,1] := 0;  TMat[2,2] := 1;  TMat[2,3] := 0;
  TMat[3,0] := TX; TMat[3,1] := TY; TMat[3,2] := TZ; TMat[3,3] := 1;
  Result := MAT_Mult(Matrix,TMat);
End; // TR_Translate

Function TR_Scale(Matrix: TMatrix; SX,SY,SZ: Single): TMatrix;
Var SMat: TMatrix;
Begin
  SMat[0,0] := SX; SMat[0,1] := 0;  SMat[0,2] := 0;  SMat[0,3] := 0;
  SMat[1,0] := 0;  SMat[1,1] := SY; SMat[1,2] := 0;  SMat[1,3] := 0;
  SMat[2,0] := 0;  SMat[2,1] := 0;  SMat[2,2] := SZ; SMat[2,3] := 0;
  SMat[3,0] := 0;  SMat[3,1] := 0;  SMat[3,2] := 0;  SMat[3,3] := 1;
  Result := MAT_Mult(Matrix,SMat);
End; // TR_Scale

Function TR_Rotate(Matrix: TMatrix; AX,AY,AZ: Single): TMatrix;
Var XMat,YMat,ZMat: TMatrix;
Begin
  XMat[0,0] := 1;        XMat[0,1] := 0;        XMat[0,2] := 0;        XMat[0,3] := 0;
  XMat[1,0] := 0;        XMat[1,1] := Cos(AX);  XMat[1,2] := Sin(AX);  XMat[1,3] := 0;
  XMat[2,0] := 0;        XMat[2,1] := -Sin(AX); XMat[2,2] := Cos(AX);  XMat[2,3] := 0;
  XMat[3,0] := 0;        XMat[3,1] := 0;        XMat[3,2] := 0;        XMat[3,3] := 1;

  YMat[0,0] := Cos(AY);  YMat[0,1] := 0;        YMat[0,2] := -Sin(AY); YMat[0,3] := 0;
  YMat[1,0] := 0;        YMat[1,1] := 1;        YMat[1,2] := 0;        YMat[1,3] := 0;
  YMat[2,0] := Sin(AY);  YMat[2,1] := 0;        YMat[2,2] := Cos(AY);  YMat[2,3] := 0;
  YMat[3,0] := 0;        YMat[3,1] := 0;        YMat[3,2] := 0;        YMat[3,3] := 1;

  ZMat[0,0] := Cos(AZ);  ZMat[0,1] := Sin(AZ);  ZMat[0,2] := 0;        ZMat[0,3] := 0;
  ZMat[1,0] := -Sin(AZ); ZMat[1,1] := Cos(AZ);  ZMat[1,2] := 0;        ZMat[1,3] := 0;
  ZMat[2,0] := 0;        ZMat[2,1] := 0;        ZMat[2,2] := 1;        ZMat[2,3] := 0;
  ZMat[3,0] := 0;        ZMat[3,1] := 0;        ZMat[3,2] := 0;        ZMat[3,3] := 1;

  Result := MAT_Mult(MAT_Mult(MAT_Mult(Matrix,YMat),XMat),ZMat);
End; // TR_Rotate

Function BEZ(V1,V2,V3,V4: TVector): TBezier;
Begin
  Result.V1 := V1;
  Result.V2 := V2;
  Result.V3 := V3;
  Result.V4 := V4;
End; // BEZ

Function BEZ_MultMatrix(B: TBezier; M: TMatrix): TBezier;
Begin
  Result.V1 := VEC_MultMatrix(B.V1,M);
  Result.V2 := VEC_MultMatrix(B.V2,M);
  Result.V3 := VEC_MultMatrix(B.V3,M);
  Result.V4 := VEC_MultMatrix(B.V4,M);
End; // BEZ_MultMatrix

// T has to range from 0..1
Procedure BEZ_SplitAt(B: TBezier; T: Double; Var B1,B2: TBezier);
Var V1,V2,V3: TVector;
Begin
  V1 := VEC_Add(VEC_Mult(VEC_Sub(B.V2,B.V1),T),B.V1);
  V2 := VEC_Add(VEC_Mult(VEC_Sub(B.V3,B.V2),T),B.V2);
  V3 := VEC_Add(VEC_Mult(VEC_Sub(B.V4,B.V3),T),B.V3);

  B1.V1 := B.V1;
  B2.V4 := B.V4;
  B1.V3 := VEC_Add(VEC_Mult(VEC_Sub(V2,V1),T),V1);
  B2.V2 := VEC_Add(VEC_Mult(VEC_Sub(V3,V2),T),V2);
  B1.V4 := VEC_Add(VEC_Mult(VEC_Sub(B2.V2,B1.V3),T),B1.V3);
  B2.V1 := B1.V4;
  B1.V2 := VEC_Add(B.V1,VEC_Mult(VEC_Sub(B.V2,B.V1),T));
  B2.V3 := VEC_Sub(B.V4,VEC_Mult(VEC_Sub(B.V4,B.V3),1 - T));
End; // BEZ_SplitAt

Function BEZ_GetPointAt(B: TBezier; T: Double): TVector;
Var
  H1,X1,X,X2,H2 : Double;
  P             : Array Of Double;
  Center        : TVector;

Begin
  SetLength(P,4);

  P[0] := B.V1.X;
  P[1] := B.V2.X;
  P[2] := B.V3.X;
  P[3] := B.V4.X;
  SplitAndGetHandles(P,T,H1,X1,X,X2,H2);
  Center.X := X;

  P[0] := B.V1.Y;
  P[1] := B.V2.Y;
  P[2] := B.V3.Y;
  P[3] := B.V4.Y;
  SplitAndGetHandles(P,T,H1,X1,X,X2,H2);
  Center.Y := X;

  P[0] := B.V1.Z;
  P[1] := B.V2.Z;
  P[2] := B.V3.Z;
  P[3] := B.V4.Z;
  SplitAndGetHandles(P,T,H1,X1,X,X2,H2);
  Center.Z := X;

  SetLength(P,0);
  Result := Center;
End; // BEZ_GetPointAt

Function PAT_MultMatrix(P: TPatch; M: TMatrix): TPatch;
Begin
  Result.B1 := BEZ_MultMatrix(P.B1,M);
  Result.B2 := BEZ_MultMatrix(P.B2,M);
  Result.B3 := BEZ_MultMatrix(P.B3,M);
  Result.B4 := BEZ_MultMatrix(P.B4,M);
End; // PAT_MultMatrix

// This is BEYOND rudimentary! :)
Function PAT_Normal(P: TPatch): TVector;
Var
  V1,V2       : TVector;
  N1,N2,N3,N4 : TVector;

Begin
  V1 := VEC_Sub(P.B1.V4,P.B1.V1);
  V2 := VEC_Sub(P.B2.V4,P.B2.V1);
  N1 := VEC_Cross(V1,V2);

  V1 := VEC_Sub(P.B2.V4,P.B2.V1);
  V2 := VEC_Sub(P.B3.V4,P.B3.V1);
  N2 := VEC_Cross(V1,V2);

  V1 := VEC_Sub(P.B3.V4,P.B3.V1);
  V2 := VEC_Sub(P.B4.V4,P.B4.V1);
  N3 := VEC_Cross(V1,V2);

  V1 := VEC_Sub(P.B4.V4,P.B4.V1);
  V2 := VEC_Sub(P.B1.V4,P.B1.V1);
  N4 := VEC_Cross(V1,V2);

  Result := VEC_Normalize(VEC_Add(VEC_Add(N1,N2),VEC_Add(N3,N4)));
End; // PAT_Normal

Function PAT_GetPointAt(P: TPatch; TX,TY: Double): TVector;
// ---------------------------------------------------------------------------
// Extremely powerful and useful function: if you consider a rectangular
// patch where each edge is a Bezier curve, and consider the lower right
// portion having coordinate (0,0) and the upper left corner having coordinate
// (1,1), this function returns the (x,y,z) value at the given coordinate.
// Coordinates are generally *not* evenly spaced (they are in Bezier-space).
// ---------------------------------------------------------------------------
Var
  C1,C2 : TVector;
  B1,B2 : TBezier;

Begin
  B1.V1  := BEZ_GetPointAt(P.B1,1 - TX);
  B1.V2  := VEC_Add(VEC_Mult(P.B4.V3,TX),VEC_Mult(P.B2.V2,1 - TX));
  B1.V3  := VEC_Add(VEC_Mult(P.B4.V2,TX),VEC_Mult(P.B2.V3,1 - TX));
  B1.V4  := BEZ_GetPointAt(P.B3,TX);
  B2.V1  := BEZ_GetPointAt(P.B2,TY);
  B2.V2  := VEC_Add(VEC_Mult(P.B3.V2,TY),VEC_Mult(P.B1.V3,1 - TY));
  B2.V3  := VEC_Add(VEC_Mult(P.B3.V3,TY),VEC_Mult(P.B1.V2,1 - TY));
  B2.V4  := BEZ_GetPointAt(P.B4,1 - TY);
  C1     := BEZ_GetPointAt(B1,TY);
  C2     := BEZ_GetPointAt(B2,TX);
  Result := VEC_Add(VEC_Mult(C1,0.5),VEC_Mult(C2,0.5));
End; // PAT_GetPointAt

Function PAT_FindClosestTo(P1: TPatch; P: TVector; ScreenCoord: Boolean): TVector;
// ---------------------------------------------------------------------------
// For the given coordinate P (where Z will automatically be set to zero if it
// is a screen coordinate) this returns the (x,y,z) value on the patch that is
// closest to that point.  This does *not* take into account the fact that
// curved patches can actually have multiple coordinates that are closest to
// the given coordinate (but where this is used in sorting patches it seems to
// work well enough, since the way we use elasticity keeps them from *really*
// twisting).
// ---------------------------------------------------------------------------
Const MaxDepth = 10;
Var RecurseDepth: Integer;

  Function GetClosestPoint(P1: TPatch; P: TVector; TX,TY,Interval: Double): TVector;
  Var
    V   : Array[0..8] Of TVector;
    Q   : Array[0..8] Of TVector;
    D   : Array[0..8] Of Single;
    X,Y : Array[0..8] Of Double;
    I,J : Integer;
    S   : Single;

  Begin
    // Find the point at T

    V[0] := PAT_GetPointAt(P1,TX,TY);
    X[0] := TX;
    Y[0] := TY;
    Inc(RecurseDepth);
    If RecurseDepth < MaxDepth Then
    Begin
      // Find the points around V

      X[1] := TX - Interval;
      Y[1] := TY - Interval;
      X[2] := TX + Interval;
      Y[2] := TY - Interval;
      X[3] := TX - Interval;
      Y[3] := TY + Interval;
      X[4] := TX + Interval;
      Y[4] := TY + Interval;

      X[5] := TX;
      Y[5] := TY - Interval;
      X[6] := TX;
      Y[6] := TY + Interval;
      X[7] := TX - Interval;
      Y[7] := TY;
      X[8] := TX + Interval;
      Y[8] := TY;

      // Bounds-check the Bezier intervals

      For I := 1 To 8 Do
      Begin
        If X[I] < 0 Then X[I] := 0;
        If X[I] > 1 Then X[I] := 1;
        If Y[I] < 0 Then Y[I] := 0;
        If Y[I] > 1 Then Y[I] := 1;
      End; // For I

      // Find the actual x, y, z points

      For I := 1 To 8 Do V[I] := PAT_GetPointAt(P1,X[I],Y[I]);

      // Project the points

      If ScreenCoord Then
      Begin
        For I := 0 To 8 Do Q[I] := RProject(V[I]);
      End
      Else
      Begin
        For I := 0 To 8 Do Q[I] := V[I];
      End;

      // Choose the one that projects closest to our destination

      For I := 0 To 8 Do D[I] := VEC_Distance2(Q[I],P);
      S := D[0];
      J := 0;
      For I := 1 To 8 Do
      Begin
        If D[I] < S Then
        Begin
          S := D[I];
          J := I;
        End;
      End; // For I

      // If we are keeping this point, halve the interval

      If J = 0 Then Interval := Interval / 2;
      Result := GetClosestPoint(P1,P,X[J],Y[J],Interval);
    End
    Else Result := V[0];
  End; // GetClosestPoint

Begin
  RecurseDepth := 0;
  If ScreenCoord Then P.Z := 0;
  Result       := GetClosestPoint(P1,P,0.5,0.5,0.25);
End; // PAT_FindClosestTo

procedure TfrmMakeFrom3D.rbConeClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtElasticityChange(Sender: TObject);
begin
  tbElasticity.Position := udElasticity.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udElasticityClick(Sender: TObject; Button: TUDBtnType);
begin
  tbElasticity.Position := udElasticity.Position;
end;

procedure TfrmMakeFrom3D.tbElasticityChange(Sender: TObject);
begin
  udElasticity.Position := tbElasticity.Position;
end;

Function GetPolygon(P: TPatch): TPolygon;
Var
  Count           : Integer;
  Poly            : TPolygon;
  P1              : TPatch;
{
  B1,B2           : TBezier;
  _Min,_Max       : Array[1..4] Of TVector;
  X1,Y1,X2,Y2     : Array[1..4] Of Single;
  S               : Array[1..4] Of Single;
  F               : Array[1..4] Of Boolean;
  I               : Integer;
}
  Procedure AddPoint(X,Y: Single);
  Begin
    Inc(Count);
    If Count > High(Poly) + 1 Then SetLength(Poly,Count + 10);
    Poly[Count - 1].X := X;
    Poly[Count - 1].Y := Y;
  End; // AddPoint

  Procedure DoBezier(Depth: Integer; B: TBezier);
  Var
    Q1,Q2,Q3 : TVector;
    R1,R2,S1 : TVector;

  Begin
    If (Depth <> 1) And (Abs(B.V1.X - B.V4.X) < 3) And (Abs(B.V1.Y - B.V4.Y) < 3) Then
     AddPoint(B.V4.X,B.V4.Y)
    Else
    Begin
      Q1 := VEC_Average(B.V1,B.V2);
      Q2 := VEC_Average(B.V2,B.V3);
      Q3 := VEC_Average(B.V3,B.V4);
      R1 := VEC_Average(Q1,Q2);
      R2 := VEC_Average(Q2,Q3);
      S1 := VEC_Average(R1,R2);

      Inc(Depth);
      DoBezier(Depth,BEZ(B.V1,Q1,R1,S1));
      DoBezier(Depth,BEZ(S1,R2,Q3,B.V4));
    End;
  End; // DoBezier

  // The following code was intended to "smooth out" faces, since we are using Beziers
  // that have already been projected onto 2D and aren't strictly accurate (but are
  // necessary for export to AutoREALM).  The code only works some of the time and
  // isn't all that helpful.

(*
  Procedure CheckBezier(Depth: Integer; B: TBezier; Var _Min,_Max: TVector; I1,I2: Single;
                        Var X1,Y1,X2,Y2: Single);
  Var
    Q1,Q2,Q3 : TVector;
    R1,R2,S1 : TVector;

  Begin
    If (Depth <> 1) And (Abs(B.V1.X - B.V4.X) < 3) And (Abs(B.V1.Y - B.V4.Y) < 3) Then
    Begin
      If B.V4.X < _Min.X Then
      Begin
        _Min.X := B.V4.X;
        X1     := I1;
      End;
      If B.V4.Y < _Min.Y Then
      Begin
        _Min.Y := B.V4.Y;
        Y1     := I1;
      End;
      If B.V4.X > _Max.X Then
      Begin
        _Max.X := B.V4.X;
        X2     := I1;
      End;
      If B.V4.Y > _Max.Y Then
      Begin
        _Max.Y := B.V4.Y;
        Y2     := I1;
      End;
    End
    Else
    Begin
      Q1 := VEC_Average(B.V1,B.V2);
      Q2 := VEC_Average(B.V2,B.V3);
      Q3 := VEC_Average(B.V3,B.V4);
      R1 := VEC_Average(Q1,Q2);
      R2 := VEC_Average(Q2,Q3);
      S1 := VEC_Average(R1,R2);

      Inc(Depth);
      CheckBezier(Depth,BEZ(B.V1,Q1,R1,S1),_Min,_Max,I1 - I2 / 2,I2 / 2,X1,Y1,X2,Y2);
      CheckBezier(Depth,BEZ(S1,R2,Q3,B.V4),_Min,_Max,I1 + I2 / 2,I2 / 2,X1,Y1,X2,Y2);
    End;
  End; // CheckBezier

  Procedure CheckForSplit(Var B: TBezier; I,J: Integer);
  Var Mid: Single;

    Function AngleLess(A1,A2: Extended): Boolean;
    Begin
      Result := ((A1 < A2) And (A1 >= 0) And (A2 >= 0)) Or
                ((A1 < A2) And (A1 <  0) And (A2 <  0)) Or
                ((A1 < A2) And (A1 <  0) And (A1 > A2 - Pi)) Or
                ((A1 > A2) And (A2 <  0) And (A1 > A2 + Pi));
    End; // AngleLess

  Begin
    If (X1[I] > 0) And (X1[I] < 1) Then
    Begin
      Mid := B.V1.Y + (B.V4.Y - B.V1.Y) * X1[I];
      If (_Min[J].Y < _Min[I].Y) Or (_Max[J].Y > _Max[I].Y) Then
      Begin
        If _Min[J].Y < _Min[I].Y Then
        Begin
          F[I] := (B.V1.Y < B.V4.Y);
          If F[I] Then
          Begin
            If AngleLess(ArcTan2(B.V1.Y - _Min[J].Y,B.V1.X - _Min[J].X),
                         ArcTan2(Mid - _Min[J].Y,_Min[I].X - _Min[J].X)) Then S[I] := X1[I];
          End
          Else
          Begin
//              If AngleLess(ArcTan2(B.V4.Y - _Min[J].Y,B.V4.X - _Min[J].X),
//                           ArcTan2(Mid - _Min[J].Y,_Min[I].X - _Min[J].X)) Then S[I] := X1[I];
          End;
        End
        Else
        Begin
          F[I] := (B.V1.Y > B.V4.Y);
          If F[I] Then
          Begin
//              If Not AngleLess(ArcTan2(B.V1.Y - _Min[J].Y,B.V1.X - _Min[J].X),
//                               ArcTan2(Mid - _Min[J].Y,_Min[I].X - _Min[J].X)) Then S[I] := X1[I];
          End
          Else
          Begin
            If Not AngleLess(ArcTan2(B.V4.Y - _Min[J].Y,B.V4.X - _Min[J].X),
                             ArcTan2(Mid - _Min[J].Y,_Min[I].X - _Min[J].X)) Then S[I] := X1[I];
          End;
        End;
      End;
    End
    Else If (X2[I] > 0) And (X2[I] < 1) Then
    Begin
      Mid := B.V1.Y + (B.V4.Y - B.V1.Y) * X2[I];
      If (_Min[J].Y < _Min[I].Y) Or (_Max[J].Y > _Max[I].Y) Then
      Begin
        If _Min[J].Y < _Min[I].Y Then
        Begin
          F[I] := (B.V1.Y < B.V4.Y);
          If F[I] Then
          Begin
//              If Not AngleLess(ArcTan2(B.V1.Y - _Min[J].Y,B.V1.X - _Max[J].X),
//                               ArcTan2(Mid - _Min[J].Y,_Max[I].X - _Max[J].X)) Then S[I] := X2[I];
          End
          Else
          Begin
            If Not AngleLess(ArcTan2(B.V4.Y - _Min[J].Y,B.V4.X - _Max[J].X),
                             ArcTan2(Mid - _Min[J].Y,_Max[I].X - _Max[J].X)) Then S[I] := X2[I];
          End;
        End
        Else
        Begin
          F[I] := (B.V1.Y > B.V4.Y);
          If F[I] Then
          Begin
            If AngleLess(ArcTan2(B.V1.Y - _Min[J].Y,B.V1.X - _Max[J].X),
                         ArcTan2(Mid - _Min[J].Y,_Max[I].X - _Max[J].X)) Then S[I] := X2[I];
          End
          Else
          Begin
//8              If AngleLess(ArcTan2(B.V4.Y - _Min[J].Y,B.V4.X - _Min[J].X),
//                           ArcTan2(Mid - _Min[J].Y,_Max[I].X - _Min[J].X)) Then S[I] := X2[I];
          End;
        End;
      End;
    End
    Else If (Y1[I] > 0) And (Y1[I] < 1) Then
    Begin
      Mid := B.V1.X + (B.V4.X - B.V1.X) * Y1[I];
      If (_Min[J].X < _Min[I].X) Or (_Max[J].X > _Max[I].X) Then
      Begin
        If _Min[J].X < _Min[I].X Then
        Begin
          F[I] := (B.V1.X < B.V4.X);
          If F[I] Then
          Begin
            If Not AngleLess(ArcTan2(B.V1.Y - _Max[J].Y,B.V1.X - _Min[J].X),
                             ArcTan2(_Min[I].Y - _Max[J].Y,Mid - _Min[J].X)) Then S[I] := Y1[I];
          End
          Else
          Begin
            If Not AngleLess(ArcTan2(B.V4.Y - _Max[J].Y,B.V4.X - _Min[J].X),
                             ArcTan2(_Min[I].Y - _Max[J].Y,Mid - _Min[J].X)) Then S[I] := Y1[I];
          End;
        End
        Else
        Begin
          F[I] := (B.V1.X > B.V4.X);
          If F[I] Then
          Begin
            If AngleLess(ArcTan2(B.V1.Y - _Max[J].Y,B.V1.X - _Min[J].X),
                         ArcTan2(_Min[I].Y - _Max[J].Y,Mid - _Min[J].X)) Then S[I] := Y1[I];
          End
          Else
          Begin
            If AngleLess(ArcTan2(B.V4.Y - _Max[J].Y,B.V4.X - _Min[J].X),
                         ArcTan2(_Min[I].Y - _Max[J].Y,Mid - _Min[J].X)) Then S[I] := Y1[I];
          End;
        End;
      End;
    End
    Else If (Y2[I] > 0) And (Y2[I] < 1) Then
    Begin
      Mid := B.V1.X + (B.V4.X - B.V1.X) * Y2[I];
      If (_Min[J].X < _Min[I].X) Or (_Max[J].X > _Max[I].X) Then
      Begin
        If _Min[J].X < _Min[I].X Then
        Begin
          F[I] := (B.V1.X < B.V4.X);
          If F[I] Then
          Begin
            If AngleLess(ArcTan2(B.V1.Y - _Max[J].Y,B.V1.X - _Min[J].X),
                         ArcTan2(_Max[I].Y - _Max[J].Y,Mid - _Max[J].X)) Then S[I] := Y2[I];
          End
          Else
          Begin
            If AngleLess(ArcTan2(B.V4.Y - _Max[J].Y,B.V4.X - _Min[J].X),
                         ArcTan2(_Max[I].Y - _Max[J].Y,Mid - _Max[J].X)) Then S[I] := Y2[I];
          End;
        End
        Else
        Begin
          F[I] := (B.V1.X > B.V4.X);
          If F[I] Then
          Begin
            If Not AngleLess(ArcTan2(B.V1.Y - _Max[J].Y,B.V1.X - _Min[J].X),
                             ArcTan2(_Max[I].Y - _Max[J].Y,Mid - _Max[J].X)) Then S[I] := Y2[I];
          End
          Else
          Begin
            If Not AngleLess(ArcTan2(B.V4.Y - _Max[J].Y,B.V4.X - _Min[J].X),
                             ArcTan2(_Max[I].Y - _Max[J].Y,Mid - _Max[J].X)) Then S[I] := Y2[I];
          End;
        End;
      End;
    End;
  End; // CheckForSplit

  Function BackFace(Var P: TPatch): Boolean;
  Var V1,V2,V3,V4: TVector;
  Begin
    V1 := VEC_Cross(P.B1.V2,P.B1.V3);
    V2 := VEC_Cross(P.B2.V2,P.B2.V3);
    V3 := VEC_Cross(P.B3.V2,P.B3.V3);
    V4 := VEC_Cross(P.B4.V2,P.B4.V3);
    V1 := VEC_Add(VEC_Add(VEC_Add(V3,V4),V2),V1);
    Result := false;//V1.Z < 0;
  End; // BackFace
*)
Begin
  Count := 0;
  SetLength(Poly,0);

  // NOTE: We are projecting all the Beziers to 2D FIRST.  This will remove
  // some accuracy from the curves, but is necessary if we want something that
  // AutoREALM can handle, without always creating polylines.  Being able to
  // describe the shape with just a handful of Bezier curves is worh the
  // slight inaccuracy (we can always tweak the curves normally if we have to).
  //
  // Note also that this technique is only reasonably accurate if each 3D curve
  // is coplanar (which they are in this case)

  P1.B1 := BProject(P.B1);
  P1.B2 := BProject(P.B2);
  P1.B3 := BProject(P.B3);
  P1.B4 := BProject(P.B4);

  // This code causes far more harm than good--it tries to smooth out
  // the faces.

{
  If cbSmooth.Checked then
  Begin
    _Min[1].X := P1.B1.V1.X;
    _Min[1].Y := P1.B1.V1.Y;
    _Min[2].X := P1.B2.V1.X;
    _Min[2].Y := P1.B2.V1.Y;
    _Min[3].X := P1.B3.V1.X;
    _Min[3].Y := P1.B3.V1.Y;
    _Min[4].X := P1.B4.V1.X;
    _Min[4].Y := P1.B4.V1.Y;
    For I := 1 To 4 Do
    Begin
      _Max[I] := _Min[I];
      X1[I]   := 0;
      Y1[I]   := 0;
      X2[I]   := 0;
      Y2[I]   := 0;
      S[I]    := 0;
    End; // For I

    If Not BackFace(P) Then CheckBezier(1,P1.B1,_Min[1],_Max[1],0.5,0.5,X1[1],Y1[1],X2[1],Y2[1]);
    If Not BackFace(P) Then CheckBezier(1,P1.B2,_Min[2],_Max[2],0.5,0.5,X1[2],Y1[2],X2[2],Y2[2]);
    If Not BackFace(P) Then CheckBezier(1,P1.B3,_Min[3],_Max[3],0.5,0.5,X1[3],Y1[3],X2[3],Y2[3]);
    If Not BackFace(P) Then CheckBezier(1,P1.B4,_Min[4],_Max[4],0.5,0.5,X1[4],Y1[4],X2[4],Y2[4]);

    For I := 1 To 4 Do
    Begin
      If X1[I] > 0.975 Then X1[I] := 1;
      If Y1[I] > 0.975 Then Y1[I] := 1;
      If X2[I] > 0.975 Then X2[I] := 1;
      If Y2[I] > 0.975 Then Y2[I] := 1;
    End; // For I

    // Form the envelope

    CheckForSplit(P1.B1,1,3);
    CheckForSplit(P1.B2,2,4);
    CheckForSplit(P1.B3,3,1);
    CheckForSplit(P1.B4,4,2);

    If S[1] <> 0 Then
    Begin
      BEZ_SplitAt(P1.B1,S[1],B1,B2);
      If Not F[1] Then
      Begin
        AddPoint(Round(B1.V1.X),Round(B1.V1.Y));
        DoBezier(1,B1);
        P1.B2.V1 := B1.V4;
        P1.B2.V2 := VEC_Add(VEC_Sub(P1.B2.V2,P1.B2.V1),B1.V4);
      End
      Else
      Begin
        AddPoint(Round(B2.V1.X),Round(B2.V1.Y));
        DoBezier(1,B2);
        P1.B4.V4 := B2.V1;
        P1.B4.V3 := VEC_Add(VEC_Sub(P1.B4.V3,P1.B4.V4),B2.V1);
      End;
    End
    Else
    Begin
      AddPoint(Round(P1.B1.V1.X),Round(P1.B1.V1.Y));
      DoBezier(1,P1.B1);
    End;

    If S[2] <> 0 Then
    Begin
      BEZ_SplitAt(P1.B2,S[2],B1,B2);
      If Not F[2] Then
      Begin
        AddPoint(Round(B1.V1.X),Round(B1.V1.Y));
        DoBezier(1,B1);
        P1.B3.V1 := B1.V4;
        P1.B3.V2 := VEC_Add(VEC_Sub(P1.B3.V2,P1.B3.V1),B1.V4);
      End
      Else
      Begin
        AddPoint(Round(B2.V1.X),Round(B2.V1.Y));
        DoBezier(1,B2);
      End;
    End
    Else
    Begin
      AddPoint(Round(P1.B2.V1.X),Round(P1.B2.V1.Y));
      DoBezier(1,P1.B2);
    End;

    If S[3] <> 0 Then
    Begin
      BEZ_SplitAt(P1.B3,S[3],B1,B2);
      If Not F[3] Then
      Begin
        AddPoint(Round(B1.V1.X),Round(B1.V1.Y));
        DoBezier(1,B1);
        P1.B4.V1 := B1.V4;
        P1.B4.V2 := VEC_Add(VEC_Sub(P1.B4.V2,P1.B4.V1),B1.V4);
      End
      Else
      Begin
        AddPoint(Round(B2.V1.X),Round(B2.V1.Y));
        DoBezier(1,B2);
      End;
    End
    Else
    Begin
      AddPoint(Round(P1.B3.V1.X),Round(P1.B3.V1.Y));
      DoBezier(1,P1.B3);
    End;

    If S[4] <> 0 Then
    Begin
      BEZ_SplitAt(P1.B4,S[4],B1,B2);
      If Not F[4] Then
      Begin
        AddPoint(Round(B1.V1.X),Round(B1.V1.Y));
        DoBezier(1,B1);
      End
      Else
      Begin
        AddPoint(Round(B2.V1.X),Round(B2.V1.Y));
        DoBezier(1,B2);
      End;
    End
    Else
    Begin
      AddPoint(Round(P1.B4.V1.X),Round(P1.B4.V1.Y));
      DoBezier(1,P1.B4);
    End;
  End
  Else
  Begin
}
    AddPoint(Round(P1.B1.V1.X),Round(P1.B1.V1.Y));
    DoBezier(1,P1.B1);
    AddPoint(Round(P1.B2.V1.X),Round(P1.B2.V1.Y));
    DoBezier(1,P1.B2);
    AddPoint(Round(P1.B3.V1.X),Round(P1.B3.V1.Y));
    DoBezier(1,P1.B3);
    AddPoint(Round(P1.B4.V1.X),Round(P1.B4.V1.Y));
    DoBezier(1,P1.B4);
//    End;
  SetLength(Poly,Count);
  Result := Poly;
End; // GetPolygon

Function TfrmMakeFrom3D.GetColor(Normal: TVector): TColor;
Var
  C        : TColor;
  MR,MG,MB : Single;
  AR,AG,AB : Single;
  LR,LG,LB : Single;
  R,G,B    : Integer;
  S        : Single;

Begin
  S := Abs(VEC_Dot(Normal,VEC_Normalize(VEC(1,-1,1))) / 3);

  MR := TRGBA(ModelColor).R;
  MG := TRGBA(ModelColor).G;
  MB := TRGBA(ModelColor).B;
  AR := TRGBA(AmbientColor).R;
  AG := TRGBA(AmbientColor).G;
  AB := TRGBA(AmbientColor).B;
  LR := TRGBA(LightColor).R;
  LG := TRGBA(LightColor).G;
  LB := TRGBA(LightColor).B;

  MR := MR / 255;
  MG := MG / 255;
  MB := MB / 255;
  AR := AR / 255;
  AG := AG / 255;
  AB := AB / 255;
  LR := LR / 255;
  LG := LG / 255;
  LB := LB / 255;

  R  := Round((LR * S + AR * MR) * 255);
  G  := Round((LG * S + AG * MG) * 255);
  B  := Round((LB * S + AB * MB) * 255);

  If R < 0   Then R := 0;
  If G < 0   Then G := 0;
  If B < 0   Then B := 0;
  If R > 255 Then R := 255;
  If G > 255 Then G := 255;
  If B > 255 Then B := 255;
  TRGBA(C).R := R;
  TRGBA(C).G := G;
  TRGBA(C).B := B;
  Result := C;
End; // TfrmMakeFrom3D.GetColor

Function TfrmMakeFrom3D.GetOutlineColor(Col: TColor): TColor;
Begin
  If cbUseOutlineColor.Checked
   Then Result := OutlineColor
   Else Result := Col;
End; // TfrmMakeFrom3D.GetOutlineColor

Procedure TfrmMakeFrom3D.PaintModel;
Var
  XC,YC   : Integer;
  I       : Integer;
  Polygon : TPolygon;
  Surface : TSurface;
  Col     : TColor;

  // Not used, as we are using polygons, but keeping it here in case we want to
  // use it later (such as creating hollow meshes or something)
  Procedure DrawBezier(Canvas: TCanvas; B: TBezier);

    Procedure DoBezier(Depth: Integer; B: TBezier);
    Var
      Q1,Q2,Q3: TVector;
      R1,R2,S1: TVector;

    Begin
      If (Depth <> 1) And (Abs(B.V1.X - B.V4.X) < 3) And (Abs(B.V1.Y - B.V4.Y) < 3) Then
       Canvas.LineTo(Round(B.V4.X) + XC,Round(B.V4.Y) + YC)
      Else
      Begin
        Q1 := VEC_Average(B.V1,B.V2);
        Q2 := VEC_Average(B.V2,B.V3);
        Q3 := VEC_Average(B.V3,B.V4);
        R1 := VEC_Average(Q1,Q2);
        R2 := VEC_Average(Q2,Q3);
        S1 := VEC_Average(R1,R2);

        Inc(Depth);
        DoBezier(Depth,BEZ(B.V1,Q1,R1,S1));
        DoBezier(Depth,BEZ(S1,R2,Q3,B.V4));
      End;
    End; // DoBezier

  Begin
    B := BProject(B);
    Canvas.MoveTo(Round(B.V1.X) + XC,Round(B.V1.Y) + YC);
    DoBezier(1,B);
  End; // DrawBezier

  // Not used.  Strictly for testing purposes, as it produces a dead-accurate
  // curve from the 3D source.  As such, it will differ from that which is
  // finally exported to AutoREALM.
  Procedure Draw3DBezier(Canvas: TCanvas; B: TBezier);
  Var P: TVector;

    Procedure DoBezier(Depth: Integer; B: TBezier);
    Var
      Q1,Q2,Q3 : TVector;
      R1,R2,S1 : TVector;
      P1,P2    : TVector;

    Begin
      P1 := RProject(B.V1);
      P2 := RProject(B.V4);
      P1 := VEC_Sub(P1,P2);

      If (Depth <> 1) And (VEC_Length(P1) < 3) Then Canvas.LineTo(Round(P2.X) + XC,Round(P2.Y) + YC)
      Else
      Begin
        Q1 := VEC_Average(B.V1,B.V2);
        Q2 := VEC_Average(B.V2,B.V3);
        Q3 := VEC_Average(B.V3,B.V4);
        R1 := VEC_Average(Q1,Q2);
        R2 := VEC_Average(Q2,Q3);
        S1 := VEC_Average(R1,R2);

        Inc(Depth);
        DoBezier(Depth,BEZ(B.V1,Q1,R1,S1));
        DoBezier(Depth,BEZ(S1,R2,Q3,B.V4));
      End;
    End; // DoBezier

  Begin
    P := RProject(B.V1);
    Canvas.MoveTo(Round(P.X) + XC,Round(P.Y) + YC);
    DoBezier(1,B);
  End; // Draw3DBezier

  Procedure DrawEnclosedFigure(Canvas: TCanvas; Points: TPolygon;
                               EdgeColor,FillColor: TColor; Closed: Boolean);
  Var
    SX1,SY1,SX2,SY2 : Integer;
    OldColor        : TColor;
    OldBrushColor   : TColor;
    I               : Integer;
    Poly            : PPointArray;
    Count           : Integer;

  Begin
    Count := High(Points) + 1;
    If Count > 0 Then
    Begin
      GetMem(Poly,Count * SizeOf(TPoint));

      For I := 0 To Count - 1 Do
      begin
        Poly^[I].X := Round(Points[I].X + XC);
        Poly^[I].Y := Round(Points[I].Y + YC);
      End; // For I
      OldColor := Canvas.Pen.Color;

      If (FillColor <> clNone) And Closed Then
      Begin
        OldBrushColor      := Canvas.Brush.Color;
        Canvas.Pen.Style   := psClear;

        Canvas.Pen.Color   := FillColor;
        Canvas.Brush.Color := FillColor;
        Canvas.Brush.Style := bsSolid;

        Windows.Polygon(Canvas.Handle, Poly^, Count);
        Canvas.Pen.Style   := psSolid;
        Canvas.Brush.Color := OldBrushColor;
      End;

      Canvas.Pen.Color := EdgeColor;

      SX1 := Poly^[0].X;
      SY1 := Poly^[0].Y;

      For I := 1 To Count - 1 Do
      Begin
        SX2 := Poly^[I].X;
        SY2 := Poly^[I].Y;
        Canvas.MoveTo(SX1,SY1);
        Canvas.LineTo(SX2,SY2);
        SX1 := SX2;
        SY1 := SY2;
      End; // For I

      Canvas.Pen.Color := OldColor;
      FreeMem(Poly,Count * SizeOf(TPoint));
    End;
  End; // DrawEnclosedFigure

Begin
  XC := imgMain.Width  Div 2;
  YC := imgMain.Height Div 2;

  // Clear the painting area

  imgMain.Canvas.Pen.Color   := clWhite;
  imgMain.Canvas.Brush.Color := clWhite;
  imgMain.Canvas.Brush.Style := bsSolid;
  imgMain.Canvas.Rectangle(0,0,imgMain.Width,imgMain.Height);

  SetLength(Polygon,0);
  Surface := GetSurface;

  // Display the surface

  For I := 0 To High(Surface.P) Do
  Begin
    Polygon := GetPolygon(Surface.P[I]);
    Col := GetColor(PAT_Normal(Surface.P[I]));
    DrawEnclosedFigure(imgMain.Canvas,Polygon,GetOutlineColor(Col),Col,True);
    SetLength(Polygon,0);
  End; // For I
  SetLength(Surface.P,0);
End; // TfrmMakeFrom3D.PaintModel

Procedure TfrmMakeFrom3D.ExportSurface;
Var
  Surface : TSurface;
  SColor  : TColor;
  OColor  : TColor;
  I,J,K   : Integer;
  Found   : Boolean;
  G       : GroupPrimitive;
  P,Q     : PolyCurvePrimitive;
  Polygon : TPolygon;
  Points  : PCoordArray;
  Style   : StyleAttrib;
  Last    : DrawPrimitive;
  P1      : TPatch;
  Center  : CoordPoint;
  Scale   : CoordPoint;
  CA      : Array Of PCoordArray;
  PC      : Array Of Integer;
  Remove  : Array Of Boolean;
  Within  : Array Of Boolean;
  Bounds  : Array Of CoordRect;
  X1,Y1   : Coord;
  X2,Y2   : Coord;

  Procedure CopyPoints(I: Integer; B: TBezier);
  Begin
    Points^[I + 0].X  := B.V1.X * Scale.X + Center.X;
    Points^[I + 1].X  := B.V2.X * Scale.X + Center.X;
    Points^[I + 2].X  := B.V3.X * Scale.X + Center.X;
    Points^[I + 3].X  := B.V4.X * Scale.X + Center.X;

    Points^[I + 0].Y  := B.V1.Y * Scale.Y + Center.Y;
    Points^[I + 1].Y  := B.V2.Y * Scale.Y + Center.Y;
    Points^[I + 2].Y  := B.V3.Y * Scale.Y + Center.Y;
    Points^[I + 3].Y  := B.V4.Y * Scale.Y + Center.Y;
  End; // CopyPoints

Begin
  Surface := GetSurface;
  If High(Surface.P) >= 0 Then
  Begin
    G           := GroupPrimitive.CreateBlank;
    G.First     := Nil;
    Last        := Nil;
    if cbUseOutlineColor.Checked
     Then Style.Line := 1
     Else Style.Line := 0;
    Style.Fill  := 0;
    Style.First := 0;
    Style.Last  := 0;
    Map.CurrentView.ScreenToCoord(320,320,Center.X,Center.Y);
    Map.CurrentView.DeltaScreenToCoord(64,64,Scale.X,Scale.Y);
    Scale.X := Scale.X / 200; // Not exact, it's a fudge factor, but good enough :)
    Scale.Y := Scale.Y / 200;

    // First do the faces

    SetLength(CA,High(Surface.P) + 1);
    SetLength(PC,High(Surface.P) + 1);
    SetLength(Remove,High(Surface.P) + 1);
    For I := 0 To High(Surface.P) Do
    Begin
      P1.B1 := BProject(Surface.P[I].B1);
      P1.B2 := BProject(Surface.P[I].B2);
      P1.B3 := BProject(Surface.P[I].B3);
      P1.B4 := BProject(Surface.P[I].B4);
      GetMem(Points,SizeOf(CoordPoint) * 13);
      CopyPoints(0,P1.B1);
      CopyPoints(3,P1.B2);
      CopyPoints(6,P1.B3);
      CopyPoints(9,P1.B4);

      SColor  := GetColor(PAT_Normal(Surface.P[I]));
      OColor  := GetOutlineColor(SColor);

      P       := PolyCurvePrimitive.Create(Points,13,Style);
      P.SetStyle(Style);
      P.SetFillColor(SColor);
      P.SetColor(OColor);

      CA[I] := P.GetLines(Nil{Map.CurrentView},PC[I]);

      If G.First = Nil Then G.First := P Else Last.Next := P;

      Last := P;
      SetLength(Polygon,0);
    End; // For I

    If cbRemoveHiddenSurfaces.Checked Then
    Begin
      // Get the bounds of all the polygons

      SetLength(Bounds,High(Surface.P) + 1);
      For I := 0 To High(Surface.P) Do
      Begin
        X1        := 0; // Makes Delphi happy
        Y1        := 0;
        X2        := 0;
        Y2        := 0;
        Remove[I] := False;
        If PC[I] > 0 Then
        Begin
          X1 := CA[I]^[0].X;
          Y1 := CA[I]^[0].Y;
          X2 := X1;
          Y2 := Y1;
          For J := 1 To PC[I] - 1 Do
          Begin
            If CA[I]^[J].X < X1 Then X1 := CA[I]^[J].X;
            If CA[I]^[J].Y < Y1 Then Y1 := CA[I]^[J].Y;
            If CA[I]^[J].X > X2 Then X2 := CA[I]^[J].X;
            If CA[I]^[J].Y > Y2 Then Y2 := CA[I]^[J].Y;
          End; // For J
        End
        Else Remove[I] := True;
        Bounds[I] := MakeCoordRect(X1,Y1,X2,Y2);
      End; // For I

      // Try to find out if we can remove some faces

      For I := High(Surface.P) - 1 DownTo 0 Do
      Begin
        SetLength(Within,PC[I]);
        For J := 0 To PC[I] - 1 Do Within[J] := False;
        J := I + 1;
        Found := False;
        Repeat
          If (PC[J] > 0) And Not Remove[J] Then
          Begin
            Found := True;
            For K := 0 To PC[I] - 1 Do
            Begin
              Within[K] := Within[K] Or PointInPolygon(CA[I]^[K],Bounds[J],CA[J],PC[J]);
              Found     := Found And Within[K];
            End; // For K
          End;
          Inc(J);
        Until (J > High(Surface.P)) Or Found;
        If Found Then Remove[I] := True;
        SetLength(Within,0);
      End; // For I

      // Remove those faces that are totally obscured

      P := G.First As PolyCurvePrimitive;
      I := 0;
      Q := Nil;
      While P <> Nil Do
      Begin
        If Remove[I] Then
        Begin
          If Q = Nil Then G.First := P.Next Else Q.Next := P.Next;
          P.Free;
          If Q = Nil Then P := PolyCurvePrimitive(G.First) Else P := PolyCurvePrimitive(Q.Next);
        End
        Else
        Begin
          Q := P;
          P := PolyCurvePrimitive(P.Next);
        End;
        Inc(I);
      End; // While
    End;
    
    // Deallocate the point lists

    For I := 0 To High(Surface.P) Do FreeMem(CA[I]);
    SetLength(CA,0);
    SetLength(PC,0);
    SetLength(Remove,0);
    SetLength(Bounds,0);

    // Add the resulting object to the map

    G.ComputeExtent;
    Map.ClearSelection;
    Map.AddObject(G);
    G.Select(Map.CurrentView,True);
    Map.CenterSelection;
    Map.DisplaySelectedSize;
  End;
  SetLength(Surface.P,0);
End; // TfrmMakeFrom3D.ExportSurface

Function TfrmMakeFrom3D.GetSurface: TSurface;
Var
  Surface      : Array Of TPatch;
  P            : TPatch;
  I            : Integer;
  CenterHeight : Single;
  Thickness    : Single;
  Elasticity   : Single;
  InnerRadius  : Single;
  OuterRadius  : Single;
  Size         : Single;
  Move         : Single;
  M            : TMatrix;
  A            : Single;
  ElasticTop   : Single;
  ElasticSides : Single;

  Procedure Arc(Var B: TBezier; Center: TVector; Angle: Single);
  // -------------------------------------------------------------------
  // Angle is in radians, and MUST be in the range 0 to Pi / 2
  // -------------------------------------------------------------------
  Var
    Handle : Single;  // Handle length
    VA,VB  : TVector;
    L,M    : Single;
    V1,V4  : TVector;

  Begin
    If Sin(Angle) <> 0 Then
    Begin
      Handle := Abs((8 / 3) * (Sqr(Sin(Angle / 4))) / Sin(Angle / 2));
      V1 := VEC_Sub(B.V1,Center);
      V4 := VEC_Sub(B.V4,Center);
      VA := VEC_Sub(V4,V1);
      L  := VEC_Length(VA) / 2;
      VB := VEC_Average(V1,V4);
      M  := VEC_Length(VB);
      If M <> 0 Then VB := VEC_Mult(VB,1 + Sqr(L / M));
      B.V2 := VEC_Sub(VB,V1);
      B.V3 := VEC_Sub(VB,V4);
      Handle := Handle * VEC_Length(V1);
      B.V2 := VEC_Add(VEC_Mult(VEC_Normalize(B.V2),Handle),B.V1);
      B.V3 := VEC_Add(VEC_Mult(VEC_Normalize(B.V3),Handle),B.V4);
    End
    Else
    Begin
      B.V2 := B.V1;   // It's a straight line
      B.V3 := B.V4;
    End;
  End; // Arc

  Procedure SortSurfaces;
  // This is by no means perfect (but it's pretty darned good)
  Var
    I,J     : Integer;
    P1,P2   : TPatch;
    C1,C2   : TVector;
    D1,D2   : TVector;
    PD1,PD2 : TVector;
    Swap    : Boolean;
    V1,V2   : TVector;
    PV1,PV2 : TVector;
    Min1    : Single;
    Min2    : Single;
    Max1    : Single;
    Max2    : Single;

  Begin
    For I := 0 To High(Surface) - 1 Do
    Begin
      P1 := Surface[I];
      For J := I + 1 To High(Surface) Do
      Begin
        P2   := Surface[J];

        // Find the approximate center of mass of both surfaces

        C1 := PAT_GetPointAt(P1,0.5,0.5);
        C2 := PAT_GetPointAt(P2,0.5,0.5);

        // For each surface, find the point that is closest to the other surface's center of mass
{
        D1 := C1;
        D2 := C2;
}

        D1 := PAT_FindClosestTo(P1,C2,False);
        D2 := PAT_FindClosestTo(P2,C1,False);

        // Move the found coordinates a little closer to the centers of mass so they can't lie on the shape edges (important for
        // the Z depth check)

        D1 := VEC_Add(VEC_Mult(D1,0.5),VEC_Mult(C1,0.5));
        D2 := VEC_Add(VEC_Mult(D2,0.5),VEC_Mult(C2,0.5));

        // Project the coordinates to the view plane

        PD1 := RProject(D1);
        PD2 := RProject(D2);

        // Find the point on the first surface that corresponds best to the second surface's center of mass

        V1   := PAT_FindClosestTo(P1,PD2,True);
        PV1  := RProject(V1);

        // Find the point on the second surface that corresponds best to the first surface's center of mass

        V2   := PAT_FindClosestTo(P2,PD1,True);
        PV2  := RProject(V2);

        // Swap if the second surface is farther away (so it will be drawn first)

        Swap := ((VEC_Distance2(PV1,PD2) < 16) And (D2.Z > V1.Z)) Or
                ((VEC_Distance2(PV2,PD1) < 16) And (D1.Z < V2.Z));

//        Swap := Swap Or (D2.Z > D1.Z);
        Swap := Swap Or (VEC_Length(D2) > VEC_Length(D1));

{
        // For the case where the center of mass doesn't intersect the other surface (which can happen often),
        // Look at the maximum and minimum Z values for both surfaces.  If they differ by a nontrivial amount
        // then use them to decide which one is in front of the other.  It's worth noting that this whole process
        // can't ever be perfect since all of the surfaces are curved, but we can come very close.

        Min1 := Min(Min(Min(P1.B1.V1.Z,P1.B2.V1.Z),P1.B3.V1.Z),P1.B4.V1.Z);
        Min2 := Min(Min(Min(P2.B1.V1.Z,P2.B2.V1.Z),P2.B3.V1.Z),P2.B4.V1.Z);
        Max1 := Max(Max(Max(P1.B1.V1.Z,P1.B2.V1.Z),P1.B3.V1.Z),P1.B4.V1.Z);
        Max2 := Max(Max(Max(P2.B1.V1.Z,P2.B2.V1.Z),P2.B3.V1.Z),P2.B4.V1.Z);

        Swap := Swap Or (Min1 < Min2 - 0.1) Or (Max2 > Max1 + 0.1);
}
        If Swap Then
        Begin
          Surface[J] := P1;
          Surface[I] := P2;
          P1         := P2;
        End;


{
        V1   := VEC_Add(VEC_Add(VEC_Add(P1.B1.V1,P1.B2.V1),P1.B3.V1),P1.B4.V1);
        V2   := VEC_Add(VEC_Add(VEC_Add(P2.B1.V1,P2.B2.V1),P2.B3.V1),P2.B4.V1);

        If (V2.Z > V1.Z) Or
           ((V2.Z = V1.Z) And (VEC_Length(V2) > VEC_Length(V1)))
//           ((V2.Z = V1.Z) And ((Max2 > Max1) Or ((Max2 = Max1) And (VEC_Length(V2) > VEC_Length(V1)))) )
        Then
//        If VEC_Length(V2) > VEC_Length(V1) Then
        Begin
          Surface[J] := P1;
          Surface[I] := P2;
          P1         := P2;
        End;
}
      End; // For J
    End; // For I
  End; // SortSurfaces

Begin
  // Setup

  Size := Max(100,udCenterHeight.Position) / 100;
  imgMain.Canvas.Pen.Color   := clBlack;
  imgMain.Canvas.Brush.Style := bsClear;
  M := MAT_Identity;
  Size := 2 / Size;
  If Size > 1 Then Size := 1;
  Move := Max(10,udCenterHeight.Position) / 100;
  If Move < 1 Then Move := 1;
  M := TR_Scale(M,Size,Size,Size);
  M := TR_Rotate(M,-udXRot.Position * Pi / 180,
                   -udYRot.Position * Pi / 180,
                   -udZRot.Position * Pi / 180);
  M := TR_Translate(M,0,-Size * Move / 2,3);

  M := TR_Translate(M,4 * (udXTrans.Position - 50) / 100,
                      4 * (udYTrans.Position - 50) / 100,
                      2 * (udZTrans.Position - 50) / 100);

  CenterHeight := udCenterHeight.Position / 100;
  Thickness    := udThickness.Position    / 100;
  InnerRadius  := udInnerRadius.Position  / 100;
  OuterRadius  := udOuterRadius.Position  / 100;
  Elasticity   := udElasticity.Position   / 100;
  A            := 2 * Pi / udPolygonSides.Position;
  If cbTop.Checked Or Not cbPolygonal.Checked Then ElasticTop := Elasticity Else ElasticTop   := 0;
  If cbSides.Checked Then ElasticSides := Elasticity Else ElasticSides := 0;
  SetLength(Surface,0);

  // Draw the model

  If rbCone.Checked Then
  Begin
    For I := 0 To udPolygonSides.Position - 1 Do
    Begin
      If cbPolygonal.Checked Then
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
        P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
        P.B3.V3.Y := P.B3.V4.Y;

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End
      Else
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        Arc(P.B1,VEC(0,0,0),A);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        Arc(P.B3,VEC(0,CenterHeight,0),A);

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,CenterHeight,0),A);

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,0,0),A);

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End;
    End; // For I
  End
  Else If rbCylindrical.Checked Then
  Begin
    For I := 0 To udPolygonSides.Position - 1 Do
    Begin
      If cbPolygonal.Checked Then
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
        P.B2.V3.Y := CenterHeight;

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
        P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
        P.B3.V3.Y := P.B3.V4.Y;

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
        P.B4.V2.Y := CenterHeight;

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V3.Y := CenterHeight;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := CenterHeight;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End
      Else
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        Arc(P.B1,VEC(0,0,0),A);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
        P.B2.V3.Y := CenterHeight;

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        Arc(P.B3,VEC(0,CenterHeight,0),A);

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
        P.B4.V2.Y := CenterHeight;

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V3.Y := CenterHeight;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := CenterHeight;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,CenterHeight,0),A);

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);
          P.B2.V2.Y := P.B2.V1.Y;
          P.B2.V3.Y := P.B2.V4.Y;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,0,0),A);

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V1,1 - ElasticTop);
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V4,1 - ElasticTop);
          P.B4.V2.Y := P.B4.V1.Y;
          P.B4.V3.Y := P.B4.V4.Y;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End;
    End; // For I
  End
  Else If rbSpherical.Checked Then
  Begin
    For I := 0 To udPolygonSides.Position - 1 Do
    Begin
      If cbPolygonal.Checked Then
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC(Cos((I + 1) * A),0,Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V2,1 - ElasticTop);
        P.B2.V3 := VEC(Cos((I + 1) * A),CenterHeight,Sin((I + 1) * A));
        P.B2.V3 := VEC_Mult(P.B2.V3,1 - ElasticTop);
        P.B2.V3.Y := CenterHeight;

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
        P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
        P.B3.V3.Y := P.B3.V4.Y;

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V2 := VEC(Cos(I * A),CenterHeight,Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V2,1 - ElasticTop);
        P.B4.V3 := VEC(Cos(I * A),0,Sin(I * A));
        P.B4.V3 := VEC_Mult(P.B4.V3,1 - ElasticTop);
        P.B4.V2.Y := CenterHeight;

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := VEC(Cos((I + 1) * A),0,Sin((I + 1) * A));
          P.B2.V2 := VEC_Mult(P.B2.V2,1 - ElasticTop);
          P.B2.V3 := VEC(Cos((I + 1) * A) * (1 - Thickness),CenterHeight,Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V3 := VEC_Mult(P.B2.V3,1 - ElasticTop);
          P.B2.V3.Y := CenterHeight;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;     // Don't want to change Y
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V2 := VEC(Cos(I * A) * (1 - Thickness),CenterHeight,Sin(I * A) * (1 - Thickness));
          P.B4.V2 := VEC_Mult(P.B4.V2,1 - ElasticTop);
          P.B4.V3 := VEC(Cos(I * A),0,Sin(I * A));
          P.B4.V3 := VEC_Mult(P.B4.V3,1 - ElasticTop);
          P.B4.V2.Y := CenterHeight;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := P.B2.V1;
          P.B2.V3 := P.B2.V4;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V2 := P.B4.V1;
          P.B4.V3 := P.B4.V4;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
          P.B1.V2.Y := P.B1.V1.Y;
          P.B1.V3.Y := P.B1.V4.Y;

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := P.B2.V1;
          P.B2.V3 := P.B2.V4;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);
          P.B3.V2.Y := P.B3.V1.Y;
          P.B3.V3.Y := P.B3.V4.Y;

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V2 := P.B4.V1;
          P.B4.V3 := P.B4.V4;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End
      Else
      Begin
        P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        Arc(P.B1,VEC(0,0,0),A);

        P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
        P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B2.V2 := VEC(Cos((I + 1) * A),0,Sin((I + 1) * A));
        P.B2.V3 := VEC(Cos((I + 1) * A),CenterHeight,Sin((I + 1) * A));
        P.B2.V2 := VEC_Mult(P.B2.V2,1 - ElasticTop);
        P.B2.V3 := VEC_Mult(P.B2.V3,1 - ElasticTop);
        P.B2.V3.Y := CenterHeight;

        P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
        P.B3.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        Arc(P.B3,VEC(0,CenterHeight,0),A);

        P.B4.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
        P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
        P.B4.V2 := VEC(Cos(I * A),CenterHeight,Sin(I * A));
        P.B4.V3 := VEC(Cos(I * A),0,Sin(I * A));
        P.B4.V2 := VEC_Mult(P.B4.V2,1 - ElasticTop);
        P.B4.V3 := VEC_Mult(P.B4.V3,1 - ElasticTop);
        P.B4.V2.Y := CenterHeight;

        P       := PAT_MultMatrix(P,M);
        SetLength(Surface,High(Surface) + 2);
        Surface[High(Surface)] := P;

        // Inside walls

        If (Thickness > 0) And (Thickness < 1) Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := VEC(Cos((I + 1) * A),0,Sin((I + 1) * A));
          P.B2.V3 := VEC(Cos((I + 1) * A) * (1 - Thickness),CenterHeight,Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := VEC_Mult(P.B2.V2,1 - ElasticTop);
          P.B2.V3 := VEC_Mult(P.B2.V3,1 - ElasticTop);
          P.B2.V3.Y := CenterHeight;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V2 := VEC(Cos(I * A) * (1 - Thickness),CenterHeight,Sin(I * A) * (1 - Thickness));
          P.B4.V3 := VEC(Cos(I * A),0,Sin(I * A));
          P.B4.V2 := VEC_Mult(P.B4.V2,1 - ElasticTop);
          P.B4.V3 := VEC_Mult(P.B4.V3,1 - ElasticTop);
          P.B4.V2.Y := CenterHeight;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Top

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B1.V4 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,CenterHeight,0),A);

          P.B2.V1 := VEC(InnerRadius * Cos((I + 1) * A),CenterHeight,InnerRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := P.B2.V1;
          P.B2.V3 := P.B2.V4;

          P.B3.V1 := VEC(InnerRadius * Cos((I + 1) * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,CenterHeight,0),A);

          P.B4.V1 := VEC(InnerRadius * Cos(I * A) * (1 - Thickness),CenterHeight,InnerRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(InnerRadius * Cos(I * A),CenterHeight,InnerRadius * Sin(I * A));
          P.B4.V2 := P.B4.V1;
          P.B4.V3 := P.B4.V4;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;

        // Bottom

        If Thickness > 0 Then
        Begin
          P.B1.V1 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B1.V4 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          Arc(P.B1,VEC(0,0,0),A);

          P.B2.V1 := VEC(OuterRadius * Cos((I + 1) * A),0,OuterRadius * Sin((I + 1) * A));
          P.B2.V4 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B2.V2 := P.B2.V1;
          P.B2.V3 := P.B2.V4;

          P.B3.V1 := VEC(OuterRadius * Cos((I + 1) * A) * (1 - Thickness),0,OuterRadius * Sin((I + 1) * A) * (1 - Thickness));
          P.B3.V4 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          Arc(P.B3,VEC(0,0,0),A);

          P.B4.V1 := VEC(OuterRadius * Cos(I * A) * (1 - Thickness),0,OuterRadius * Sin(I * A) * (1 - Thickness));
          P.B4.V4 := VEC(OuterRadius * Cos(I * A),0,OuterRadius * Sin(I * A));
          P.B4.V2 := P.B4.V1;
          P.B4.V3 := P.B4.V4;

          P       := PAT_MultMatrix(P,M);
          SetLength(Surface,High(Surface) + 2);
          Surface[High(Surface)] := P;
        End;
      End;
    End; // For I
  End
  Else
  Begin
    // Rectangular

    // Left slope

    P.B1.V1 := VEC(-OuterRadius,0,-1);
    P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
    P.B1.V4 := VEC(0,CenterHeight,-InnerRadius);
    P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
    P.B1.V3.Z := P.B1.V4.Z;

    P.B2.V1 := P.B1.V4;
    P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
    P.B2.V4 := VEC(0,CenterHeight,InnerRadius);
    P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

    P.B3.V1 := P.B2.V4;
    P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
    P.B3.V2.Z := P.B3.V1.Z;
    P.B3.V4 := VEC(-OuterRadius,0,1);
    P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

    P.B4.V1 := P.B3.V4;
    P.B4.V2 := P.B4.V1;
    P.B4.V4 := P.B1.V1;
    P.B4.V3 := P.B4.V4;

    P       := PAT_MultMatrix(P,M);
    SetLength(Surface,High(Surface) + 2);
    Surface[High(Surface)] := P;

    // Right slope

    P.B1.V1 := VEC(OuterRadius,0,-1);
    P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
    P.B1.V4 := VEC(0,CenterHeight,-InnerRadius);
    P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
    P.B1.V3.Z := P.B1.V4.Z;

    P.B2.V1 := P.B1.V4;
    P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
    P.B2.V4 := VEC(0,CenterHeight,InnerRadius);
    P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

    P.B3.V1 := P.B2.V4;
    P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
    P.B3.V2.Z := P.B3.V1.Z;
    P.B3.V4 := VEC(OuterRadius,0,1);
    P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

    P.B4.V1 := P.B3.V4;
    P.B4.V2 := P.B4.V1;
    P.B4.V4 := P.B1.V1;
    P.B4.V3 := P.B4.V4;

    P       := PAT_MultMatrix(P,M);
    SetLength(Surface,High(Surface) + 2);
    Surface[High(Surface)] := P;

    // Front slope

    P.B1.V1 := VEC(-OuterRadius,0,-1);
    P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
    P.B1.V4 := VEC(0,CenterHeight,-InnerRadius);
    P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
    P.B1.V3.Z := P.B1.V4.Z;

    P.B2.V1 := P.B1.V4;  // Point of the triangle
    P.B2.V2 := P.B1.V4;
    P.B2.V4 := P.B1.V4;
    P.B2.V3 := P.B1.V4;

    P.B3.V1 := P.B2.V4;
    P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
    P.B3.V2.Z := P.B3.V1.Z;
    P.B3.V4 := VEC(OuterRadius,0,-1);
    P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

    P.B4.V1 := P.B3.V4;
    P.B4.V2 := P.B4.V1;
    P.B4.V4 := P.B1.V1;
    P.B4.V3 := P.B4.V4;

    P       := PAT_MultMatrix(P,M);
    SetLength(Surface,High(Surface) + 2);
    Surface[High(Surface)] := P;

    // Rear slope

    P.B1.V1 := VEC(OuterRadius,0,1);
    P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
    P.B1.V4 := VEC(0,CenterHeight,InnerRadius);
    P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
    P.B1.V3.Z := P.B1.V4.Z;

    P.B2.V1 := P.B1.V4;  // Point of the triangle
    P.B2.V2 := P.B1.V4;
    P.B2.V4 := P.B1.V4;
    P.B2.V3 := P.B1.V4;

    P.B3.V1 := P.B2.V4;
    P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
    P.B3.V2.Z := P.B3.V1.Z;
    P.B3.V4 := VEC(-OuterRadius,0,1);
    P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

    P.B4.V1 := P.B3.V4;
    P.B4.V2 := P.B4.V1;
    P.B4.V4 := P.B1.V1;
    P.B4.V3 := P.B4.V4;

    P       := PAT_MultMatrix(P,M);
    SetLength(Surface,High(Surface) + 2);
    Surface[High(Surface)] := P;

    // Inside walls

    If (Thickness > 0) And (Thickness < 1) Then
    Begin
      // Left slope

      P.B1.V1 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
      P.B1.V4 := VEC(0,CenterHeight * (1 - Thickness),-InnerRadius * (1 - Thickness));
      P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
      P.B1.V3.Z := P.B1.V4.Z;

      P.B2.V1 := P.B1.V4;
      P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
      P.B2.V4 := VEC(0,CenterHeight * (1 - Thickness),InnerRadius * (1 - Thickness));
      P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

      P.B3.V1 := P.B2.V4;
      P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
      P.B3.V2.Z := P.B3.V1.Z;
      P.B3.V4 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

      P.B4.V1 := P.B3.V4;
      P.B4.V2 := P.B4.V1;
      P.B4.V4 := P.B1.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Right slope

      P.B1.V1 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
      P.B1.V4 := VEC(0,CenterHeight * (1 - Thickness),-InnerRadius * (1 - Thickness));
      P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
      P.B1.V3.Z := P.B1.V4.Z;

      P.B2.V1 := P.B1.V4;
      P.B2.V2 := VEC_Mult(P.B2.V1,1 - ElasticTop);
      P.B2.V4 := VEC(0,CenterHeight * (1 - Thickness),InnerRadius * (1 - Thickness));
      P.B2.V3 := VEC_Mult(P.B2.V4,1 - ElasticTop);

      P.B3.V1 := P.B2.V4;
      P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
      P.B3.V2.Z := P.B3.V1.Z;
      P.B3.V4 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

      P.B4.V1 := P.B3.V4;
      P.B4.V2 := P.B4.V1;
      P.B4.V4 := P.B1.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Front slope

      P.B1.V1 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
      P.B1.V4 := VEC(0,CenterHeight * (1 - Thickness),-InnerRadius * (1 - Thickness));
      P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
      P.B1.V3.Z := P.B1.V4.Z;

      P.B2.V1 := P.B1.V4;  // Point of the triangle
      P.B2.V2 := P.B1.V4;
      P.B2.V4 := P.B1.V4;
      P.B2.V3 := P.B1.V4;

      P.B3.V1 := P.B2.V4;
      P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
      P.B3.V2.Z := P.B3.V1.Z;
      P.B3.V4 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

      P.B4.V1 := P.B3.V4;
      P.B4.V2 := P.B4.V1;
      P.B4.V4 := P.B1.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Rear slope

      P.B1.V1 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B1.V2 := VEC_Mult(P.B1.V1,1 - ElasticSides);
      P.B1.V4 := VEC(0,CenterHeight * (1 - Thickness),InnerRadius * (1 - Thickness));
      P.B1.V3 := VEC_Mult(P.B1.V4,1 - ElasticSides);
      P.B1.V3.Z := P.B1.V4.Z;

      P.B2.V1 := P.B1.V4;  // Point of the triangle
      P.B2.V2 := P.B1.V4;
      P.B2.V4 := P.B1.V4;
      P.B2.V3 := P.B1.V4;

      P.B3.V1 := P.B2.V4;
      P.B3.V2 := VEC_Mult(P.B3.V1,1 - ElasticSides);
      P.B3.V2.Z := P.B3.V1.Z;
      P.B3.V4 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V3 := VEC_Mult(P.B3.V4,1 - ElasticSides);

      P.B4.V1 := P.B3.V4;
      P.B4.V2 := P.B4.V1;
      P.B4.V4 := P.B1.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;
    End;

    // Bottom

    If Thickness > 0 Then
    Begin
      // Left

      P.B1.V1 := VEC(-OuterRadius,0,-1);
      P.B1.V4 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := P.B1.V1;
      P.B1.V3 := P.B1.V4;

      P.B2.V1 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B2.V4 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B2.V2 := P.B2.V1;
      P.B2.V3 := P.B2.V4;

      P.B3.V1 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V4 := VEC(-OuterRadius,0,1);
      P.B3.V2 := P.B3.V1;
      P.B3.V3 := P.B3.V4;

      P.B4.V1 := VEC(-OuterRadius,0,1);
      P.B4.V4 := VEC(-OuterRadius,0,-1);
      P.B4.V2 := P.B4.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Right

      P.B1.V1 := VEC(OuterRadius,0,-1);
      P.B1.V4 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := P.B1.V1;
      P.B1.V3 := P.B1.V4;

      P.B2.V1 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B2.V4 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B2.V2 := P.B2.V1;
      P.B2.V3 := P.B2.V4;

      P.B3.V1 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V4 := VEC(OuterRadius,0,1);
      P.B3.V2 := P.B3.V1;
      P.B3.V3 := P.B3.V4;

      P.B4.V1 := VEC(OuterRadius,0,1);
      P.B4.V4 := VEC(OuterRadius,0,-1);
      P.B4.V2 := P.B4.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Front

      P.B1.V1 := VEC(-OuterRadius,0,-1);
      P.B1.V4 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B1.V2 := P.B1.V1;
      P.B1.V3 := P.B1.V4;

      P.B2.V1 := VEC(-OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B2.V4 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B2.V2 := P.B2.V1;
      P.B2.V3 := P.B2.V4;

      P.B3.V1 := VEC(OuterRadius * (1 - Thickness),0,-(1 - Thickness));
      P.B3.V4 := VEC(OuterRadius,0,-1);
      P.B3.V2 := P.B3.V1;
      P.B3.V3 := P.B3.V4;

      P.B4.V1 := VEC(OuterRadius,0,-1);
      P.B4.V4 := VEC(-OuterRadius,0,-1);
      P.B4.V2 := P.B4.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;

      // Rear

      P.B1.V1 := VEC(-OuterRadius,0,1);
      P.B1.V4 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B1.V2 := P.B1.V1;
      P.B1.V3 := P.B1.V4;

      P.B2.V1 := VEC(-OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B2.V4 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B2.V2 := P.B2.V1;
      P.B2.V3 := P.B2.V4;

      P.B3.V1 := VEC(OuterRadius * (1 - Thickness),0,1 - Thickness);
      P.B3.V4 := VEC(OuterRadius,0,1);
      P.B3.V2 := P.B3.V1;
      P.B3.V3 := P.B3.V4;

      P.B4.V1 := VEC(OuterRadius,0,1);
      P.B4.V4 := VEC(-OuterRadius,0,1);
      P.B4.V2 := P.B4.V1;
      P.B4.V3 := P.B4.V4;

      P       := PAT_MultMatrix(P,M);
      SetLength(Surface,High(Surface) + 2);
      Surface[High(Surface)] := P;
    End;
  End;

  SortSurfaces;
  SetLength(Result.P,High(Surface) + 1);
  For I := 0 To High(Surface) Do Result.P[I] := Surface[I];
  SetLength(Surface,0);
End; // TfrmMakeFrom3D.GetSurface

procedure TfrmMakeFrom3D.edtPolygonSidesChange(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtCenterHeightChange(Sender: TObject);
begin
  tbCenterHeight.Position := udCenterHeight.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udCenterHeightClick(Sender: TObject;
  Button: TUDBtnType);
begin
  tbCenterHeight.Position := udCenterHeight.Position;
end;

procedure TfrmMakeFrom3D.tbCenterHeightChange(Sender: TObject);
begin
  udCenterHeight.Position := tbCenterHeight.Position;
end;

procedure TfrmMakeFrom3D.edtXRotChange(Sender: TObject);
begin
  tbXRot.Position := udXRot.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtYRotChange(Sender: TObject);
begin
  tbYRot.Position := udYRot.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtZRotChange(Sender: TObject);
begin
  tbZRot.Position := udZRot.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udXRotClick(Sender: TObject; Button: TUDBtnType);
begin
  tbXRot.Position := udXRot.Position;
end;

procedure TfrmMakeFrom3D.udYRotClick(Sender: TObject; Button: TUDBtnType);
begin
  tbYRot.Position := udYRot.Position;
end;

procedure TfrmMakeFrom3D.udZRotClick(Sender: TObject; Button: TUDBtnType);
begin
  tbZRot.Position := udZRot.Position;
end;

procedure TfrmMakeFrom3D.tbXRotChange(Sender: TObject);
begin
  udXRot.Position := tbXRot.Position;
end;

procedure TfrmMakeFrom3D.tbYRotChange(Sender: TObject);
begin
  udYRot.Position := tbYRot.Position;
end;

procedure TfrmMakeFrom3D.tbZRotChange(Sender: TObject);
begin
  udZRot.Position := tbZRot.Position;
end;

procedure TfrmMakeFrom3D.cbTopClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.cbSidesClick(Sender: TObject);
begin
  PaintModel;
end;

Procedure TfrmMakeFrom3D.SetModelColorFromRGB;
Var
  R,G,B,H,S,V : Single;
  HH,SS,VV    : Integer;

Begin
  If Not SettingModelHSV Then
  Begin
    TRGBA(ModelColor).R := udMR.Position;
    TRGBA(ModelColor).G := udMG.Position;
    TRGBA(ModelColor).B := udMB.Position;
    TRGBA(ModelColor).A := 0;
    pnlModelColor.Color := ModelColor;
    R := udMR.Position;
    G := udMG.Position;
    B := udMB.Position;
    RGBtoHSV(R / 255,G / 255,B / 255,H,S,V);
    HH := Round(H);
    SS := Round(S * 100);
    VV := Round(V * 100);
    If HH < 0   Then HH := 0;
    If SS < 0   Then SS := 0;
    If VV < 0   Then VV := 0;
    If HH > 360 Then HH := 360;
    If SS > 100 Then SS := 100;
    If VV > 100 Then VV := 100;
    udMH.Position := HH;
    udMS.Position := SS;
    udMV.Position := VV;
    SettingModelRGB := False;
    SettingModelHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetModelColorFromRGB

Procedure TfrmMakeFrom3D.SetModelColorFromHSV;
Var
  R,G,B,H,S,V : Single;
  RR,GG,BB    : Integer;

Begin
  If Not SettingModelRGB Then
  Begin
    H := udMH.Position;
    S := udMS.Position / 100;
    V := udMV.Position / 100;
    HSVtoRGB(H,S,V,R,G,B);
    RR := Round(R * 255);
    GG := Round(G * 255);
    BB := Round(B * 255);
    If RR < 0   Then RR := 0;
    If GG < 0   Then GG := 0;
    If BB < 0   Then BB := 0;
    If RR > 255 Then RR := 255;
    If GG > 255 Then GG := 255;
    If BB > 255 Then BB := 255;
    TRGBA(ModelColor).R := RR;
    TRGBA(ModelColor).G := GG;
    TRGBA(ModelColor).B := BB;
    TRGBA(ModelColor).A := 0;
    pnlModelColor.Color := ModelColor;
    udMR.Position := RR;
    udMG.Position := GG;
    udMB.Position := BB;
    SettingModelRGB := False;
    SettingModelHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetModelColorFromHSV

Procedure TfrmMakeFrom3D.SetLightColorFromRGB;
Var
  R,G,B,H,S,V : Single;
  HH,SS,VV    : Integer;

Begin
  If Not SettingLightHSV Then
  Begin
    TRGBA(LightColor).R := udLR.Position;
    TRGBA(LightColor).G := udLG.Position;
    TRGBA(LightColor).B := udLB.Position;
    TRGBA(LightColor).A := 0;
    pnlLightColor.Color := LightColor;
    R := udLR.Position;
    G := udLG.Position;
    B := udLB.Position;
    RGBtoHSV(R / 255,G / 255,B / 255,H,S,V);
    HH := Round(H);
    SS := Round(S * 100);
    VV := Round(V * 100);
    If HH < 0   Then HH := 0;
    If SS < 0   Then SS := 0;
    If VV < 0   Then VV := 0;
    If HH > 360 Then HH := 360;
    If SS > 100 Then SS := 100;
    If VV > 100 Then VV := 100;
    udLH.Position := HH;
    udLS.Position := SS;
    udLV.Position := VV;
    SettingLightRGB := False;
    SettingLightHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetLightColorFromRGB

Procedure TfrmMakeFrom3D.SetLightColorFromHSV;
Var
  R,G,B,H,S,V : Single;
  RR,GG,BB    : Integer;

Begin
  If Not SettingLightRGB Then
  Begin
    H := udLH.Position;
    S := udLS.Position / 100;
    V := udLV.Position / 100;
    HSVtoRGB(H,S,V,R,G,B);
    RR := Round(R * 255);
    GG := Round(G * 255);
    BB := Round(B * 255);
    If RR < 0   Then RR := 0;
    If GG < 0   Then GG := 0;
    If BB < 0   Then BB := 0;
    If RR > 255 Then RR := 255;
    If GG > 255 Then GG := 255;
    If BB > 255 Then BB := 255;
    TRGBA(LightColor).R := RR;
    TRGBA(LightColor).G := GG;
    TRGBA(LightColor).B := BB;
    TRGBA(LightColor).A := 0;
    pnlLightColor.Color := LightColor;
    udLR.Position := RR;
    udLG.Position := GG;
    udLB.Position := BB;
    SettingLightRGB := False;
    SettingLightHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetLightColorFromHSV

Procedure TfrmMakeFrom3D.SetAmbientColorFromRGB;
Var
  R,G,B,H,S,V : Single;
  HH,SS,VV    : Integer;

Begin
  If Not SettingAmbientHSV Then
  Begin
    TRGBA(AmbientColor).R := udAR.Position;
    TRGBA(AmbientColor).G := udAG.Position;
    TRGBA(AmbientColor).B := udAB.Position;
    TRGBA(AmbientColor).A := 0;
    pnlAmbientColor.Color := AmbientColor;
    R := udAR.Position;
    G := udAG.Position;
    B := udAB.Position;
    RGBtoHSV(R / 255,G / 255,B / 255,H,S,V);
    HH := Round(H);
    SS := Round(S * 100);
    VV := Round(V * 100);
    If HH < 0   Then HH := 0;
    If SS < 0   Then SS := 0;
    If VV < 0   Then VV := 0;
    If HH > 360 Then HH := 360;
    If SS > 100 Then SS := 100;
    If VV > 100 Then VV := 100;
    udAH.Position := HH;
    udAS.Position := SS;
    udAV.Position := VV;
    SettingAmbientRGB := False;
    SettingAmbientHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetAmbientColorFromRGB

Procedure TfrmMakeFrom3D.SetAmbientColorFromHSV;
Var
  R,G,B,H,S,V : Single;
  RR,GG,BB    : Integer;

Begin
  If Not SettingAmbientRGB Then
  Begin
    H := udAH.Position;
    S := udAS.Position / 100;
    V := udAV.Position / 100;
    HSVtoRGB(H,S,V,R,G,B);
    RR := Round(R * 255);
    GG := Round(G * 255);
    BB := Round(B * 255);
    If RR < 0   Then RR := 0;
    If GG < 0   Then GG := 0;
    If BB < 0   Then BB := 0;
    If RR > 255 Then RR := 255;
    If GG > 255 Then GG := 255;
    If BB > 255 Then BB := 255;
    TRGBA(AmbientColor).R := RR;
    TRGBA(AmbientColor).G := GG;
    TRGBA(AmbientColor).B := BB;
    TRGBA(AmbientColor).A := 0;
    pnlAmbientColor.Color := AmbientColor;
    udAR.Position := RR;
    udAG.Position := GG;
    udAB.Position := BB;
    SettingAmbientRGB := False;
    SettingAmbientHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetAmbientColorFromHSV

Procedure TfrmMakeFrom3D.SetOutlineColorFromRGB;
Var
  R,G,B,H,S,V : Single;
  HH,SS,VV    : Integer;

Begin
  If Not SettingOutlineHSV Then
  Begin
    TRGBA(OutlineColor).R := udOR.Position;
    TRGBA(OutlineColor).G := udOG.Position;
    TRGBA(OutlineColor).B := udOB.Position;
    TRGBA(OutlineColor).A := 0;
    pnlOutlineColor.Color := OutlineColor;
    R := udOR.Position;
    G := udOG.Position;
    B := udOB.Position;
    RGBtoHSV(R / 255,G / 255,B / 255,H,S,V);
    HH := Round(H);
    SS := Round(S * 100);
    VV := Round(V * 100);
    If HH < 0   Then HH := 0;
    If SS < 0   Then SS := 0;
    If VV < 0   Then VV := 0;
    If HH > 360 Then HH := 360;
    If SS > 100 Then SS := 100;
    If VV > 100 Then VV := 100;
    udOH.Position := HH;
    udOS.Position := SS;
    udOV.Position := VV;
    SettingOutlineRGB := False;
    SettingOutlineHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetOutlineColorFromRGB

Procedure TfrmMakeFrom3D.SetOutlineColorFromHSV;
Var
  R,G,B,H,S,V : Single;
  RR,GG,BB    : Integer;

Begin
  If Not SettingOutlineRGB Then
  Begin
    H := udOH.Position;
    S := udOS.Position / 100;
    V := udOV.Position / 100;
    HSVtoRGB(H,S,V,R,G,B);
    RR := Round(R * 255);
    GG := Round(G * 255);
    BB := Round(B * 255);
    If RR < 0   Then RR := 0;
    If GG < 0   Then GG := 0;
    If BB < 0   Then BB := 0;
    If RR > 255 Then RR := 255;
    If GG > 255 Then GG := 255;
    If BB > 255 Then BB := 255;
    TRGBA(OutlineColor).R := RR;
    TRGBA(OutlineColor).G := GG;
    TRGBA(OutlineColor).B := BB;
    TRGBA(OutlineColor).A := 0;
    pnlOutlineColor.Color := OutlineColor;
    udOR.Position := RR;
    udOG.Position := GG;
    udOB.Position := BB;
    SettingOutlineRGB := False;
    SettingOutlineHSV := False;
    PaintModel;
  End;
End; // TfrmMakeFrom3D.SetOutlineColorFromHSV

procedure TfrmMakeFrom3D.edtMRChange(Sender: TObject);
begin
  If Not SettingModelHSV Then SettingModelRGB := True;
  tbMR.Position := udMR.Position;
  SetModelColorFromRGB;
end;

procedure TfrmMakeFrom3D.udMRClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMR.Position := udMR.Position;
end;

procedure TfrmMakeFrom3D.tbMRChange(Sender: TObject);
begin
  udMR.Position := tbMR.Position;
end;

procedure TfrmMakeFrom3D.edtMGChange(Sender: TObject);
begin
  If Not SettingModelHSV Then SettingModelRGB := True;
  tbMG.Position := udMG.Position;
  SetModelColorFromRGB;
end;

procedure TfrmMakeFrom3D.udMGClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMG.Position := udMG.Position;
end;

procedure TfrmMakeFrom3D.tbMGChange(Sender: TObject);
begin
  udMG.Position := tbMG.Position;
end;

procedure TfrmMakeFrom3D.edtMBChange(Sender: TObject);
begin
  If Not SettingModelHSV Then SettingModelRGB := True;
  tbMB.Position := udMB.Position;
  SetModelColorFromRGB;
end;

procedure TfrmMakeFrom3D.udMBClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMB.Position := udMB.Position;
end;

procedure TfrmMakeFrom3D.tbMBChange(Sender: TObject);
begin
  udMB.Position := tbMB.Position;
end;

// r,g,b values are from 0 to 1
// h = [0,360], s = [0,1], v = [0,1]
//		if s == 0, then h = -1 (undefined)
Procedure TfrmMakeFrom3D.RGBtoHSV(R,G,B: Single; Var H,S,V: Single);
Var _Min,_Max,Delta: Single;
Begin
  _Min  := Min(Min(R,G),B);
  _Max  := Max(Max(R,G),B);
  V     := _Max;
  Delta := _Max - _Min;
  If _Max <> 0 Then S := Delta / _Max
  Else
  Begin
    S := 0;
    H := -1;
    Exit;
  End;
  If Delta = 0 Then
  Begin
    H := 0;
    S := 0;
    V := R;
    Exit;
  End;
       If R = _Max Then H :=     (G - B) / Delta // Between yellow & magenta
  Else If G = _Max Then H := 2 + (B - R) / Delta // Between cyan & yellow
  Else H := 4 + (R - G) / Delta;                 // Between magenta & cyan
  H := H * 60;                                   // Degrees
  If H < 0 Then H := H + 360;
End; // TfrmMakeFrom3D.RGBtoHSV

Procedure TfrmMakeFrom3D.HSVtoRGB(H,S,V: Single; Var R,G,B: Single);
Var
  I       : Integer;
  F,P,Q,T : Single;

Begin
  If S = 0 Then
  Begin
    // achromatic (grey)
    R := V;
    G := V;
    B := V;
    Exit;
  End;

  H := H / 60;     // sector 0 to 5
  I := Trunc(H);
  F := H - I;      // factorial part of h
  P := V * (1 - S);
  Q := V * (1 - S * F);
  T := V * (1 - S * (1 - F));

  Case I Of
    0: Begin
         R := V;
         G := T;
         B := P;
       End;
    1: Begin
         R := Q;
         G := V;
         B := P;
       End;
    2: Begin
         R := P;
         G := V;
         B := T;
       End;
    3: Begin
         R := P;
         G := Q;
         B := V;
       End;
    4: Begin
         R := T;
         G := P;
         B := V;
       End;
  Else    // case 5:
    R := V;
    G := P;
    B := Q;
  End; // Case
End; // TfrmMakeFrom3D.HSVtoRGB

procedure TfrmMakeFrom3D.edtMHChange(Sender: TObject);
begin
  If Not SettingModelRGB Then SettingModelHSV := True;
  tbMH.Position := udMH.Position;
  SetModelColorFromHSV;
end;

procedure TfrmMakeFrom3D.udMHClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMH.Position := udMH.Position;
end;

procedure TfrmMakeFrom3D.tbMHChange(Sender: TObject);
begin
  udMH.Position := tbMH.Position;
end;

procedure TfrmMakeFrom3D.edtMSChange(Sender: TObject);
begin
  If Not SettingModelRGB Then SettingModelHSV := True;
  tbMS.Position := udMS.Position;
  SetModelColorFromHSV;
end;

procedure TfrmMakeFrom3D.udMSClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMS.Position := udMS.Position;
end;

procedure TfrmMakeFrom3D.tbMSChange(Sender: TObject);
begin
  udMS.Position := tbMS.Position;
end;

procedure TfrmMakeFrom3D.edtMVChange(Sender: TObject);
begin
  If Not SettingModelRGB Then SettingModelHSV := True;
  tbMV.Position := udMV.Position;
  SetModelColorFromHSV;
end;

procedure TfrmMakeFrom3D.udMVClick(Sender: TObject; Button: TUDBtnType);
begin
  tbMV.Position := udMV.Position;
end;

procedure TfrmMakeFrom3D.tbMVChange(Sender: TObject);
begin
  udMV.Position := tbMV.Position;
end;

procedure TfrmMakeFrom3D.edtLRChange(Sender: TObject);
begin
  If Not SettingLightHSV Then SettingLightRGB := True;
  tbLR.Position := udLR.Position;
  SetLightColorFromRGB;
end;

procedure TfrmMakeFrom3D.udLRClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLR.Position := udLR.Position;
end;

procedure TfrmMakeFrom3D.tbLRChange(Sender: TObject);
begin
  udLR.Position := tbLR.Position;
end;

procedure TfrmMakeFrom3D.edtLGChange(Sender: TObject);
begin
  If Not SettingLightHSV Then SettingLightRGB := True;
  tbLG.Position := udLG.Position;
  SetLightColorFromRGB;
end;

procedure TfrmMakeFrom3D.udLGClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLG.Position := udLG.Position;
end;

procedure TfrmMakeFrom3D.tbLGChange(Sender: TObject);
begin
  udLG.Position := tbLG.Position;
end;

procedure TfrmMakeFrom3D.edtLBChange(Sender: TObject);
begin
  If Not SettingLightHSV Then SettingLightRGB := True;
  tbLB.Position := udLB.Position;
  SetLightColorFromRGB;
end;

procedure TfrmMakeFrom3D.udLBClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLB.Position := udLB.Position;
end;

procedure TfrmMakeFrom3D.tbLBChange(Sender: TObject);
begin
  udLB.Position := tbLB.Position;
end;

procedure TfrmMakeFrom3D.edtLHChange(Sender: TObject);
begin
  If Not SettingLightRGB Then SettingLightHSV := True;
  tbLH.Position := udLH.Position;
  SetLightColorFromHSV;
end;

procedure TfrmMakeFrom3D.udLHClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLH.Position := udLH.Position;
end;

procedure TfrmMakeFrom3D.tbLHChange(Sender: TObject);
begin
  udLH.Position := tbLH.Position;
end;

procedure TfrmMakeFrom3D.edtLSChange(Sender: TObject);
begin
  If Not SettingLightRGB Then SettingLightHSV := True;
  tbLS.Position := udLS.Position;
  SetLightColorFromHSV;
end;

procedure TfrmMakeFrom3D.udLSClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLS.Position := udLS.Position;
end;

procedure TfrmMakeFrom3D.tbLSChange(Sender: TObject);
begin
  udLS.Position := tbLS.Position;
end;

procedure TfrmMakeFrom3D.edtLVChange(Sender: TObject);
begin
  If Not SettingLightRGB Then SettingLightHSV := True;
  tbLV.Position := udLV.Position;
  SetLightColorFromHSV;
end;

procedure TfrmMakeFrom3D.udLVClick(Sender: TObject; Button: TUDBtnType);
begin
  tbLV.Position := udLV.Position;
end;

procedure TfrmMakeFrom3D.tbLVChange(Sender: TObject);
begin
  udLV.Position := tbLV.Position;
end;

procedure TfrmMakeFrom3D.edtARChange(Sender: TObject);
begin
  If Not SettingAmbientHSV Then SettingAmbientRGB := True;
  tbAR.Position := udAR.Position;
  SetAmbientColorFromRGB;
end;

procedure TfrmMakeFrom3D.udARClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAR.Position := udAR.Position;
end;

procedure TfrmMakeFrom3D.tbARChange(Sender: TObject);
begin
  udAR.Position := tbAR.Position;
end;

procedure TfrmMakeFrom3D.edtAGChange(Sender: TObject);
begin
  If Not SettingAmbientHSV Then SettingAmbientRGB := True;
  tbAG.Position := udAG.Position;
  SetAmbientColorFromRGB;
end;

procedure TfrmMakeFrom3D.udAGClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAG.Position := udAG.Position;
end;

procedure TfrmMakeFrom3D.tbAGChange(Sender: TObject);
begin
  udAG.Position := tbAG.Position;
end;

procedure TfrmMakeFrom3D.edtABChange(Sender: TObject);
begin
  If Not SettingAmbientHSV Then SettingAmbientRGB := True;
  tbAB.Position := udAB.Position;
  SetAmbientColorFromRGB;
end;

procedure TfrmMakeFrom3D.udABClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAB.Position := udAB.Position;
end;

procedure TfrmMakeFrom3D.tbABChange(Sender: TObject);
begin
  udAB.Position := tbAB.Position;
end;

procedure TfrmMakeFrom3D.edtAHChange(Sender: TObject);
begin
  If Not SettingAmbientRGB Then SettingAmbientHSV := True;
  tbAH.Position := udAH.Position;
  SetAmbientColorFromHSV;
end;

procedure TfrmMakeFrom3D.udAHClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAH.Position := udAH.Position;
end;

procedure TfrmMakeFrom3D.tbAHChange(Sender: TObject);
begin
  udAH.Position := tbAH.Position;
end;

procedure TfrmMakeFrom3D.edtASChange(Sender: TObject);
begin
  If Not SettingAmbientRGB Then SettingAmbientHSV := True;
  tbAS.Position := udAS.Position;
  SetAmbientColorFromHSV;
end;

procedure TfrmMakeFrom3D.udASClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAS.Position := udAS.Position;
end;

procedure TfrmMakeFrom3D.tbASChange(Sender: TObject);
begin
  udAS.Position := tbAS.Position;
end;

procedure TfrmMakeFrom3D.edtAVChange(Sender: TObject);
begin
  If Not SettingAmbientRGB Then SettingAmbientHSV := True;
  tbAV.Position := udAV.Position;
  SetAmbientColorFromHSV;
end;

procedure TfrmMakeFrom3D.udAVClick(Sender: TObject; Button: TUDBtnType);
begin
  tbAV.Position := udAV.Position;
end;

procedure TfrmMakeFrom3D.tbAVChange(Sender: TObject);
begin
  udAV.Position := tbAV.Position;
end;

procedure TfrmMakeFrom3D.edtInnerRadiusChange(Sender: TObject);
begin
  tbInnerRadius.Position := udInnerRadius.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udInnerRadiusClick(Sender: TObject;
  Button: TUDBtnType);
begin
  tbInnerRadius.Position := udInnerRadius.Position;
end;

procedure TfrmMakeFrom3D.tbInnerRadiusChange(Sender: TObject);
begin
  udInnerRadius.Position := tbInnerRadius.Position;
end;

procedure TfrmMakeFrom3D.rbCylindricalClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtOuterRadiusChange(Sender: TObject);
begin
  tbOuterRadius.Position := udOuterRadius.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udOuterRadiusClick(Sender: TObject;
  Button: TUDBtnType);
begin
  tbOuterRadius.Position := udOuterRadius.Position;
end;

procedure TfrmMakeFrom3D.tbOuterRadiusChange(Sender: TObject);
begin
  udOuterRadius.Position := tbOuterRadius.Position;
end;

procedure TfrmMakeFrom3D.cbPolygonalClick(Sender: TObject);
begin
  cbTop.Enabled   := cbPolygonal.Checked;
  cbSides.Enabled := cbPolygonal.Checked;
  PaintModel;
end;

procedure TfrmMakeFrom3D.edtORChange(Sender: TObject);
begin
  If Not SettingOutlineHSV Then SettingOutlineRGB := True;
  tbOR.Position := udOR.Position;
  SetOutlineColorFromRGB;
end;

procedure TfrmMakeFrom3D.udORClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOR.Position := udOR.Position;
end;

procedure TfrmMakeFrom3D.tbORChange(Sender: TObject);
begin
  udOR.Position := tbOR.Position;
end;

procedure TfrmMakeFrom3D.edtOGChange(Sender: TObject);
begin
  If Not SettingOutlineHSV Then SettingOutlineRGB := True;
  tbOG.Position := udOG.Position;
  SetOutlineColorFromRGB;
end;

procedure TfrmMakeFrom3D.udOGClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOG.Position := udOG.Position;
end;

procedure TfrmMakeFrom3D.tbOGChange(Sender: TObject);
begin
  udOG.Position := tbOG.Position;
end;

procedure TfrmMakeFrom3D.edtOBChange(Sender: TObject);
begin
  If Not SettingOutlineHSV Then SettingOutlineRGB := True;
  tbOB.Position := udOB.Position;
  SetOutlineColorFromRGB;
end;

procedure TfrmMakeFrom3D.udOBClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOB.Position := udOB.Position;
end;

procedure TfrmMakeFrom3D.tbOBChange(Sender: TObject);
begin
  udOB.Position := tbOB.Position;
end;

procedure TfrmMakeFrom3D.edtOHChange(Sender: TObject);
begin
  If Not SettingOutlineRGB Then SettingOutlineHSV := True;
  tbOH.Position := udOH.Position;
  SetOutlineColorFromHSV;
end;

procedure TfrmMakeFrom3D.udOHClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOH.Position := udOH.Position;
end;

procedure TfrmMakeFrom3D.tbOHChange(Sender: TObject);
begin
  udOH.Position := tbOH.Position;
end;

procedure TfrmMakeFrom3D.edtOSChange(Sender: TObject);
begin
  If Not SettingOutlineRGB Then SettingOutlineHSV := True;
  tbOS.Position := udOS.Position;
  SetOutlineColorFromHSV;
end;

procedure TfrmMakeFrom3D.udOSClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOS.Position := udOS.Position;
end;

procedure TfrmMakeFrom3D.tbOSChange(Sender: TObject);
begin
  udOS.Position := tbOS.Position;
end;

procedure TfrmMakeFrom3D.edtOVChange(Sender: TObject);
begin
  If Not SettingOutlineRGB Then SettingOutlineHSV := True;
  tbOV.Position := udOV.Position;
  SetOutlineColorFromHSV;
end;

procedure TfrmMakeFrom3D.udOVClick(Sender: TObject; Button: TUDBtnType);
begin
  tbOV.Position := udOV.Position;
end;

procedure TfrmMakeFrom3D.tbOVChange(Sender: TObject);
begin
  udOV.Position := tbOV.Position;
end;

procedure TfrmMakeFrom3D.cbUseOutlineColorClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.rbSphericalClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.cbSmoothClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.FormCreate(Sender: TObject);
begin
  ModelColor        := clRed;
  LightColor        := clWhite;
  AmbientColor      := $00808080;
  SettingModelRGB   := True;
  SettingModelHSV   := False;
  SettingLightRGB   := True;
  SettingLightHSV   := False;
  SettingAmbientRGB := True;
  SettingAmbientHSV := False;
  udMR.Position     := TRGBA(ModelColor).R;
  udMG.Position     := TRGBA(ModelColor).G;
  udMB.Position     := TRGBA(ModelColor).B;
  udLR.Position     := TRGBA(LightColor).R;
  udLG.Position     := TRGBA(LightColor).G;
  udLB.Position     := TRGBA(LightColor).B;
  udAR.Position     := TRGBA(AmbientColor).R;
  udAG.Position     := TRGBA(AmbientColor).G;
  udAB.Position     := TRGBA(AmbientColor).B;
end;

procedure TfrmMakeFrom3D.cbRemoveHiddenSurfacesClick(Sender: TObject);
begin
  If Not cbRemoveHiddenSurfaces.Checked Then
   ShowMessage('Are you sure?'#13#10#13#10 +
               'This option removes surfaces that the wizard'#13#10 +
               'thinks will be totally obscured.');
end;

procedure TfrmMakeFrom3D.edtYTransChange(Sender: TObject);
begin
  tbYTrans.Position := udYTrans.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udYTransClick(Sender: TObject; Button: TUDBtnType);
begin
  tbYTrans.Position := udYTrans.Position;
end;

procedure TfrmMakeFrom3D.tbYTransChange(Sender: TObject);
begin
  udYTrans.Position := tbYTrans.Position;
end;

procedure TfrmMakeFrom3D.edtXTransChange(Sender: TObject);
begin
  tbXTrans.Position := udXTrans.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udXTransClick(Sender: TObject; Button: TUDBtnType);
begin
  tbXTrans.Position := udXTrans.Position;
end;

procedure TfrmMakeFrom3D.tbXTransChange(Sender: TObject);
begin
  udXTrans.Position := tbXTrans.Position;
end;

procedure TfrmMakeFrom3D.edtZTransChange(Sender: TObject);
begin
  tbZTrans.Position := udZTrans.Position;
  PaintModel;
end;

procedure TfrmMakeFrom3D.udZTransClick(Sender: TObject; Button: TUDBtnType);
begin
  tbZTrans.Position := udZTrans.Position;
end;

procedure TfrmMakeFrom3D.tbZTransChange(Sender: TObject);
begin
  udZTrans.Position := tbZTrans.Position;
end;

procedure TfrmMakeFrom3D.rbRectangularClick(Sender: TObject);
begin
  PaintModel;
end;

procedure TfrmMakeFrom3D.udThicknessClick(Sender: TObject; Button: TUDBtnType);
begin
  tbThickness.Position := udThickness.Position;
end;

procedure TfrmMakeFrom3D.tbThicknessChange(Sender: TObject);
begin
  udThickness.Position := tbThickness.Position;
end;

procedure TfrmMakeFrom3D.edtThicknessChange(Sender: TObject);
begin
  tbThickness.Position := udThickness.Position;
  PaintModel;
end;

end.
