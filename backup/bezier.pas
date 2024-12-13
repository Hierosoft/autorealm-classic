Unit Bezier;
{
  Portions of this file adapted from:

  Solving the Nearest Point-on-Curve Problem
  and
  A Bezier Curve-Based Root-Finder
  by Philip J. Schneider
  from "Graphics Gems", Academic Press, 1990
  http://www.graphicsgems.org
}

Interface

Type
  PDouble = ^Double;
  CPoint = Record
    X : Double;
    Y : Double;
  End;

Function  FindNearestPoint(P: CPoint; V: Array Of CPoint; Var Root: Double): CPoint;
Function  FindAngleAt(V: Array Of CPoint; Root: Double): Double;
Function  SolveCubic(A,B,C,D: Double; Var V: Array Of Double): Integer;
Procedure SplitAndGetHandles(V: Array Of Double; T: Double; Var H1,X1,X,X2,H2: Double);
Function  SliceAlongLine(X,Y: Array Of Double; S1X,S1Y,S2X,S2Y: Double; Var XN,YN: Array Of Double): Integer;

Implementation

Uses Math;

Function GetDouble(P: PDouble; Index: Integer): Double;
Begin
  Result := PDouble(LongInt(P) + Index * SizeOf(Double))^;
End; // GetDouble

Procedure SetDouble(P: PDouble; Index: Integer; D: Double);
Begin
  PDouble(LongInt(P) + Index * SizeOf(Double))^ := D;
End; // SetDouble

Procedure ConvertTo5ThDegree(P: CPoint; V: Array Of CPoint; Var W: Array Of CPoint);
Const Z: Array[0..2,0..3] Of Double = ((1.0, 0.6, 0.3, 0.1), (0.4, 0.6, 0.6, 0.4), (0.1, 0.3, 0.6, 1.0));
Var
  I,J,K,M,N : Integer;
  UB,LB     : Integer;
  C         : Array[0..3] Of CPoint;
  D         : Array[0..2] Of CPoint;
  CDTable   : Array[0..2,0..3] Of Double;
  Row       : Integer;
  Column    : Integer;

Begin
  For I := 0 To 3 Do
  Begin
    C[I].X := V[I].X - P.X;
    C[I].Y := V[I].Y - P.Y;
  End; // For I
  For I := 0 To 2 Do
  Begin
    D[I].X := (V[I + 1].X - V[I].X) * 3;
    D[I].Y := (V[I + 1].Y - V[I].Y) * 3;
  End; // For I
  For Row := 0 To 2 Do
  Begin
    For Column := 0 To 3 Do CDTable[Row,Column] := D[Row].X * C[Column].X + D[Row].Y * C[Column].Y;
  End; // For Row
  For I := 0 To 5 Do
  Begin
    W[I].Y := 0;
    W[I].X := I / 5;
  End; // For I
  N := 3;
  M := 2;
  For K := 0 To N + M Do
  Begin
    LB := Max(0,K - M);
    UB := Min(K,N);
    For I := LB To UB Do
    Begin
      J := K - I;
      W[I + J].Y := W[I + J].Y + CDTable[J,I] * Z[J,I];
    End; // For I
  End; // For K
End; // ConvertTo5thDegree

Function DoBezier(V: Array Of CPoint; Degree: Integer; T: Double; Var Left,Right: Array Of CPoint): CPoint;
Var
  I,J   : Integer;
  VTemp : Array[0..5,0..5] Of CPoint;

Begin
  For J := 0 To Degree Do VTemp[0,J] := V[J];
  For I := 1 To Degree Do
  Begin
    For J := 0 To Degree - I Do
    Begin
      VTemp[I,J].X := (1 - T) * VTemp[I - 1,J].X + T * VTemp[I - 1,J + 1].X;
      VTemp[I,J].Y := (1 - T) * VTemp[I - 1,J].Y + T * VTemp[I - 1,J + 1].Y;
    End; // For J
  End; // For I
  For J := 0 To Degree Do Left[J]  := VTemp[J,0];
  For J := 0 To Degree Do Right[J] := VTemp[Degree - J,J];
  Result := VTemp[Degree,0];
End; // DoBezier

Function CrossingCount(V: Array Of CPoint; Degree: Integer): Integer;
Var
  I       : Integer;
  Num     : Integer;
  Sign    : Integer;
  OldSign : Integer;

Begin
  Num     := 0;
  Sign    := Math.Sign(V[0].Y);
  OldSign := Sign;
  For I := 0 To Degree Do
  Begin
    Sign := Math.Sign(V[I].Y);
    If Sign <> OldSign Then Inc(Num);
    OldSign := Sign;
  End; // For I
  Result := Num;
End; // CrossingCount

Function ControlPolygonFlatEnough(V: Array Of CPoint; Degree: Integer): Boolean;
Var
  I                : Integer;
  Distance         : Array Of Double;
  MaxDistanceAbove : Double;
  MaxDistanceBelow : Double;
  Error            : Double;
  Intercept1       : Double;
  Intercept2       : Double;
  LeftIntercept    : Double;
  RightIntercept   : Double;
  A,B,C            : Double;
  ABSquared        : Double;
  Det              : Double;
  DInv             : Double;
  A1,B1,C1         : Double;
  A2,B2,C2         : Double;

Begin
  SetLength(Distance,Degree + 1);
  A         := V[0].Y - V[Degree].Y;
  B         := V[Degree].X - V[0].X;
  C         := V[0].X * V[Degree].Y - V[Degree].X * V[0].Y;
  ABSquared := Sqr(A) + Sqr(B);

  For I := 1 To Degree - 1 Do
  Begin
    Distance[I] := A * V[I].X + B * V[I].Y + C;
    If Distance[I] > 0 Then Distance[I] := Sqr(Distance[I]) / ABSquared;
    If Distance[I] < 0 Then Distance[I] := -(Sqr(Distance[I]) / ABSquared);
  End; // For I

  MaxDistanceAbove := 0;
  MaxDistanceBelow := 0;

  For I := 1 To Degree - 1 Do
  Begin
    If Distance[I] < 0 Then MaxDistanceBelow := Min(MaxDistanceBelow,Distance[I]);
    If Distance[I] > 0 Then MaxDistanceAbove := Max(MaxDistanceAbove,Distance[I]);
  End; // For I

  SetLength(Distance,0);

  A1 := 0;
  B1 := 1;
  C1 := 0;

  A2         := A;
  B2         := B;
  C2         := C + MaxDistanceAbove;
  Det        := A1 * B2 - A2 * B1;
  DInv       := 1 / Det;
  Intercept1 := (B1 * C2 - B2 * C1) * DInv;

  A2         := A;
  B2         := B;
  C2         := C + MaxDistanceBelow;
  Det        := A1 * B2 - A2 * B1;
  DInv       := 1 / Det;
  Intercept2 := (B1 * C2 - B2 * C1) * DInv;

  LeftIntercept  := Min(Intercept1,Intercept2);
  RightIntercept := Max(Intercept1,Intercept2);

  Error  := (RightIntercept - LeftIntercept) / 2;
  Result := (Error < Power(2,-65));
End; // ControlPolygonFlatEnough

Function ComputeXIntercept(V: Array Of CPoint; Degree: Integer): Double;
Var
  XLK,YLK : Double;
  XNM,YNM : Double;
  XMK,YMK : Double;
  Det     : Double;
  DetInv  : Double;
  S       : Double;
//  T       : Double;
  X       : Double;
//  Y       : Double;

Begin
  XLK    := 1;
  YLK    := 0;
  XNM    := V[Degree].X - V[0].X;
  YNM    := V[Degree].Y - V[0].Y;
  XMK    := V[0].X;
  YMK    := V[0].Y;
  Det    := XNM * YLK - YNM * XLK;
  DetInv := 1 / Det;
  S      := (XNM * YMK - YNM * XMK) * DetInv;
  X      := XLK * S;
  Result := X;
End; // ComputeXIntercept

Function FindRoots(W: Array Of CPoint; Degree: Integer; Var T: PDouble; Depth: Integer): Integer;
Var
  Left       : Array Of CPoint;
  Right      : Array Of CPoint;
  LeftCount  : Integer;
  RightCount : Integer;
  LeftT      : PDouble;
  RightT     : PDouble;

Begin
  Case CrossingCount(W,Degree) Of
    0: Begin
         T      := Nil;
         Result := 0;
         Exit;
       End;
    1: Begin
         If Depth >= 64 Then
         Begin
           GetMem(T,SizeOf(Double));
           SetDouble(T,0,(W[0].X + W[5].X) / 2);
           Result := 1;
           Exit;
         End;
         If ControlPolygonFlatEnough(W,Degree) Then
         Begin
           GetMem(T,SizeOf(Double));
           SetDouble(T,0,ComputeXIntercept(W,Degree));
           Result := 1;
           Exit;
         End;
    End;
  End; // Case

  SetLength(Left,Degree + 1);
  SetLength(Right,Degree + 1);
  DoBezier(W, Degree, 0.5, Left, Right);
  LeftT      := Nil;
  RightT     := Nil;
  LeftCount  := FindRoots(Left,  Degree, LeftT,  Depth + 1);
  RightCount := FindRoots(Right, Degree, RightT, Depth + 1);
  If LeftCount + RightCount > 0 Then
  Begin
    GetMem(T,(LeftCount + RightCount) * SizeOf(Double));
    If LeftCount > 0 Then
    Begin
      Move(LeftT^,T^,LeftCount * SizeOf(Double));
      FreeMem(LeftT, LeftCount * SizeOf(Double));
    End;
    If RightCount > 0 Then
    Begin
      Move(RightT^,PDouble(LongInt(T) + LeftCount * SizeOf(Double))^,RightCount * SizeOf(Double));
      FreeMem(RightT,RightCount * SizeOf(Double));
    End;
  End;
  Result := LeftCount + RightCount;
  SetLength(Left,0);
  SetLength(Right,0);
End; // FindRoots

Function FindNearestPoint(P: CPoint; V: Array Of CPoint; Var Root: Double): CPoint;
Var
  Poss    : Array Of CPoint;
  Roots   : PDouble;
  Found   : Integer;
  T       : Double;
  Dist    : Double;
  NewDist : Double;
  Pt      : CPoint;
  Vec     : CPoint;
  I       : Integer;
  Left    : Array Of CPoint;
  Right   : Array Of CPoint;

Begin
  SetLength(Poss,6);
  SetLength(Left,4);
  SetLength(Right,4);
  ConvertTo5thDegree(P,V,Poss);
  Found := FindRoots(Poss, 5, Roots, 0);
  SetLength(Poss,0);
  Vec.X := P.X - V[0].X;
  Vec.Y := P.Y - V[0].Y;
  Dist  := Sqr(Vec.X) + Sqr(Vec.Y);
  T     := 0;
  For I := 0 To Found - 1 Do
  Begin
    Pt := DoBezier(V, 3, GetDouble(Roots,I), Left, Right);
    Vec.X   := P.X - Pt.X;
    Vec.Y   := P.Y - Pt.Y;
    NewDist := Sqr(Vec.X) + Sqr(Vec.Y);
    If NewDist < Dist Then
    Begin
      Dist := NewDist;
      T    := GetDouble(Roots,I);
    End;
  End; // For I
  Vec.X   := P.X - V[3].X;
  Vec.Y   := P.Y - V[3].Y;
  NewDist := Sqr(Vec.X) + Sqr(Vec.Y);
  If NewDist < Dist Then T := 1;
  Result := DoBezier(V, 3, T, Left, Right);
  SetLength(Left, 0);
  SetLength(Right, 0);
  If Found > 0 Then FreeMem(Roots,SizeOf(Double) * Found);
  Root := T;
End; // FindNearestPoint

Function SolveLinear(A,B: Double; Var V: Array Of Double): Integer;
Begin
  If A <> 0 Then
  Begin
    V[0]   := -B/A;
    Result := 1;
  End
  Else Result := 0;
End; // SolveLinear

Function SolveQuadratic(A,B,C: Double; Var V: Array Of Double): Integer;
Var
  S : Double;
Begin
  If A = 0 Then Result := SolveLinear(B,C,V)
  Else
  Begin
    S := Sqr(B) - 4 * A * C;
    If S > 0 Then            // Two real roots
    Begin
      V[0]   := (-B + Sqrt(S)) / (2 * A);
      V[1]   := (-B - Sqrt(S)) / (2 * A);
      Result := 2;
    End
    Else If S = 0 Then       // One real root
    Begin
      V[0]   := -B / (2 * A);
      Result := 1;
    End
    Else Result := 0;        // Two imaginary roots
  End;
End; // SolveQuadratic

// Finds the real roots for the equation ax^3 + bx^2 + cx + d = 0
// V must be already able to contain up to three elements
Function SolveCubic(A,B,C,D: Double; Var V: Array Of Double): Integer;
Var
  XN  : Double;
  YN  : Double;
  D2  : Double;
  H   : Double;
  H2  : Double;
  YN2 : Double;
  R   : Double;
  Del : Double;
  T   : Double;

  Function CubeRoot(R: Extended): Extended;
  Begin
    If R < 0 Then Result := -Power(-R,1/3) Else Result := Power(R,1/3);
  End; // CubeRoot

Begin
  If A = 0 Then Result := SolveQuadratic(B,C,D,V)
  Else
  Begin
    XN := -B / (3 * A);
    YN := A * XN * Sqr(XN) + B * Sqr(XN) + C * XN + D;
    D2 := (Sqr(B) - 3 * A * C) / (9 * Sqr(A));
    H2 := 4 * Sqr(A) * Sqr(D2) * D2;

    YN2 := Sqr(YN);
    If YN2 > H2 Then               // One real root
    Begin
      R := YN2 - H2;
      V[0] := XN + CubeRoot((-YN + Sqrt(R)) / (2 * A)) + CubeRoot((-YN - Sqrt(R)) / (2 * A));
      Result := 1;
    End
    Else If YN2 = H2 Then          // Three real roots (two or three equal real roots)
    Begin
      If H2 <> 0 Then              // Two equal roots
      Begin
        Del    := CubeRoot(YN / (2 * A));
        V[0]   := XN + Del;
        V[1]   := XN - 2 * Del;
        Result := 2;
      End
      Else
      Begin                        // Three equal roots
        V[0]   := XN;
        Result := 1;
      End;
    End
    Else
    Begin                          // Three distinct real roots
      H      := Sqrt(H2);
      Del    := CubeRoot(H / (2 * A));
      T      := ArcCos(-YN / H) / 3;
      V[0]   := XN + 2 * Del * Cos(T);
      V[1]   := XN + 2 * Del * Cos(T + 2 * Pi / 3);
      V[2]   := XN + 2 * Del * Cos(T + 4 * Pi / 3);
      Result := 3;
    End;
  End;
End; // SolveCubic

// T has to range from 0..1
Procedure SplitAndGetHandles(V: Array Of Double; T: Double; Var H1,X1,X,X2,H2: Double);
Var P1,P2,P3: Double;
Begin
  P1 := V[0] + (V[1] - V[0]) * T;
  P2 := V[1] + (V[2] - V[1]) * T;
  P3 := V[2] + (V[3] - V[2]) * T;
  X1 := P1 + (P2 - P1) * T;
  X2 := P2 + (P3 - P2) * T;
  X  := X1 + (X2 - X1) * T;
  H1 := V[0] + (V[1] - V[0]) * T;
  H2 := V[3] - (V[3] - V[2]) * (1 - T);
End; // SplitAndGetHandles

// X and Y must have 4 elements each
// XN and YN must have room for 13 elements
Function SliceAlongLine(X,Y: Array Of Double; S1X,S1Y,S2X,S2Y: Double; Var XN,YN: Array Of Double): Integer;
Var
  V           : Array Of Double;
  I,J,K       : Integer;
  A0,A1,A2,A3 : Double;
  B0,B1,B2,B3 : Double;
  C0,C1,C2,C3 : Double;
  D,M         : Double;
  XP,YP       : Double;

begin
  Result := 4;
  XN[0]  := X[0];
  XN[1]  := X[1];
  XN[2]  := X[2];
  XN[3]  := X[3];
  YN[0]  := Y[0];
  YN[1]  := Y[1];
  YN[2]  := Y[2];
  YN[3]  := Y[3];

  // Calculate x Bezier coefficients

  A3 := (X[3] + 3 * (X[1] - X[2]) - X[0]) / 8;
  A2 := (3 / 8) * (X[3] - X[2] - X[1] + X[0]);
  A1 := ((X[3] - X[0]) / 2) - A3;
  A0 := ((X[3] + X[0]) / 2) - A2;

  // Calculate y Bezier coefficients

  B3 := (Y[3] + 3 * (Y[1] - Y[2]) - Y[0]) / 8;
  B2 := (3 / 8) * (Y[3] - Y[2] - Y[1] + Y[0]);
  B1 := ((Y[3] - Y[0]) / 2) - B3;
  B0 := ((Y[3] + Y[0]) / 2) - B2;

  // Calculate coefficients for the cubic equation and get the real roots of the equation

  SetLength(V,3);
  If S1X = S2X Then                    // Horizontal lines are a special case
  Begin
    C3 := A3;
    C2 := A2;
    C1 := A1;
    C0 := A0;
  End
  Else
  Begin
    M  := (S2Y - S1Y) / (S2X - S1X);   // Slope of scalpel line
    D  := S1Y - M * S1X;               // Intercept of scalpel line
    C3 := M * A3 - B3;                 // Do it this way to avoid divides by zero
    C2 := M * A2 - B2;
    C1 := M * A1 - B1;
    C0 := M * A0 - B0 + D;
  End;
  I := SolveCubic(C3,C2,C1,C0,V);

  // Remove roots outside the range [-1,1] (the solver routine already culls out duplicates)

  J := 0;
  While J < I Do
  Begin
    XP := A3 * V[J] * Sqr(V[J]) + A2 * Sqr(V[J]) + A1 * V[J] + A0;
    YP := B3 * V[J] * Sqr(V[J]) + B2 * Sqr(V[J]) + B1 * V[J] + B0;
    If (V[J] < -1) Or (V[J] > 1) Or
       ((S1X <= S2X) And ((XP < S1X) Or (XP > S2X))) Or
       ((S1X >  S2X) And ((XP > S1X) Or (XP < S2X))) Or
       ((S1Y <= S2Y) And ((YP < S1Y) Or (YP > S2Y))) Or
       ((S1Y >  S2Y) And ((YP > S1Y) Or (YP < S2Y))) Then
    Begin
      For K := J To I - 2 Do V[K] := V[K + 1];
      Dec(I);
    End
    Else Inc(J);
  End; // While
  If I > 0 Then
  Begin

    // Sort the roots in ascending order

    For J := 0 To I - 2 Do
    Begin
      For K := J + 1 To I - 1 Do
      Begin
        If V[J] > V[K] Then
        Begin
          D    := V[J];
          V[J] := V[K];
          V[K] := D;
        End;
      End; // For K
    End; // For J

    // Rescale the roots

    For J := 0 To I - 1 Do V[J] := (V[J] / 2) + 0.5;

    // Construct a polycurve from the resulting points

    K := (I + 1) * 3 + 1;
    XN[0] := X[0];
    YN[0] := Y[0];
    XN[1] := X[1];
    YN[1] := Y[1];

    XN[K - 2] := X[2];
    YN[K - 2] := Y[2];
    XN[K - 1] := X[3];
    YN[K - 1] := Y[3];

    If I = 3 Then
    Begin
      If V[1] < 1 Then V[2] := (V[2] - V[1]) / (1 - V[1]) Else V[2] := 0;
      If V[1] > 0 Then V[0] := V[0] / V[1] Else V[0] := 0;

      SplitAndGetHandles([X[0],X[1],X[2],X[3]],       V[1], XN[1],XN[5],XN[6],XN[7],XN[11]);
      SplitAndGetHandles([Y[0],Y[1],Y[2],Y[3]],       V[1], YN[1],YN[5],YN[6],YN[7],YN[11]);
      SplitAndGetHandles([XN[6],XN[7],XN[11],XN[12]], V[2], XN[7],XN[8],XN[9],XN[10],XN[11]);
      SplitAndGetHandles([YN[6],YN[7],YN[11],YN[12]], V[2], YN[7],YN[8],YN[9],YN[10],YN[11]);
      SplitAndGetHandles([XN[0],XN[1],XN[5],XN[6]],   V[0], XN[1],XN[2],XN[3],XN[4],XN[5]);
      SplitAndGetHandles([YN[0],YN[1],YN[5],YN[6]],   V[0], YN[1],YN[2],YN[3],YN[4],YN[5]);
    End
    Else If I = 2 Then
    Begin
      If V[1] > 0 Then V[0] := V[0] / V[1] Else V[0] := 0;
      SplitAndGetHandles([X[0],X[1],X[2],X[3]],     V[1], XN[1],XN[5],XN[6],XN[7],XN[8]);
      SplitAndGetHandles([Y[0],Y[1],Y[2],Y[3]],     V[1], YN[1],YN[5],YN[6],YN[7],YN[8]);
      SplitAndGetHandles([XN[0],XN[1],XN[5],XN[6]], V[0], XN[1],XN[2],XN[3],XN[4],XN[5]);
      SplitAndGetHandles([YN[0],YN[1],YN[5],YN[6]], V[0], YN[1],YN[2],YN[3],YN[4],YN[5]);
    End
    Else
    Begin
      SplitAndGetHandles([X[0],X[1],X[2],X[3]],          V[0], XN[1],XN[2],XN[3],XN[4],XN[5]);
      SplitAndGetHandles([Y[0],Y[1],Y[2],Y[3]],          V[0], YN[1],YN[2],YN[3],YN[4],YN[5]);
    End;

    Result := K;
  End;
  SetLength(V,0);
End; // SliceAlongLine

Function FindAngleAt(V: Array Of CPoint; Root: Double): Double;
Var
  A1,A2,A3 : Double;
  B1,B2,B3 : Double;
  XP,YP    : Double; // dy/du and dx/du

Begin
  // Calculate x Bezier coefficients

  A3 := (V[3].X + 3 * (V[1].X - V[2].X) - V[0].X) / 8;
  A2 := (3 / 8) * (V[3].X - V[2].X - V[1].X + V[0].X);
  A1 := ((V[3].X - V[0].X) / 2) - A3;

  // Calculate y Bezier coefficients

  B3 := (V[3].Y + 3 * (V[1].Y - V[2].Y) - V[0].Y) / 8;
  B2 := (3 / 8) * (V[3].Y - V[2].Y - V[1].Y + V[0].Y);
  B1 := ((V[3].Y - V[0].Y) / 2) - B3;

  XP := 3 * A3 * Sqr(Root) + 2 * A2 * Root + A1;
  YP := 3 * B3 * Sqr(Root) + 2 * B2 * Root + B1;
  Result := ArcTan2(YP,XP);
End; // FindAngleAt

End.
