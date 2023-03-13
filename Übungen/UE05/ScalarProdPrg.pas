(* ScalarProductPrg:                                        MFL2, 2022-11-09 *)
(* ------                                                                    *)
(* Calculates DotProduct of two scalars                                      *)
(* ========================================================================= *)
PROGRAM ScalarProductPrg;
  CONST
    max = 10;
  TYPE
    Vector = ARRAY [1..max] OF INTEGER;

  FUNCTION CalculateScalarProduct(v1, v2 : ARRAY OF INTEGER; n: INTEGER): INTEGER; (* wird umgemapt auf array mit index start 0*)
  Var
    i: INTEGER;
    result : INTEGER;
  BEGIN (* CalculateScalarProduct *)
    IF (Length(v1) <> Length(v2)) THEN BEGIN
      WriteLN('Error: Vektoren ungleicher länge');
      Halt;
    END; (* IF *)
    IF ((n > Length(v1)) OR ((n < 0))) THEN BEGIN
      WriteLN('Error: n größer der Vektorenlänge');
      Halt;
    END; (* IF *)

    result := 0;
    i := 0;
    WHILE (i <= (n - 1)) DO BEGIN
      result := (v1[i] * v2[i]) + result;
      i := i + 1;
    END; (* WHILE *)
    CalculateScalarProduct := result;
  END; (* CalculateScalarProduct *)


CONST
  v1 : ARRAY[1..5] OF INTEGER = (1,2,3,4,5);
  v2 : ARRAY[5..9] OF INTEGER = (5,4,3,2,1);
BEGIN (* ScalarProductPrg *)
  WriteLN(CalculateScalarProduct(v1,v2,5));
END. (* ScalarProductPrg *)
