(* DiagonalSumsPrg:                                       Author, 2022-11-06 *)
(* ------                                                                    *)
(* calculates diagonals of a lower triangular matrix                         *)
(* ========================================================================= *)
PROGRAM DiagonalSumsPrg;
  CONST
    n = 4;
  TYPE
    Matrix = ARRAY [1..n, 1..n] OF INTEGER;
    DiagonalSums = ARRAY [1..n] OF INTEGER;
  CONST
    m : matrix = (
  (10,0,0,10),
  (10,10,0,10),
  (10,0,10,0),
  (10,10,10,10)
  );

  PROCEDURE CaluculateDiagonalSums(m: Matrix; Var d: DiagonalSums);
    VAR
      i, j, valuesInRow, sumIndex: INTEGER;
  BEGIN (* CaluculateDiagonalSums *)
    valuesInRow := 1; (* amount of values in the first row*)
    sumIndex := 1; 
    FOR j := 1 TO Length(m) DO BEGIN (* row *)
      FOR i := 1 TO valuesInRow DO BEGIN (* collumn *)
        d[sumIndex] := d[sumIndex] + m[j,i];
        Dec(sumIndex);
      END; (* FOR *)
      Inc(valuesInRow); (* amount of values in the next row *)
      sumIndex:= valuesInRow;
    END; (* FOR *)
  END; (* CaluculateDiagonalSums *)

VAR
  result: DiagonalSums;
  i: INTEGER;
BEGIN (* DiagonalSumsPrg *)

  CaluculateDiagonalSums(m, result);
  Write('Diagonalsummen: ');
  FOR i := 1 TO Length(result) DO BEGIN
    Write(result[i], ' ');
  END; (* FOR *)
  WriteLN();

END. (* DiagonalSumsPrg *)
