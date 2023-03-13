(* NetwonSquareRootPrg:                                     MFL2, 2022-11-09 *)
(* ------                                                                    *)
(* Calculates SquareRoot of value x                                          *)
(* ========================================================================= *)
PROGRAM NewtonSquareRootPrg;
  
  FUNCTION NewtonSquareRoot(x: REAL; e: REAL): REAL;
    VAR
      currentY, nextY: REAL;
  BEGIN (* NewtonSquareRoot *)
    currentY := 1;
    nextY := 1;

    REPEAT
      currentY := nextY;
      nextY := (currentY + x / currentY) / 2;
    UNTIL(Abs(currentY - nextY) < e); (* REPEAT *)

    NewtonSquareRoot := nextY;
  END; (* NewtonSquareRoot *)

BEGIN (* NewtonSquareRootPrg *)
  WriteLN('NewtonSquareRoot(2, 0.0001) =', NewtonSquareRoot(2,0.0000001));
  WriteLN('Sqrt(2) =                    ',Sqrt(2));
END. (* NewtonSquareRootPrg *)
