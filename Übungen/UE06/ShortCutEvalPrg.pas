(* ShortCutEvalPrg:                                          MFL, 2022-11-16 *)
(* ------                                                                    *)
(* single demonstration of shortcut evaluations of boolean expressions       *)
(* ========================================================================= *)
PROGRAM ShortCutEvalPrg;

  FUNCTION Contains(arr: ARRAY OF INTEGER; n: INTEGER; value: INTEGER): BOOLEAN;
    VAR
      i: INTEGER;
  BEGIN (* Contains *)
    i := Low(arr);
    WHILE ((i < n) AND (arr[i] <> value)) DO BEGIN
      i := i + 1;
    END; (* WHILE *)
    Contains := (i < n);
  END; (* Contains *)

  CONST
    a : ARRAY [1..5] OF INTEGER = (1, 2, 3, 4, 5);

BEGIN (* ShortCutEvalPrg *)
  WriteLn('Contains(a, 3, 10) = ', Contains(a, 3, 10));
  WriteLn('Contains(a, 3, 2)  = ', Contains(a, 3, 2));
  WriteLn('Contains(a, 3, 4)  = ', Contains(a, 3, 4));
  WriteLn('Contains(a, 5, 5)  = ', Contains(a, 5, 5));
  WriteLn('Contains(a, 5, 10)  = ', Contains(a, 5, 10));
END. (* ShortCutEvalPrg *)
