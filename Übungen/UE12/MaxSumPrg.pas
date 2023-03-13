(* Title:                                                 Author, 2023-01-11 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM MaxSumPrg;
  
  FUNCTION M(a, b: INTEGER): INTEGER;
  BEGIN (* Max *)
    IF (a > b) THEN BEGIN
      M := a;
    END ELSE BEGIN
      M:= b;
    END; (* IF *)
  END; (* Max *)

  FUNCTION MaxSum1(a: ARRAY OF INTEGER): INTEGER;
    VAR
      max, sum: INTEGER;
      lower, upper, i : INTEGER;
  BEGIN (* MaxSum1 *)
    max := 0;
    FOR lower := 0 TO High(a) DO BEGIN
      FOR upper := lower TO High(a) DO BEGIN
        sum := 0;
        FOR i := lower TO upper DO BEGIN
          sum := sum + a[i];
        END; (* FOR *)
        max := M(max,sum);
      END; (* FOR *)
    END; (* FOR *)
    MaxSum1 := max;
  END; (* MaxSum1 *)

   FUNCTION MaxSum2(a: ARRAY OF INTEGER): INTEGER;
    VAR
      max, sum: INTEGER;
      lower, upper : INTEGER;
  BEGIN (* MaxSum1 *)
    max := 0;
    FOR lower := 0 TO High(a) DO BEGIN
      sum := 0;
      FOR upper := lower TO High(a) DO BEGIN
        sum := sum + a[upper];
        max := M(max,sum);
      END; (* FOR *)
    END; (* FOR *)
    MaxSum2 := max;
  END; (* MaxSum1 *)

BEGIN (* MaxSumPrg *)
  
END. (* MaxSumPrg *)
