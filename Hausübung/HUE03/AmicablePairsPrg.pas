(* AmicablePairsPrg:                                         MFL, 2022-10-21 *)
(* ------                                                                    *)
(* Checks if two numbers are an amicable pair                                *)
(* ========================================================================= *)
PROGRAM AmicablePairsPrg;

  FUNCTION DivisorsSum(number: INTEGER): INTEGER;
    VAR
      i: INTEGER;
      sum: INTEGER;
  BEGIN (* DivisorsSum *)
    i := 1;
    sum := 0;

    WHILE (i < number) DO BEGIN
      IF ((number MOD i) = 0) THEN BEGIN (* remainder = 0, i is a divisor of number *)
        sum := sum + i;
      END; (* IF *)
      i := i + 1;
    END; (* WHILE *)
    DivisorsSum := sum;

  END; (* DivisorsSum *)

  VAR
    a, b: INTEGER;
    aDivisorsSum, bDivisorsSum: INTEGER;
BEGIN (* AmicablePairsPrg *)
  WriteLN('AmicablePairsPrg');
  Write('a > ');
  ReadLN(a);
  Write('b > ');
  ReadLN(b);

  aDivisorsSum := DivisorsSum(a);
  bDivisorsSum := DivisorsSum(b);

  (* check if the numbers are a amicable pair *)
  IF ((aDivisorsSum = b) AND (bDivisorsSum = a)) THEN BEGIN
    WriteLN(a, ' und ',b , ' sind ein Paar befreundeter Zahlen.');
  END ELSE BEGIN
    WriteLN(a, ' und ',b , ' sind kein Paar befreundeter Zahlen.');
  END; (* IF *)

END. (* AmicablePairsPrg *)