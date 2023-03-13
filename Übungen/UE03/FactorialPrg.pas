(* FactorialPrg:                                             MFL, 2022-10-20 *)
(* ------                                                                    *)
(* Computes the faculty of a given number                                    *)
(* ========================================================================= *)
PROGRAM FactorialProgram;
  VAR
    fact, n, i: INTEGER;
BEGIN (* FactorialProgram *)
  Write('n > ');
  ReadLN(n);

  fact := 1;
  FOR i := 1 TO n DO BEGIN
    fact := fact * i;
  END; (* FOR *)
  
  WriteLN(n,'! = ', fact);
END. (* FactorialProgram *)