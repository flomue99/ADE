(* BinomialPrg:                                              MFL, 2022-10-20 *)
(* ------                                                                    *)
(* Computes the binomial coefficient of two numbers                                       *)
(* ========================================================================= *)
PROGRAM BinomialProgram;
  
  (* forward declaration of procedures and functions*)
  FUNCTION Binomial(n, k: INTEGER): LONGINT; FORWARD;
  PROCEDURE FactorialProc(n: INTEGER; VAR fact: REAL); FORWARD; 
  FUNCTION FactorialFunc(n: INTEGER): REAL; FORWARD;
  PROCEDURE TestBinomial(n, k, expectedResult: INTEGER); FORWARD;


  FUNCTION Binomial(n, k: INTEGER): LONGINT;
    VAR
      binom: REAL;
  BEGIN (* Binomial *)
    binom := FactorialFunc(n) / (FactorialFunc(k) * FactorialFunc(n-k));
    Binomial := Round(binom);
  END; (* Binomial *)


  FUNCTION FactorialFunc(n: INTEGER): REAL; //Funktionskoff, Schnittstelle
    VAR
      i: INTEGER;
      fact: INTEGER;
  BEGIN (* FactorialFunc *)
    fact := 1;
    FOR i := 1 TO n DO BEGIN
      fact := fact * i;
    END; (* FOR *)
    FactorialFunc := fact;  //Statt return
  END; (* FactorialFunc *)
 

  PROCEDURE FactorialProc(n: INTEGER; VAR fact: REAL); //Procedur, n: Eingagngsparameter (call by value) ,Var singalieseirt ausgangs/durchgangs parameter (call by reference)
    VAR
      i: INTEGER;
  BEGIN (* FactorialProc *)
     fact := 1;
    FOR i := 1 TO n DO BEGIN
      fact := fact * i;
    END; (* FOR *)
  END; (* FactorialProc *) 

   PROCEDURE ReadPostivInteger(VAR i: INTEGER);
   BEGIN (* ReadPostivInteger *)
     ReadLN(i);
     IF (i < 0) THEN BEGIN
      WriteLN('Value ', i, ' is not positive');
      HALT;
     END; (* IF *)
   END; (* ReadPostivInteger *)


  PROCEDURE TestBinomial(n, k, expectedResult: INTEGER);
    VAR
      result: INTEGER;
  BEGIN (* TestBinomial *)
    result := Binomial(n,k);
    IF (result = expectedResult) THEN BEGIN
      Write('PASS');
    END ELSE BEGIN
      Write('FAIL');
    END; (* IF *)
    WriteLN('  Binomial(',n, ', ', k, ') =', result);
  END; (* TestBinomial *) 

  VAR
    n, k: INTEGER;
    binom: LONGINT;
BEGIN (* BinomialProgram *)
  ReadPostivInteger(n);
  ReadPostivInteger(k);
  binom:= Binomial(n,k);
  WriteLN(binom);

END. (* BinomialProgram *)