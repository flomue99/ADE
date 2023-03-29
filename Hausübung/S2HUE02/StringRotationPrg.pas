(* StringRotationPrg:                                        MFL, 2023-03-20 *)
(* ------                                                                    *)
(* Program to check if to words are a cyclic rotation                        *)
(* ========================================================================= *)
PROGRAM StringRotationPrg;

(* modified Bruteforce 2, able to search pattern over corners *)
FUNCTION BruteForce2(s, p: STRING): INTEGER; 
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
BEGIN (* BruteForce2 *)
  sLen := Length(s);
  pLen := Length(p);
  i := 1; j := 1;
  WHILE (j <= pLen) AND ((i > 0) AND (i <= slen)) DO BEGIN
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
      IF (i > sLen) THEN BEGIN (* i = 1 and start to compare from front*)
        i := 1;
      END; (* IF *)
    END ELSE BEGIN
      (* missmatch*)
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  END; (* WHILE *)
  IF (j > plen) THEN BEGIN
    BruteForce2 := i + slen - j + 1;
  END ELSE BEGIN
    BruteForce2 := 0;
  END; (* IF *)
END; (* BruteForce2 *)

FUNCTION IsCyclicRotation(a, b: STRING): BOOLEAN;
VAR
  foundAt: INTEGER;  
BEGIN (* IsCyclicRotation *)
   (* strings not the same sice => cant be a rotation, one ore two strings empty *)
  IF ((Length(a) <> Length(b)) OR ((a ='') AND (b = ''))) THEN BEGIN
    EXIT(FALSE);
  END; (* IF *)
  foundAt := BruteForce2(a, b);
  IsCyclicRotation := foundAt <> 0;
END; (* IsCyclicRotation *)

BEGIN (* StringRotationPrg *)
  WriteLn('a: Hagenberg, b: agenbergH, IsRotation = ',IsCyclicRotation('Hagenberg', 'agenbergH'));
  WriteLn('a: Hagenberg, b: genbergHa, IsRotation = ',IsCyclicRotation('Hagenberg', 'genbergHa'));
  WriteLn('a: Hagenberg, b: enbergHag, IsRotation = ',IsCyclicRotation('Hagenberg', 'enbergHag'));
  WriteLn('a: Hagenberg, b: nbergHage, IsRotation = ',IsCyclicRotation('Hagenberg', 'nbergHage'));
  WriteLn('a: Hagenberg, b: bergHagen, IsRotation = ',IsCyclicRotation('Hagenberg', 'bergHagen'));
  WriteLn('a: Hagenberg, b: ergHagenb, IsRotation = ',IsCyclicRotation('Hagenberg', 'ergHagenb'));
  WriteLn('a: Hagenberg, b: rgHagenbe, IsRotation = ',IsCyclicRotation('Hagenberg', 'rgHagenbe'));
  WriteLn('a: Hagenberg, b: gHagenber, IsRotation = ',IsCyclicRotation('Hagenberg', 'gHagenber'));
  WriteLn('a: Hagenberg, b: gHagenber, IsRotation = ',IsCyclicRotation('Hagenberg', 'Hagenberg'));
  WriteLn('a: Jonas, b: jonasJ, IsRotation = ',IsCyclicRotation('Jonas', 'onasJ'));
  WriteLn('a: 011011, b: 110110, IsRotation = ',IsCyclicRotation('011011', '110110'));
  WriteLn('a: '', b: '', IsRotation = ',IsCyclicRotation('', ''));
  WriteLn('a: Florian, b: Jonas, IsRotation = ',IsCyclicRotation('Florian', 'Jonas'));
  WriteLn('a: Florian, b: Manfred, IsRotation = ',IsCyclicRotation('Florian', 'Manfred'));
  WriteLn('a: Hagenberg, b: Berg, IsRotation = ',IsCyclicRotation('Hagenberg', 'Berg'));
  WriteLn('a: Test, b: Fast, IsRotation = ',IsCyclicRotation('Test', 'Fast'));
END. (* StringRotationPrg *)