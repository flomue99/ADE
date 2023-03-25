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
  WHILE (j <= pLen) DO BEGIN
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
      IF (i > sLen) THEN BEGIN (* set i to 1 so we cant acces s outside of slen *)
        i := 1;
      END; (* IF *)
    END ELSE BEGIN
      (* missmatch*)
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  END; (* WHILE *)
  IF (j > plen) THEN BEGIN
    BruteForce2 := i + slen - j + 1; (* plus slen to get the right startpoint *)
  END ELSE BEGIN
    BruteForce2 := 0;
  END; (* IF *)
END; (* BruteForce2 *)

FUNCTION IsCyclicRotation(a, b: STRING): BOOLEAN;
VAR
  foundAt: INTEGER;  
BEGIN (* IsCyclicRotation *)
  IF (Length(a) <> Length(b)) THEN BEGIN (* strings not the same sice => cant be a rotation *)
    EXIT(FALSE);
  END; (* IF *)
  foundAt := BruteForce2(a, b);
  IsCyclicRotation := foundAt <> 0;
END; (* IsCyclicRotation *)

BEGIN (* StringRotationPrg *)
  WriteLn(IsCyclicRotation('Hagenberg', 'Hagenberg'));
  WriteLn(IsCyclicRotation('Hagenberg', 'agenbergH'));
  WriteLn(IsCyclicRotation('Hagenberg', 'genbergHa'));
  WriteLn(IsCyclicRotation('Hagenberg', 'enbergHag'));
  WriteLn(IsCyclicRotation('Hagenberg', 'nbergHage'));
  WriteLn(IsCyclicRotation('Hagenberg', 'bergHagen'));
  WriteLn(IsCyclicRotation('Hagenberg', 'ergHagenb'));
  WriteLn(IsCyclicRotation('Hagenberg', 'rgHagenbe'));
  WriteLn(IsCyclicRotation('Hagenberg', 'gHagenber'));
  WriteLn(IsCyclicRotation('Hagenberg', 'Hagenberg'));
  WriteLn(IsCyclicRotation('agenbergH', 'Hagenberg'));
  WriteLn(IsCyclicRotation('genbergHa', 'Hagenberg'));
  WriteLn(IsCyclicRotation('enbergHag', 'Hagenberg'));
  WriteLn(IsCyclicRotation('nbergHage', 'Hagenberg'));
  WriteLn(IsCyclicRotation('bergHagen', 'Hagenberg'));
  WriteLn(IsCyclicRotation('ergHagenb', 'Hagenberg'));
  WriteLn(IsCyclicRotation('rgHagenbe', 'Hagenberg'));
  WriteLn(IsCyclicRotation('gHagenber', 'Hagenberg'));
END. (* StringRotationPrg *)
