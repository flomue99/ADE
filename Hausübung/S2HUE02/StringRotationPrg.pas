(* Title:                                                 Author, 2023-03-20 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM StringRotationPrg;

FUNCTION BruteForce2(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
BEGIN (* BruteForce2 *)
  sLen := Length(s);
  pLen := Length(p);
  i := 1; j := 1;
  WHILE (i + plen - j <= sLen) AND (j <= pLen ) DO BEGIN
  Write(p[i]);
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    //END ELSE IF (i = sLen) THEN BEGIN
    //  i := 1;
    END ELSE BEGIN
      (* missmatch*)
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  END; (* WHILE *)
  IF (j > plen) THEN BEGIN
    BruteForce2 := i - j + 1;
  END ELSE BEGIN
    BruteForce2 := 0;
  END; (* IF *)
END; (* BruteForce2 *)

FUNCTION IsCyclicRotation(a, b: STRING): BOOLEAN;
  VAR
    i, j, k, actualVal: INTEGER;
    s: STRING;    
BEGIN (* IsCyclicRotation *)
  IF (Length(a) <> Length(b)) THEN BEGIN (* strings not the same sice *)
    EXIT(FALSE);
  END; (* IF *)
  s := '';
  FOR i := 1 TO Length(a) DO BEGIN
    s := a[i];
    FOR j := i + 1 TO Length(a) DO BEGIN
      s := s + a[j];
    END; (* FOR *)
    FOR k := 1 TO i -1 DO BEGIN
     s := s + a[k];
    END; (* FOR *)
    actualVal := BruteForce2(s, b);
    IF (actualVal <> 0) THEN BEGIN (* found? *)
      EXIT(TRUE);
    END; (* IF *)
    s := '';
  END; (* FOR *)
  IsCyclicRotation := FALSE;
END; (* IsCyclicRotation *)

BEGIN (* StringRotationPrg *)
  WriteLn(BruteForce2('rgHagenbe', 'Hagenbe'));
  //WriteLn(IsCyclicRotation('Hagenberg', 'rgHagenbe'));
END. (* StringRotationPrg *)
