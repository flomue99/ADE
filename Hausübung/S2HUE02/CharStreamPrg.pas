(* CharStreamPrg:                                            MFL, 2023-03-20 *)
(* ------                                                                    *)
(* find pattern in char stream                                               *)
(* ========================================================================= *)
PROGRAM CharStreamPrg;

(* Modified Bruteforce *)
PROCEDURE BruteForce(p: STRING);
  VAR
    i, j, z: INTEGER;
    sLen, plen: INTEGER;
    match: BOOLEAN;
    str: STRING;
    s: CHAR;
BEGIN (* BruteForce *)
  IF (p = '') THEN BEGIN
    WriteLn('ERROR: Empty Pattern!');
    EXIT;
  END; (* IF *)
  match := FALSE;
  pLen := Length(p);
  sLen := plen;
  z := 1;
  str := '';
  Read(s);
  WHILE ((s <> ' ' ) AND NOT match) DO BEGIN
    str := str + s; 
    Inc(z);
    IF (z = plen + 1) THEN BEGIN 
      i := 1; j := 1;
      WHILE (i + plen - j <= sLen) AND (j <= pLen) DO BEGIN
        IF (str[i] = p[j]) THEN BEGIN
          Inc(i);
          Inc(j);
        END ELSE BEGIN
          (* missmatch*)
          i := i - j + 2;
          j := 1;
        END; (* IF *)
      END; (* WHILE *)
      IF (j > plen) THEN BEGIN
        match := TRUE;
      END; (* IF *)  
      str := Copy(str, 2, plen - 1);
      z := plen;
    END;
    Read(s);
  END; (* WHILE *)
  IF (match) THEN BEGIN
    WriteLn('pattern: ', p, '  -> found!')
  END ElSE BEGIN
    WriteLn('pattern: ', p, '  -> not found!');
  END; (* IF *)     
END; (* BruteForce *)

BEGIN (* CharStreamPrg *)
  BruteForce('vw');
END. (* CharStreamPrg *)