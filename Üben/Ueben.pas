PROGRAM Ueben;

FUNCTION WeakSearch(s, p: STRING): INTEGER;
  VAR
    i, j, plen, slen: INTEGER;
    allowedMiss: BOOLEAN;
BEGIN (* WeakSearch *)
  i := 1;
  j := 1;
  allowedMiss := FALSE;
  plen := Length(p);
  slen := Length(s);
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      Inc(j);
      Inc(i);
    END ELSE IF (s[i] <> p[j]) AND (j > 1) AND (allowedMiss = FALSE) THEN BEGIN
      Inc(j);
      Inc(i);
      allowedMiss := TRUE;
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
      allowedMiss := FALSE;
    END; (* IF *)
  UNTIL (j > plen ) OR (i > slen); (* REPEAT *)
  IF (j > plen) THEN BEGIN
    WeakSearch := i - plen;
  END ELSE BEGIN
    WeakSearch := 0;
  END; (* IF *)
END; (* WeakSearch *)

FUNCTION Searcho(s, p: STRING): INTEGER;
  VAR
    i, j, plen, slen: INTEGER;
    offset: INTEGER;
BEGIN (* WeakSearch *)
  i := 1;
  j := 1;
  offset := 0;
  plen := Length(p);
  slen := Length(s);
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      Inc(j);
      Inc(i);
    END ELSE IF (s[i] = '�') AND (p[j] = 'o') AND (p[j + 1] = 'e')  THEN BEGIN
      Inc(i);
      //WriteLn(offset);
      j := j + 2;
      Dec(offset);
    END ELSE IF (p[j] = '�') AND (s[i] = 'o') AND (s[i + 1] = 'e')  THEN BEGIN
      Inc(j);
       //WriteLn(offset);
      i := i + 2;
      Inc(offset);
    END ELSE BEGIN
      WriteLn(offset);
      i := i - j + 2 - offset;
      offset := 0;
      j := 1;
    END; (* IF *)
  UNTIL (j > plen ) OR (i > slen); (* REPEAT *)
  IF (j > plen) THEN BEGIN
    Searcho := i - plen - offset;
  END ELSE BEGIN
    Searcho := 0;
  END; (* IF *)
END; (* WeakSearch *)


FUNCTION Search(s, p: STRING): INTEGER;
  VAR
    i, j, plen, slen: INTEGER;
    offset: INTEGER;
BEGIN (* WeakSearch *)
  i := 1;
  j := 1;
  offset := 0;
  plen := Length(p);
  slen := Length(s);
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      j := i + 1;
      i  := i + 1;
      WHILE (s[i] = s[i - 1]) AND (i<= sLen) DO BEGIN
        i := i + 1;
        inc(offset);
      END; (* WHILE *)
      WHILE (p[j] = p[j - 1]) AND (j <= plen) DO BEGIN
        j := j + 1;
        Dec(offset);
      END; (* WHILE *)
    END ELSE BEGIN
      i := i - j + 2 - offset;
      j := 1;
      offset := 0;
    END; (* IF *)
  UNTIL (j > plen ) OR (i > slen); (* REPEAT *)
  IF (j > plen) THEN BEGIN
    Search := i - plen- offset;
  END ELSE BEGIN
    Search := 0;
  END; (* IF *)
END; (* WeakSearch *)

FUNCTION WeakSerach2(s, p : STRING): INTEGER; 
  VAR
    sLen, pLen, i, j, offset: INTEGER;
BEGIN (* WeakSerach2 *)
  sLen := Length(s);
  pLen := Length(p);
  offset := 0;
  i := 1;
  j := 1;
  REPEAT
    IF (s[i] = p[j])THEN BEGIN
      Inc(i);
      Inc(j);
      WHILE  (i <=sLen) AND (s[i] = s[i-1]) DO BEGIN
        inc(i);
        Inc(offset);
      END; (* WHILE *)
      WHILE (j <= plen) AND (p[j] = p[j-1]) DO BEGIN
        Inc(j);
        Dec(offset);
      END; (* WHILE *)
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  //WriteLn(offset);
  IF j > pLen THEN
    WeakSerach2 := i - pLen - offset
  ELSE
    WeakSerach2 := 0;
END; (* WeakSerach2 *)


PROCEDURE FindAll(s, p : STRING; VAR count: INTEGER); 
  VAR
    sLen, pLen, i, j: INTEGER;
BEGIN (* WeakSerach2 *)
  sLen := Length(s);
  pLen := Length(p);
  count := 0;
  i := 1;
  j := 1;
  REPEAT
      IF(j > plen) THEN BEGIN
      inc(count); j := 1; i := i - 1;
      END ELSE BEGIN
        IF (s[i] = p[j])THEN BEGIN
          Inc(i);
          Inc(j);
        END ELSE BEGIN
          i := i - j + 2;
          j := 1;
        END; (* IF *)
      END;
  UNTIL(i > slen); (* REPEAT *)
  //WriteLn(offset);
  //IF (j > plen) THEN BEGIN
   // Inc(count);
  //END; (* IF *)
END; (* WeakSerach2 *)

VAR
 i : INTEGER;
BEGIN (* Ueben *)
  i:= 0;
  FindAll('abbbccbb', 'bb', i);
  WriteLn(i);
  i := 0;
  FindAll('abbbccbba', 'bb', i);
  WriteLn(i);
END. (* Ueben *)