(* ?PatternMatching:                                         MFL, 2023-03-15 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM PatternMatching;

TYPE
  PMProc = FUNCTION(s, p: STRING): INTEGER;

VAR
  numComp: INTEGER;

FUNCTION Eq(a, b: CHAR): BOOLEAN;
BEGIN (* Eq *)
  Inc(numComp);
  Eq := a = b;
END; (* Eq *)

(* s is full string, p is pattern *)
FUNCTION BruteForce1(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
    pos: INTEGER;
BEGIN (* BruteForce1 *)
  sLen := Length(s);
  pLen := Length(p);
  i := 1;
  pos := 0;
  WHILE (i <= sLen - pLen + 1) AND (pos = 0) DO BEGIN
    j := 1;
    WHILE (j <= pLen) AND Eq(s[i + j - 1], p[j]) DO BEGIN
      Inc(j);
    END; (* WHILE *)
    (* found *)
    IF (j > plen) THEN BEGIN
      pos := i;
    END; (* IF *)
    Inc(i);
  END; (* WHILE *)
  BruteForce1 := pos;
END; (* BruteForce1 *)

FUNCTION BruteForce2(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
BEGIN (* BruteForce2 *)
  sLen := Length(s);
  pLen := Length(p);
  i := 1; j := 1;
  WHILE (i + plen - j <= sLen) AND (j <= pLen) DO BEGIN
    IF EQ(s[i], p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
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

(* s is full string, p is patterm*) (* rechts nach links *)
FUNCTION BruteForceRL(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, pLen: INTEGER;
BEGIN (* BruteForceRL *)
  sLen := Length(s);
  pLen := Length(p);
  i := pLen;
  j := pLen;
  WHILE (i <= sLen) AND (j > 0) DO BEGIN
    IF (Eq(s[i], p[j])) THEN BEGIN
      i := i - 1;
      j := j - 1;
    END ELSE BEGIN
      (* mismatch*)
      i := i + plen - j + 1;
      j := plen;
    END; (* IF *)
  END; (* WHILE *)
  IF (j = 0) THEN BEGIN
    BruteForceRL :=  i + 1;
  END ELSE BEGIN
    BruteForceRL := 0;
  END; (* IF *)
END; (*?BruteForceRL?*)

FUNCTION RabinKarp(s, p: STRING): INTEGER;
CONST
  Base = 256;
  M = 32099; (* primennumber < 2^15 *)

VAR
  i, j, pos: INTEGER;
  hp, hs: INTEGER;
  pLen, sLen: INTEGER;
  bP: INTEGER; (* (base ^ (plen - 1)) MOD M*)
BEGIN (* RabinKarp *)
  plen := Length(p);
  sLen := Length(s);
  hp := 0;
  hs := 0;
  FOR i := 1 TO pLen DO BEGIN
    hs := (hs * Base + Ord(s[i])) MOD M;
    hp := (hp * Base + Ord(p[i])) MOD M;
  END; (* FOR *)
  bP := 1;
  FOR i := 1 TO plen - 1 DO BEGIN
    bp := bp * Base MOD M;
  END; (* FOR *)
  i := 1;
  pos := 0;
  WHILE (i <= sLen - plen + 1) AND (pos = 0) DO BEGIN
    IF (hp = hs) THEN BEGIN
      j := 1;
      WHILE (j <= plen) AND Eq(s[i + j - 1], p[j]) DO BEGIN
        Inc(j);
      END; (* WHILE *)
      IF (j > plen) THEN BEGIN
        pos := i;
      END; (* IF *)
    END; (* IF *)
    IF (i < sLen - plen + 1) AND (pos = 0) THEN BEGIN
      hs := (hs + M * bp - Ord(s[i]) * bP) MOD M;
      hs := (hs * Base) MOD M;
      hs := (hs + Ord(s[i + plen])) MOD M;
    END; (* IF *)
    Inc(i);
  END; (* WHILE *)
  RabinKarp := pos;
END; (* RabinKarp *)

FUNCTION BoyerMoore(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, pLen: INTEGER;
    skip: ARRAY [CHAR] OF INTEGER;
    PROCEDURE InitSkip;
      VAR
        ch: CHAR;
        k: INTEGER;
    BEGIN (* InitSkip *)
      FOR ch := Low(CHAR) TO High(CHAR) DO BEGIN
        skip[ch] := plen;
      END; (* FOR *)
      FOR k := 1 TO plen DO BEGIN
        skip[p[k]] := plen - k;
      END; (* FOR *)
    END; (* InitSkip *)

BEGIN (* BoyerMoore *)
  sLen := Length(s);
  pLen := Length(p);
  InitSkip;
  i := pLen;
  j := pLen;
  WHILE (i <= sLen) AND (j > 0) DO BEGIN
    IF (Eq(s[i], p[j])) THEN BEGIN
      i := i - 1;
      j := j - 1;
    END ELSE BEGIN
      (* mismatch*)
      IF (plen - j> skip[s[i]]) THEN BEGIN
        i := plen + i;
      END ELSE BEGIN
        i := i + skip[s[i]];
      END; (* IF *)
      j := plen;
    END; (* IF *)
  END; (* WHILE *)

  IF (j = 0) THEN BEGIN
    BoyerMoore :=  i + 1;
  END ELSE BEGIN
    BoyerMoore := 0;
  END; (* IF *)
END; (*BoyerMoore*)

FUNCTION KnuthMorrisPratt1(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
    next: ARRAY [1..255] OF BYTE;
  
  PROCEDURE InitNext;
  BEGIN (* InitNext *)
    i := 1;
    j := 0;
    next[1] := 0;
    WHILE (i < pLen) DO BEGIN
      IF (j = 0) OR EQ(p[i], p[j]) THEN BEGIN
        Inc(i);
        Inc(j);
        next[i] := j;
      END ELSE BEGIN
        j := next[j];
      END;
    END; (* WHILE *)
  END; (* InitNext *)

BEGIN (* KnuthMorrisPratt1 *)
  sLen := Length(s);
  pLen := Length(p);
  
  InitNext;
  (* debug *)
  FOR i := 1 TO pLen DO BEGIN
    Write(next[i], ' ');
  END; (* FOR *)
  i := 1; j := 1;
  WHILE (i + plen - j <= sLen) AND (j <= pLen) DO BEGIN
    IF (j = 0) OR EQ(s[i], p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    END ELSE BEGIN
      (* missmatch*)
      j := next[j];
    END; (* IF *)
  END; (* WHILE *)
  IF (j > plen) THEN BEGIN
    KnuthMorrisPratt1 := i - j + 1;
  END ELSE BEGIN
    KnuthMorrisPratt1 := 0;
  END; (* IF *)
END; (* KnuthMorrisPratt1 *)

FUNCTION KnuthMorrisPratt2(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
    next: ARRAY [1..255] OF BYTE;
  
  PROCEDURE InitNext;
  BEGIN (* InitNext *)
    i := 1;
    j := 0;
    next[1] := 0;
    WHILE (i < pLen) DO BEGIN
      IF (j = 0) OR EQ(p[i], p[j]) THEN BEGIN
        Inc(i);
        Inc(j);
        (* next[i] := j; *)
        IF (NOT Eq(p[j], p[i])) THEN BEGIN
          next[i] := j;
        END ELSE BEGIN
          next[i] := next[j];
        END;
      END ELSE BEGIN
        j := next[j];
      END;
    END; (* WHILE *)
  END; (* InitNext *)

BEGIN (* KnuthMorrisPratt2 *)
  sLen := Length(s);
  pLen := Length(p);
  
  InitNext;
  (* debug *)
  FOR i := 1 TO pLen DO BEGIN
    Write(next[i], ' ');
  END; (* FOR *)
  i := 1; j := 1;
  WHILE (i + plen - j <= sLen) AND (j <= pLen) DO BEGIN
    IF (j = 0) OR EQ(s[i], p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    END ELSE BEGIN
      (* missmatch*)
      j := next[j];
    END; (* IF *)
  END; (* WHILE *)
  IF (j > plen) THEN BEGIN
    KnuthMorrisPratt2 := i - j + 1;
  END ELSE BEGIN
    KnuthMorrisPratt2 := 0;
  END; (* IF *)
END; (* KnuthMorrisPratt2 *)

FUNCTION BruteForce(s, p : STRING): INTEGER;
  VAR
    sLen, pLen, i, j: INTEGER;
BEGIN (* BruteForce *)
  sLen := Length(s);
  pLen := Length(p);
  i := 1;
  j := 1;
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  IF j > pLen THEN
    BruteForce := i - pLen
  ELSE
    BruteForce := 0;
END; (* BruteForce *)

FUNCTION Serach(s, p : STRING): INTEGER;
  VAR
    sLen, pLen, i, j, x, z:INTEGER;
    offset : INTEGER;
BEGIN (* Serach *)
   sLen := Length(s);
  pLen := Length(p);
  offset := 0;
  i := 1;
  j := 1;
  REPEAT
    IF (s[i] = p[j])THEN BEGIN
      z := j;
      x := i;
      WHILE (s[i] = p[z]) DO BEGIN
        Inc(z);
        Dec(offset);
      END; (* WHILE *)
      WHILE (s[x] = p[j]) DO BEGIN
        Inc(x);
        Inc(offset);
      END; (* WHILE *)
      i := z;
      j := x;
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  IF j > pLen THEN
    Serach := i - pLen
  ELSE
    Serach := 0;
END; (* Search *)

FUNCTION WeakSerach2(s, p : STRING): INTEGER; (* s ss ? *)
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
        Dec(offset);
      END; (* WHILE *)
      WHILE (j <= plen) AND (p[j] = p[j-1]) DO BEGIN
        Inc(j);
        Inc(offset);
      END; (* WHILE *)
    END ELSE BEGIN
      i := i - j + 2 ;
      offset := 0;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  //WriteLn(offset);
  IF j > pLen THEN
    WeakSerach2 := i - pLen + offset
  ELSE
    WeakSerach2 := 0;
END; (* WeakSerach2 *)

FUNCTION WeakSerach(s, p : STRING): INTEGER; (* s ss ? *)
  VAR
    sLen, pLen, i, j, offset: INTEGER;
BEGIN (* WeakSerach *)
  sLen := Length(s);
  pLen := Length(p);
  offset := 0;
  i := 1;
  j := 1;
  REPEAT
     IF (p[j] = '�') AND ((s[i] = 's') AND (s[i+1] = 's'))  THEN BEGIN (* s = ss, p = � *)
        Inc(j);
        Inc(offset);
        i := i + 2;
    END ELSE IF ((s[i] = '�')) AND ((p[j] = 's') AND (p[j+1] = 's'))  THEN BEGIN (* s = �, p = ss *)
        Inc(i);
        Dec(offset);
        j := j + 2;
    END ELSE IF (s[i] = p[j]) OR (p[j] = 's') AND (s[i] = '�') OR (s[i] = 's') AND (p[j] = '�') THEN BEGIN (*s = s, p = s oder s = s, p = � oder s = �, p = s *)
      Inc(i);
      Inc(j);
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  //WriteLn(offset);
  IF j > pLen THEN
    WeakSerach := i - pLen - offset
  ELSE
    WeakSerach := 0;
END; (* WeakSerach *)

FUNCTION WeakSerach1(s, p : STRING): INTEGER; (* o und o? *)
  VAR
    sLen, pLen, i, j, offset: INTEGER;
BEGIN (* WeakSerach1 *)
  sLen := Length(s);
  pLen := Length(p);
  offset := 0;
  i := 1;
  j := 1;
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    END ELSE IF (s[i] = '�') AND (p[j]= 'o') AND (p[j+1] = 'e')THEN BEGIN (* s = �, p = oe *)
        Inc(i);
        Dec(offset);
        j := j + 2;
    END ELSE IF (p[j] = '�') AND (s[i] = 'o') AND (s[i+1] = 'e') THEN BEGIN (* s = oe, p = � *)
        Inc(j);
        Inc(offset);
        i := i + 2;
    END ELSE BEGIN
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  IF j > pLen THEN
    WeakSerach1 := i - pLen - offset
  ELSE
    WeakSerach1 := 0;
END; (* WeakSerach *)

FUNCTION BruteForce5(s, p : STRING): INTEGER;
  VAR
    sLen, pLen, i, j: INTEGER;
    missCount: INTEGER;
BEGIN (* BruteForce *)
  missCount := 0;
  sLen := Length(s);
  pLen := Length(p);
  i := 1;
  j := 1;
  REPEAT
    IF (s[i] = p[j]) THEN BEGIN
      Inc(i);
      Inc(j);
    END ELSE IF( (j > 1) AND(missCount = 0)) THEN BEGIN
      Inc(i);
      Inc(j);
      Inc(missCount);
    END ELSE BEGIN
      missCount := 0;
      i := i - j + 2;
      j := 1;
    END; (* IF *)
  UNTIL (i > sLen) OR (j > pLen); (* REPEAT *)
  IF j > pLen THEN
    BruteForce5 := i - pLen
  ELSE
    BruteForce5 := 0;
END; (* BruteForce *)


PROCEDURE TestCase(pos: PMProc; s, p: STRING; expectedVal: INTEGER);
  VAR
    actualVal: INTEGER;
BEGIN (* TestCase *)
  numComp := 0;
  actualVal := pos(s,p);
  IF (actualVal <> expectedVal) THEN BEGIN
    WriteLn(s,' ',p, ' ERROR: expected ', expectedVal, ' actual ', actualVal);
  END ELSE BEGIN
    WriteLn(s,' ',p, ' ', expectedVal,'=', actualVal, ' num. comp.: ', numComp);
  END;
END; (* TestCase *)

PROCEDURE Test(pos: PMProc);
BEGIN (* Test *)
  TestCase(pos,'aaaaaaaaaaaaaaaaaaaaaab', 'aaab', 20);
  TestCase(pos,'Hagenberg', 'berg', 6 );
  TestCase(pos,'abc', 'abc', 1 );
  TestCase(pos,'aaaaaaaaabc', 'abc', 9 );
  TestCase(pos,'aaaaaaaaa', 'abc', 0 );
  TestCase(pos,'anabaanaanabanana', 'anabanana', 9);
END; (* Test *)

BEGIN (* PatternMatching *)
  //WriteLn('BruteForce2');
  //Test(BruteForce2);
  //WriteLn('KnuthMorrisPratt1');
  //Test(KnuthMorrisPratt1);
  //WriteLn('KnuthMorrisPratt2');
  //Test(KnuthMorrisPratt2);
  //WriteLn('BruteForceRL');
  //Test(BruteForceRL);
  //WriteLn('BoyerMoore');
  //Test(BoyerMoore);
  //WriteLn('RabinKarp');
  //Test(RabinKarp);
  WriteLn(WeakSerach('Stra�', 'as'));
  WriteLn(WeakSerach('Stra�', 'a�'));
  WriteLn(WeakSerach('Stra�', 'ass'));
  WriteLn(WeakSerach('Strass', 'as'));
  WriteLn(WeakSerach('Strass', 'a�'));
  WriteLn(WeakSerach('Strass', 'ass'));
  WriteLn(WeakSerach1('Soehne', 'hne�'));
  WriteLn(WeakSerach1('Soehne', 'Soehne'));
  WriteLn(WeakSerach1('S�hne', 'Soehne'));
  WriteLn(WeakSerach1('S�hne', 'S�hne'));
  WriteLn(WeakSerach2('AACD', 'ACD'));
  WriteLn(WeakSerach2('ACD', 'AACD'));
  WriteLn(WeakSerach2('ACD', 'CCC'));
  WriteLn(WeakSerach2('AADD', 'ACD'));
  WriteLn(BruteForce5('AACD', 'ABD'));
  WriteLn(BruteForce5('ACD', 'BCD'));
  WriteLn(BruteForce5('ACD', 'ADD'));
  WriteLn(BruteForce5('AADD', 'ABDD'));
END. (* PatternMatching *)
