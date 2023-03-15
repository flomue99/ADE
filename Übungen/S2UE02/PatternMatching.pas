(* ´PatternMatching:                                         MFL, 2023-03-15 *)
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

FUNCTION BruteForceRl(s, p: STRING): INTEGER;
  VAR
    i, j: INTEGER;
    sLen, plen: INTEGER;
BEGIN (* BruteForceRl *)
  sLen := Length(s);
  pLen := Length(p);
  i := plen; j := plen;
  WHILE (i <= sLen) AND (j > 0) DO BEGIN
    IF (Eq(s[i], p[j])) THEN BEGIN
      Dec(i);
      Dec(j);
    END ELSE BEGIN
      (* missmach *)
      i := i + plen - j + 1;
      j := pLen;
    END;
  END; (* WHILE *)
  IF (j = 0) THEN BEGIN
    BruteForceRl := i + 1;
  END ELSE BEGIN
    BruteForceRl := 0;
  END; (* IF *)
END; (* BruteForceRl *)

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
  WriteLn('BruteForce2');
  Test(BruteForce2);
  WriteLn('KnuthMorrisPratt1');
  Test(KnuthMorrisPratt1);
  WriteLn('KnuthMorrisPratt2');
  Test(KnuthMorrisPratt2);
    WriteLn('BruteForceRL');
  Test(BruteForceRL);
END. (* PatternMatching *)
