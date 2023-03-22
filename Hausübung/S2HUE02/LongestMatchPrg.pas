(* Title:                                                 Author, 2023-03-17 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM LongestMatchPrg;

TYPE 
  NodePtr = ^Node;
  Node = RECORD
    subStr: STRING;
    idx: INTEGER;
    next: NodePtr;
  END; (* Node *)
  ListPtr = NodePtr;

VAR
  numComp: INTEGER;

FUNCTION NewList: ListPtr;
BEGIN (* NewList *)
  NewList := NiL;
END; (* NewList *)

FUNCTION NewNode(str: STRING; idx: INTEGER): NodePtr;
  VAR
    n: NodePtr;
BEGIN (* NewNode *)
  New(n);
  n^.subStr := str;   
  n^.next := NiL;
  n^.idx := idx;
  NewNode := n;
END; (* NewNode *)

PROCEDURE Insert(VAR l: ListPtr; str: STRING; idx: INTEGER);
  VAR
    n: NodePtr;
    pred, succ: NodePtr;
BEGIN (* Insert *)
  n := NewNode(str, idx);
  IF (l = NIL) THEN BEGIN
    l := n;
  END ELSE BEGIN
    succ := l;
    pred := NIL;
    WHILE ((succ <> NIL) AND (Length(succ^.subStr)  >= Length(n^.subStr))) DO BEGIN
      pred := succ;
      succ := succ^.next;
    END; (* WHILE *)
    n^.next := succ;
    IF (pred = NIL) THEN BEGIN
      l := n;
    END ELSE BEGIN
      pred^.next := n;
    END; (* IF *)
  END; (* IF *)
END; (* Insert *)

PROCEDURE WriteList(l: ListPtr);
  VAR
    n: NodePtr;
BEGIN (* WriteList *)
  n := l;
  WHILE (n <> NIL) DO BEGIN
    Write(n^.subStr, '-> ');
    n := n^.next; (*set next node*)
  END; (* WHILE *)
  WriteLn('|');
END; (* WriteList *)

PROCEDURE DisposeList(VAR l: ListPtr);
  VAR
    n: NodePtr;
BEGIN (* DisposeList *)
  WHILE (l <> NIL) DO BEGIN
    n := l^.next;  (*setzten auf zweites elemnts*)
    Dispose(l); (* dispose kopf*)
    l := n; (*neuer Kopf ist n*)
  END; (* WHILE *)
END; (* DisposeList *)

FUNCTION Eq(a, b: CHAR): BOOLEAN;
BEGIN (* Eq *)
  Inc(numComp);
  Eq := a = b;
END; (* Eq *)

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

PROCEDURE CreateSubStrings(VAR l: ListPtr; str: STRING);
  VAR
    i, j, n: INTEGER;
    s: STRING;
BEGIN (* CreateSubStrings *)
  s := '';
  n := Length(str);
  FOR i := 1 TO n DO BEGIN
    s := str[i];
    Insert(l,s, i);
    FOR j := i + 1 TO n DO BEGIN
      s := s + str[j];
      Insert(l,s,i);
    END; (* FOR *)
    s := '';
  END; (* FOR *)
END; (* CreateSubStrings *)

PROCEDURE FindLongestMatch(s1, s2: STRING; VAR sub: STRING; VAR start1, start2: INTEGER);
  VAR
    p, s: STRING;
    actualVal, pIdx: INTEGER;
    n : NodePtr;
    subStringList: ListPtr;
BEGIN (* FindLongestMatch *)
  subStringList := NewList; 
  actualVal := 0;
  IF (Length(s1) > Length(s2)) THEN BEGIN
    p := s2;
    s := s1;
  END ELSE BEGIN
    p := s1;
    s := s2;
  END; (* IF *) 
  CreateSubStrings(subStringList, p); (* VAR substringList *)
  n := subStringList;
  WHILE ((actualVal = 0) AND (n <> NIL)) DO BEGIN
    p := n^.subStr;
    pIdx := n^.idx;
    actualVal := KnuthMorrisPratt2(s, p);
    n := n^.next;
  END;
  WriteList(subStringList);
  DisposeList(subStringList);
  IF (actualVal <> 0) THEN BEGIN
    sub := p;
    IF (Length(s1) > Length(s2)) THEN BEGIN
      start1 := actualVal;
      start2 := pIdx;
    END ELSE BEGIN
      start2 := actualVal;
      start1 := pIdx;
    END; (* IF *)  
  END ELSE BEGIN
   start1 := 0;
   start2 := 0;
   sub := '';
  END; (* IF *)
END; (* FindLongestMatch *)


VAR
  sub, s1, s2: STRING;
  start1, start2: INTEGER;
BEGIN (* LongestMatchPrg *)
  sub := '';
  //s1 := 'abcdefg';
  //s2 := 'adeb';
  s1 := 'uvwxy';
  s2 := 'rstqvwxz';
  FindLongestMatch(s1, s2, sub, start1, start2);
  WriteLn('sub: ',sub);
  WriteLn('start1: ', start1);
  WriteLn('start2: ', start2);
  WriteLn('comp: ', numComp);
END. (* LongestMatchPrg *)
