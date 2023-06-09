UNIT ModHashChaining;
INTERFACE
  PROCEDURE Insert(str: String);
  PROCEDURE WriteHashtable;

IMPLEMENTATION
USES ModHashFunctions;

CONST M = 211; (* should be a primenumber *)

TYPE
  NodePtr = ^Node;
  Node = RECORD
    val: String;
    next: NodePtr;
  END;
  HashTable = ARRAY [0..M-1] OF NodePtr;

VAR
  ht: HashTable;

PROCEDURE Insert(str: STRING);
VAR
  h: WORD;
  n: NodePtr;
BEGIN (* Insert *)
  h := Hash1(str) MOD M;
  n := ht[h];
  WHILE ((n <> NIL) AND (n^.val <> str)) DO BEGIN
    n:= n^.next;
  END; (* WHILE *)
  (* insert only if not found*)
  IF (n = NIL) THEN BEGIN
    New(n);
    n^.val := str;
    n^.next := ht[h];
    ht[h] := n;
  END; (* IF *)
END; (* Insert *)

PROCEDURE WriteHashtable;
  VAR 
    i: INTEGER;
    n: NodePtr;
BEGIN (* WriteHashtable *)
  FOR i:= 0 TO M-1 DO BEGIN
    n := ht[i];
    IF (n <> NIL) THEN BEGIN
      Write(i, ': ');
      WHILE (n <> NIL) DO BEGIN
        Write(' -> ', n^.val);
        n := n^.next;
      END; (* WHILE *)
      WriteLn();
    END; (* IF *)
  END; (* FOR *)
END; (* WriteHashtable *)  


BEGIN (* ModHashChaining *)
  
END. (* ModHashChaining *)