(* ModHashChaining:                                          MFL, 2023-03-14 *)
(* ------                                                                    *)
(* Create hashtable with chaining                                            *)
(* ========================================================================= *)
UNIT ModHashChaining;
INTERFACE
  PROCEDURE Insert(str: String);
  FUNCTION CountEntries: LONGINT;
  PROCEDURE DisposeHashTable;
IMPLEMENTATION
USES ModHashFunctions;

CONST M = 31259; (* should be a primenumber *)

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
 
FUNCTION CountEntries: LONGINT;
  VAR
    count: LONGINT;
    i: INTEGER;
    n: NodePtr;
BEGIN (* CountEntries *)
  count := 0;
  FOR i := 0 TO M-1 DO BEGIN
    n := ht[i];
    IF (n <> NIL) THEN BEGIN
      WHILE (n <> NIL) DO BEGIN
        count := count + 1;
        n := n^.next;
      END; (* WHILE *)
    END; (* IF *)
  END; (* FOR *)
  CountEntries := count;
END; (* CountEntries *)

PROCEDURE DisposeChain(VAR l: NodePtr);
  VAR 
    n: NodePtr;
BEGIN (* DisposeChain *)
  WHILE (l <> NIL) DO BEGIN
    n := l^.next;
    Dispose(l);
    l := n;
  END; (* WHILE *)
END; (* DisposeChain *)

PROCEDURE DisposeHashTable;
  VAR
    i: INTEGER;
BEGIN (* DisposeHashTable *)
  FOR i := 0 TO M-1 DO BEGIN
    IF (ht[i] <> NIL) THEN BEGIN
      DisposeChain(ht[i]);
    END; (* IF *)
  END; (* FOR *)
END; (* DisposeHashTable *)

BEGIN (* ModHashChaining *)
END. (* ModHashChaining *)