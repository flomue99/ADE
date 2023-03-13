UNIT ModIndexHashProbing;

INTERFACE
  PROCEDURE Insert(str: STRING; idx: INTEGER);
  PROCEDURE WriteHashtable;
  //FUNCTION CountEntries: INTEGER;
IMPLEMENTATION
USES ModHashFunctions;

CONST
  M = 31259;
TYPE
  IndexNodePtr = ^IndexNode;
  IndexNode = RECORD
    idx: INTEGER;
    next: IndexNodePtr;
  END;(* Node *)

  IdxType = RECORD
    w: STRING;
    indices: IndexNodePtr;
  END; (* HtType *)
  HashTable = ARRAY[0..M-1] OF IdxType;

VAR
  ht: HashTable;
  
  PROCEDURE Insert(str: STRING; idx: INTEGER);
  VAR
    h, step: WORD;
    tries: WORD;
    succ, n: IndexNodePtr;
  BEGIN (* Insert *)
    succ := NIL;
    h := Hash1(str) MOD M;
    step := Hash2(str) MOD M;
    IF STEP = 0 THEN STEP := 1;
    tries := 0;
    WHILE ((ht[h].w <> str) AND (ht[h].w <> '')) DO BEGIN
      Inc(tries);
      (* double hashing *)
      h := (h + step) MOD M;
      IF (tries >= M) THEN BEGIN
        WriteLn('Hashtable is full!');
        HALT;
      END; (* IF *)
    END; (* WHILE *)
    (* insert only if not found*)
    IF (ht[h].w = '') THEN BEGIN
      New(n);
      n^.idx := idx;
      n^.next := NIL;
      ht[h].indices := n;
      ht[h].w := str;
    END ELSE BEGIN
      New(n);
      n^.idx := idx;
      n^.next := NIL;
      succ := ht[h].indices;
      WHILE (succ^.next <> NIL) DO BEGIN
        succ := succ^.next;
      END; (* WHILE *)
      succ^.next := n;
    END; (* IF *)
  END; (* Insert *)

PROCEDURE WriteHashtable;
  VAR 
    i: INTEGER;
    n: IndexNodePtr;
BEGIN (* WriteHashtable *)
  FOR i:= 0 TO M-1 DO BEGIN
    IF (ht[i].w <> '') THEN BEGIN
      n := ht[i].indices;
      Write(ht[i].w, '     ');
      WHILE (n <> NIL) DO BEGIN
        IF (n^.next = NIL) THEN BEGIN
          Write(n^.idx);
        END ELSE BEGIN   
          Write(n^.idx, ', ');
        END; (* IF *)
        n := n^.next;
      END; (* WHILE *)
      WriteLn();
    END; (* IF *)
  END; (* FOR *)
END; (* WriteHashtable *)  

  //FUNCTION CountEntries: INTEGER;
  //  VAR
  //    i, count: INTEGER;
  //BEGIN (* CountEntries *)
  //  count := 0;
  //  FOR i := 0 TO M-1 DO BEGIN
  //    IF (ht[i] <> '') THEN BEGIN
  //      Inc(count);
  //    END; (* IF *)
  //  END; (* FOR *)
  //  CountEntries := count;
  //END; (* CountEntries *)
VAR
  i: INTEGER;
BEGIN (* ModIndexHashProbing *)
  FOR i := 0 TO M-1 DO BEGIN
    ht[i].w := '';
  END; (* FOR *)
END. (* ModIndexHashProbing *)