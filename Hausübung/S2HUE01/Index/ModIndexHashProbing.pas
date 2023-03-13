UNIT ModIndexHashProbing;

INTERFACE
  PROCEDURE Insert(str: STRING; idx: INTEGER);
  PROCEDURE CreateIndex;
  PROCEDURE DisposeIndex;
IMPLEMENTATION
USES ModHashFunctions;

CONST
  M = 31259;

TYPE
  (* Type for indices *)
  IndexNodePtr = ^IndexNode;
  IndexNode = RECORD
    idx: INTEGER;
    next: IndexNodePtr;
  END;(* Node *)

  (* type for word *)
  IdxType = RECORD
    w: STRING;
    indices: IndexNodePtr;
  END; (* HtType *)
  HashTable = ARRAY[0..M-1] OF IdxType;
  
  (* type for sorted List*)
  ListNodePtr = ^ListNode;
  ListNode = RECORD
    idx: IdxType;
    next: ListNodePtr;
  END; (* ListNode *)

VAR
  ht: HashTable;
  sortedIndex: ListNodePtr;

  FUNCTION NewIndex: ListNodePtr;
  BEGIN (* NewIndex *)
    NewIndex := NIL;
  END; (* NewIndex *)
  
  PROCEDURE InitIndex;
  BEGIN (* InitIndex *)
    sortedIndex := NewIndex;
  END; (* InitIndex *)

  PROCEDURE InsertNode(idx: IdxType);
  VAR 
    n: ListNodePtr;
    pred, succ: ListNodePtr;
  BEGIN (* Insert *)
    (* Create Node*)
    New(n);
    n^.idx := idx;
    n^.next := NIL;
    IF (sortedIndex = NIL) THEN BEGIN
      sortedIndex := n;
    END ELSE BEGIN
      succ := sortedIndex;
      pred := NIL;
      WHILE ((succ <> NIL) AND (succ^.idx.w <= n^.idx.w)) DO BEGIN
        pred := succ;
        succ := succ^.next;
      END; (* WHILE *)
      n^.next := succ;
      IF (pred = NIL) THEN BEGIN
        sortedIndex := n;
      END ELSE BEGIN
        pred^.next := n;
      END; (* IF *)
    END; (* IF *)
  END; (* Insert *)

  PROCEDURE DisposeIndicesList(VAR l: IndexNodePtr);
    VAR 
      n: IndexNodePtr;
  BEGIN (* DisposeIndicesList *)
    WHILE (l <> NIL) DO BEGIN
      n := l^.next;
      Dispose(l);
      l := n;
    END; (* WHILE *)
  END; (* DisposeIndicesList *)
  
  PROCEDURE DisposeIndex;
    VAR 
      n: ListNodePtr;
  BEGIN (* DisposeIndex *)
    WHILE (sortedIndex <> NIL) DO BEGIN
      n := sortedIndex^.next;
      DisposeIndicesList(sortedIndex^.idx.indices);
      Dispose(sortedIndex);
      sortedIndex := n;
    END; (* WHILE *)
    InitIndex;
  END; (* DisposeIndex *)

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

PROCEDURE WriteSortedIndex;
  VAR 
    n1: IndexNodePtr;
    n2: ListNodePtr;
BEGIN (* WriteSortedIndex *)
  n2 := sortedIndex;
  IF (sortedIndex <> NIL) THEN BEGIN
    WHILE (n2 <> NIL) DO BEGIN
       n1 := n2^.idx.indices;
      Write(n2^.idx.w:25, ' | ');
      WHILE (n1 <> NIL) DO BEGIN
        IF (n1^.next = NIL) THEN BEGIN
          Write(n1^.idx);
        END ELSE BEGIN   
          Write(n1^.idx, ', ');
        END; (* IF *)
        n1 := n1^.next;
      END; (* WHILE *)
      WriteLn();
      n2 := n2^.next
    END; (* WHILE *)
  END ELSE BEGIN
    WriteLn('List is empty')
  END; (* IF *)
END; (* WriteSortedIndex *)  

  PROCEDURE CreateIndex;
    VAR
      i: INTEGER;
  BEGIN (* CreateIndex  *)
    FOR i := 0 TO M-1 DO BEGIN
      IF (ht[i].w <> '') THEN BEGIN     
        InsertNode(ht[i]);
      END; (* IF *)
    END; (* FOR *)
    WriteSortedIndex;
  END; (* CreateIndex *)

VAR
  i: INTEGER;
BEGIN (* ModIndexHashProbing *)
  InitIndex();
  FOR i := 0 TO M-1 DO BEGIN
    ht[i].w := '';
  END; (* FOR *)
END. (* ModIndexHashProbing *)