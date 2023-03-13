(* DLListPrg:                                            Author, 2022-12-07 *)
(* ------                                                                    *)
(* double linked liner list                                                  *)
(* ========================================================================= *)
PROGRAM DLListPrg;
  {$C+}
  TYPE 
    NodePtr = ^Node;
    Node = RECORD
      value: INTEGER;
      next: NodePtr;
      prev: NodePtr;
    END; (* Node *)
    List = RECORD
      first : NodePtr;
      last: NodePtr;
    END;

  FUNCTION NewNode(v: INTEGER): NodePtr;
    VAR
      n: NodePtr;
  BEGIN (* NewNode *)
    New(n);
    IF (n = NIL) THEN BEGIN  (* if we want to do it correctly*)
      WriteLn('ERROR: Out of memory!');
      HALT;
    END; (* IF *)
    n^.value := v;   
    n^.prev := NIL;
    n^.next := NIL;
    NewNode := n;
  END; (* NewNode *)

  FUNCTION NewList: List;
    VAR
      l: List;
  BEGIN (* NewList *)
    l.first := NIL;
    l.last := NIL;
    NewList := l;
  END; (* NewList *)

  PROCEDURE Prepend(VAR l: List; v: INTEGER);
    VAR
      n: NodePtr;
  BEGIN (* Prepend *)
    n := NewNode(v);
    IF (l.first = NIL) THEN BEGIN
      l.first := n;
      l.last := n;
    END ELSE BEGIN
      n^.next := l.first;
      l.first^.prev := n;
      l.first := n;
    END; (* IF *)
  END; (* Prepend *)

  PROCEDURE Append(VAR l: List; v: INTEGER);
    VAR
      n: NodePtr;
  BEGIN (* Append *)
    n := NewNode(v);
    IF (l.first = NIL) THEN BEGIN
      l.first := n;
      l.last := n;
    END ELSE BEGIN
      n^.prev := l.last;
      l.last^.next := n;
      l.last := n;
    END; (* IF *)
  END; (* Append *)

  FUNCTION IsSorted(l: List): BOOLEAN;
    VAR
      n: NodePtr;
  BEGIN (* IsSorted *)
    IF (l.first = NIL) THEN BEGIN
      IsSorted := TRUE;
    END ELSE BEGIN
      n := l.first;
      WHILE ((n^.next <> NIL) AND (n^.value <= n^.next^.value)) DO BEGIN
        n := n^.next;
      END; (* WHILE *)
      IsSorted := n^.next = NIL;
    END; (* IF *)
  END; (* IsSorted *)

  PROCEDURE Insert(VAR l: List; v: INTEGER);
    VAR
      n: NodePtr;
      succ: NodePtr;
  BEGIN (* Insert *)
    Assert(IsSorted(l), 'List not sorted before insert');
    n := NewNode(v);
    IF (l.first = NIL) THEN BEGIN
      l.first := n;
      l.last := n;
    END ELSE BEGIN
      succ := l.first;
      WHILE ((succ <> NIL) AND (succ^.value <= n^.value)) DO BEGIN
        succ := succ^.next;
      END; (* WHILE *)
      n^.next := succ;
      IF (succ = l.first) THEN BEGIN //Insert at first position
        n^.next := succ;
        succ^.prev := n;
        l.first := n;
      END ELSE IF (succ = NIL) THEN BEGIN //Inser at last position
        n^.prev := l.last;
        l.last^.next := n;
        l.last := n;
      END ELSE BEGIN // Insert between
        n^.prev := succ^.prev;
        n^.next := succ;
        succ^.prev^.next := n;
        succ^.prev := n;
      END; (* IF *)
    END; (* IF *)
    Assert(IsSorted(l), 'List not sorted after insert');
  END; (* Insert *)

  PROCEDURE WriteList(l: List);
    VAR
      n: NodePtr;
  BEGIN (* WriteList *)
    n := l.first;
    WHILE (n <> NIL) DO BEGIN
      Write(n^.value, '-> ');
      n := n^.next; (*set next node*)
    END; (* WHILE *)
    WriteLn('|');
  END; (* WriteList *)

  PROCEDURE DelteLastNodes(lst : List; n: INTEGER);
    VAR
      count: INTEGER;
      helpNode, node: NodePtr;
  BEGIN (* DelteLastNodes *)
    helpnode := NIL;
    node := lst. first;
    count := 0;
    WHILE (node^.next <> NIL) DO BEGIN
      count := count + 1;
    END; (* WHILE *)
    WriteLn(count);
  END; (* DelteLastNodes *)

  PROCEDURE DisposeList(VAR l: List);
    VAR
      n: NodePtr;
  BEGIN (* DisposeList *)
    WHILE (l.first <> NIL) DO BEGIN
      n := l.first^.next;  (*setzten auf zweites elemnts*)
      Dispose(l.first^.next); (* dispose kopf*)
      l.first := n; (*neuer Kopf ist n*)
    END; (* WHILE *)
    l.last := NIL;
  END; (* DisposeList *)

VAR
  l: List;
  s: List;
BEGIN (* DLListPrg *)

  Insert(s,3);
  Insert(s,5);
  Insert(s,7);
  Insert(s,4);
  WriteList(s);
  DisposeList(s);
END. (* DLListPrg *)
