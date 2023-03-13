(* DCListPrg:                                            Author, 2022-12-07 *)
(* ------                                                                    *)
(* double linked cyclic list                                                  *)
(* ========================================================================= *)
PROGRAM DCListPrg;
  {$C+}
  TYPE 
    NodePtr = ^Node;
    Node = RECORD
      value: INTEGER;
      next: NodePtr;
      prev: NodePtr;
    END; (* Node *)
    ListPtr = NodePtr;

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
    n^.prev := n;
    n^.next := n;
    NewNode := n;
  END; (* NewNode *)

  FUNCTION NewList: ListPtr;
    VAR
      l: ListPtr;
  BEGIN (* NewList *)
    l := NewNode(0);
    NewList := l;
  END; (* NewList *)

  PROCEDURE Prepend(l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
  BEGIN (* Prepend *)
    n := NewNode(v);
    n^.next := l^.next;
    n^.prev := l;
    l^.next^.prev := n;
    l^.next := n;
  END; (* Prepend *)

  PROCEDURE Append(l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
  BEGIN (* Append *)
    n := NewNode(v);
    n^.next := l^.prev;
    n^.next := l;
    l^.prev^.next := n;
    l^.prev := n;
  END; (* Append *)

  FUNCTION IsSorted(l: ListPtr): BOOLEAN;
    VAR
      n: NodePtr;
  BEGIN (* IsSorted *)
    n := l^.next;
    WHILE ((n^.next <> l) AND (n^.value <= n^.next^.value)) DO BEGIN
      n := n^.next;
    END; (* WHILE *)
    IsSorted := n^.next = l;
  END; (* IsSorted *)

  PROCEDURE Insert(l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
      succ: NodePtr;
  BEGIN (* Insert *)
    Assert(IsSorted(l), 'List not sorted before insert');
    n := NewNode(v);
    succ := l^.next;
    WHILE ((succ <> l) AND (succ^.value <= n^.value)) DO BEGIN
      succ := succ^.next;
    END; (* WHILE *)
    n^.prev := succ^.prev;
    n^.next := succ;
    succ^.prev^.next := n;
    succ^.prev := n;
    Assert(IsSorted(l), 'List not sorted after insert');
  END; (* Insert *)

  PROCEDURE WriteList(l: ListPtr);
    VAR
      n: NodePtr;
  BEGIN (* WriteList *)
    n := l^.next;
    WHILE (n <> l) DO BEGIN
      Write(n^.value, '-> ');
      n := n^.next; (*set next node*)
    END; (* WHILE *)
    WriteLn('|');
  END; (* WriteList *)

  PROCEDURE DisposeList(VAR l: ListPtr);
    VAR
      n: NodePtr;
  BEGIN (* DisposeList *)
    l^.prev^.next := NIL;
    WHILE (l <> NIL) DO BEGIN
      n := l^.next;  (*setzten auf zweites elemnts*)
      Dispose(l); (* dispose kopf*)
      l := n; (*neuer Kopf ist n*)
    END; (* WHILE *)
  END; (* DisposeList *)

  FUNCTION Contains(l: ListPtr; v: INTEGER): BOOLEAN;
    VAR
      n: NodePtr;
  BEGIN (* Contains *)
    l^.value := v;
    n := l^.next;
    WHILE (n^.value <> v) DO BEGIN
      n := n^.next;
    END; (* WHILE *)
    Contains := n <> l;
  END; (* Contains *)
VAR
  l: ListPtr;
  s: ListPtr;
BEGIN (* DCListPrg *)
  l := NewList();
  s := NewList();
  Prepend(l,12);
  Append(l,12);
  WriteList(l);
  DisposeList(l);
  //WriteList(l);
  Insert(s,3);
  Insert(s,5);
  Insert(s,7);
  WriteList(s);
  Insert(s,4);
  WriteList(s);
  WriteLn(Contains(s,4));
  DisposeList(s);
END. (* DCListPrg *)
