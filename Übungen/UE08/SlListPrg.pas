(* SlListPrg:                                                 Author, 2022-11-30 *)
(* ------                                                                    *)
(* fun with singly-linked lists.                                                               *)
(* ========================================================================= *)
{$C+}
PROGRAM SlListsPrg;

  TYPE 
    NodePtr = ^Node;
    Node = RECORD
      value: INTEGER;
      next: NodePtr;
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
    n^.next := NiL;
    NewNode := n;
  END; (* NewNode *)

  FUNCTION NewList: ListPtr;
  BEGIN (* NewList *)
    NewList := NiL;
  END; (* NewList *)

  PROCEDURE Prepend(VAR l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
  BEGIN (* Prepend *)
    n := NewNode(v);
    n^.next := l;
    l := n;
  END; (* Prepend *)

  PROCEDURE Apped(VAR l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
      curr: NodePtr;
  BEGIN (* Apped *)
    n := NewNode(v);
    IF (l = NIL) THEN BEGIN  (*falls l NIL, wird neuer Knoten Kopf der Liste*)
      l := n;
    END ELSE BEGIN
      curr := l;
      WHILE (curr^.next <> NIL) DO BEGIN
        curr := curr^.next; (*zeigt auf nächsten*)
      END; (* WHILE *)
      curr^.next := n; (*letzes element zeigt nun auf n*)
    END; (* IF *)
  END; (* Apped *)

  FUNCTION IsSorted(l: ListPtr): BOOLEAN;
    VAR
      n: NodePtr;
  BEGIN (* IsSorted *)
    IF (l = NIL) THEN BEGIN
      IsSorted := TRUE;
    END ELSE BEGIN
      n := l;
      WHILE ((n^.next <> NIL) AND (n^.value <= n^.next^.value)) DO BEGIN
        n := n^.next;
      END; (* WHILE *)
      IsSorted := n^.next = NIL;
    END; (* IF *)
  END; (* IsSorted *)

  PROCEDURE Insert(VAR l: ListPtr; v: INTEGER);
    VAR
      n: NodePtr;
      pred, succ: NodePtr;
  BEGIN (* Insert *)
    Assert(IsSorted(l), 'List not sorted before insert');
    n := NewNode(v);
    IF (l = NIL) THEN BEGIN
      l := n;
    END ELSE BEGIN
      succ := l;
      pred := NIL;
      WHILE ((succ <> NIL) AND (succ^.value <= n^.value)) DO BEGIN
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
    Assert(IsSorted(l), 'List not sorted after insert');
  END; (* Insert *)

  PROCEDURE WriteList(l: ListPtr);
    VAR
      n: NodePtr;
  BEGIN (* WriteList *)
    n := l;
    WHILE (n <> NIL) DO BEGIN
      Write(n^.value, '-> ');
      n := n^.next; (*set next node*)
    END; (* WHILE *)
    WriteLn('|');
  END; (* WriteList *)

  FUNCTION NumberOfNodes(l: ListPtr): INTEGER;
    VAR 
      i: INTEGER;
      n: NodePtr;
  BEGIN (* NumberOfNodes *)
    n := l;
    i := 0;
    WHILE (n <> NIL) DO BEGIN
      i := i + 1;
      n := n^.next;
    END; (* WHILE *)
    NumberOfNodes := i;
  END; (* NumberOfNodes *)

  FUNCTION NrOfOccurances(l: ListPtr; value : INTEGER): INTEGER;
    VAR
      node: NodePtr;
      count: INTEGER;
  BEGIN (* NrOfOccurances *)
    count := 0;
    node := l;
    WHILE (node <> NIL) DO BEGIN
      IF (node^.value = value) THEN BEGIN
        count := count +1;
      END; (* IF *)
      node := node^.next;
    END; (* WHILE *)
    NrOfOccurances := count;
  END; (* NrOfOccurances *)

  PROCEDURE BringToFront(VAR l: ListPtr; val: INTEGER);
    VAR
      pred, node: NodePtr;
  BEGIN (* BringToFront *)
    node := l;
    pred := NIL;
    WHILE ((node <> NIL) AND (node^.value <> val)) DO BEGIN
      pred := node;
      node := node^.next;
    END; (* WHILE *)
    IF ((node <> NIL) AND (pred <> NIL)) THEN BEGIN
      pred^.next := node^.next;
      node^.next := l;
      l := node;
    END; (* IF *)
  END; (* BringToFront *)
  
  PROCEDURE FilterLarger(VAR l: ListPtr; value: INTEGER);
    VAR
      pred, node: NodePtr;
  BEGIN (* FilterLarger *)
    node := l;
    pred := NIL;
    WHILE (node <> NIL) DO BEGIN
      IF ((node^.value <= value)) THEN BEGIN
        IF (pred = NIL) THEN BEGIN
          l := node^.next;
          WriteLn('test');
          Dispose(node);
          node := l;
        END ELSE BEGIN
          pred^.next := node^.next;
          Dispose(node);
          node := pred^.next;
        END;
      END ELSE BEGIN
        pred := node;
        node := node^.next;
      END;
    END; (* WHILE *)
  END; (* FilterLarger *)
   
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

  VAR
    l: ListPtr;
    s: ListPtr;

BEGIN (* SlListsPrg *)
  l := NewList;
  s := NewList;
  Apped(l, 2);
  Apped(l, 6);
  Apped(l, 4);
  Apped(l, 3);
  Apped(l, 10);
  Apped(l,2);
  WriteLn(NrOfOccurances(l,4));
  //BringToFront(l, 7);
  //BringToFront(l, 9);
  WriteList(l);
  FilterLarger(l,7);

  WriteList(l);
  Insert(s, 16);
  Insert(s, 20);
  Insert(s, 11);
  Insert(s, 12);
  WriteList(s);
  DisposeList(s);
  WriteList(s);
  WriteLn(NumberOfNodes(l));
  DisposeList(l);
  WriteLn(NumberOfNodes(l));
END. (* SlListsPrg *)
