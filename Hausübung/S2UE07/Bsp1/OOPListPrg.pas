(* OOPListPrg:                                               MFL, 2023-05-11 *)
(* ------                                                                    *)
(* list with oop                                                             *)
(* ========================================================================= *)
PROGRAM OOPListPrg;

TYPE 
  NodePtr = ^Node;
  Node = RECORD
    value: INTEGER;
    next: NodePtr;
  END; (* Node *)
  ListPtr = NodePtr;
  
  L = ^ListObj;
  ListObj = OBJECT
              list: ListPtr;
              listSize: INTEGER;
              CONSTRUCTOR Init;
              PROCEDURE Add(val: INTEGER); VIRTUAL;
              FUNCTION Contains(val: INTEGER): BOOLEAN;
              FUNCTION Size: INTEGER;
              PROCEDURE Remove(val: INTEGER);
              PROCEDURE Clear;
              PROCEDURE WriteList;
              DESTRUCTOR Done; VIRTUAL;
            END;
  
  SL = ^SLObj;
  SLObj = OBJECT(ListObj)
            PROCEDURE Add(val: INTEGER); VIRTUAL;
          END;

CONSTRUCTOR ListObj.Init;
BEGIN (* ListObj.Init *)
  list := NIL;
  listSize := 0;
END; (* ListObj.Init *)

FUNCTION NewNode(val: INTEGER): NodePtr;
  VAR
    n: NodePtr;
BEGIN (* NewNode *)
  New(n);
  n^.value := val;
  n^.next := NIL;
  NewNode := n;
END; (* NewNode *)

PROCEDURE ListObj.Add(val: INTEGER);
  VAR
    n, curr: NodePtr;
BEGIN (* ListObj.Add *)
  n := NewNode(val);
  Inc(listSize);
  IF (list = NIL) THEN BEGIN  (*falls l NIL, wird neuer Knoten Kopf der Liste*)
    list := n;
  END ELSE BEGIN
    curr := list;
    WHILE (curr^.next <> NIL) DO BEGIN
      curr := curr^.next; (*zeigt auf nächsten*)
    END; (* WHILE *)
    curr^.next := n; (*letzes element zeigt nun auf n*)
  END; (* IF *)
END; (* ListObj.Add *)

FUNCTION ListObj.Contains(val: INTEGER): BOOLEAN;
  VAR
    n: NodePtr;
BEGIN (* ListObj.Contains *)
  n := list;
  WHILE (n <> NIL) AND (n^.value <> val)DO BEGIN
    n := n^.next;
  END; (* WHILE *)
  Contains := n <> NIL;
END; (* ListObj.Contains *)

FUNCTION ListObj.Size: INTEGER;
BEGIN (* ListObj.Size *)
  Size := listSize;
END; (* ListObj.Size *)

PROCEDURE ListObj.Remove(val: INTEGER);
  VAR
    pred, succ: NodePtr;
BEGIN (* ListObj.Remove *)
  WHILE (Contains(val)) DO BEGIN
    Dec(listSize);
    pred := NIL;
    succ := list;   
    IF (succ^.value = val) THEN BEGIN
      list := succ^.next;
      Dispose(succ);
    END ELSE BEGIN
      WHILE (succ <> NIL) AND (succ^.value <> val) DO BEGIN
        pred := succ;
        succ := succ^.next;
      END; (* WHILE *)
      IF (succ <> NIL) THEN BEGIN
        pred^.next := succ^.next;
        Dispose(succ);
      END; (* IF *)   
    END; (* IF *)
  END; (* WHILE *) 
END; (* ListObj.Remove *)


PROCEDURE ListObj.Clear;
  VAR
    n: NodePtr;
BEGIN (* ListObj.Clear *)
  WHILE (list <> NIL) DO BEGIN
      n := list^.next;  (*setzten auf zweites elemnts*)
      Dispose(list); (* dispose kopf*)
      List := n; (*neuer Kopf ist n*)
  END; (* WHILE *)
END; (* ListObj.Clear *)

PROCEDURE ListObj.WriteList;
  VAR
    n: NodePtr;
BEGIN (* ListObj.Write *)
  n := list;
    WHILE (n <> NIL) DO BEGIN
      Write(n^.value, '-> ');
      n := n^.next; (*set next node*)
    END; (* WHILE *)
  WriteLn('|');
END; (* ListObj.Write *)

// needed?ß
DESTRUCTOR ListObj.Done;
BEGIN (* ListObj.Done *)
END; (* ListObj.Done *)

PROCEDURE SLObj.Add(val: INTEGER);
  VAR
    n: NodePtr;
    pred, succ: NodePtr;
BEGIN (* SLObj.Add *)
  n := NewNode(val);
  Inc(listSize);
  IF (list = NIL) THEN BEGIN
      list := n;
  END ELSE BEGIN
    succ := list;
    pred := NIL;
    WHILE ((succ <> NIL) AND (succ^.value <= n^.value)) DO BEGIN
      pred := succ;
      succ := succ^.next;
    END; (* WHILE *)
    n^.next := succ;
    IF (pred = NIL) THEN BEGIN
      list := n;
    END ELSE BEGIN
      pred^.next := n;
    END; (* IF *)
  END; (* IF *)
END; (* SLObj.Add *)

VAR
  list: L;
  sortedList: SL;
BEGIN (* OOPListPrg *)
  New(list, Init);
  list^.Add(1);
  list^.Add(4);
  list^.Add(13);
  list^.Add(0);
  list^.Add(-1);
  list^.Add(13);
  list^.WriteList;
  WriteLn(list^.Size);
  WriteLn(list^.Contains(5));
  WriteLn(list^.Contains(13));
  list^.Remove(13);
  WriteLn(list^.Contains(13));
  WriteLn(list^.Size);
  list^.Remove(1);
  WriteLn(list^.Size);
  list^.WriteList;
  list^.Clear;
  Dispose(list);
  WriteLn('.................................');
  New(sortedList, Init);
  sortedList^.Add(1);
  sortedList^.Add(4);
  sortedList^.Add(13);
  sortedList^.Add(0);
  sortedList^.Add(-1);
  sortedList^.Add(13);
  sortedList^.WriteList;
  WriteLn(sortedList^.Size);
  WriteLn(sortedList^.Contains(5));
  WriteLn(sortedList^.Contains(13));
  sortedList^.Remove(13);
  WriteLn(sortedList^.Contains(13));
  WriteLn(sortedList^.Size);
  sortedList^.Remove(1);
  WriteLn(sortedList^.Size);
  sortedList^.WriteList;
  sortedList^.Clear;
  Dispose(sortedList);
END. (* OOPListPrg *)