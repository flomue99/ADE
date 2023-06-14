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
  
  List = ^ListObj;
  ListObj = OBJECT
              list: ListPtr;
              listSize: INTEGER;
              CONSTRUCTOR Init;
              PROCEDURE Add(val: INTEGER); VIRTUAL;
              FUNCTION Contains(val: INTEGER): BOOLEAN; VIRTUAL;
              FUNCTION Size: INTEGER;
              PROCEDURE Remove(val: INTEGER);
              PROCEDURE Clear;
              PROCEDURE WriteList;
            END;
  
  SortedList = ^SortedListObj;
  SortedListObj = OBJECT(ListObj)
            //CONSTRUCTOR Init;
            PROCEDURE Add(val: INTEGER); VIRTUAL;   
            FUNCTION Contains(val: INTEGER): BOOLEAN; VIRTUAL;
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
  IF (list = NIL) THEN BEGIN
    list := n;
  END ELSE BEGIN
    curr := list;
    WHILE (curr^.next <> NIL) DO BEGIN
      curr := curr^.next; 
    END; (* WHILE *)
    curr^.next := n; 
  END; (* IF *)
END; (* ListObj.Add *)

FUNCTION ListObj.Contains(val: INTEGER): BOOLEAN;
  VAR
    n: NodePtr;
    found: BOOLEAN;
BEGIN (* ListObj.Contains *)
  n := list;
  found := FALSE;
  WHILE (Not found) AND (n <> NIL) AND (n^.value <= val)DO BEGIN
    IF (n^.value = val) THEN BEGIN
      found := TRUE;
    END ELSE BEGIN
      n := n^.next;  
    END; (* IF *)
  END; (* WHILE *)
  Contains := found;
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
      n := list^.next; 
      Dispose(list); 
      List := n;
  END; (* WHILE *)
END; (* ListObj.Clear *)

PROCEDURE ListObj.WriteList;
  VAR
    n: NodePtr;
BEGIN (* ListObj.Write *)
  n := list;
    WHILE (n <> NIL) DO BEGIN
      Write(n^.value, '-> ');
      n := n^.next;
    END; (* WHILE *)
  WriteLn('|');
END; (* ListObj.Write *)

PROCEDURE SortedListObj.Add(val: INTEGER);
  VAR
    n: NodePtr;
    pred, succ: NodePtr;
BEGIN (* SortedListObj.Add *)
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
END; (* SortedListObj.Add *)

FUNCTION SortedListObj.Contains(val: INTEGER): BOOLEAN;
  VAR
    n: NodePtr;
BEGIN (* SortedListObj.Contains *)
  n := list;
  WHILE (n <> NIL) AND (n^.value <> val)DO BEGIN
    n := n^.next;
  END; (* WHILE *)
  Contains := n <> NIL;
END; (* SortedListObj.Contains *)

VAR
  l: List;
  sl: SortedList;
BEGIN (* OOPListPrg *)
  New(l, Init);
  l^.Add(1);
  l^.Add(4);
  l^.Add(13);
  l^.Add(0);
  l^.Add(-1);
  l^.Add(13);
  l^.WriteList;
  WriteLn('Size = ', l^.Size);
  WriteLn('Contains(5) = ', l^.Contains(5));
  WriteLn('Contains(13) = ', l^.Contains(13));
  l^.Remove(13);
  WriteLn('Contains(13) =', l^.Contains(13));
  WriteLn('Size = ', l^.Size);
  l^.Remove(1);
  WriteLn('Size = ', l^.Size);
  l^.WriteList;
  l^.Clear;
  Dispose(l);
  WriteLn('.................................');
  New(sl, Init);
  sl^.Add(1);
  sl^.Add(4);
  sl^.Add(13);
  sl^.Add(0);
  sl^.Add(-1);
  sl^.Add(13);
  sl^.WriteList;
  WriteLn('Size = ', sl^.Size);
  WriteLn('Contains(5) =', sl^.Contains(5));
  WriteLn('Contains(13) =', sl^.Contains(13));
  sl^.Remove(13);
  WriteLn('Contains(13) =', sl^.Contains(13));
  WriteLn('Size = ', sl^.Size);
  sl^.Remove(1);
  WriteLn('Size = ', sl^.Size);
  sl^.WriteList;
  sl^.Clear;
  Dispose(sl);
END. (* OOPListPrg *)