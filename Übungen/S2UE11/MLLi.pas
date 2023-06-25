(* MLLi:                                                    MFL, 2023-06-19 *)
(* ------                                                                    *)
(*                                                                           *)
(* ========================================================================= *)
UNIT MLLi;

INTERFACE

USES MLColl, MLObj;

TYPE

  NodePtr = ^Node;
  Node = RECORD
    obj: MLObject;
    next: NodePtr;
  END; (* Node *)
  ListPtr = NodePtr;

  MLList = ^MLListObj;
  MLListObj = OBJECT(MLCollectionObj)
                PRIVATE 
                  listSize: INTEGER;
                  l: ListPtr;
                PUBLIC
                  CONSTRUCTOR Init;
                  FUNCTION Size: INTEGER; VIRTUAL;
                  PROCEDURE Add(o: MLObject); VIRTUAL;
                  FUNCTION Remove(o: MLObject): MLObject; VIRTUAL;
                  FUNCTION Contains(o: MLObject): BOOLEAN; VIRTUAL;
                  PROCEDURE Clear; VIRTUAL;
                  FUNCTION NewIterator: MLIterator; VIRTUAL;
                  PROCEDURE Prepend(o: MLObject);
                  DESTRUCTOR Done; VIRTUAL;
              END;
  
  MLListIterator = ^MLListIteratorObj;
  MLListIteratorObj = OBJECT(MLIteratorObj)                        l:  MLList;
                        curNode: NodePtr;
                        CONSTRUCTOR Init(list: MLList);
                        FUNCTION Next: MLObject; VIRTUAL;
                      END;

  FUNCTION NewMLList: MLList;

IMPLEMENTATION

  USES 
    MetaInfo;
(*-----------------MLList---------------------*)
FUNCTION NewMLList: MLList;
  VAR
    l: MLList;
BEGIN (* NewMLList *)
  New(l, Init);
  NewMLList := l;
END; (* NewMLList *)

FUNCTION NewList: ListPtr;
BEGIN (* NewList *)
  NewList := NIL;
END; (* NewList *)

FUNCTION NewNode(o: MLObject): NodePtr;
  VAR
    n: NodePtr;
BEGIN (* NewNode *)
  New(n);
  n^.obj := o;  
  n^.next := NiL;
  NewNode := n;
END; (* NewNode *)

CONSTRUCTOR MLListObj.Init;
BEGIN (* MLListObj.Init *)
  INHERITED Init;
  Register('MLList', 'MLCollection');
  SELF.listSize := 0;
  SELF.l := NewList;
END; (* MLListObj.Init *)

PROCEDURE MLListObj.Add(o: MLObject);
  VAR
   n: NodePtr;
   curr: NodePtr;
BEGIN (* MLListObj.Add *)
  n := NewNode(o);
  IF (l = NIL) THEN BEGIN
    l := n;
  END ELSE BEGIN
    curr := l;
    WHILE (curr^.next <> NIL) DO BEGIN
      curr := curr^.next;
    END; (* WHILE *)
    curr^.next := n;
  END; (* IF *)
  Inc(listSize); 
END; (* MLListObj.Add *)

PROCEDURE MLListObj.Prepend(o: MLObject);
  VAR
   n: NodePtr;
BEGIN (* MLListObj.Prepend *)
  n := NewNode(o);
  n^.next := l;
  l := n;
  Inc(listSize);
END; (* MLListObj.Prepend *)

FUNCTION MLListObj.Size: INTEGER;
BEGIN (* MLListObj.Size *)
  Size := listSize;
END; (* MLListObj.Size *)

FUNCTION MLListObj.Remove(o: MLObject): MLObject;
  VAR
    pred, succ: NodePtr;
    result: MLObject;
BEGIN (* MLListObj.Remove *)
  Dec(listSize);
  pred := NIL;
  succ := l;  
  result := NIL; 
  IF (succ^.obj = o) THEN BEGIN
    l := succ^.next;
    Dispose(succ);
  END ELSE BEGIN
    WHILE (succ <> NIL) AND (succ^.obj <> o) DO BEGIN
      pred := succ;
      succ := succ^.next;
    END; (* WHILE *)
    IF (succ <> NIL) THEN BEGIN
      pred^.next := succ^.next;
      result := succ^.obj;
      Dispose(succ);
    END; (* IF *)   
  END; (* IF *) 
  Remove := result;
END; (* MLListObj.Remove *)

FUNCTION MLListObj.Contains(o: MLObject): BOOLEAN;
  VAR
    n: NodePtr;
    found: BOOLEAN;
BEGIN (* MLListObj.Contains *)
  n := l;
  found := FALSE;
  WHILE (Not found) AND (n <> NIL)DO BEGIN
    IF (n^.obj = o) THEN BEGIN
      found := TRUE;
    END ELSE BEGIN
      n := n^.next;  
    END; (* IF *)
  END; (* WHILE *)
  Contains := found;
END; (* MLListObj.Contains *)

PROCEDURE MLListObj.Clear;
  VAR
   n: NodePtr;
BEGIN (* MLListObj.Clear *)
  n := l;
  WHILE (n <> NIL) DO BEGIN
    n := n^.next;
    Dispose(l^.obj, Done);
    Dispose(l);
    l := n;
  END; (* WHILE *)
  listSize := 0;
END; (* MLListObj.Clear *)


DESTRUCTOR MLListObj.Done;
BEGIN (* MLListObj.Done *)
  SELF.Clear;
  INHERITED Done;
END; (* MLListObj.Done *)


(*-----------------MLListIterator---------------------*)
FUNCTION MLListObj.NewIterator: MLIterator;
  VAR
    it: MLListIterator;
BEGIN (* MLListObj.NewIterator *)
  NEW(it, Init(@SELF));
  NewIterator := it;
END; (* MLListObj.NewIterator *)

CONSTRUCTOR MLListIteratorObj.Init(list: MLList);
BEGIN
  INHERITED Init;
  Register('MLListIterator', 'MLIterator');
  l := list;
  curNode := l^.l;
END;

FUNCTION MLListIteratorObj.Next: MLObject;
  VAR
    o: MLObject;
BEGIN (* MLListIteratorObj.Next *)
  o := NIL;
  IF (curNode <> NIL) THEN BEGIN
    o:= curNode^.obj;
    curNode := curNode^.next;
  END; (* IF *)
  Next := o;
END; (* MLListIteratorObj.Next *)

BEGIN (* MLLi *)
  
END. (* MLLi *)