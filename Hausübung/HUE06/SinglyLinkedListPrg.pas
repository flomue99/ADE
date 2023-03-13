(* SinglyLinkedList:                                         MFl, 2022-11-30 *)
(* ------                                                                    *)
(* remove maxnode of a list and return maxnode                               *)
(* ========================================================================= *)
PROGRAM SinglyLinkedList;
  
  TYPE
    ListNodePtr = ^ListNode;
    ListNode = RECORD
      next: ListNodePtr;
      data: INTEGER;
    END; (* ListNode *)
    ListPtr = ListNodePtr;
  
  FUNCTION NewNode(d: INTEGER): ListNodePtr;
    VAR 
      node: ListNodePtr;
  BEGIN (* NewNode *)
    New(node);
    node^.data := d;
    node^.next := NIL;
    NewNode := node;
  END; (* NewNode *)

  FUNCTION NewList: ListPtr;
  BEGIN (* NewList *)
    NewList := NIL;
  END; (* NewList *) 

  PROCEDURE PrependNode(VAR list: ListPtr; d: INTEGER);
    VAR
      node: ListNodePtr;
  BEGIN (* PrependNode *)
    node := NewNode(d);
    node^.next := list;
    list := node; 
  END; (* PrependNode *)

  PROCEDURE WriteList(list: ListPtr);
    VAR
      node: ListNodePtr;
  BEGIN (* WriteList *)
    node := list;
    WHILE (node <> NIL) DO BEGIN
      Write(node^.data, ' -> ');
      node := node^.next;
    END; (* WHILE *)
    Write('|');
    WriteLn();
  END; (* WriteList *)

  PROCEDURE WriteMaxNode(maxNode: ListNodePtr);
  BEGIN (* WriteMaxNode *)
    IF (maxNode = NIL) THEN BEGIN
      WriteLn('maxNode = NIL');
    END ELSE BEGIN
      WriteLn('maxNode: ', maxNode^.data);
    END; (* IF *)
  END; (* WriteMaxNode *)
 
  PROCEDURE RemoveMax(VAR list: ListPtr; VAR maxNode: ListNodePtr);
    VAR
      node, pred, predMax: ListNodePtr;
  BEGIN (* RemoveMax *)
    node := list;
    maxNode := NIL;
    pred := NIL;
    predMax := NIL;
    WHILE (node <> NIL) DO BEGIN
      IF ((maxNode = NIL ) OR (node^.data > maxNode^.data)) THEN BEGIN
        predMax := pred;
        maxNode := node;
      END; (* IF *)
      pred := node;
      node := node^.next;
    END; (* WHILE *)
    IF (maxNode = NIL) THEN BEGIN
      List := NIL;
    END ELSE BEGIN
      IF (predMax = NIL) THEN BEGIN
        list := list^.next; (* maxNode is the first node *)
      END ELSE BEGIN
        predMax^.next := maxnode^.next;
      END; (* IF *) 
      maxNode^.next := NIL;
    END; (* IF *)
  END; (* RemoveMax *)

  PROCEDURE DisposeList(VAR l: ListPtr);
    VAR
      n: ListNodePtr;
  BEGIN (* DisposeList *)
    WHILE (l <> NIL) DO BEGIN
      n := l^.next; 
      Dispose(l); 
      l := n; 
    END; (* WHILE *)
  END; (* DisposeList *)

  VAR
    list: ListPtr;
    maxNode: ListNodePtr;
BEGIN (* SinglyLinkedList *)
  list := NewList();
  WriteLn('Leer Liste: ');
  WriteList(list);
  WriteLn('MaxNode einer LeerenListe: ');
  RemoveMax(list, maxNode);
  WriteMaxNode(maxNode);
  PrependNode(list, -50);
  PrependNode(list, -13);
  PrependNode(list, -20);
  PrependNode(list, -10);
  PrependNode(list, -50);
  PrependNode(list, -25);
  WriteList(list);
  RemoveMax(list, maxNode);
  WriteMaxNode(maxNode);
  WriteList(list);
  Dispose(maxNode);
  maxNode := NIL;
  RemoveMax(list, maxNode);
  WriteMaxNode(maxNode);
  Dispose(maxNode);
  WriteList(List);
  DisposeList(list);
  
END. (* SinglyLinkedList *)
