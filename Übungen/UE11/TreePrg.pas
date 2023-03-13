(* TreePrg:                                                  MFL, 2022-12-21 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM TreePrg;
  TYPE
    NodePtr = ^Node;
    Node = RECORD
      left, right: NodePtr;
      value: INTEGER;
    END; (* Node *)
    TreePtr = NodePtr;

  FUNCTION NewNode(v: INTEGER): NodePtr;
    VAR
      n: NodePtr;
  BEGIN (* NewNode *)
    New(n);
    n^.value := v;
    n^.left := NIL;
    n^.right := NIL;
    NewNode := n;
  END; (* NewNode *)

  FUNCTION NewTree: TreePtr;
  BEGIN (*NewTree*)
    NewTree := NIL;
  END; (*NewTree*)

  FUNCTION TreeOf(v: INTEGER; lt, rt: TreePtr): TreePtr;
    VAR
      root: NodePtr;
  BEGIN (* TreeOf *)
    root := NewNode(v);
    root^.left := lt;
    root^.right := rt;
    TreeOf := root;
  END; (* TreeOf *)
  
  PROCEDURE WriteTreePreOrder(t: TreePtr);
  BEGIN (* WriteTreePreOrder *)
    IF(t <> NIL) THEN BEGIN
      Write(t^.value, ' ');
      WriteTreePreOrder(t^.left);
      WriteTreePreOrder(t^.right);
    END;
  END; (* WriteTreePreOrder *)

  PROCEDURE WriteTreeInOrder(t: TreePtr);
  BEGIN (* WriteTreeInOrder *)
    IF(t <> NIL) THEN BEGIN
      WriteTreeInOrder(t^.left);
      Write(t^.value, ' ');
      WriteTreeInOrder(t^.right);
    END;
  END; (* WriteTreeInOrder *)

  PROCEDURE WriteTreePostOrder(t: TreePtr);
  BEGIN (* WriteTreePostOrder *)
    IF(t <> NIL) THEN BEGIN
      WriteTreePostOrder(t^.left);
      WriteTreePostOrder(t^.right);
      Write(t^.value, ' ');
    END;
  END; (* WriteTreePostOrder *)

  PROCEDURE Insert(VAR t: TreePtr; n: NodePtr);  
  BEGIN (* Insert *)
    IF(t = NIL ) THEN BEGIN
    t := n;
    END ELSE IF(n^.value < t^.value) THEN BEGIN
        Insert(t^.left, n);
    END ELSE BEGIN
        Insert(t^.right, n);
    END;
  END; (* Insert *)

  FUNCTION Contains(VAR t: TreePtr; v: INTEGER): BOOLEAN;
  BEGIN (* Contains *)
    IF(t = NIL ) THEN BEGIN
      Contains := FALSE;
    END ELSE IF (v = t^.value) THEN BEGIN
      Contains := TRUE;
    END ELSE IF(v < t^.value) THEN BEGIN
        Contains := Contains(t^.left, v);
    END ELSE BEGIN
        Contains := Contains(t^.right, v);
    END;
  END; (* Contains *)

  FUNCTION Find(VAR t: TreePtr; v: INTEGER): NodePtr;
  BEGIN (* Find *)
    IF((t = NIL ) OR (v = t^.value)) THEN BEGIN
      Find := t;
    END ELSE IF(v < t^.value) THEN BEGIN
        Find := Find(t^.left, v);
    END ELSE BEGIN
        Find := Find(t^.right, v);
    END;
  END; (* Find *)

  PROCEDURE DisposeTree(VAR t: TreePtr);
  BEGIN (* DisposeTree *)
    IF(t <> NIL) THEN BEGIN
      DisposeTree(t^.left);
      DisposeTree(t^.right);
      Dispose(t);
      t := NIL;
    END;
  END; (* DisposeTree *)

  PROCEDURE Remove(VAR t: TreePtr; v: INTEGER);
    VAR 
      n, parent, child, succ, succParnet: NodePtr;
  BEGIN (* Remove *)
    parent := NIL;
    n := t;
    WHILE ((n <> NIL) AND (n^.value <> v)) DO BEGIN (* find node n to remove*)
      parent := n;
      IF(v < n^.value) THEN BEGIN
        n := n^.left;
      END ELSE BEGIN
        n := n^.right;
      END;
    END; (* WHILE *)
    IF(n = NIL) THEN Exit; (* value v not found *)

    IF(n^.right = NiL) THEN BEGIN  (* replace n by left child*)
      child := n^.left;
    END ELSE IF(n^.right^.left = NIL) THEN BEGIN (* repalce n by right child and add left child as left child of right child *)
      child := n^.right;
      child^.left := n^.left;
    END ELSE BEGIN (* replace n by next larger value in t *)
      succ := n^.right^.left;
      succParnet := n^.right;
      WHILE (succ^.left <> NIL) DO BEGIN
        succParnet := succ;
        succ := succ^.left;
      END; (* WHILE *)
      succParnet^.left := succ^.right;
      child := succ;
      child^.left := n^.left;
      child^.right := n^.right;
    END;
    IF(parent = NIL) THEN BEGIN (* repalce root node *)
      t := child;
    END ELSE IF(n^.value < parent^.value) THEN BEGIN
      parent^.left := child;
    END ELSE BEGIN
      parent^.right := child;
    END; 
    Dispose(n);
  END; (* Remove *)

  FUNCTION CountEvenNodes(t: TreePtr; data: INTEGER): INTEGER;
  BEGIN (* Count EvenNodes *)
    IF (t = NIL) THEN BEGIN
      CountEvenNodes := 0;
    END; (* IF *)
    IF (t <> NIL) THEN BEGIN
      IF (t^.value = data) THEN BEGIN
        CountEvenNodes := 1 + CountEvenNodes(t^.left, data) + CountEvenNodes(t^.right,data);
      END ELSE BEGIN
        CountEvenNodes := CountEvenNodes(t^.left, data) + CountEvenNodes(t^.right,data);
      END; (* IF *)
    END; (* IF *)
  END; (* Count EvenNodes *)

  PROCEDURE WriteTreeGraphically(t: TreePtr);
    PROCEDURE WTGRec(t: TreePtr; l: INTEGER);
    BEGIN (* WTGRec *)
      IF(t <> NIL) THEN BEGIN
      WTGRec(t^.right, l + 1); 
      WriteLn('  ':l * 4, t^.value, '<');
      WTGRec(t^.left, l + 1);
      END;
    END; (* WTGRec *)
  BEGIN (* WriteTreeGraphically *)
    WTGRec(t, 0);
  END; (* WriteTreeGraphically *)

  VAR
    t: TreePtr;
BEGIN (* TreePrg *)
  t := TreeOf(9,
             TreeOf(5, 
                    NewNode(3), 
                    TreeOf(7,
                         NIL,
                         NewNode(8))),
             TreeOf(12,
                    NewNode(7), 
                    NIL));
  WriteLn('PreOrder');           
  WriteTreePreOrder(t);
  WriteLn();
  WriteLn('Inorder'); 
  WriteTreeInOrder(t);
  WriteLn();
  WriteLn('Postorder');
  WriteTreePostOrder(t);
  WriteLn();
  Insert(t, (NewNode(6)));
  Insert(t, (NewNode(12)));
  Insert(t, (NewNode(14)));
  Insert(t, (NewNode(12)));
  WriteTreeInOrder(t);
  WriteLn();
  WriteTreeGraphically(t);
  WriteLn(Find(t,14) <> NIL);
  WriteLn(Find(t,15) <> NIL);
  WriteLn('EvenNodes 7: ', CountEvenNodes(t,7));
  Remove(t,12);
  WriteTreeInOrder(t);
  WriteLn();
  Remove(t,6);
  WriteTreeInOrder(t);
  WriteLn();
  Remove(t,9);
  WriteTreeInOrder(t);
  WriteLn();
  Remove(t,12);
  WriteTreeInOrder(t);
  WriteLn();
  Remove(t,5);
  WriteTreeInOrder(t);
  WriteLn();

  DisposeTree(t);
END. (* TreePrg *)
