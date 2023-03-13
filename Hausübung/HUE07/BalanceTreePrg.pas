(* BalanceTreePrg:                                           MFl, 2023-01-03 *)
(* ------                                                                    *)
(* Balance an unbalanced tree                                                *)
(* ========================================================================= *)
PROGRAM BalanceTreePrg;
  USES
    DynArrayUnit; (* DynArrayUnit from teaching lesson *)

  FUNCTION NewTree: TreePtr;
  BEGIN (*NewTree*)
    NewTree := NIL;
  END; (*NewTree*)

  FUNCTION NewNode(data: INTEGER): TreeNodePtr;
    VAR
      n: TreeNodePtr;
  BEGIN (* NewNode *)
    New(n);
    n^.data := data;
    n^.left := NIL;
    n^.right := NIL;
    NewNode := n;
  END; (* NewNode *)

  PROCEDURE Insert(VAR t: TreePtr; n: TreeNodePtr);  
  BEGIN (* Insert *)
    IF(t = NIL ) THEN BEGIN
      t := n;
    END ELSE IF(n^.data = t^.data) THEN BEGIN (* to avoid double data *)
      Dispose(n);
    END ELSE IF(n^.data < t^.data) THEN BEGIN
      Insert(t^.left, n);
    END ELSE BEGIN
      Insert(t^.right, n);
    END;
  END; (* Insert *)

  FUNCTION CountNodes(t: TreePtr): INTEGER;
  BEGIN (* CountNodes *)
    IF (t = NIL) THEN BEGIN
      CountNodes := 0;
    END ELSE BEGIN
      CountNodes := 1 + CountNodes(t^.right) + CountNodes(t^.left);
    END; (* IF *)
  END; (* CountNodes *)

  PROCEDURE WriteTreeInOrder(t: TreePtr);
  BEGIN (* WriteTreeInOrder *)
    IF(t <> NIL) THEN BEGIN
      WriteTreeInOrder(t^.left);
      Write(t^.data, ' ');
      WriteTreeInOrder(t^.right);
    END;
  END; (* WriteTreeInOrder *)

  PROCEDURE WriteTreeGraphically(t: TreePtr); (* Procedure from ADE lesson to print the tree *)
    PROCEDURE WTGRec(t: TreePtr; l: INTEGER);
    BEGIN (* WTGRec *)
      IF(t <> NIL) THEN BEGIN
      WTGRec(t^.right, l + 1);
      WriteLn('  ':l * 5, t^.data, '<');
      WTGRec(t^.left, l + 1);
      END;
    END; (* WTGRec *)
  BEGIN (* WriteTreeGraphically *)
    WTGRec(t, 0);
  END; (* WriteTreeGraphically *)

  PROCEDURE DisposeTree(VAR t: TreePtr);
  BEGIN (* DisposeTree *)
    IF(t <> NIL) THEN BEGIN
      DisposeTree(t^.left);
      DisposeTree(t^.right);
      Dispose(t);
      t := NIL;
    END;
  END; (* DisposeTree *)

  Procedure FillDynArray(VAR dynArray: DynArrayPtr; VAR i: INTEGER; tree: TreePtr);
  BEGIN (* FillDynArray *)
    IF(tree <> NIL) THEN BEGIN
      FillDynArray(dynArray, i, tree^.left);
      SetValueAt(dynArray, i, tree);
      i := i + 1;
      FillDynArray(dynArray, i, tree^.right);
    END;
  END; (* FillDynArray *)

  PROCEDURE Balance(VAR tree: TreePtr; dynArray: DynArrayPtr; left: INTEGER; right: INTEGER);
    VAR
      i: INTEGER;
  BEGIN (* Balance *)
    i := ((left + right) DIV 2); //Calculate index of value
    IF ((left > right)) THEN BEGIN
      tree := NIL;
    END ELSE BEGIN
      tree := GetValueAt(dynArray, i);
      Balance(tree^.left, dynArray, left, (i - 1)); (* index -1, so ((left + (index -1)) DIV 2) = middle index of left side*)
      Balance(tree^.right, dynArray, (i + 1) , right); (* index +1, so (((index +1) + right) DIV 2) = middle index of right side*)
    END; (* IF *)
  END; (* Balance *)

  VAR
    tree: TreePtr;
    dynArray: DynArrayPtr;
    i: INTEGER;
BEGIN (* BalanceTreePrg *)
  tree := NewTree();
  i := 1;
  Insert(tree, (NewNode(12410)));
  Insert(tree, (NewNode(1000)));
  Insert(tree, (NewNode(117)));
  Insert(tree, (NewNode(19)));
  Insert(tree, (NewNode(-200)));
  Insert(tree, (NewNode(123)));
  Insert(tree, (NewNode(53)));
  Insert(tree, (NewNode(21)));
  Insert(tree, (NewNode(33)));
  Insert(tree, (NewNode(0)));
  Insert(tree, (NewNode(10)));
  Insert(tree, (NewNode(1)));
  Insert(tree, (NewNode(5)));
  Insert(tree, (NewNode(12)));
  Insert(tree, (NewNode(4)));
  Insert(tree, (NewNode(-3)));
  Insert(tree, (NewNode(-14)));
  Insert(tree, (NewNode(-13)));
  Insert(tree, (NewNode(44)));
  Insert(tree, (NewNode(-35)));
  Insert(tree, (NewNode(45)));
  Insert(tree, (NewNode(-13)));
  Insert(tree, (NewNode(250)));
  Insert(tree, (NewNode(2)));
  Insert(tree, (NewNode(50)));
  Insert(tree, (NewNode(-330)));
  Insert(tree, (NewNode(0)));
  Insert(tree, (NewNode(-1455)));
  WriteTreeGraphically(tree);
  WriteTreeInOrder(tree);
  WriteLn();
  WriteLn('-------------------------------------------------------------------------------');
  dynArray := NewDynArray(CountNodes(tree));
  FillDynArray(dynArray, i , tree);
  Balance(tree, dynArray, 1 , dynArray^.size);
  WriteLn();
  WriteLn();
  WriteLn();
  WriteTreeGraphically(tree);
  WriteTreeInOrder(tree);
  WriteLn();
  DisPoseDynArray(dynArray);
  DisposeTree(tree);
END. (* BalanceTreePrg *)
