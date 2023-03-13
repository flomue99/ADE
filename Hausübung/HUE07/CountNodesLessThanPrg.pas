(* CountNodesLessThanPrg:                                    MFL, 2022-12-29 *)
(* ------                                                                    *)
(* Calculate the amount of nodes with a smaller value the x                  *)
(* ========================================================================= *)
PROGRAM CountNodesLessThanPrg;
  TYPE
    TreeNodePtr = ^TreeNode;
    TreeNode = RECORD
      left, right: TreeNodePtr;
      data: INTEGER;
    END; (* TreeNode *)
    TreePtr = TreeNodePtr;

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
    END ELSE IF(n^.data < t^.data) THEN BEGIN
        Insert(t^.left, n);
    END ELSE BEGIN
        Insert(t^.right, n);
    END;
  END; (* Insert *)

  FUNCTION CountNodesLessThan(tree: TreePtr; x: INTEGER): INTEGER;
  BEGIN (* CountNodesLessThan *)
    IF (tree = NIL) THEN BEGIN
      CountNodesLessThan := 0;
    END ELSE BEGIN
      IF(tree^.data < x) THEN BEGIN
        CountNodesLessThan := 1 + CountNodesLessThan(tree^.right, x) + CountNodesLessThan(tree^.left, x);
      END ELSE BEGIN
        CountNodesLessThan := CountNodesLessThan(tree^.right, x) + CountNodesLessThan(tree^.left, x);
      END;
    END; (* IF *)
  END; (* CountNodesLessThan *)

  PROCEDURE DisposeTree(VAR t: TreePtr);
  BEGIN (* DisposeTree *)
    IF(t <> NIL) THEN BEGIN
      DisposeTree(t^.left);
      DisposeTree(t^.right);
      Dispose(t);
      t := NIL;
    END;
  END; (* DisposeTree *)

  PROCEDURE WriteTreeInOrder(tree: TreePtr);
  BEGIN (* WriteTreeInOrder *)
    IF(tree <> NIL) THEN BEGIN
      WriteTreeInOrder(tree^.left);
      Write(tree^.data, ' ');
      WriteTreeInOrder(tree^.right);
    END;
  END; (* WriteTreeInOrder *)

  VAR
    tree: TreePtr;
BEGIN (* CountNodesLessThanPrg *)
  tree := NewTree();
  WriteLn('Amount of Nodes smaller than 10: ', CountNodesLessThan(tree, 10));
  Insert(tree, (NewNode(10)));
  Insert(tree, (NewNode(1)));
  Insert(tree, (NewNode(5)));
  Insert(tree, (NewNode(12)));
  Insert(tree, (NewNode(4)));
  Insert(tree, (NewNode(-3)));
  Insert(tree, (NewNode(-14)));
  Insert(tree, (NewNode(117)));
  Insert(tree, (NewNode(19)));
  Insert(tree, (NewNode(123)));
  Insert(tree, (NewNode(53)));
  Insert(tree, (NewNode(21)));
  Insert(tree, (NewNode(33)));
  Insert(tree, (NewNode(0)));
  Insert(tree, (NewNode(-13)));
  Insert(tree, (NewNode(-2)));
  WriteTreeInOrder(tree);
  WriteLn();
  WriteLn('Amount of Nodes smaller than -5: ', CountNodesLessThan(tree, -5));
  DisposeTree(tree);
END. (* CountNodesLessThanPrg *)
