(*  TreeUnit:                                             MFL, 2023-05-09 *)
(* ------                                                                    *)
(* Tree type, Procedures and functions for the tree optimization             *)
(* ========================================================================= *)


UNIT TreeUnit;

INTERFACE
TYPE
    NodePtr = ^Node;
    Node = RECORD
    left, right: NodePtr;
    val: STRING; 
    valType: INTEGER; (* 1 = operation, 2 = numberString, r = indentString *)
    END; (* Node *)
    TreePtr = NodePtr;
  
  FUNCTION NewNode(v: STRING; vType: INTEGER): NodePtr;
  PROCEDURE WriteTreeGraphically(t: TreePtr);
  PROCEDURE DisposeTree(VAR t: TreePtr);
  FUNCTION CalcNewValue(VAR n1, n2: NodePtr; op: STRING): STRING;
  FUNCTION ConnectNodes(n, l, r: NodePtr): NodePtr;

IMPLEMENTATION

  FUNCTION NewNode(v: STRING; vType: INTEGER): NodePtr;
    VAR 
      n: NodePtr;
  BEGIN (* NewNode *)
    New(n);
    n^.val := v;
    n^.valType := vType;
    n^.left := NIL;
    n^.right := NIL;
    NewNode := n;
  END; (* NewNode *)

  PROCEDURE WriteTreeGraphically(t: TreePtr);
    PROCEDURE WTGRec(t: TreePtr; l: INTEGER);
    BEGIN (* WTGRec *)
      IF(t <> NIL) THEN BEGIN
      WTGRec(t^.right, l + 1); 
      WriteLn('  ':l * 4, t^.val, '<');
      WTGRec(t^.left, l + 1);
      END;
    END; (* WTGRec *)
  BEGIN (* WriteTreeGraphically *)
    WTGRec(t, 1);
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

  FUNCTION ConnectNodes(n, l, r: NodePtr): NodePtr;
  BEGIN (* ConnectNodes *)
    n^.left := l;
    n^.right := r;
    ConnectNodes := n;
  END; (* ConnectNodes *)

  FUNCTION CalcNewValue(VAR n1, n2: NodePtr; op: STRING): STRING;
    VAR 
      resultStr: STRING;
      val1, val2, resultVal: INTEGER;
  BEGIN (* CalcNewValue *)
    Val(n1^.val, val1);
    val(n2^.val, val2);
    IF (op = '+') THEN BEGIN
      resultVal := val1 + val2;
    END ELSE IF(op = '-') THEN BEGIN
      resultVal := val1 - val2;
    END ELSE IF(op = '/') THEN BEGIN
      resultVal := val1 DIV val2;
    END ELSE BEGIN
      resultVal := val1 * val2;
    END; (* IF *)
    Dispose(n1);
    Dispose(n2);
    Str(resultVal, resultStr);
    CalcNewValue := resultStr;
  END; (* CalcNewValue *)

BEGIN (* TreeUnit *)
  
END. (* TreeUnit *)