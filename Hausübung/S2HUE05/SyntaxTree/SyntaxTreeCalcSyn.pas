(* SyntaxTreeCalcSyn:                                        MFL, 2023-05-02 *)
(* ------                                                                    *)
(* CalcSyn Unit                                                              *)
(* ========================================================================= *)
UNIT SyntaxTreeCalcSyn;

INTERFACE
  VAR
    success: BOOLEAN;
  
  PROCEDURE Start;

IMPLEMENTATION
  USES SyntaxTreeCalcLex;
  TYPE
    NodePtr = ^Node;
    Node = RECORD
    firstChild, sibling: NodePtr;
    val: STRING; (* nonterminal, operator or operand as string *)
    id: INTEGER;
    END; (* Node *)
    TreePtr = NodePtr;

  VAR
    globalId :INTEGER;

  PROCEDURE Expr(VAR e: TreePtr); FORWARD;
  PROCEDURE Term(VAR t: TreePtr); FORWARD;
  PROCEDURE Fact(VAR f: NodePtr); FORWARD;

  FUNCTION NewNode(v: STRING): NodePtr;
    VAR 
      n: NodePtr;
  BEGIN (* NewNode *)
    New(n);
    Inc(globalId);
    n^.val := v;
    n^.id := globalId;
    n^.firstChild := NIL;
    n^.sibling := NIL;
    NewNode := n;
  END; (* NewNode *)

  PROCEDURE WriteNodes(t: TreePtr);
  BEGIN (* WriteNodes *)
    IF(t <> NIL) THEN BEGIN
      IF (t^.id > 0) THEN BEGIN
       WriteLn('n',t^.id, '[label="', t^.val,'"];');  
      END; (* IF *)
      WriteNodes(t^.firstChild);
      WriteNodes(t^.sibling);
    END;
  END; (* WriteNodes *)

  PROCEDURE WriteCanonicConnections(t: TreePtr);
  BEGIN (* WriteCanonicConnections *)
    IF(t <> NIL) THEN BEGIN
      IF (t^.firstChild <> NIL) THEN BEGIN
        WriteLn('n', t^.id, ' -> ','n', t^.firstChild^.id,';')
      END; (* IF *)
      IF (t^.sibling <> NIL) THEN BEGIN
        WriteLn('n', t^.id, ' -> ','n', t^.sibling^.id,';');
      END; (* IF *)
      WriteCanonicConnections(t^.firstChild);
      WriteCanonicConnections(t^.sibling);
    END;
  END; (* WriteCanonicConnections *)

  PROCEDURE WriteRanks(t: TreePtr);
    VAR
      n: NodePtr;
  BEGIN (* WriteRanks *)
    IF (t <> NIL) THEN BEGIN
      IF (t^.sibling <> NIL) THEN BEGIN
        n := t^.sibling;
        Write('{rank=same; ','n', t^.id, ' n',t^.sibling^.id);
        n := n^.sibling;
        WHILE (n <> NIL) DO BEGIN
          Write(' n', n^.id);
          n := n^.sibling
        END; (* WHILE *)
        Write('}');
        WriteLn;
      END; (* IF *)
      WriteRanks(t^.firstChild);
      WriteRanks(t^.sibling);
    END; (* IF *)
  END; (* WriteRanks *)

  PROCEDURE WriteAsCanonicTree(t: TreePtr);
  BEGIN (* WriteAsCanonicTree *)
    WriteLn('Syntaxtree in canonical form.');
    WriteLn('digraph G {');
    WriteNodes(t);
    WriteCanonicConnections(t);
    WriteRanks(t);
    WriteLn('}');
  END; (* WriteAsCanonicTree *)

  PROCEDURE WriteConnections(VAR t: TreePtr);
    VAR 
      n: NodePtr;
  BEGIN (* WriteConnections *)
    IF (t <> NIL) THEN BEGIN
      IF (t^.firstChild <> NIL) THEN BEGIN
        WriteLn('n', t^.id, ' -> ','n', t^.firstChild^.id,';');
        n := t^.firstChild^.sibling;
        WHILE (n <> NIL) DO BEGIN
          WriteLn('n', t^.id, ' -> ','n', n^.id,';');
          n := n^.sibling;
        END; (* WHILE *)
      END; (* IF *)
      WriteConnections(t^.firstChild);
      WriteConnections(t^.sibling);
    END; (* IF *)
  END; (* WriteConnections *)

  PROCEDURE WriteAsTree(t: TreePtr);
  BEGIN (* WriteAsTree *)
    WriteLn('Syntaxtree in tree form.');
    WriteLn('digraph G {');
    WriteNodes(t);
    WriteConnections(t);
    WriteLn('}');
  END; (* WriteAsTree *)

  PROCEDURE DisposeTree(VAR t: TreePtr);
  BEGIN (* DisposeTree *)
    IF(t <> NIL) THEN BEGIN
      DisposeTree(t^.firstChild);
      DisposeTree(t^.sibling);
      Dispose(t);
      t := NIL;
    END;
  END; (* DisposeTree *)

  PROCEDURE AddSibling(t, n: NodePtr);
  BEGIN (* AddSibling *)
    IF (t^.sibling <> NIL) THEN BEGIN
      AddSibling(t^.sibling, n)
    END ELSE BEGIN
      t^.sibling := n;
    END; (* IF *)
  END; (* AddSibling *)

  PROCEDURE AddFirstChild(t, n: NodePtr);
  BEGIN (* AddFirstChild *)
    IF (t^.firstChild <> NIL) THEN BEGIN
      AddFirstChild(t^.firstChild, n)
    END ELSE BEGIN
      t^.firstChild := n;
    END; (* IF *)
  END; (* AddFirstChild *)

  PROCEDURE Start;
    VAR
      e: TreePtr;
  BEGIN (* Start *)
    success := TRUE;
    Expr(e); IF NOT success THEN BEGIN DisposeTree(e); Exit; END;
    IF sy <> eofSy THEN BEGIN success := FALSE; DisposeTree(e); Exit; END;
    NewSy;
    (*SEM*)
    WriteAsCanonicTree(e);
    WriteAsTree(e);
    (*ENDSEM*)
    DisposeTree(e);
  END; (* Start *)
  
  PROCEDURE Expr(VAR e: TreePtr);
    VAR
      t: NodePtr;
  BEGIN (* Expr *) 
    (*SEM*)
    e := NewNode('Expr');
    (*ENDSEM*)
    Term(t);
    (*SEM*)
    AddFirstChild(e, t);
    (*ENDSEM*)
    IF NOT success THEN EXIT;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy: BEGIN
                  NewSy;
                  (*SEM*)
                  AddSibling(e^.firstChild, NewNode('+'));
                  Term(t); IF NOT success THEN EXIT;
                  AddSibling(e^.firstChild, t);
                  (*ENDSEM*)
                END;
        minusSy:BEGIN
                  NewSy;
                  (*SEM*)
                  AddSibling(e^.firstChild, NewNode('-'));
                  Term(t); IF NOT success THEN EXIT;
                  AddSibling(e^.firstChild, t);
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)

  PROCEDURE Term(VAR t: TreePtr);
    VAR
      f: NodePtr;
  BEGIN (* Term *)
    (*SEM*)
    t := NewNode('Term');
    (*ENDSEM*)
    Fact(f); IF NOT success THEN EXIT;
    (*SEM*)
    AddFirstChild(t, f);
    (*ENDSEM*)
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                  NewSy;
                  (*SEM*)
                  AddSibling(t^.firstChild, NewNode('*'));
                  Fact(f); IF NOT success THEN EXIT;
                  AddSibling(t^.firstChild, f);
                  (*ENDSEM*)
                END;
        divSy: BEGIN
                  NewSy;
                  (*SEM*)
                  AddSibling(t^.firstChild, NewNode('/'));
                  Fact(f); IF NOT success THEN EXIT;
                  AddSibling(t^.firstChild, f);
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END;    
  END;(* Term *)

  PROCEDURE Fact(VAR f: NodePtr);
    VAR
      e : NodePtr;
  BEGIN (* Fact *)
    (*SEM*)
    f := NewNode('Fact');
    (*ENDSEM*)
    CASE sy OF
      identSy : BEGIN
                  (*SEM*)
                  AddFirstChild(f, NewNode(identVal));
                  (*ENDSEM*)
                  NewSy;
                END;
      numSy: BEGIN
                (*SEM*)
                AddFirstChild(f, NewNode(numberValStr));
                (*ENDSEM*)
                NewSy;
              END;
      leftParSy:  BEGIN
                   NewSy; 
                   (*SEM*)
                   AddFirstChild(f, NewNode('('));
                   Expr(e); IF NOT success THEN Exit;
                   AddSibling(f^.firstChild, e);
                   AddSibling(e, NewNode(')'));
                   (*ENDSEM*)
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      ELSE BEGIN
        success := FALSE;
        Exit;
      END;
    END; (*CASE*)
  END; (* Fact *)

BEGIN (* SyntaxTreeCalcSyn *)
END. (* SyntaxTreeCalcSyn *)