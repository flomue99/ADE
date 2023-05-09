(* MP_SemComp:                                               MLF, 2023-05-09 *)
(* ------                                                                    *)
(* SemComp                                                                   *)
(* ========================================================================= *)
UNIT MP_SemComp;

INTERFACE
  VAR 
    success : BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION
  USES MP_Lex, SymTab, CodeDef, CodeGen, TreeUnit;

  FUNCTION IsOne(val: STRING): BOOLEAN;
  BEGIN (* IsOne *)
    IsOne := val = '1';
  END; (* IsOne *)

  FUNCTION IsZero(val: STRING): BOOLEAN;
  BEGIN (* IsZero *)
    IsZero := val = '0';
  END; (* IsZero *)

  PROCEDURE MP; FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat; FORWARD;
  PROCEDURE Expr(VAR e: TreePtr); FORWARD;
  PROCEDURE Term(VAR t: TreePtr); FORWARD;
  PROCEDURE Fact(VAR f: TreePtr); FORWARD;
 
  PROCEDURE SemErr(msg : STRING);
  BEGIN
    WriteLn('Semantic error ', msg, ' in line ', lineNr);
  END;

  PROCEDURE S;
  BEGIN
    success := TRUE;
    NewSy;

    MP; IF NOT success THEN Exit; 
    IF sy <> eofSy THEN BEGIN success := FALSE; Exit; End;
    NewSy;
  END; (* S *) 

  (* "PROGRAM" ident ";" [ VarDecl ] "BEGIN" StatSeq "END" "."  *)
  PROCEDURE MP;
  BEGIN
    (* SEM *)
    InitSymbolTable;
    InitCodeGenerator;
    (* ENDSEM *)
    IF sy <> programSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> semicolonSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    
    IF sy = varSy THEN BEGIN
      VarDecl; IF NOT success THEN Exit;            
    END;

    IF sy <> beginSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;   

    StatSeq; IF NOT success THEN Exit; 

    (* SEM *)
    Emit1(EndOpc);
    (* ENDSEM *)

    IF sy <> endSy THEN BEGIN success := FALSE; Exit; END;
    NewSy; 

    IF sy <> periodSy THEN BEGIN success := FALSE; Exit; END;
    NewSy; 

  END;  (* MP *)

  (* VarDecl = "VAR" ident { "," ident } ":" "INTEGER" ";" . *)
  PROCEDURE VarDecl;
  VAR ok : BOOLEAN;
  BEGIN
    IF sy <> varSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
    (* SEM *)
    DeclVar(identStr, ok);
    (* ENDSEM *)
    NewSy;

    WHILE sy = commaSy DO BEGIN
      NewSy; (* skip comma *)
      IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
      (* SEM *)
      DeclVar(identStr, ok);
      IF NOT ok THEN 
        SemErr('multiple declaration');
      (* ENDSEM *)
      NewSy;
    END;

    IF sy <> colonSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> integerSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> semicolonSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;    
  END; (* VarDecl *)
 
  (* StatSeq = Stat { ";" Stat } . *)
  PROCEDURE StatSeq;
  BEGIN
    Stat; IF NOT success THEN Exit;
    WHILE sy = semicolonSy DO BEGIN
      NewSy; (* skip ; *)
      Stat; IF NOT success THEN Exit;
    END;
  END;

  (*
  Stat = [ ident ":=" Expr
         | "READ" "(" ident ")"
         | "WRITE" "(" Expr ")"
         ] .
  *)
  PROCEDURE Stat;
    VAR 
      destId : STRING;
      addr, addr1, addr2: INTEGER;
      e: TreePtr;
  BEGIN
    IF (sy = identSy) OR (sy = readSy) OR (sy = writeSy)
        OR (sy = beginSy) OR (sy = ifSy) OR (sy = whileSy) THEN BEGIN
      CASE sy OF
        identSy: BEGIN
                   (* SEM *)
                   destId := identStr;
                   IF NOT IsDecl(destId) THEN
                     SemErr('variable is not declared')
                   ELSE
                     Emit2(LoadAddrOpc, AddrOf(destId));
                   (* ENDSEM *)
                   NewSy; (* skip ident *)
                   IF sy <> assignSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;

                   Expr(e); IF NOT success THEN Exit;
                   WriteTreeGraphically(e);
                   EmitCodeForExprTree(e);
                   DisposeTree(e);
                   (* SEM *)
                   IF IsDecl(destId) THEN
                     Emit1(StoreOpc);
                   (* ENDSEM *)
                 END;
        readSy: BEGIN
                  NewSy; (* skip readSy *)
                  IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                  IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
                  (* SEM *)
                  IF NOT IsDecl(identStr) THEN
                    SemErr('variable is not declared')
                  ELSE
                    Emit2(ReadOpc, AddrOf(identStr));
                  (* ENDSEM *)
                  NewSy; 
                  IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                END;
        writeSy: BEGIN
                   NewSy; (* skip write *)
                   IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;

                   Expr(e); IF NOT success THEN Exit;
                   WriteTreeGraphically(e);
                   EmitCodeForExprTree(e);
                   DisposeTree(e);
                   (* SEM *)
                   Emit1(WriteOpc);
                   (* ENDSEM *)
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
         beginSy: BEGIN (* "BEGIN" StatSeq "END" *)
                    NewSy; (*skip BEGIN*)

                    StatSeq; IF NOT success THEN Exit;

                    IF sy <> endSy THEN BEGIN success := FALSE; Exit; END;
                    NewSy;
                  END;
          ifSy: BEGIN (* "IF" ident "THEN" Stat ["Else" STAT]*)
                  NewSy; (* skip "IF" *)

                  IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
                  (*SEM*)
                  IF NOT IsDecl(identStr) THEN BEGIN
                  SemErr('variable not declared');
                  END; (*IF*)
                  Emit2(LoadValOpc, AddrOf(identStr));
                  Emit2(JmpZOpc, 0); (*0 as dummy address*)
                  addr := CurAddr - 2; 
                  (*ENDSEM*)
                  NewSy;
                  
                  IF sy <> thenSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                  Stat; IF NOT success THEN Exit;

                  IF sy = elseSy THEN BEGIN
                    (*SEM*)
                    Emit2(JmpOpc, 0); (*0 as dummy address*)
                    FixUp(addr, CurAddr);
                    addr := CurAddr - 2; 
                    (*ENDSEM*)
                    NewSy;
                    Stat; IF NOT success THEN Exit;
                  END;
                  (*SEM*)
                  FixUp(addr, CurAddr);
                  (*ENDSEM*)
                END;
          whileSy: BEGIN  (*"WHILE" ident "Do" Stat*)
                     NewSy;

                     IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
                     (*SEM*)
                     IF NOT IsDecl(identStr) THEN BEGIN
                       SemErr('variable not declared');
                     END; (*IF*)
                     addr1 := CurAddr;
                     Emit2(LoadValOpc, AddrOf(identStr));
                     Emit2(JmpZOpc, 0); (*0 as dummy address*)
                     addr2 := CurAddr - 2;
                     (*ENDSEM*)
                     NewSy;   
                     
                     IF sy <> doSy THEN BEGIN success := FALSE; Exit; END;
                     NewSy;
                     Stat; IF NOT success THEN Exit;
                     (*SEM*)
                      Emit2(JmpOpc, addr1);
                      FixUp(addr2, CurAddr);
                     (*ENDSEM*)
                   END;
      END; (* CASE *)
    END; (* IF *)
  END; (* Stat *)


  (* Expr = Term {  "+" Term | "-"  Term } . *)
  PROCEDURE Expr(Var e: TreePtr);
    VAR
      t: NodePtr;
  BEGIN
    Term(e); IF NOT success THEN Exit;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy:  BEGIN 
                   NewSy;
                   (* SEM *)
                   t := NIL;
                   Term(t); IF NOT success THEN Exit; 
                   IF (NOT IsZero(e^.val)) AND (NOT IsZero(t^.val)) THEN BEGIN
                     IF (e^.valType = 2) AND (t^.valType = 2) THEN BEGIN (* 2 numbers *)
                       e := NewNode(CalcNewValue(e, t, '+'),2);
                     END ELSE BEGIN
                       e := ConnectNodes(NewNode('+', 1), e, t);   
                     END; (* IF *)
                   END ELSE BEGIN
                     IF (IsZero(e^.val)) THEN BEGIN
                       Dispose(e);
                       e := t;
                     END ELSE BEGIN
                       Dispose(t);
                     END; (* IF *)
                   END; (* IF *)
                   (* ENDSEM *)
                 END;
        minusSy: BEGIN 
                   NewSy; 
                   (* SEM *)
                   t := NIL;
                   Term(t); 
                   IF NOT success THEN Exit;
                   IF (NOT IsZero(e^.val)) AND (NOT IsZero(t^.val)) THEN BEGIN
                     IF (e^.valType = 2) AND (t^.valType = 2) THEN BEGIN (* 2 numbers *)
                       e := NewNode(CalcNewValue(e, t, '-'),2);
                     END ELSE BEGIN
                       e := ConnectNodes(NewNode('-', 1), e, t);   
                     END; (* IF *)
                   END ELSE BEGIN
                     IF (IsZero(e^.val)) THEN BEGIN
                       Dispose(e);
                       e := t;
                     END ELSE BEGIN
                       Dispose(t);
                     END; (* IF *)
                   END; (* IF *)
                   (* ENDSEM *)
                 END;
      END; (* CASE *)
    END;
  END;

  (* Term = Fact {  "*" Fact | "/" Fact } . *)
  PROCEDURE Term(Var t: TreePtr);
    VAR
      f: NodePtr;
  BEGIN
    Fact(t); IF NOT success THEN Exit;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN 
                 NewSy; 
                 (* SEM *)
                 f := NIL;
                 Fact(f); 
                 IF NOT success THEN Exit;
                 IF (NOT IsOne(t^.val)) AND (NOT IsOne(f^.val)) THEN BEGIN (* optimice ecpr * 1 *)
                     IF (t^.valType = 2) AND (f^.valType = 2) THEN BEGIN (* 2 numbers *)
                       t := NewNode(CalcNewValue(t, f, '*'),2);
                     END ELSE BEGIN
                       t := ConnectNodes(NewNode('*', 1), t, f);     
                     END; (* IF *)
                 END ELSE BEGIN
                   IF (IsOne(t^.val)) THEN BEGIN
                     Dispose(t);
                     t := f;
                   END ELSE BEGIN
                     Dispose(f); 
                   END; (* IF *)
                 END; (* IF *)
                 (* ENDSEM *)
               END;
        divSy: BEGIN 
                 NewSy; 
                 (* SEM *)
                 f := NIL;
                 Fact(f); 
                 IF NOT success THEN Exit;
                 IF (NOT IsOne(f^.val)) THEN BEGIN
                   IF (t^.valType = 2) AND (f^.valType = 2) THEN BEGIN (* 2 numbers *)
                       t := NewNode(CalcNewValue(t, f, '/'),2);
                    END ELSE BEGIN
                       t := ConnectNodes(NewNode('/', 1), t, f);     
                    END; (* IF *)
                 END ELSE BEGIN
                   Dispose(f);
                 END; (* IF *)
                 (* ENDSEM *)
               END;
      END; (* CASE *)
    END;
  END;

  (* Fact = number | ident | '(' Expr ')' . *)
  PROCEDURE Fact(Var f: TreePtr);
    VAR
      numberStr: STRING;
  BEGIN
    CASE sy OF
      numSy: BEGIN 
               (* SEM *)
               Str(numberVal, numberStr);
               f := NewNode(numberStr, 2);
               (* ENDSEM *)
               NewSy;                
             END;
      identSy: BEGIN 
                  (* SEM *)
                  IF NOT IsDecl(identstr) THEN BEGIN
                    SemErr('variable is not declared')
                  END ELSE BEGIN 
                    f := NewNode(identStr, 3);
                  END;
                  (* ENDSEM *)
                  NewSy; 
               END;
      leftParSy: BEGIN
                   NewSy; (* skip ( *)
                   Expr(f); IF NOT success THEN Exit;
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      ELSE BEGIN success := FALSE; Exit; END;
    END; (* CASE *)
  END;

BEGIN
END.