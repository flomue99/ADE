UNIT MP_SemComp;

INTERFACE
  VAR 
    success : BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION
USES MP_Lex, SymTab, CodeDef, CodeGen;

  PROCEDURE MP; FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat; FORWARD;
  PROCEDURE Expr; FORWARD;
  PROCEDURE Term; FORWARD;
  PROCEDURE Fact; FORWARD;

  PROCEDURE SemErr(msg: STRING);
  BEGIN (* SemErr *)
    WriteLn('Sematic error ', msg ,' in line', lineNr );
  END; (* SemErr *)
 
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
    (*SEM*)
    InitSymbolTable;
    InitCodeGenerator;
    (*ENDSEM*)
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

    (*SEM*)
    Emit1(EndOpc);
    (*ENDSEM*)
    IF sy <> endSy THEN BEGIN success := FALSE; Exit; END;
    NewSy; 

    IF sy <> periodSy THEN BEGIN success := FALSE; Exit; END;
    NewSy; 

  END;  (* MP *)

  (* VarDecl = "VAR" ident { "," ident } ":" "INTEGER" ";" . *)
  PROCEDURE VarDecl;
    VAR
      ok: BOOLEAN;
  BEGIN
    IF sy <> varSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
    (*SEM*)
    DeclVar(identStr, ok);
    (*ENDSEM*)
    
    NewSy;

    WHILE sy = commaSy DO BEGIN
      NewSy; (* skip comma *)
      IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
    (*SEM*)
    DeclVar(identStr, ok);
    IF NOT OK THEN
      SemErr('multiple declaration.');
    (*ENDSEM*)
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
    destId: STRING;
  BEGIN
    IF (sy = identSy) OR (sy = readSy) OR (sy = writeSy) THEN BEGIN
      CASE sy OF
        identSy: BEGIN
                   (*SEM*)
                   destId := identStr;
                   IF NOT IsDecl(destId) THEN
                     SemErr('variable is not declared.')
                   ELSE
                     Emit2(LoadAddrOpc, AddrOf(destId));
                   (*ENDSEM*)
                   NewSy; (* skip ident *)
                   IF sy <> assignSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                   Expr; IF NOT success THEN Exit;
                   (*SEM*)
                   IF IsDecl(destId) THEN
                     Emit1(StoreOpc);
                   (*ENDSEM*)
                 END;
        readSy: BEGIN
                  NewSy; (* skip readSy *)
                  IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                  IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
                  (*SEM*)
                  IF NOT IsDecl(identStr) THEN
                    SemErr('var. not decl.')
                  ELSE
                    Emit2(ReadOpc, AddrOf(identStr));
                  (*ENDSEM*)
                  NewSy; 
                  IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                END;
        writeSy: BEGIN
                   NewSy; (* skip write *)
                   IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;

                   Expr; IF NOT success THEN Exit;
                   (*SEM*)
                   Emit1(WriteOpc);
                   (*ENDSEM*)
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      END; (* CASE *)
    END; (* IF *)
  END; (* Stat *)


  (* Expr = Term {  "+" Term | "-"  Term } . *)
  PROCEDURE Expr;
  BEGIN
    Term; IF NOT success THEN Exit;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy:  BEGIN
                  NewSy;
                  Term; 
                  (*SEM*)
                  Emit1(AddOpc);
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;

        minusSy: BEGIN
                  NewSy; 
                  Term; 
                  (*SEM*)
                  Emit1(SubOpc);
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Term = Fact {  "*" Fact | "/" Fact } . *)
  PROCEDURE Term;
  BEGIN
    Fact; IF NOT success THEN Exit;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                 NewSy;
                  Fact; 
                  (*SEM*)
                  Emit1(MulOpc);
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;
        divSy: BEGIN
                 NewSy; 
                 Fact; 
                 (*SEM*)
                 Emit1(DivOpc);
                 (*ENDSEM*)
                 IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Fact = number | ident | '(' Expr ')' . *)
  PROCEDURE Fact;
  BEGIN
    CASE sy OF
      numSy: BEGIN
               (*SEM*)
                Emit2(LoadConstOpc, numberVal);
               (*ENDSEM*) 
              NewSy; END;
      identSy: 
              BEGIN 
               (*SEM*)
                IF NOT IsDecl(identStr) THEN
                  SemErr('var. not decl')
                ELSE
                  Emit2(LoadValOpc, AddrOf(identStr));
               (*ENDSEM*)
              NewSy; END;
      leftParSy: BEGIN
                   NewSy; (* skip ( *)
                   Expr; IF NOT success THEN Exit;
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      ELSE BEGIN success := FALSE; Exit; END;
    END; (* CASE *)
  END;

BEGIN
END.