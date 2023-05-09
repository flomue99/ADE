UNIT MP_Sem;

INTERFACE
  VAR 
    success : BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION
USES MP_Lex, SymTab;

  PROCEDURE MP; FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat; FORWARD;
  PROCEDURE Expr(VAR e: INTEGER); FORWARD;
  PROCEDURE Term(VAR t: INTEGER); FORWARD;
  PROCEDURE Fact(VAR f: INTEGER); FORWARD;

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
      SemErr('multiple declaration');
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
    e: INTEGER;
  BEGIN
    IF (sy = identSy) OR (sy = readSy) OR (sy = writeSy) THEN BEGIN
      CASE sy OF
        identSy: BEGIN
                   (*SEM*)
                   destId := identStr;
                   IF NOT IsDecl(destId) THEN
                     SemErr('variable is not declared');
                   (*ENDSEM*)
                   NewSy; (* skip ident *)
                   IF sy <> assignSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                   Expr(e); IF NOT success THEN Exit;
                   (*SEM*)
                   IF IsDecl(destId) THEN
                     SetVal(destId, e);
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
                  ELSE BEGIN
                    Write(identStr, ' > ');
                    ReadLn(e);
                    SetVal(identStr, e);
                  END; 
                  (*ENDSEM*)
                  NewSy; 
                  IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                END;
        writeSy: BEGIN
                   NewSy; (* skip write *)
                   IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;

                   Expr(e); IF NOT success THEN Exit;
                   (*SEM*)
                    WriteLn(e);
                   (*ENDSEM*)
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      END; (* CASE *)
    END; (* IF *)
  END; (* Stat *)


  (* Expr = Term {  "+" Term | "-"  Term } . *)
  PROCEDURE Expr(VAR e: INTEGER);
    VAR
      t: INTEGER;
  BEGIN
    Term(e); IF NOT success THEN Exit;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy:  BEGIN
                  NewSy;
                  Term(t); 
                  (*SEM*)
                    e := e + t;
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;

        minusSy: BEGIN
                  NewSy; 
                  Term(t); 
                  (*SEM*)
                  e := e -t;
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Term = Fact {  "*" Fact | "/" Fact } . *)
  PROCEDURE Term(VAR t: INTEGER);
    VAR
      f: INTEGER;
  BEGIN
    Fact(t); IF NOT success THEN Exit;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                 NewSy;
                  Fact(f); 
                  (*SEM*)
                  t := t * f;
                  (*ENDSEM*)
                  IF NOT success THEN Exit; END;
        divSy: BEGIN
                 NewSy; 
                 Fact(f); 
                 (*SEM*)
                 IF f = 0 THEN
                   SemErr('zeri division')
                 ELSE
                  t := t DIV f;
                 (*ENDSEM*)
                 IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Fact = number | ident | '(' Expr ')' . *)
  PROCEDURE Fact(VAR f: INTEGER);
  BEGIN
    CASE sy OF
      numSy: BEGIN
               (*SEM*)
                f := numberVal;
               (*ENDSEM*) 
              NewSy; END;
      identSy: 
              BEGIN 
               (*SEM*)
                IF NOT IsDecl(identStr) THEN
                  SemErr('var. not decl')
                ELSE
                  GetVal(identStr, f);
               (*ENDSEM*)
              NewSy; END;
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