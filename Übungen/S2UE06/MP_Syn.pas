UNIT MP_Syn;

INTERFACE
  VAR 
    success : BOOLEAN;

  PROCEDURE S;

IMPLEMENTATION
USES MP_Lex;

  PROCEDURE MP; FORWARD;
  PROCEDURE VarDecl; FORWARD;
  PROCEDURE StatSeq; FORWARD;
  PROCEDURE Stat; FORWARD;
  PROCEDURE Expr; FORWARD;
  PROCEDURE Term; FORWARD;
  PROCEDURE Fact; FORWARD;
 
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
  BEGIN
    IF sy <> varSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;

    WHILE sy = commaSy DO BEGIN
      NewSy; (* skip comma *)
      IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
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
  BEGIN
    IF (sy = identSy) OR (sy = readSy) OR (sy = writeSy) THEN BEGIN
      CASE sy OF
        identSy: BEGIN
                   NewSy; (* skip ident *)
                   IF sy <> assignSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                   Expr; IF NOT success THEN Exit;
                 END;
        readSy: BEGIN
                  NewSy; (* skip readSy *)
                  IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                  IF sy <> identSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy; 
                  IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                  NewSy;
                END;
        writeSy: BEGIN
                   NewSy; (* skip write *)
                   IF sy <> leftParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;

                   Expr; IF NOT success THEN Exit;

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
        plusSy:  BEGIN NewSy; Term; IF NOT success THEN Exit; END;
        minusSy: BEGIN NewSy; Term; IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Term = Fact {  "*" Fact | "/" Fact } . *)
  PROCEDURE Term;
  BEGIN
    Fact; IF NOT success THEN Exit;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN NewSy; Fact; IF NOT success THEN Exit; END;
        divSy: BEGIN NewSy; Fact; IF NOT success THEN Exit; END;
      END; (* CASE *)
    END;
  END;

  (* Fact = number | ident | '(' Expr ')' . *)
  PROCEDURE Fact;
  BEGIN
    CASE sy OF
      numSy: BEGIN NewSy; END;
      identSy: BEGIN NewSy; END;
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