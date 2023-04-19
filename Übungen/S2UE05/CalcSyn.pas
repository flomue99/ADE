UNIT CalcSyn;

INTERFACE
  VAR
    success: BOOLEAN;
  
  PROCEDURE Start;

IMPLEMENTATION
   USES CalcLex;
  
  (*
  G(Start):
  Start = Expr eofSy .
  Expr = Term | Term '+' Expr | Term '-' Expr .
  Term = Fact | Fact '*' Term | Fact '/' Term .
  Fact = number | '(' Expr ')' .
  *)
  (* Expr = Term | Term '+' Expr | Term '-' Expr .*)

  PROCEDURE Expr(VAR e: INTEGER); FORWARD;
  PROCEDURE TERM(VAR t: INTEGER); FORWARD;
  PROCEDURE Fact(VAR f: INTEGER); FORWARD;

  PROCEDURE Start;
    VAR e: INTEGER;
  BEGIN (* Start *)
    success := TRUE;
    Expr(e); IF NOT success THEN Exit;
    IF sy <> eofSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    (*SEM*)
    Write(e);
    (*ENDSEM*)
  END; (* Start *)
  
 (* Expr = Term { '+' Term | '-' Term } . *)
  PROCEDURE Expr(VAR e: INTEGER);
    VAR t: INTEGER;
  BEGIN (* Expr *)
    Term(e); IF NOT success THEN EXIT;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy: BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e := e + t;
                  (*ENDSEM*)
                END;
        minusSy:BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e := e - t;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)


(* Term = Fact { '*' Fact | '/' Fact} . *)
  PROCEDURE Term(VAR t: INTEGER);
    VAR f: INTEGER;
  BEGIN (* Term *)
    Fact(t); IF NOT success THEN EXIT;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                  t := t * f;
                  (*ENDSEM*)
                END;
        divSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                  t := t DIV f;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END;    
  END;(* Term *)

  (* Fact = number | '(' Expr ')' . *)
  PROCEDURE Fact(VAR f: INTEGER);
  BEGIN (* Fact *)
    CASE sy OF
      numSy: BEGIN 
                (*SEM*)
                f := numberVal;
                (*ENDSEM*)
                NewSy; (*skip leftbarsy*)
              END;
      leftParSy:  BEGIN
                   NewSy; (*skip leftbarsy*)
                   Expr(f); IF NOT success THEN Exit;
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   NewSy;
                 END;
      ELSE BEGIN
        success := FALSE;
        Exit;
      END;
    END; (*CASE*)
  END; (* Fact *)

BEGIN (* CalcSyn *)
  
END. (* CalcSyn *)