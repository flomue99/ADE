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

  PROCEDURE Expr(VAR e: STRING); FORWARD;
  PROCEDURE Term(VAR t: STRING); FORWARD;
  PROCEDURE Fact(VAR f: STRING); FORWARD;

  PROCEDURE Start;
    VAR
      x: STRING;
  BEGIN (* Start *)
    success := TRUE;
    Expr(x); IF NOT success THEN Exit;
    Write(x);
    IF sy <> eofSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
  END; (* Start *)
  
 (* Expr = Term { '+' Term | '-' Term } . *)
  PROCEDURE Expr(VAR e: STRING);
    VAR
      t: STRING;
  BEGIN (* Expr *)
    Term(t); IF NOT success THEN EXIT;
    (*SEM*)
    e := t;
    (*ENDSEM*)
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy: BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e  := '+' + e  + ' ' + t;
                  (*ENDSEM*)
                END;
        minusSy:BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e := '-' + e  + ' ' + t;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)


(* Term = Fact { '*' Fact | '/' Fact} . *)
  PROCEDURE Term(VAR t: STRING);
    VAR
     f: STRING;
  BEGIN (* Term *)
    Fact(f); IF NOT success THEN EXIT;
    t := f;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                  t := '*'+ t + ' ' + f;
                  (*ENDSEM*)
                END;
        divSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                   t := '/' + t  + ' ' + f;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END;    
  END;(* Term *)

  (* Fact = number | ident '(' Expr ')' . *)
  PROCEDURE Fact(VAR f: STRING);
  VAR
    e: STRING;
  BEGIN (* Fact *)
    CASE sy OF
      identSy : BEGIN
                  (*SEM *)
                  f := charVal;
                  (*ENDSEM*)
                  NewSy;
                END;
      numSy: BEGIN 
                (*SEM*)
                //Str(numberVal, numberValStr);
                f := '1';
                Write(' ', numberVal, ' ');
                (*ENDSEM*)
                NewSy; (*skip leftbarsy*)
              END;
      leftParSy:  BEGIN
                   NewSy; (*skip leftbarsy*)
                   Expr(e); IF NOT success THEN Exit;
                   IF sy <> rightParSy THEN BEGIN success := FALSE; Exit; END;
                   f := e;
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