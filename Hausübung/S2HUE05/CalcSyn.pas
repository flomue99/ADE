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

  PROCEDURE Expr; FORWARD;
  PROCEDURE Term; FORWARD;
  PROCEDURE Fact; FORWARD;

  PROCEDURE Start;
  BEGIN (* Start *)
    success := TRUE;
    Expr; IF NOT success THEN Exit;
    IF sy <> eofSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
  END; (* Start *)
  
 (* Expr = Term { '+' Term | '-' Term } . *)
  PROCEDURE Expr;
  BEGIN (* Expr *)
    Term; IF NOT success THEN EXIT;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy: BEGIN
                  NewSy;
                  Term; IF NOT success THEN EXIT;
                  (*SEM*)
                  Write('+');
                  (*ENDSEM*)
                END;
        minusSy:BEGIN
                  NewSy;
                  Term; IF NOT success THEN EXIT;
                  (*SEM*)
                  Write('-');
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)


(* Term = Fact { '*' Fact | '/' Fact} . *)
  PROCEDURE Term;
  BEGIN (* Term *)
    Fact; IF NOT success THEN EXIT;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                  NewSy;
                  Fact; IF NOT success THEN EXIT;
                  (*SEM*)
                  Write('*');
                  (*ENDSEM*)
                END;
        divSy: BEGIN
                  NewSy;
                  Fact; IF NOT success THEN EXIT;
                  (*SEM*)
                  Write('/');
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END;    
  END;(* Term *)

  (* Fact = number | '(' Expr ')' . *)
  PROCEDURE Fact;
  BEGIN (* Fact *)
    CASE sy OF
      numSy: BEGIN 
                (*SEM*)
                Write(' ', numberVal, ' ');
                (*ENDSEM*)
                NewSy; (*skip leftbarsy*)
              END;
      leftParSy:  BEGIN
                   NewSy; (*skip leftbarsy*)
                   Expr; IF NOT success THEN Exit;
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