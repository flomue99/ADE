(* CalcSyn:                                                  MFl, 2023-05-02 *)
(* ------                                                                    *)
(* syntax parser                                                             *)
(* ========================================================================= *)
UNIT CalcSyn;

INTERFACE
  VAR
    success: BOOLEAN;
  
  PROCEDURE Start;

IMPLEMENTATION
   USES CalcLex;

  PROCEDURE Expr(VAR e: STRING); FORWARD;
  PROCEDURE Term(VAR t: STRING); FORWARD;
  PROCEDURE Fact(VAR f: STRING); FORWARD;

  PROCEDURE Start;
    VAR
      e: STRING;
  BEGIN (* Start *)
    success := TRUE;
    Expr(e); IF NOT success THEN Exit;
    IF sy <> eofSy THEN BEGIN success := FALSE; Exit; END;
    NewSy;
    (*SEM*)
    WriteLn(e);
    (*ENDSEM*)
  END; (* Start *)
  
  PROCEDURE Expr(VAR e: STRING);
    VAR
      t: STRING;
  BEGIN (* Expr *)
    Term(e); IF NOT success THEN EXIT;
    WHILE (sy = plusSy) OR (sy = minusSy) DO BEGIN
      CASE sy OF
        plusSy: BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e  := '+ ' + e + t;
                  (*ENDSEM*)
                END;
        minusSy:BEGIN
                  NewSy;
                  Term(t); IF NOT success THEN EXIT;
                  (*SEM*)
                  e :=  '- ' + e  + t;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END; (* WHILE *)
  END; (* Expr *)

  PROCEDURE Term(VAR t: STRING);
    VAR
     f: STRING;
  BEGIN (* Term *)
    Fact(t); IF NOT success THEN EXIT;
    WHILE (sy = mulSy) OR (sy = divSy) DO BEGIN
      CASE sy OF
        mulSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                  t := '* ' + t + f;
                  (*ENDSEM*)
                END;
        divSy: BEGIN
                  NewSy;
                  Fact(f); IF NOT success THEN EXIT;
                  (*SEM*)
                   t := '/ ' + t + f;
                  (*ENDSEM*)
                END;
      END; (* CASE *)
    END;    
  END;(* Term *)

  PROCEDURE Fact(VAR f: STRING);
  BEGIN (* Fact *)
    CASE sy OF
      identSy : BEGIN
                  (*SEM*)
                  f := identVal + ' ';
                  (*ENDSEM*)
                  NewSy;
                END;
      numSy: BEGIN 
              (*SEM*)
              f := numberValStr + ' ';
              (*ENDSEM*)
              NewSy;
            END;
      leftParSy:  BEGIN
                   NewSy;
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