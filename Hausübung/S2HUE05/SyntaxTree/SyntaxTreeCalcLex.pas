(* SyntaxTreeCalcLex:                                        MFL, 2023-05-02 *)
(* ------                                                                    *)
(* Calxlex Unit                                                              *)
(* ========================================================================= *)
UNIT SyntaxTreeCalcLex;

INTERFACE
  TYPE
    Symbol = (IdentSy, NumSy, PlusSy, MinusSy, 
              MulSy, DivSy, 
              LeftParSy, RightParSy, 
              NoSy, EofSy);
  VAR
    sy: Symbol; (* current symbol *)
    numberVal: INTEGER;
    numberValStr: STRING;
    identVal: CHAR;
  PROCEDURE InitLex(s: STRING); 
  PROCEDURE NewSy;

IMPLEMENTATION
  CONST eofCh = Chr(0);
        tabCh = Chr(9);
  VAR
    s: STRING;
    pos: INTEGER;
    ch: CHAR;

  PROCEDURE NewCh;
  BEGIN (* NewCh *)
    IF pos < Length(s) THEN BEGIN
      Inc(pos);
      ch := s[pos];
    END ELSE BEGIN
      ch := eofCh;
    END;
  END; (* NewCh *)

  PROCEDURE InitLex(s: STRING);
  BEGIN (* InitLex *)
    SyntaxTreeCalcLex.s := s;
    pos := 0;
    NewCh;
    NewSy;
  END; (* InitLex *)

  PROCEDURE NewSy;
  BEGIN (* NewSy *)
    WHILE (ch = ' ') OR (ch = tabCh) DO NewCh;
    CASE ch OF 
      '+': BEGIN sy := PlusSy; NewCh; END;
      '-': BEGIN sy := MinusSy; NewCh; END;
      '*': BEGIN sy := MulSy; NewCh; END;
      '/': BEGIN sy := DivSy; NewCh; END;
      '(': BEGIN sy := LeftParSy; NewCh; END;
      ')': BEGIN sy := RightParSy; NewCh; END;
      'a'..'z': BEGIN
                     identVal := ch;
                     sy := IdentSy; NewCh; 
                END;
      eofCh: BEGIN sy := EofSy; NewCh; END;
      '0'..'9': BEGIN 
                  numberVal := Ord(ch) - Ord('0');
                  NewCh;
                  WHILE ch in ['0'..'9'] DO BEGIN
                    numberVal := numberVal * 10 + Ord(ch) - Ord('0');
                    NewCh;
                  END;
                  Str(numberVal, numberValStr);
                  sy := NumSy
                END;
      ELSE BEGIN sy := NoSy; NewCh; END;
    END; (* case *)
  END; (* NewSy *)

BEGIN (* SyntaxTreeCalcLex *)
  
END. (* SyntaxTreeCalcLex *)