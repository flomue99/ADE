UNIT MP_Lex;

INTERFACE
  TYPE
    Symbol = (eofSy, noSy, 
              programSy, identSy, semicolonSy,
              beginSy, endSy, periodSy,
              varSy, commaSy, colonSy,
              integerSy, assignSy,
              readSy, writeSy,
              leftParSy, rightParSy,
              plusSy, minusSy,
              mulSy, divSy,
              numSy,
              ifSy, thenSy, elseSy,
              whileSy, doSy
             );
  VAR 
    sy : Symbol;
    identStr : STRING;
    numberVal : INTEGER;
    lineNr : INTEGER;

  PROCEDURE InitLex(fileName : STRING);
  PROCEDURE NewSy;

IMPLEMENTATION
  CONST 
    eofCh = Chr(0);
    tabCh = Chr(9);

  VAR
    input : TEXT;
    line : STRING;
    pos : INTEGER;
    ch : CHAR;    

  PROCEDURE InitLex(fileName : STRING);
  BEGIN
    Assign(input, fileName);
    {$I-}
    Reset(input);
    IF IOResult <> 0 THEN BEGIN
      WriteLn('Cannot open ', fileName,'.');
      HALT;
    END;
    {$I+}
    ReadLn(input, line);
    pos := 0;
    lineNr := 1;
    ch := ' ';
    sy := noSy;
  END;

  PROCEDURE NewCh;
  BEGIN
    IF pos >= Length(line) THEN BEGIN
      IF NOT Eof(input) THEN BEGIN
        ReadLn(input, line);
        pos := 0;
        Inc(lineNr);
        ch := ' '; (* treat newline as a space char *)
      END ELSE BEGIN
        ch := eofCh;
      END;
    END ELSE BEGIN
      Inc(pos);
      ch := line[pos];
    END;   
  END;

  PROCEDURE NewSy;
  BEGIN
    WHILE (ch = ' ') OR (ch = tabCh) DO NewCh;
    CASE ch OF
      '+': BEGIN sy := plusSy; NewCh; END;
      '-': BEGIN sy := minusSy; NewCh; END;
      '*': BEGIN sy := mulSy; NewCh; END;
      '/': BEGIN sy := divSy; NewCh; END;
      ';': BEGIN sy := semicolonSy; NewCh; END;
      ':': BEGIN 
             NewCh; 
             IF ch = '=' THEN BEGIN
               sy := assignSy;
               NewCh;
             END ELSE
               sy := colonSy;               
           END;
      '.': BEGIN sy := periodSy; NewCh; END;
      ',': BEGIN sy := commaSy; NewCh; END;
      '(': BEGIN sy := leftParSy; NewCh; END;
      ')': BEGIN sy := rightParSy; NewCh; END;
      '0'..'9': BEGIN 
                  numberVal := Ord(ch) - Ord('0');
                  NewCh;
                  WHILE ch in ['0'..'9'] DO BEGIN
                    numberVal := numberVal * 10 + Ord(ch) - Ord('0');
                    NewCh;
                  END;
                  sy := NumSy
                END;
      'a'..'z','A'..'Z','_': BEGIN 
                           identStr := LowerCase(ch);
                           NewCh;
                           WHILE ch IN ['a'..'z','A'..'Z','0'..'9','_'] DO BEGIN
                              identStr := identStr + LowerCase(ch);
                              NewCh;
                           END; (* WHILE *)
                           IF identStr = 'begin' THEN sy := beginSy
                           ELSE IF identStr = 'end' THEN sy := endSy
                           ELSE IF identStr = 'integer' THEN sy := integerSy
                           ELSE IF identStr = 'program' THEN sy := programSy
                           ELSE IF identStr = 'read' THEN sy := readSy
                           ELSE IF identStr = 'var' THEN sy := varSy
                           ELSE IF identStr = 'write' THEN sy := writeSy
                           ELSE IF identStr = 'if' THEN sy := ifSy
                           ELSE IF IdentStr = 'then' THEN sy := thenSy
                           ELSE IF IdentStr = 'else' THEN sy := elseSy
                           ELSE IF IdentStr = 'while' THEN sy := whileSy
                           ELSE IF IdentStr = 'do' THEN sy := doSy
                           ELSE sy := identSy;
                         END;
      eofCh: BEGIN sy := eofSy; NewCh; END;
      ELSE BEGIN sy := noSy; NewCh; END;
    END; (* CASE *)
    (* Debug *)
    //WriteLn(sy);
  END; (* NewSy *)

BEGIN
END.