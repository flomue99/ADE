(* UniqueSubstr:                                      v   MFL, 2023-03-08 *)
(* ------                                                                    *)
(* Calculates the amount of unique substrings                                *)
(* ========================================================================= *)
PROGRAM UniqueSubstr;
  USES 
    ModHashChaining;
  CONST
    StringLenght = 5;

  PROCEDURE ReadInput;
    VAR
      s: CHAR;
      i: INTEGER;
      str: STRING;
  BEGIN (* ReadInput *)
    i := 1;
    str := '';
    Read(s);
    WHILE (s <> ' ') DO BEGIN
      str := str + s; 
      Inc(i);
      IF (i = StringLenght + 1) THEN BEGIN 
        Insert(str);
        str := Copy(str, 2, StringLenght - 1);
        i := StringLenght;
      END;
      Read(s);
    END; (* WHILE *)
  END; (* ReadInput *)
  
BEGIN (* UniqueSubstr *)
  ReadInput;
  WriteLn('Die eingegebene Zeichenkette hat: ',CountEntries, ' eindeutige Teilketten');
  DisposeHashTable;
END. (* UniqueSubstr *) 