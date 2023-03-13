(* Title:                                                 Author, 2023-03-08 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM UniqueSubStr;
  USES 
    ModHashChaining;
  CONST
    StringLenght = 3;

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
      IF (i = StringLenght + 1) THEN BEGIN (* first word already added to hashtable *)
        Insert(str);
        str := Copy(str, 2, StringLenght - 1);
        i:= 3;
      END;
      Read(s);
    END; (* WHILE *)
  END; (* ReadInput *)
  
BEGIN (* UniqueSubStr *)
  ReadInput;
  WriteHashtable;
  WriteLn(CountEntries:2, ' Eindeutige SubStrings');
END. (* UniqueSubStr *)