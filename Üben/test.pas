(* OOPStringBuilder:                                          FL, 2023-05-11 *)
(* ------                                                                    *)
(* oop stringbuilder                                                         *)
(* ========================================================================= *)
PROGRAM OOPStringBuilder;
 
TYPE
  StringBuilder= ^StringBuilderObj;
  StringBuilderObj = OBJECT
                    buffer: STRING;
                    CONSTRUCTOR Init;
                    PROCEDURE AppendStr(e: STRING); VIRTUAL;
                    PROCEDURE AppendChar(e: CHAR); VIRTUAL;
                    PROCEDURE AppendInt(e: INTEGER); VIRTUAL;
                    PROCEDURE AppendBool(e: BOOLEAN); VIRTUAL;
                    FUNCTION AsString: STRING;
                  END;

  TabStringbuilder = ^TabStringBuilderObj;
  TabStringBuilderObj = OBJECT(StringBuilderObj)
                        tabSize: INTEGER; 
                        CONSTRUCTOR Init(tabSize: INTEGER);
                        PROCEDURE AppendStr(e: STRING); VIRTUAL;
                        PROCEDURE AppendChar(e: CHAR); VIRTUAL;
                        PROCEDURE AppendInt(e: INTEGER); VIRTUAL;
                        PROCEDURE AppendBool(e: BOOLEAN); VIRTUAL;
                        END;

(*-------------------StringBuilder-------------------*)

CONSTRUCTOR StringBuilderObj.Init;
BEGIN (* StringBuilderObj.Init *)
  buffer := '';
END; (* StringBuilderObj.Init *)

PROCEDURE StringBuilderObj.AppendStr(e: STRING);
BEGIN (* StringBuilderObj.AppendStr *)
  buffer := buffer + e;
END; (* StringBuilderObj.AppendStr *)

PROCEDURE StringBuilderObj.AppendChar(e: CHAR);
BEGIN (* StringBuilderObj.AppendChar *)
  buffer := buffer + e;
END; (* StringBuilderObj.AppendChar *)

PROCEDURE StringBuilderObj.AppendInt(e: INTEGER);
  VAR
    eStr: STRING;
BEGIN (* StringBuilderObj.AppendInt *)
  Str(e, eStr);
  buffer := buffer + eStr;
END; (* StringBuilderObj.AppendInt *)


PROCEDURE StringBuilderObj.AppendBool(e: BOOLEAN);
BEGIN (* StringBuilderObj.AppendBool *)
  IF (e = TRUE) THEN BEGIN
    buffer := buffer + 'TRUE';
  END ELSE BEGIN
    buffer := buffer + 'FALSE';
  END; (* IF *)
END; (* StringBuilderObj.AppendBool *)

FUNCTION StringBuilderObj.AsString: STRING;
BEGIN (* StringBuilderObj.AsString *)
  AsString := Buffer;
END; (* StringBuilderObj.AsString *)

(*-------------------TabStringBuilder-------------------*)

CONSTRUCTOR TabStringBuilderObj.Init(tabSize: INTEGER);
BEGIN (* TabStringBuilderObj.Init *)
  INHERITED Init; (* ctor from base class *)
  SELF.tabSize := tabSize;
END; (* TabStringBuilderObj.Init *)

PROCEDURE TabStringBuilderObj.AppendStr(e: STRING);
BEGIN (* TabStringBuilderObj.AppendStr *)
  buffer := buffer + e;
END; (* TabStringBuilderObj.AppendStr *)

PROCEDURE TabStringBuilderObj.AppendChar(e: CHAR);
BEGIN (* TabStringBuilderObj.AppendChar *)
  buffer := buffer + e;
END; (* TabStringBuilderObj.AppendChar *)

PROCEDURE TabStringBuilderObj.AppendInt(e: INTEGER);
  VAR
    eStr: STRING;
BEGIN (* TabStringBuilderObj.AppendInt *)
  Str(e, eStr);
  buffer := buffer + eStr;
END; (* TabStringBuilderObj.AppendInt *)

PROCEDURE StringBuilderObj.AppendBool(e: BOOLEAN);
BEGIN (* TabStringBuilderObj.AppendBool *)
  IF (e = TRUE) THEN BEGIN
    buffer := buffer + 'TRUE';
  END ELSE BEGIN
    buffer := buffer + 'FALSE';
  END; (* IF *)
END; (* TabStringBuilderObj.AppendBool *)

VAR
  s: StringBuilder;
BEGIN;
  New(s, Init);
  s^.AppendStr('Florian');
  s^.AppendChar('/');
  s^.AppendInt(0909);
  s^.AppendBool(TRUE);
  WriteLn(s^.AsString);
  Dispose(s)

END. (* OOPStringBuilder *)