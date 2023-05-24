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

FUNCTION CalculateTabs(s: STRING; tabSize: INTEGER): INTEGER;
BEGIN (* CalculateTabs *)
  CalculateTabs := tabSize - Length(s);
END; (* CalculateTabs *)

FUNCTION TabString(tabs : INTEGER): STRING;
  VAR
    i: INTEGER;
    result: STRING;
BEGIN (* TabString *)
  result := '';
  FOR i := 1 TO tabs DO BEGIN
    result := result + ' ';
  END; (* FOR *)
  TabString := result;
END; (* TabString *)

CONSTRUCTOR TabStringBuilderObj.Init(tabSize: INTEGER);
BEGIN (* TabStringBuilderObj.Init *)
  INHERITED Init; (* ctor from base class *)
  IF (tabSize <= 0) THEN BEGIN (* if tabSize is not legal, tabsize = 10; *)
    SELF.tabSize := 10;
  END ELSE BEGIN 
    SELF.tabSize := tabSize;
  END; (* IF *)
END; (* TabStringBuilderObj.Init *)

PROCEDURE TabStringBuilderObj.AppendStr(e: STRING);
  VAR
    tabs: INTEGER;
BEGIN (* TabStringBuilderObj.AppendStr *)
  tabs := CalculateTabs(e,tabSize);
  IF (tabs < 0) THEN BEGIN (*ERROR*)
    WriteLn('ERROR: Word is longer then tabsize!');
    buffer := buffer+ TabString(tabSize);
  END ELSE BEGIN
    buffer := buffer + e + TabString(tabs);
  END; (* IF *)
END; (* TabStringBuilderObj.AppendStr *)

PROCEDURE TabStringBuilderObj.AppendChar(e: CHAR);
BEGIN (* TabStringBuilderObj.AppendChar *)
  buffer := buffer + e + TabString(tabSize - 1);
END; (* TabStringBuilderObj.AppendChar *)

PROCEDURE TabStringBuilderObj.AppendInt(e: INTEGER);
  VAR
    eStr: STRING;
    tabs: INTEGER;
BEGIN (* TabStringBuilderObj.AppendInt *)
  Str(e, eStr);
  tabs := CalculateTabs(eStr ,tabSize);
  buffer := buffer + eStr + TabString(tabs);
END; (* TabStringBuilderObj.AppendInt *)

PROCEDURE TabStringBuilderObj.AppendBool(e: BOOLEAN);
BEGIN (* TabStringBuilderObj.AppendBool *)
  IF (e = TRUE) THEN BEGIN
    buffer := buffer + 'TRUE' + TabString(tabSize - 4);
  END ELSE BEGIN
    buffer := buffer + 'FALSE' + TabString(tabSize - 5);
  END; (* IF *)
END; (* TabStringBuilderObj.AppendBool *)

VAR
  s: StringBuilder;
  t1: TabStringBuilder;
  t2: TabStringBuilder;
  t3: TabStringBuilder;
BEGIN;
  New(s, Init);
  s^.AppendStr('Florian');
  s^.AppendChar('/');
  s^.AppendInt(0909);
  s^.AppendBool(TRUE);
  WriteLn(s^.AsString);
  Dispose(s);

  New(t1, Init(10));
  t1^.AppendStr('Florian');
  t1^.AppendChar('/');
  t1^.AppendInt(0909);
  t1^.AppendBool(TRUE);
  WriteLn(t1^.AsString);
  Dispose(t1);


  New(t2, Init(10));
  t2^.AppendStr('Jonas45234533452345234523453');
  t2^.AppendChar('-');
  t2^.AppendInt(132);
  t2^.AppendBool(FALSE);
  WriteLn(t2^.AsString);
  Dispose(t2);

  New(t3, Init(10));
  t3^.AppendStr('Doris');
  t3^.AppendChar('+');
  t3^.AppendInt(137);
  t3^.AppendBool(TRUE);
  WriteLn(t3^.AsString);
  Dispose(t3);
END. (* OOPStringBuilder *)