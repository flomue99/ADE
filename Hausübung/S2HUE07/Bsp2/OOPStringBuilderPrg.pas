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

  StringJoiner = ^StringJoinerObj;
  StringJoinerObj = OBJECT
                     delimiter: CHAR;
                     sBuilder: StringBuilder;
                     CONSTRUCTOR Init(delimiter: CHAR);
                     PROCEDURE Add(e: STRING);
                     FUNCTION AsString: STRING;
                     DESTRUCTOR Done; VIRTUAL;
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
  IF (tabSize < 5) THEN BEGIN (* if tabSize is not legal, tabsize = 5; *)
    SELF.tabSize := 5;
  END ELSE BEGIN 
    SELF.tabSize := tabSize;
  END; (* IF *)
END; (* TabStringBuilderObj.Init *)

PROCEDURE TabStringBuilderObj.AppendStr(e: STRING);
  VAR
    tabs: INTEGER;
BEGIN (* TabStringBuilderObj.AppendStr *)
  IF (Length(e) > tabSize) THEN BEGIN
    buffer := buffer + Copy(e, 1, tabSize);
  END ELSE BEGIN
    tabs := CalculateTabs(e,tabSize);
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
  IF (Length(eStr) > tabSize) THEN BEGIN
    buffer := buffer + Copy(eStr, 1, tabSize);
  END ELSE BEGIN
    tabs := CalculateTabs(eStr,tabSize);
    buffer := buffer + eStr + TabString(tabs);
  END; (* IF *)
END; (* TabStringBuilderObj.AppendInt *)

PROCEDURE TabStringBuilderObj.AppendBool(e: BOOLEAN);
BEGIN (* TabStringBuilderObj.AppendBool *)
  IF (e = TRUE) THEN BEGIN
    buffer := buffer + 'TRUE' + TabString(tabSize - 4);
  END ELSE BEGIN
    buffer := buffer + 'FALSE' + TabString(tabSize - 5);
  END; (* IF *)
END; (* TabStringBuilderObj.AppendBool *)

(*-------------------StringJoiner-------------------*)
CONSTRUCTOR StringJoinerObj.Init(delimiter: CHAR);
BEGIN (* StringJoiner.Init *)
  New(SELF.sBuilder, Init);
  SELF.delimiter := delimiter;
END; (* StringJoiner.Init *) 

PROCEDURE StringJoinerObj.Add(e: STRING);
BEGIN (* StringJoinerObj.Add *)
  IF (sBuilder^.buffer <> '') THEN BEGIN
    e := delimiter + e;
  END; (* IF *)
  sBuilder^.AppendStr(e);
END; (* StringJoinerObj.Add *)

FUNCTION StringJoinerObj.AsString: STRING;
BEGIN (* StringJoinerObj.AsString *)
  AsString := sBuilder^.AsString;
END; (* StringJoinerObj.AsString *)
  
DESTRUCTOR StringJoinerObj.Done;
BEGIN  (* StringJoinerObj.Done *)
  Dispose(sBuilder);
END; (* StringJoinerObj.Done *)


VAR
  s: StringBuilder;
  t: TabStringBuilder;
  t1: TabStringBuilder;
  t2: TabStringBuilder;
  t3: TabStringBuilder;
  j: StringJoiner;
BEGIN;
  //New(s, Init);
  //s^.AppendStr('Eins');
  //s^.AppendChar(' ');
  //s^.AppendInt(2);
  //s^.AppendChar(' ');
  //s^.AppendBool(TRUE);
  //WriteLn(s^.AsString);
  //
  //New(t, Init(8));
  //t^.AppendStr('Eins');
  //t^.AppendInt(2);
  //t^.AppendBool(TRUE);
  //WriteLn(t^.AsString);
  //Dispose(t);


  //New(s, Init);
  //s^.AppendStr('Florian');
  //s^.AppendChar('/');
  //s^.AppendInt(0909);
  //s^.AppendBool(TRUE);
  //WriteLn(s^.AsString);
  //Dispose(s);

  New(t1, Init(6));
  t1^.AppendStr('Florian');
  t1^.AppendChar('/');
  t1^.AppendInt(0909);
  t1^.AppendBool(TRUE);
  WriteLn(t1^.AsString);

  New(t2, Init(6));
  t2^.AppendStr('Jonas45234533452345234523453');
  t2^.AppendChar('-');
  t2^.AppendInt(11111);
  t2^.AppendBool(FALSE);
  WriteLn(t2^.AsString);

  New(t3, Init(6));
  t3^.AppendStr('Doris');
  t3^.AppendChar('+');
  t3^.AppendInt(137);
  t3^.AppendBool(TRUE);
  WriteLn(t3^.AsString);
  
  Dispose(t1);
  Dispose(t2);
  Dispose(t3);

  New(j, Init('-'));
  j^.Add('AbA');
  j^.Add('AbA');
  j^.Add('AbA');
  WriteLn(j^.AsString);
  Dispose(j,Done);

END. (* OOPStringBuilder *)