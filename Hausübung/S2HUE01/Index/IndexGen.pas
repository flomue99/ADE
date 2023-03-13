(* IndexGen:                                     HDO, 2002-02-28 *)
(* --------                                                      *)
(* Generation of a sorted index of all words in a text file.     *)
(* Command line:                                                 *)
(*    IndexGen [ textFileName ]                                  *)
(* ============================================================= *)
PROGRAM IndexGen;
  USES
    Timer,
    ModIndexHashProbing;

  CONST
    ef = Chr(0);       (* end of file character *)
    maxWordLen = 30;   (* max. number of characters per word *)
    chars = ['a' .. 'z', 'ä', 'ü', 'ö', 'ß',
             'A' .. 'Z', 'Ä', 'Ü', 'Ö'];    
  TYPE
    Word = STRING[maxWordLen];

  VAR
    txt: TEXT;           (* text file *)
    curLine: STRING;     (* current line from file txt *)
    curCh: CHAR;         (* current character *)
    curLineNr: INTEGER;  (* current line number *)
    curColNr: INTEGER;   (* current column number *)

  FUNCTION LowerCase(ch: CHAR): STRING;
  BEGIN (* LowerCase *)
    CASE ch OF
      'A'..'Z': LowerCase := Chr(Ord(ch) + (Ord('a') - Ord('A')));
      'Ä', 'ä': LowerCase := 'ae';
      'Ö', 'ö': LowerCase := 'oe';
      'Ü', 'ü': LowerCase := 'ue';
      'ß':      LowerCase := 'ss';
      ELSE (* all the others *)
                LowerCase := ch;
      END; (* CASE *)
  END; (* LowerCase *)

  PROCEDURE GetNextChar; (* updates curChar, ... *)
  BEGIN (* GetNextChar *)
    IF (curColNr < Length(curLine)) THEN BEGIN
      curColNr := curColNr + 1;
      curCh := curLine[curColNr]
    END ELSE BEGIN (* curColNr >= Length(curLine) *)
      IF NOT Eof(txt) THEN BEGIN
        ReadLn(txt, curLine);
        curLineNr:= curLineNr + 1;
        curColNr := 0;
        curCh := ' '; (* separate lines by ' ' *)
      END ELSE BEGIN (* Eof(txt) *)
        curCh := EF;
      END; (* IF *)
    END; (* IF *)
  END; (* GetNextChar *)

  PROCEDURE GetNextWord(VAR w: Word; VAR lnr: INTEGER);
  BEGIN (* GetNextWord *)
    WHILE (curCh <> ef) AND NOT (curCh IN chars) DO BEGIN
      GetNextChar;
    END; (* WHILE *)
    lnr := curLineNr;
    IF curCh <> ef THEN BEGIN
      w := LowerCase(curCh);
      GetNextChar;
      WHILE (curCh <> ef) AND (curCh IN chars) DO BEGIN
        w := Concat(w , LowerCase(curCh));
        GetNextChar;
      END; (* WHILE *)
    END ELSE BEGIN (* curCh = ef *)
      w := '';
    END; (* IF *)
  END; (* GetNextWord *)

  VAR
    txtName: STRING;
    w: Word;        (* current word *)
    lnr: INTEGER;   (* line number of current word *)
    n: LONGINT;     (* number of words *)

BEGIN (* IndexGen *)

  Write('IndexGen: index generation for text file ');

  IF ParamCount = 0 THEN BEGIN
    WriteLn;
    WriteLn;
    Write('name of text file > ');
    ReadLn(txtName);
  END ELSE BEGIN
    txtName := ParamStr(1);
    WriteLn(txtName);
  END; (* IF *)
  WriteLn;

  (* --- read text from text file --- *)
  Assign(txt, txtName);
  Reset(txt);
  curLine := '';
  curLineNr := 0;
  curColNr := 1; (* curColNr > Length(curLine) forces reading of first line *)
  GetNextChar;   (* curCh now holds first character *)

  StartTimer;
  GetNextWord(w, lnr);
  n := 0;
  WHILE Length(w) > 0 DO BEGIN
    //WriteLn(w, ' ', lnr);
    Insert(w, lnr);
    n := n + 1;
    GetNextWord(w, lnr);
  END; (* WHILE *)
  StopTimer;
  Close(txt);
  CreateIndex;
  DisposeIndex;
END. (* IndexGen *)