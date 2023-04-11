(* RLE:                                                      MFL, 2023-03-29 *)
(* ------                                                                    *)
(* Programm to compress and decompress text                                  *)
(* ========================================================================= *)
PROGRAM RLE;

FUNCTION IsNumber(ch: CHAR): BOOLEAN;
BEGIN (* IsNumber *)
  IsNumber := (Ord(ch) >= 48) AND (Ord(ch) <= 57);
END; (* IsNumber *)

FUNCTION ValidFile(VAR inFile: TEXT): BOOLEAN;
  VAR
    ch: CHAR;
BEGIN (* ValidFile *)
  REPEAT
    Read(inFile, ch);
  UNTIL EOF(inFile) OR IsNumber(ch);
  ValidFile := EOF(inFile);
END; (* ValidFile *)

PROCEDURE WriteCharWithCountToFile(ch: CHAR; count: INTEGER; VAR outFile: TEXT);
BEGIN (* WriteCharWithCountToFile *)
  Write(outFile, ch);
  IF (count > 2) THEN BEGIN
    Write(outFile, count);
  END ELSE IF (count = 2) THEN BEGIN
    Write(outFile, ch);
  END; (* IF *)
END; (* WriteCharWithCountToFile *)

PROCEDURE DecompressCharWithCountToFile(ch: CHAR; count: INTEGER; VAR outFile: TEXT);
  VAR
    i: INTEGER;
BEGIN (* DecompressCharWithCountToFile *)
  FOR i := 1 TO (count - 1) DO BEGIN
    Write(outFile, ch);
  END; (* FOR *)
END; (* DecompressCharWithCountToFile *)

PROCEDURE WriteCharWithCount(ch: CHAR; count: INTEGER);
BEGIN (* WriteCharWithCount *)
  Write(ch);
  IF (count > 2) THEN BEGIN
    Write(count);
  END ELSE IF (count = 2) THEN BEGIN
    Write(ch);
  END; (* IF *)
END; (* WriteCharWithCount *)

PROCEDURE DecompressCharWithCount(ch: CHAR; count: INTEGER);
  VAR
    i: INTEGER;
BEGIN (* DecompressCharWithCount *)
  FOR i := 1 TO (count - 1) DO BEGIN
    Write(ch);
  END; (* FOR *)
END; (* DecompressCharWithCount *)

PROCEDURE CompressText(inFileName, outFileName: STRING);
  VAR
    inFile, outFile: TEXT;
    ch, prevCh: CHAR;
    count : INTEGER;
BEGIN (* CompressText *)
  (* open files *)
  Assign(inFile, inFileName);
  {$i-}
  Reset(inFile); 
  {$i+}
  IF (IOResult <> 0) THEN BEGIN
    WriteLn('ERROR: File ', inFileName, ' does not exist!');
  END ELSE IF(not ValidFile(inFile)) THEN BEGIN
    WriteLn('ERROR: File ', inFileName, ' is not valid!');
    Close(inFile);
  END ELSE BEGIN
    Reset(inFile);
    count := 0;
    prevCh := '0';
    IF (outFileName <> '') THEN BEGIN
      Assign(outFile, outFileName);
      Rewrite(outFile);
    END; (* IF *)
    REPEAT
      Read(inFile, ch);
      IF (ch = prevCh) OR (prevCh = '0') THEN BEGIN
        Inc(count);
      END ELSE BEGIN
         IF (outFileName <> '') THEN BEGIN
          WriteCharWithCountToFile(prevCh, count, outFile);
        END ELSE BEGIN
          WriteCharWithCount(prevCh, count);
        END; (* IF *)
        count := 1;
      END; (* IF *)
      prevCh := ch;
    UNTIL EOF(inFile);
  
    IF (prevCh <> '0') THEN BEGIN
        IF (outFileName <> '') THEN BEGIN
          Write(outFile, ch);
        END ELSE BEGIN
          Write(ch);
        END; (* IF *)
    END; (* IF *)
  
    (* Close files*)
    Close(inFile); 
    IF (outFileName <> '') THEN BEGIN
      Close(outFile);
    END; (* IF *) 
  END;
END; (* CompressText *)

PROCEDURE DecompressText(inFileName, outFileName: STRING);
  VAR
    inFile, outFile: TEXT;
    ch, prevCh: CHAR;
    number: STRING;
    count : INTEGER;
BEGIN (* DecompressText *)
  (* open files *)
  Assign(inFile, inFileName);
  {$i-}
  Reset(inFile); 
  {$i+}
  IF (IOResult <> 0) THEN BEGIN
    WriteLn('ERROR: File ', inFileName, ' does not exist!');
  END ELSE BEGIN
    count := 0;
    number := '';
    prevCh := '0';
    IF (outFileName <> '') THEN BEGIN
      Assign(outFile, outFileName);
      Rewrite(outFile);
    END; (* IF *)

    REPEAT
      Read(inFile, ch);
      WHILE (IsNumber(ch)) DO BEGIN
        number := number + ch;
        Read(inFile, ch);
      END; (* WHILE *)
      IF (number <> '') THEN BEGIN
        Val(number, count);
        IF (outFileName <> '') THEN BEGIN
          DecompressCharWithCountToFile(prevCh, count, outFile);
          Write(outFile, ch);
        END ELSE BEGIN
          DecompressCharWithCount(prevCh, count);
          Write(ch);
        END; (* IF *)
        number := '';
      END ELSE BEGIN
         IF (outFileName <> '') THEN BEGIN
           Write(outFile, ch);
         END ELSE BEGIN
           Write(ch);
        END; (* IF *)
      END; (* IF *)
      prevCh := ch;
    UNTIL EOF(inFile);
  
    (* Close files*)
    Close(inFile); 
    IF (outFileName <> '') THEN BEGIN
      Close(outFile);
    END; (* IF *) 
  END;
END; (* DecompressText *)

BEGIN (* RLE *)
  IF (ParamCount = 3) THEN BEGIN (* -c infile.txt outfile.txt ||  -d infile.txt outfile.txt *)
    IF (ParamStr(1) = '-d') THEN BEGIN
      DecompressText(ParamStr(2), ParamStr(3)); 
    END ELSE BEGIN
      CompressText(ParamStr(2), ParamStr(3));
    END; (* IF *)
  END ELSE IF ((ParamStr(1) <> '-d')  AND (ParamStr(1) <> '-c')) AND (ParamCount = 2) THEN BEGIN   (* infile.txt outfile.txt *)
    CompressText(ParamStr(1), ParamStr(2));
  END ELSE IF ((ParamStr(1) <> '-d')  OR (ParamStr(1) <> '-c')) AND (ParamCount = 1) THEN BEGIN   (* infile.txt*)
    CompressText(ParamStr(1), '');
  END ELSE IF ((ParamStr(1) = '-d') AND (ParamCount = 2)) THEN BEGIN (* -d infile.txt *)
    DecompressText(ParamStr(2), '');
  END ELSE IF ((ParamStr(1) = '-c') AND (ParamCount = 2)) THEN BEGIN (* -c infile.txt *)
    CompressText(ParamStr(2), '');
  END ELSE BEGIN
    WriteLn('ERROR: Usage: RLE.pas [ -c | -d ] inFile [outFile]')
  END; (* IF *)
END. (* RLE *)
