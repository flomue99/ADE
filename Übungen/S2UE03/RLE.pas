(* RLE:                                                 Author, 2023-03-29 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM RLE;

PROCEDURE WriteCharWithCount(ch: CHAR; count: INTEGER);
BEGIN (* WriteCharWithCount *)
  Write(ch);
  IF (count > 2) THEN BEGIN
    Write(count);
  END ELSE IF (count = 2) THEN BEGIN
    Write(ch);
  END; (* IF *)
END; (* WriteCharWithCount *)

VAR
  t: TEXT;
  ch, prevCh: CHAR;
  count: INTEGER;
  infilename: STRING;
BEGIN (* RLE *)
  IF ParamCount > 0 THEN
    infilename := ParamStr(1)
  ELSE
    infilename := '';

  Assign(t, 'c:/temp/rle_test.txt');
  Reset(t);
  count := 0;
  prevCh := '0';
  REPEAT
    Read(t, ch);
    IF (ch = prevCh) OR (prevCh = '0') THEN BEGIN
      Inc(count);
    END ELSE BEGIN
      WriteCharWithCount(prevCh, count);
      count := 1;
    END; (* IF *)
    prevCh := ch;
  UNTIL EOF(t);
  IF (prevCh <> '0') THEN BEGIN
    WriteCharWithCount(ch, count);
  END; (* IF *)
  Close(t);
END. (* RLE *)
