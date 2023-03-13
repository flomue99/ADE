(* StringSortPrg:                                            MFL, 2023-01-19 *)
(* ------                                                                    *)
(* sort string arrays                                                        *)
(* ========================================================================= *)
PROGRAM StringSortPrg;

  CONST 
    n = 10;
  TYPE
    StringArray = ARRAY [0..n-1] OF STRING;

  FUNCTION StringContains(str, substr: STRING): BOOLEAN;
    VAR
      i, j: INTEGER;
      contains: BOOLEAN;
  BEGIN (* StringContains *)
    IF ((str = '') OR (substr = '')) THEN BEGIN
      Exit(FALSE);
    END; (* IF *)
    contains := FALSE;
    i := 1;
    j := 1;
    WHILE ((contains <> TRUE) AND (i <= Length(str))) DO BEGIN
      IF (str[i] = substr[j]) THEN BEGIN (* firstleter found, index from subtr + 1 *)
        IF (j = Length(substr)) THEN BEGIN
          contains := TRUE;
        END ELSE BEGIN
          j := j + 1;
        END;
      END ELSE BEGIN
        j := 1;
      END;
      i := i + 1;
    END; (* WHILE *)
    StringContains := contains;
  END; (* StringContains *)

  PROCEDURE StringInsertionSort(VAR arr: ARRAY OF STRING; left, right: INTEGER);
    VAR
      i, j: INTEGER;
      h: STRING;
  BEGIN (* StringInsertionSort *)
    FOR i := left TO (right - 1) DO BEGIN
      h := arr[i + 1];
      j := i;
      WHILE ((j >= left) AND (h < arr[j])) DO BEGIN
        arr[j + 1] := arr[j];
        j := j - 1;
      END; (* WHILE *)
      arr[j + 1] := h;
    END; (* FOR *)
  END; (* StringInsertionSort *)

  PROCEDURE SubstrStringInsertionSort(VAR arr: ARRAY OF STRING; substr: STRING; left, right: INTEGER);
    VAR
      i, j: INTEGER;
      h: STRING;
  BEGIN (* SubstrStringInsertionSort *)
    FOR i := left TO (right - 1) DO BEGIN
      h := arr[i + 1];
      j := i;
      IF (StringContains(h, substr)) THEN BEGIN
        WHILE ((j >= left) AND (NOT StringContains(arr[j], substr))) DO BEGIN
          arr[j + 1] := arr[j];
          j := j - 1;
        END; (* WHILE *)
        arr[j + 1] := h;
      END ELSE BEGIN
        WHILE ((j >= left) AND (h < arr[j])) DO BEGIN
          arr[j + 1] := arr[j];
          j := j - 1;
        END; (* WHILE *)
        arr[j + 1] := h;
      END;
    END; (* FOR *)
  END; (* SubstrStringInsertionSort *)

  PROCEDURE WriteStringArray(arr: ARRAY OF STRING);
    VAR
      i: INTEGER;
  BEGIN (* WriteStringArray *)
    FOR i := 0 TO High(arr) DO BEGIN
      Write(i:3,' |');
      WriteLn(arr[i]);
    END; (* FOR *)
  END; (* WriteStringArray *)

  VAR
    stringArr: StringArray;
BEGIN (* StringSortPrg *)
  stringArr[0] := 'Montag';
  stringArr[1] := 'Dienstag';
  stringArr[2] := 'Mittwoch';
  stringArr[3] := 'Donnerstag';
  stringArr[4] := 'Freitag';
  stringArr[5] := 'Samstag';
  stringArr[6] := 'Sonntag';
  stringArr[7] := 'C';
  stringArr[8] := 'B';
  stringArr[9] := 'A';
  StringInsertionSort(stringArr, 0, n-1);
  WriteStringArray(stringArr);
  WriteLn('----------------------------------------');
  SubstrStringInsertionSort(stringArr,'tag' ,0, n-1);
  WriteStringArray(stringArr);
END. (* StringSortPrg *)
