UNIT ModHashProbing;

INTERFACE
  PROCEDURE Insert(str: STRING);
  PROCEDURE WriteHashtable;
  
IMPLEMENTATION
USES ModHashFunctions;

CONST
  M = 31259;
TYPE
  HashTable = ARRAY[0..M-1] OF STRING;

VAR
  ht: HashTable;
  
  PROCEDURE Insert(str: STRING);
  VAR
    h, step: WORD;
    tries: WORD;
  BEGIN (* Insert *)
    h := Hash1(str) MOD M;
    step := Hash2(str) MOD M;
    IF STEP = 0 THEN STEP := 1;
    tries := 0;
    WHILE ((ht[h] <> str) AND (ht[h] <> '')) DO BEGIN
      Inc(tries);
      (* linear probing *)
      //h := (h + 1) MOD M;
      (* quadratic probing *)
      //h := (h + (tries * tries)) MOD M;
      (* double hashing *)
      h := (h + step) MOD M;
      IF (tries >= M) THEN BEGIN
        WriteLn('Hashtable is full!');
        HALT;
      END; (* IF *)
    END; (* WHILE *)
    (* insert only if not found*)
    IF (ht[h] = '') THEN BEGIN
      ht[h] := str;
    END; (* IF *)
  END; (* Insert *)

  PROCEDURE WriteHashtable;
  VAR
    i: INTEGER;
  BEGIN (* WriteHashtable*)
    FOR i := 0 TO M-1 DO BEGIN
      //IF (ht[i] <> '') THEN BEGIN
        WriteLn(i, ': ', ht[i]);
      //END; (* IF *)
    END; (* FOR *)
  END; (* WriteHashtable*)

VAR
  i: INTEGER;
BEGIN (* ModHashProbing *)
  FOR i := 0 TO M-1 DO BEGIN
    ht[i] := '';
  END; (* FOR *)
END. (* ModHashProbing *)