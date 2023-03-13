(* CamelCasePrg:                                             MFL, 2022-11-09 *)
(* ------                                                                    *)
(* Convert words to CamelCase and back                                       *)
(* ========================================================================= *)
PROGRAM CamelCasePrg;
    
  PROCEDURE Tokenize(s: STRING; delimiter: CHAR; VAR tokens: ARRAY OF STRING; VAR n: INTEGER);
    VAR
      i, pos, wordIndex: INTEGER;
  BEGIN (* Tokenize *)
    pos := 1;
    wordIndex := 0;

    FOR i := 1 TO Length(s) DO BEGIN
      IF (s[i] = delimiter) THEN BEGIN
        IF (pos > 1) THEN BEGIN
          tokens[wordIndex][0] := Chr(pos - 1); (* set length byte*)
          Inc(wordIndex);
          pos := 1;
        END; (* IF *)
      END ELSE BEGIN
        tokens[wordIndex][pos] := s[i];
        Inc(pos);
      END; (* IF *)
    END; (* FOR *)

    IF (pos > 1) THEN BEGIN
      tokens[wordIndex][0] := Chr(pos - 1); (* set length byte*)
      Inc(wordIndex);
    END; (* IF *)
    n := wordIndex;
 
  END; (* Tokenize *)


  FUNCTION Words2CamelCase(words: STRING): STRING;
  BEGIN (* Words2CamelCase *)
    

  END; (* Words2CamelCase *)
 
  CONST
   max = 100;
  VAR 
    tokens: ARRAY[1..max] OF STRING;
    i, n: INTEGER;
BEGIN (* CamelCasePrg *)
  Tokenize('     Hallo Welt! a a a a b b b ',' ', tokens,n);
 
  FOR i := 1 TO n DO BEGIN
    WriteLN(tokens[i]);
  END; (* FOR *)
END. (* CamelCasePrg *)