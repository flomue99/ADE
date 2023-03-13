(* StringFunctionsPrg:                                       MFL, 2022-11-20 *)
(* ------                                                                    *)
(* modify strings with functions                                             *)
(* ========================================================================= *)
PROGRAM StringFunctionsPrg;

  FUNCTION WithoutLastChar(s: STRING): STRING;
  BEGIN (* WithoutLastChar *)
    Delete(s, Length(s), 1); (* delete last element*)
    WithoutLastChar := s;
  END; (* WithoutLastChar *)

  FUNCTION EqualsIgnoreCase(a: STRING; b: STRING): BOOLEAN;
  BEGIN (* EqualsIgnoreCase *)
    EqualsIgnoreCase := (upCase(a) = upCase(b));
  END; (* EqualsIgnoreCase *)

  FUNCTION StringToLowerCase(s: STRING ): STRING; (* converts all uppercase chars to lowercase chars*)
    VAR
      i, tempOrd: INTEGER;
  BEGIN (* StringToLowerCase *)
    tempOrd := 0;
    FOR i := 1 TO Ord(s[0]) DO BEGIN
      tempOrd := Ord(s[i]);
      IF ((tempOrd >= 65) AND (tempOrd <= 90)) THEN BEGIN
        s[i] := Chr(tempOrd+32);
      END; (* IF *)
    END; (* FOR *)
    StringToLowerCase := s;
  END; (* StringToLowerCase *)

  FUNCTION StringValidation(words: ARRAY OF STRING; n: INTEGER): BOOLEAN;
    VAR
      result: BOOLEAN;
      i, j, tempOrd: INTEGER;
  BEGIN (* StringValidation *)
    tempOrd := 0;
    i := 0;
    result := TRUE;
    WHILE (result AND (i <= n-1)) DO BEGIN
      IF (words[i] = '') THEN BEGIN (* empty word *)
        result := FALSE;
      END ELSE BEGIN
        FOR j := 1 TO Length(words[i]) DO BEGIN     
          tempOrd := Ord(words[i][j]);
          (* invalid char *)
          IF ((tempOrd < 48) OR ((tempOrd > 57) AND (tempOrd < 65)) OR ((tempOrd > 90) AND (tempOrd < 97)) OR  (tempOrd > 122)) THEN BEGIN
            result := FALSE;
          END; (* IF *)
        END; (* FOR *)       
      END; (* IF *)
      i := i + 1;
    END; (* WHILE *)
    StringValidation := result;
  END; (* StringValidation *)

  FUNCTION SizeValidation(words: ARRAY OF STRING; n: INTEGER): BOOLEAN;
    VAR
      i, count: INTEGER;
  BEGIN (* SizeValidation *)
    count := 0;
    FOR i := 0 TO (n - 1) DO BEGIN
      count := count + Length(words[i]);
    END; (* FOR *)
    SizeValidation := count < 256; (* 255 = max string size *)
  END; (* SizeValidation *)

  FUNCTION CamelCase(words: ARRAY OF STRING; n: INTEGER): STRING;
    VAR
      result: STRING;
      i: INTEGER;
  BEGIN (* CamelCase *)
    result := '';
    IF ((NOT StringValidation(words,n)) OR (NOT SizeValidation(words,n) OR (n <= 0)))THEN BEGIN
      result := 'ERROR';
      Exit(result);
    END;
    FOR i := 0 TO (n - 1) DO BEGIN
      words[i] := StringToLowerCase(words[i]); (* lowercase the string *)
      IF (i > 0) THEN BEGIN
        words[i][1] := upCase(words[i][1]); (* upcase first char *)   
      END; (* IF *)
      result := result + words[i];
    END; (* FOR *)
    CamelCase := result;
  END; (* CamelCase *)

  CONST
    words1: Array OF STRING = ('My', 'cAMEL', 'CaSe', 'nAMe2'); 
    words2: Array OF STRING = ('Camel', 'Case', 'Test');
    words3: Array OF STRING = ('C#mel', 'C!se', 'Test');
    words4: Array OF STRING = ('Florian', '', 'Test');
    words5: Array OF STRING = ('FloRiAnS', 'fAnCy', 'tEsT');
BEGIN (* StringFunctionsPrg *)
  WriteLn('Eingabe1: ', EqualsIgnoreCase('PASCAl','Pascal'));
  WriteLn('Eingabe2: ',EqualsIgnoreCase('Florian','Flori'));
  WriteLn('Eingabe3: ',EqualsIgnoreCase('SoftWAre','sOFTwAre'));
  WriteLn('Eingabe1: ',WithoutLastChar('Florian'));
  WriteLn('Eingabe2: ',WithoutLastChar('Test'));
  WriteLn('Eingabe3: ',WithoutLastChar('Software'));
  WriteLn('Eingabe1: ',CamelCase(words1,4));
  WriteLn('Eingabe2: ',CamelCase(words5,3));
  WriteLn('Eingabe3: ',CamelCase(words2,0));
  WriteLn('Eingabe4: ',CamelCase(words3,3));
  WriteLn('Eingabe5: ',CamelCase(words4,3));
END. (* StringFunctionsPrg *)
