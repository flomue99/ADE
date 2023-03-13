(* :ValueConverterPrg                                        MFL, 2022-11-11 *)
(* ------                                                                    *)
(* Convert values with different base                                        *)
(* ========================================================================= *)
PROGRAM ValueConverterPrg;

  FUNCTION GetDigitValue(digit: CHAR): INTEGER;
    VAR
      digitValue, digitOrd: INTEGER;
  BEGIN (* GetDigitValue *)
    digitOrd := Ord(digit); (* gets the ordinal number of the given digit *)
    IF ((digitOrd >= 48) AND (digitOrd <= 57)) THEN BEGIN    (* Values 0..9 = 48..57 in ordinal numbers *)
      digitValue := digitOrd - 48; 
    END; (* IF *)
    IF ((digitOrd >= 65) AND (digitOrd <= 90)) THEN BEGIN (* Chars A..Z = 65..90 in ordinalnumbers *)
      digitValue := digitOrd - 55;
    END; (* IF *)  
    GetDigitValue := digitValue; 
  END; (* GetDigitValue *)

  (* validates all digits, value range  = [48..57] and [65..90] (ordinalnumbers of valid input digits) *)
  FUNCTION DigitsValidation(digits: STRING; base:INTEGER): BOOLEAN;
    VAR
      digitValue, i: INTEGER;
      result: BOOLEAN;
  BEGIN (* DigitsValidation *)
    result := True;
    i := 1;
    WHILE ((result = True) AND (i <= Length(digits))) DO BEGIN (* if one digit is invalid to the base, the function returns false *)
      digitValue := GetDigitValue(digits[i]);
      IF ((digitValue >= 0) AND (digitValue < base)) THEN BEGIN (* digit is valid to the base *)
        result := True;
      END ELSE BEGIN
        result := False; (* digit is invalid to the base *)
      END; (* IF *)
      Inc(i);
    END; (* WHILE *)
    DigitsValidation := result;
  END; (* DigitsValidation *)

  (* validates the base, value range = [2..36] *)
  FUNCTION BaseValidation(base: INTEGER): BOOLEAN; 
    VAR
      result: BOOLEAN;
  BEGIN (* BaseValidation *)
    IF ((base <= 36) AND (base >= 2)) THEN BEGIN
      result := True; (* valid base *)
    END ELSE BEGIN
      result := False; (* invalid base *)
    END; (* IF *)
    BaseValidation := result;
  END; (* BaseValidation *)

  FUNCTION Power(value, base, exp: INTEGER): INTEGER;
    VAR
      i, result: INTEGER;
  BEGIN (* Power *)
    result := 1;
    IF (exp >= 1) THEN BEGIN
      FOR i := 1 TO exp DO BEGIN
        result := result * base;
      END; (* FOR *)
    END; (* IF *)
    Power := result * value;
  END; (* Power *)

  FUNCTION ValueOf(digits: STRING; base:INTEGER): INTEGER;
    VAR
      i, exp: INTEGER;
      result: INTEGER;
  BEGIN (* ValueOf *)   
    result := 0;
    exp := Length(digits) - 1; (* highest exponet is always the amount of digits minus one *)
    (* if the base is between 1-36 and the digits are valid to the base and the result will be calculated, otherwise result := -1 *)
    IF ((BaseValidation(base)) AND (DigitsValidation(digits, base))) THEN BEGIN
      FOR i := 1 TO Length(digits) DO BEGIN
        result := result + Power(GetDigitValue(digits[i]), base, exp); (* result + value of the digit*)
        Dec(exp);
      END; (* FOR *)
    END ELSE BEGIN
      result := -1;
    END; (* IF *)
    ValueOf := result;
  END; (* ValueOf *)

  FUNCTION DigitsOf(value: INTEGER; base: INTEGER): STRING;
    VAR
      remainder, quotient: INTEGER;
      result: STRING;
  BEGIN (* DigitsOf *)
    result := '';
    quotient := value;
    IF (BaseValidation(base)) THEN BEGIN (* base validation*)
      WHILE (quotient <> 0) DO BEGIN 
        remainder := quotient MOD base; 
        IF (remainder <= 9) THEN BEGIN (* adding a number to the result string *)
          result := Chr(remainder + 48) + result; (* to get the number to the given ordinal number *)
        END; (* IF *)
        IF (remainder > 9) THEN BEGIN (* adding an char to the result string *)
          result := Chr(remainder + 55) + result ; (* to get the char to the given ordinal number *)
        END; (* IF *)
        quotient := quotient DIV base;
      END; (* WHILE *) 
    END ELSE BEGIN
      result:= 'Ungueltige Basis!'
    END; (* IF *)
    DigitsOf := result;
  END; (* DigitsOf *)

  FUNCTION Sum(v1: STRING; b1: INTEGER; v2: STRING; b2: INTEGER): INTEGER;
    VAR
      value1, value2: INTEGER;
      result: INTEGER;
  BEGIN (* Sum *)
    result := 0;
    value1 := ValueOf(v1, b1);
    value2 := ValueOf(v2, b2);
    IF ((value1 <> -1) AND (value2 <> -1)) THEN BEGIN (* one value with invalid base or invalid digits *)
      result := value1 + value2;
    END ELSE BEGIN
      result := -1;
    END; (* IF *)
    Sum := result;
  END; (* Sum *)

  FUNCTION Diff(v1: STRING; b1: INTEGER; v2: STRING; b2: INTEGER): INTEGER;
    VAR
      value1, value2: INTEGER;
      result: INTEGER;
  BEGIN (* Diff *)
    result := 0;
    value1 := ValueOf(v1, b1);
    value2 := ValueOf(v2, b2);
    IF ((value1 <> -1) AND (value2 <> -1)) THEN BEGIN (* one value with invalid base or invalid digits *)
      result := value1 - value2;
    END ELSE BEGIN
      result := -1;
    END; (* IF *)
    Diff := result;
  END; (* Diff *)

  FUNCTION Prod(v1: STRING; b1: INTEGER; v2: STRING; b2: INTEGER): INTEGER;
    VAR
      value1, value2: INTEGER;
      result: INTEGER;
  BEGIN (* Prod *)
    result := 0;
    value1 := ValueOf(v1, b1);
    value2 := ValueOf(v2, b2);
    IF ((value1 <> -1) AND (value2 <> -1)) THEN BEGIN (* one value with invalid base or invalid digits *)
      result := value1 * value2;
    END ELSE BEGIN
      result := -1;
    END; (* IF *)
    Prod := result;
  END; (* Prod *)

  FUNCTION Quot(v1: STRING; b1: INTEGER; v2: STRING; b2: INTEGER): INTEGER;
    VAR
      value1, value2: INTEGER;
      result: INTEGER;
  BEGIN (* Quot *)
    result := 0;
    value1 := ValueOf(v1, b1);
    value2 := ValueOf(v2, b2);
    IF ((value1 <> -1) AND (value2 <> -1)) THEN BEGIN (* one value with invalid base or invalid digits *)
      result := value1 DIV value2;
    END ELSE BEGIN
      result := -1;
    END; (* IF *)
    Quot := result;
  END; (* Quot *)
  
BEGIN (* ValueConverterPrg *)
  WriteLN('Input: value = Z,    base = 16)   expected result = -1     | ValueOf = ',ValueOf('Z', 16));
  WriteLN('Input: value = Z,    base = 1)    expected result = -1     | ValueOf = ',ValueOf('Z', 1));
  WriteLN('Input: value = Z,    base = 37)   expected result = -1     | ValueOf = ',ValueOf('Z', 37));
  WriteLN('Input: value = AGT,  base = 33)   expected result =  11447 | ValueOf = ',ValueOf('AGT', 33));
  WriteLN('Input: value = 1001, base = 2)    expected result =  9     | ValueOf = ',ValueOf('1001', 2));
  WriteLN('Input: value = ABC,  base = 16)   expected result =  2748  | ValueOf = ',ValueOf('ABC', 16));
  WriteLN(DigitsOf(77,7));
  WriteLN(DigitsOf(2748,16));
  WriteLN(DigitsOf(11477,33));
  WriteLN(DigitsOf(1234,36));
  WriteLN(DigitsOf(999,1));
  WriteLN('value1 = ', ValueOf('11000001', 2));
  WriteLN('value2 = ', ValueOf('Z', 36));
  WriteLN('----------------');
  WriteLN('Sum    = ',  Sum('11000001', 2 , 'Z', 36));
  WriteLN('Diff   = ', Diff('11000001', 2 , 'Z', 36));
  WriteLN('Prod   = ', Prod('11000001', 2 , 'Z', 36));
  WriteLN('Quot   = ', Quot('11000001', 2 , 'Z', 36));
  WriteLN('#################');
  WriteLN('value1 = ', ValueOf('110001', 2));
  WriteLN('value2 = ', ValueOf('111', 2));
  WriteLN('----------------');
  WriteLN('Sum    = ',  Sum('110001', 2 , '111', 2));
  WriteLN('Diff   = ', Diff('110001', 2 , '111', 2));
  WriteLN('Prod   = ', Prod('110001', 2 , '111', 2));
  WriteLN('Quot   = ', Quot('110001', 2 , '111', 2));


END. (* ValueConverterPrg *)