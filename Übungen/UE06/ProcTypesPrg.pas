(* Title:                                                 Author, 2022-11-16 *)
(* ------                                                                    *)
(* Demonstation of Procedure and Function types                              *)
(* ========================================================================= *)
PROGRAM ProcTypesPrg;
  
  TYPE
    BinaryFunc = Function(x, y: INTEGER): INTEGER;

  FUNCTION Add(x, y: INTEGER): INTEGER;
  BEGIN (* Add *)
    Add := x + y;
  END; (* Add *)

  FUNCTION Subtract(x, y: INTEGER): INTEGER;
  BEGIN (* Subtract *)
    Subtract := x - y;
  END; (* Subtract *)

  FUNCTION Multiply(x, y: INTEGER): INTEGER;
  BEGIN (* Multiply *)
    Multiply := x * y;
  END; (* Multiply *)

  FUNCTION Divide(x, y: INTEGER): INTEGER;
  BEGIN (* Divide *)
    Divide := x DIV y;
  END; (* Divide *)

  PROCEDURE Test(func: BinaryFunc; funcName: STRING; x, y: INTEGER; expected: INTEGER);
    VAR
      result: INTEGER;
  BEGIN (* Test *)
    result := func(x, y);
    IF (result <> expected) THEN BEGIN
      Write('Fail   ');
    END ELSE BEGIN
      Write('Pass   ');
    END; (* IF *)
    WriteLn(funcName, '(', x, ', ', y, ') = ', result, '   (expected ', expected, ')');
  END; (* Test *)

BEGIN (* ProcTypesPrg *)
  Test(Add, 'ADD', 3, 4, 7);
  Test(Add, 'ADD', 3, 5, 7);
END. (* ProcTypesPrg *)