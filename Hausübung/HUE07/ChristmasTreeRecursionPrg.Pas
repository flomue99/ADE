(* ChristmasTreeRecursionPrg:                                MFL, 2023-01-03 *)
(* ------                                                                    *)
(* Christmastree Recursion                                                   *)
(* ========================================================================= *)
PROGRAM ChristmasTreeRecursionPrg;

  FUNCTION XFire(wordLenght, letterIndex : INTEGER): INTEGER;
  BEGIN (* XFire *)
    IF (((wordLenght MOD 2) = 0) OR (wordLenght <= 0) OR (letterIndex < 1) OR (letterIndex > wordLenght)) THEN BEGIN
      XFire := 0;
    END ELSE BEGIN
      IF ((((wordLenght + 1) DIV 2) = letterIndex) OR (letterIndex = wordLenght) OR (letterIndex = 1)) THEN BEGIN
        XFire := 1;
      END ELSE IF (((wordLenght + 1) DIV 2) < letterIndex) THEN BEGIN
        XFire := XFire(wordLenght -2, letterIndex - 2) + XFire(wordLenght - 2 , letterIndex -1 );
      END ELSE BEGIN
        XFire := XFire(wordLenght -2, letterIndex) + XFire(wordLenght - 2 , letterIndex -1 );
      END; (* IF *)
    END; (* IF *)
  END; (* XFire *)

BEGIN (* ChristmasTreeRecursionPrg *)
  WriteLn('Fehlerhafte Eingaben');
  WriteLn('Moegliche Wege, Wortlaenge -2, Index 1: ',XFire(-2,1));
  WriteLn('Moegliche Wege, Wortlaenge 10, Index 1: ',XFire(10,1));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index -1: ',XFire(9,-1));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 10: ',XFire(9,10));
  WriteLn('.........................');
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 1: ',XFire(9,1));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 2: ',XFire(9,2));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 3: ',XFire(9,3));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 4: ',XFire(9,4));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 5: ',XFire(9,5));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 6: ',XFire(9,6));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 7: ',XFire(9,7));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 8: ',XFire(9,8));
  WriteLn('Moegliche Wege, Wortlaenge 9, Index 9: ',XFire(9,9));
  WriteLn('.........................');
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 1: ',XFire(7,1));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 2: ',XFire(7,2));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 3: ',XFire(7,3));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 4: ',XFire(7,4));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 5: ',XFire(7,5));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 6: ',XFire(7,6));
  WriteLn('Moegliche Wege, Wortlaenge 7, Index 7: ',XFire(7,7));
  WriteLn('.........................');
  WriteLn('Moegliche Wege, Wortlaenge 5, Index 1: ',XFire(5,1));
  WriteLn('Moegliche Wege, Wortlaenge 5, Index 2: ',XFire(5,2));
  WriteLn('Moegliche Wege, Wortlaenge 5, Index 3: ',XFire(5,3));
  WriteLn('Moegliche Wege, Wortlaenge 5, Index 4: ',XFire(5,4));
  WriteLn('Moegliche Wege, Wortlaenge 5, Index 5: ',XFire(5,5));
  WriteLn('.........................');
  WriteLn('Moegliche Wege, Wortlaenge 3, Index 1: ',XFire(3,2));
  WriteLn('Moegliche Wege, Wortlaenge 3, Index 2: ',XFire(3,1));
  WriteLn('Moegliche Wege, Wortlaenge 3, Index 3: ',XFire(3,3));
  WriteLn('.........................');
  WriteLn('Moegliche Wege, Wortlaenge 1, Index 1: ',XFire(1,1));
END. (* ChristmasTreeRecursionPrg *)
