(* PlateauPrg:                                              MFL2, 2022-11-03 *)
(* ------                                                                    *)
(* Calculates the longest plateau in an array of sorted integer values       *)
(* ========================================================================= *)
PROGRAM PlateauPrg;

  TYPE 
    IntArray = ARRAY [1..15] OF INTEGER;

  FUNCTION Plateau(a: IntArray; n: INTEGER): INTEGER;
    VAR
      maxPlateu, tempPlateu: INTEGER;
      i : INTEGER;
  BEGIN (* Plateau *)
    maxPlateu := 1;
    tempPlateu := 0;
    FOR i := 1 TO (n - 1) DO BEGIN
      IF (a[i] > a[i+1]) THEN BEGIN
        WriteLN('Error: No sorted Array!');
        Halt;
      END; (* IF *)
      IF (a[i] = a[i+1]) THEN BEGIN
        tempPlateu := tempPlateu +1;    
      END ELSE BEGIN
        IF (tempPlateu > maxPlateu) THEN BEGIN
          maxPlateu := tempPlateu;
          tempPlateu := 1;
        END; (* IF *)
        tempPlateu := 1;
      END; (* IF *)
    END; (* FOR *)
    IF (tempPlateu >= maxPlateu) THEN BEGIN (* plateau on the end of the array *)
      maxPlateu := tempPlateu;
    END; (* IF *)
    Plateau := maxPlateu;
  END; (* Plateau *)

CONST
   a1 : IntArray = (1,2,2,3,4,4,7,7,7,7,0,0,0,0,0);

BEGIN (* PlateauPrg *)
  WriteLN(' Plateau = ', Plateau(a1, 10));

END. (* PlateauPrg *)
