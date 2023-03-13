(* MutliplicationTable                          Florian Mühleder, 2022-10-18 *)
(* ------                                                                    *)
(* Calulcates mulitplication table                                           *)
(* ========================================================================= *)
PROGRAM MultiplicationTable;
VAR
  n,m,i,j,res: INTEGER;
BEGIN (* MultiplicationTable *)
  WriteLN('Multiplikationstabelle: (Wertebereich 1<=n,m<=20)');
  i:=1;
  j:=1;
 
  Write('n: ');
  ReadLN(n);

  IF (n=0) THEN BEGIN
    WriteLN('n = 0, Programm Abbruch.');
  END ELSE BEGIN
    Write('m: ');
    ReadLN(m);
    IF (((n>=1) AND (n<=20)) AND ((m>=1) AND (m<=20))) THEN BEGIN

      WHILE (i<=n) DO BEGIN
      j:=1;
        WHILE (j<=m) DO BEGIN
          res:= i*j; 
          Write(,res:5:4);
          j:= j+1;
        END; (* WHILE *)
        WriteLN();
        i:= i+1;
      END; (* WHILE *)

    END ELSE BEGIN
      WriteLN('Eingabe liegt ausserhalb des Wertebereichs.')
    End;(* IF *)
  END; (* IF *)

END. (* MultiplicationTable *)