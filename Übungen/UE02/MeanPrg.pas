(* MeanPrg:                                     Florian Mühleder, 2022-10-10 *)
(* ------                                                                    *)
(* Read values from console and compute the arithmetic mean.                 *)
(* ========================================================================= *)
PROGRAM MeanPrg;
 VAR
    total, numbers,value: INTEGER;
    mean: REAL;
BEGIN (* MeanPrg *)

  WriteLN('MeanPrg: Calculation of Mean');
  WriteLN;
  total:=0;
  numbers:=0;

  Write('value >');
  ReadLN(value);

  WHILE (value>0) DO BEGIN
    total:= total + value;
    numbers:= numbers +1;
    Write('value >');
    ReadLN(value);
  END; (* WHILE *)
  WriteLN;

  IF (numbers >0) THEN BEGIN
    mean:= total / numbers;
    WriteLN('mean = ', mean:5:2);
  END ELSE BEGIN
    WriteLN('no values');  
  END; (* IF *)
END. (* MeanPrg *)
