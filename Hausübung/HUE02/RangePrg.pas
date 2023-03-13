(* RangePrg:                                    Florian Mühleder, 2022-10-18 *)
(* ------                                                                    *)
(* Calculates the range of the hightest and lowest value                     *)
(* ========================================================================= *)
PROGRAM RangePrg;
VAR
  min,max,value,result: INTEGER;
BEGIN (* RangePrg *)
  max:= 0;
  min:= 0;

  Write('value >');
  ReadLN(value);
  max:= value;
  min:= value;
   WHILE (value>0) DO BEGIN

    IF (min>value) THEN BEGIN
      min:= value;
    END; (* IF *)
    IF (max<value) THEN BEGIN
      max:= value;
    END; (* IF *)

    Write('value >');
    ReadLN(value);
  END; (* WHILE *)

  result:= max-min;
  
WriteLN('Spannweite = ',result);

END. (* RangePrg *)