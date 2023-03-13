PROGRAM RangePrg;
VAR
  num1,num2,num3,value,x1,x2,x3,count: INTEGER;
BEGIN (* RangePrg *)
  num1:= 0;
  num2:= 0;
  num3:= 0;
  count:= 0;

  Write('num1 >');
  ReadLN(value);
  num1:= value;
WHILE (count<3) DO BEGIN

  IF (count=1) THEN BEGIN
    Write('num2 >');
    ReadLN(value);
    num2:= value;
    IF (num1<num2) THEN BEGIN
      x1:= num1;
      x2:= num2;
    END ELSE BEGIN
      x1:= num2;
      x2:= num1;
    END; (* IF *)
  END; (* IF *)

  IF (count=2) THEN BEGIN
    Write('num3 >');
    ReadLN(value);
    num3:= value;
    IF (num3>x2) THEN BEGIN
      x3:= num3;
    END 
    ELSE IF (num3<x1) THEN BEGIN
      x3:= x2;
      x2:= x1;
      x1:= num3;
    END ELSE BEGIN
      x3:= x2;
      x2:= num3;
    END;
  END; (* IF *)
  count:= count+1;
END; (* WHILE *)

WriteLN(x2);
END. (* RangePrg *)