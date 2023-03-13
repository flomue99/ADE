(* BarChartPrg:                                              MFL, 2022-10-20 *)
(* ------                                                                    *)
(* Drawing barchart                                                          *)
(* ========================================================================= *)
PROGRAM BarChartPrg;

  FUNCTION Max2(num1, num2:INTEGER): INTEGER;
  BEGIN (* Max2 *)
    IF (num1 >= num2) THEN BEGIN
      Max2 := num1;
    END ELSE BEGIN
      Max2 := num2;
    END; (* IF *)
  END; (* Max2 *)

  FUNCTION Max3(num1, num2, num3:INTEGER): INTEGER;
  BEGIN (* Max3 *)
    Max3 := Max2(Max2(num1, num2), num3);
  END; (* Max3 *)

  PROCEDURE WriteChar(ch: CHAR; i, number: INTEGER);
  BEGIN (* WriteChar *)

     IF (number >= i) THEN BEGIN
        Write(' ', ch);
      END ELSE BEGIN
        Write('  ');
      END; (* IF *)
  END; (* WriteChar *)

  PROCEDURE BarChart(ch: CHAR; n1, n2, n3, n4, n5: INTEGER);
  VAR
    i: INTEGER;
  BEGIN (* BarChart *)
    i := Max3(Max3(n1, n2, n3), n4, n5); (* calculates the highest number *)

    WHILE (i > 0) DO BEGIN (* write each sinlge line*)
        
      Write(i:2,'|');  (* vertical labeling *)
      WriteChar(ch, i, n1);
      WriteChar(ch, i, n2);
      WriteChar(ch, i, n3);
      WriteChar(ch, i, n4);
      WriteChar(ch, i, n5);
      WriteLN(); (* new line *)
      i := i-1;

    END; (* WHILE *)
    
    WriteLN('  +----------'); (* horizontal labeling *)
    WriteLN('    1 2 3 4 5');

  END; (* BarChart *)

  VAR
    ch: CHAR;
    n1, n2, n3, n4 ,n5 : INTEGER;
BEGIN (* BarChartPrg *)
  Write('ch: ');
  ReadLN(ch);
  Write('n1: ');
  ReadLN(n1);
  Write('n2: ');
  ReadLN(n2);
  Write('n3: ');
  ReadLN(n3);
  Write('n4: ');
  ReadLN(n4);
  Write('n5: '); 
  ReadLN(n5);
  WriteLN();

  (* value range check range = [1..10] *)
  IF (((n1>= 1) AND ( n1<= 10)) AND ((n2>= 1) AND ( n2<= 10)) AND ((n3>= 1) AND ( n3<= 10)) AND ((n4>= 1) AND ( n4<= 10)) AND ((n5>= 1) AND ( n5<= 10))) THEN BEGIN
     BarChart(ch ,n1 , n2 ,n3, n4, n5);
  END ELSE BEGIN
    WriteLN('Ungueltige Eingabe! [Mindestens ein n ausserhalb des Wertebereichs]');
  END; (* IF *)
  
END. (* BarChartPrg *)