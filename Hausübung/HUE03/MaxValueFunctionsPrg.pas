(* MaxValueFunctionsPrg:                                     MFL, 2022-10-21 *)
(* ------                                                                    *)
(* Max value functions                                                       *)
(* ========================================================================= *)
PROGRAM MaxValueFunctionsPrg;

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


BEGIN (* MaxValueFunctionsPrg *)
  WriteLN('Max value = ', Max2(-10, -30));
  WriteLN('Max value = ', Max2(10, 100));
  WriteLN('Max value = ', Max3(20, 50, 40));
  WriteLN('Max value = ', Max3(0, 20, 20));
  WriteLN('Max value = ', Max3(-10, 0, 200));
END. (* MaxValueFunctionsPrg *)
