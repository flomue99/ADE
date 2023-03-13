(* AverageWithParametersPrg:                                MFL2, 2022-11-23 *)
(* ------                                                                    *)
(* Algorithm mith memory using parameters.                                   *)
(* ========================================================================= *)
PROGRAM AverageWithParametersPrg;

  TYPE
    MemoryType = RECORD
      sum: REAL;
      count: INTEGER;
    END; (* MemoryType *)


  PROCEDURE InitMemory(VAR memory: MemoryType);
  BEGIN (* InitMemory *)
    memory.sum := 0;
    memory.count := 0;
  END; (* InitMemory *)

  FUNCTION Average(value: REAL;VAR memory:MemoryType): REAL;
  BEGIN (* Average *)
    memory.sum := memory.sum + value;
    memory.count := memory.count + 1;
    Average := memory.sum / memory.count;
  END; (* Average *)

  VAR
    memory: MemoryType;
BEGIN (* AverageWithParametersPrg *)
  InitMemory(memory);
  WriteLn(Average(1,memory):0:2);
  WriteLn(Average(2,memory):0:2);
  WriteLn(Average(1,memory):0:2);
END. (* AverageWithParametersPrg *)
