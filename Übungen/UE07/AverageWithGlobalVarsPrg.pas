(* AverageWithGlobalVarsPrg:                                MFL2, 2022-11-23 *)
(* ------                                                                    *)
(* Algorithm mith memory using global variables.                             *)
(* ========================================================================= *)
PROGRAM AverageWithGlobalVarsPrg;

  TYPE
    MemoryType = RECORD
      sum: REAL;
      count: INTEGER;
    END; (* MemoryType *)

  VAR
    memory: MemoryType;

  PROCEDURE InitMemory;
  BEGIN (* InitMemory *)
    memory.sum := 0;
    memory.count := 0;
  END; (* InitMemory *)

  FUNCTION Average(value: REAL): REAL;
  BEGIN (* Average *)
    memory.sum := memory.sum + value;
    memory.count := memory.count + 1;
    Average := memory.sum / memory.count;
  END; (* Average *)

BEGIN (* AverageWithGlobalVarsPrg *)
  InitMemory;
  WriteLn(Average(1):0:2);
  WriteLn(Average(2):0:2);
  WriteLn(Average(1):0:2);
END. (* AverageWithGlobalVarsPrg *)
