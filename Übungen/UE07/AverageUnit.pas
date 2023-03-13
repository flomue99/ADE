(* AverageUnit:                                             MFL2, 2022-11-23 *)
(* ------                                                                    *)
(* Unit to comoute an average as alogrithm with memory                       *)
(* ========================================================================= *)
UNIT AverageUnit;

INTERFACE

  FUNCTION Average(value: REAL): REAL;

IMPLEMENTATION

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

BEGIN (* AverageUnit *)
  InitMemory;
END. (* AverageUnit *)
