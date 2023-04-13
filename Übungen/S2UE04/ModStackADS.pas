(* ModStackADS:                                                 Author, 2023-04-12 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
UNIT ModStackADS;

INTERFACE
  
  PROCEDURE Push(val: INTEGER);
  FUNCTION Pop: INTEGER;
  FUNCTION IsEmpty: BOOLEAN;
IMPLEMENTATION
  
  TYPE 
    IntArray = ARRAY [1..1] OF INTEGER;
  
  VAR
    arrPtr :^IntArray;
    numElem : INTEGER;
    capacity : INTEGER;

  PROCEDURE IncreaseCapacity; FORWARD;
  PROCEDURE DecreaseCapacity; FORWARD;

  PROCEDURE Push(val: INTEGER);
  BEGIN (* Push *)
    IF numElem >= capacity THEN BEGIN
      IncreaseCapacity;
    END;
    Inc(numElem);
    {$R-}
    arrPtr^[numElem] := val;
    {$R+}
  END; (* Push *)
  
  FUNCTION Pop: INTEGER;
  BEGIN (* Pop *)
    IF numElem <= 0 THEN BEGIN
      WriteLn('Stack is empty');
      HALT;
    END;

    IF (numElem * 4 <= capacity) AND (capacity > 10)THEN BEGIN
      DecreaseCapacity;
    END; (* IF *)
    {$R-}
    Pop := arrPtr^[numElem];
    {$R+}
    Dec(numElem);
  END; (* Pop *)

  FUNCTION IsEmpty: BOOLEAN;
  BEGIN (* IsEmpty *)
    IsEmpty := numElem = 0;
  END; (* IsEmpty *)

  PROCEDURE ChangeCapacity(newCapacity: INTEGER);
    VAR
      newPtr: ^IntArray;
      i: INTEGER;
  BEGIN (* ChangeCapacity *)
    GetMem(newPtr, newCapacity * SIZEOF(INTEGER));
    FOR i := 1 TO numElem DO BEGIN
      {$R-}
      newPtr^[i] := arrPtr^[i];
      {$R+}
    END; (* FOR *)
    FreeMem(arrPtr, capacity * SIZEOF(INTEGER));
    arrPtr := newPtr;
    capacity := newCapacity;
  END; (* ChangeCapacity *)

  PROCEDURE DecreaseCapacity;
  BEGIN (* DecreaseCapacity *)
    WriteLn('decrease');
    ChangeCapacity(capacity DIV 2);
  END; (* DecreaseCapacity *)

  PROCEDURE IncreaseCapacity;
  BEGIN (* IncreaseCapacity *)
    WriteLn('increase');
    ChangeCapacity(2* capacity);
  END; (* IncreaseCapacity *)

  

BEGIN (* ModStackADS *)
  GetMem(arrPtr, 10 * SIZEOF(INTEGER));
  capacity := 10;
  numElem := 0;
END. (* ModStackADS *)
