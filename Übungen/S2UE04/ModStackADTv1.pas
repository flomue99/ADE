(* ModStackADTv1:                                                 Author, 2023-04-12 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
UNIT ModStackADTv1;

INTERFACE
  TYPE 
    Stack = POINTER;
  (* implementation ist nicht abstracat *)
  (* verwendet dynamisches Array *)

  PROCEDURE Push(s: Stack; val: INTEGER);
  FUNCTION Pop(s: Stack): INTEGER;
  FUNCTION IsEmpty(s: Stack): BOOLEAN;
  PROCEDURE Init(VAR s: Stack);
  PROCEDURE DisposeStack(VAR s: Stack);
IMPLEMENTATION

  TYPE 
    IntArray = ARRAY [1..1] OF INTEGER;

    InternalStack = ^StackRec;
    StackRec = RECORD
      arrPtr: ^IntArray;
      numElem : INTEGER;
      capacity : INTEGER;
    END; (* StackRec *)
  

  PROCEDURE IncreaseCapacity(s: InternalStack); FORWARD;
  PROCEDURE DecreaseCapacity(s: InternalStack); FORWARD;

  PROCEDURE Push(s: Stack; val: INTEGER);
    VAR
      internalS: InternalStack;
  BEGIN (* Push *)
    internalS := InternalStack(s);
    IF internalS^.numElem >= internalS^.capacity THEN BEGIN
      IncreaseCapacity(internalS);
    END;
    Inc(internalS^.numElem);
    {$R-}
    internalS^.arrPtr^[internalS^.numElem] := val;
    {$R+}
  END; (* Push *)
  
  FUNCTION Pop(s: Stack): INTEGER;
    VAR
      internalS: InternalStack;
  BEGIN (* Pop *)
    internalS := InternalStack(s);
    IF internalS^.numElem <= 0 THEN BEGIN
      WriteLn('Stack is empty');
      HALT;
    END;

    IF (internalS^.numElem * 4 <= internalS^.capacity) AND (internalS^.capacity > 10)THEN BEGIN
      DecreaseCapacity(internalS);
    END; (* IF *)
    {$R-}
    Pop := internalS^.arrPtr^[internalS^.numElem];
    {$R+}
    Dec(internalS^.numElem);
  END; (* Pop *)

  FUNCTION IsEmpty(s: Stack): BOOLEAN;
  BEGIN (* IsEmpty *)
    IsEmpty := InternalStack(s)^.numElem = 0;
  END; (* IsEmpty *)

  PROCEDURE Init(VAR s: Stack);
    VAR
      internalS: InternalStack;
  BEGIN (* INIT *)
    New(internalS);
    GetMem(internalS^.arrPtr, 10 * SIZEOF(INTEGER));
    internalS^.capacity := 10;
    internalS^.numElem := 0;
    s := internalS;
  END; (* INIT *)

  PROCEDURE DisposeStack(VAR s: Stack);
    VAR
      internalS: InternalStack;
  BEGIN (* Dispose *)
    internalS := InternalStack(s);
    FreeMem(internalS^.arrPtr, internalS^.capacity * SIZEOF(INTEGER));
    Dispose(internalS);
    s := NIL;
  END; (* Dispose *)

  (* interne Proceduren *)

  PROCEDURE ChangeCapacity(s: InternalStack; newCapacity: INTEGER);
    VAR
      newPtr: ^IntArray;
      i: INTEGER;
  BEGIN (* ChangeCapacity *)
    GetMem(newPtr, newCapacity * SIZEOF(INTEGER));
    FOR i := 1 TO s^.numElem DO BEGIN
      {$R-}
      newPtr^[i] := s^.arrPtr^[i];
      {$R+}
    END; (* FOR *)
    FreeMem(s^.arrPtr, s^.capacity * SIZEOF(INTEGER));
    s^.arrPtr := newPtr;
    s^.capacity := newCapacity;
  END; (* ChangeCapacity *)

  PROCEDURE DecreaseCapacity(s: InternalStack);
  BEGIN (* DecreaseCapacity *)
    WriteLn('decrease');
    ChangeCapacity(s, s^.capacity DIV 2);
  END; (* DecreaseCapacity *)

  PROCEDURE IncreaseCapacity(s: InternalStack);
  BEGIN (* IncreaseCapacity *)
    WriteLn('increase');
    ChangeCapacity(s, 2* s^.capacity);
  END; (* IncreaseCapacity *)

BEGIN (* ModStackADTv1 *)
END. (* ModStackADTv1 *)
