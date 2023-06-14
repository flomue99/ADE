UNIT OOStack;

INTERFACE
TYPE 
  Stack = ^StackObj;
  StackObj = OBJECT
               PROCEDURE Push(val : INTEGER); VIRTUAL; ABSTRACT;
               FUNCTION Pop : INTEGER; VIRTUAL; ABSTRACT;
               FUNCTION IsEmpty : BOOLEAN; VIRTUAL; ABSTRACT;
               DESTRUCTOR Done; VIRTUAL; ABSTRACT;
             END;

  FUNCTION CreateArrayStack : Stack;
  FUNCTION CreateDynArrayStack : Stack;

IMPLEMENTATION
CONST max = 10;
TYPE
  IntArray = ARRAY [1 .. max] OF INTEGER;
 
  ArrayStack = ^ArrayStackObj;
  ArrayStackObj = OBJECT(StackObj)
                    PRIVATE
                      arrPtr : ^IntArray;
                      numElem : INTEGER;
                    PUBLIC
                      CONSTRUCTOR Init;
                      PROCEDURE Push(val : INTEGER); VIRTUAL;
                      FUNCTION Pop : INTEGER; VIRTUAL;
                      FUNCTION IsEmpty : BOOLEAN; VIRTUAL;
                      DESTRUCTOR Done; VIRTUAL;
                  END;

  DynArrayStack = ^DynArrayStackObj;
  DynArrayStackObj = OBJECT(ArrayStackObj)
                       PRIVATE
                         capacity : INTEGER;
                         PROCEDURE Reallocate(newCapacity : INTEGER);
                       PUBLIC 
                         CONSTRUCTOR Init;
                         PROCEDURE Push(val : INTEGER); VIRTUAL;
                         FUNCTION Pop : INTEGER; VIRTUAL;
                         DESTRUCTOR Done; VIRTUAL;
                     END;

FUNCTION CreateArrayStack : Stack;
VAR s : ArrayStack;
BEGIN
  New(s, Init);
  CreateArrayStack := s;
END;

FUNCTION CreateDynArrayStack : Stack;
VAR s : DynArrayStack;
BEGIN
  New(s, Init);
  CreateDynArrayStack := s;
END;

(********************** ArrayStack ***************************)
CONSTRUCTOR ArrayStackObj.Init;
BEGIN
  numElem := 0;
  GetMem(arrPtr, max * SIZEOF(INTEGER));
END;

PROCEDURE ArrayStackObj.Push(val : INTEGER);
BEGIN
  Inc(numElem);
  IF numElem > max THEN BEGIN
    WriteLn('Stack is full');
    HALT;
  END;

  arrPtr^[numElem] := val;
END;

FUNCTION ArrayStackObj.Pop : INTEGER;
BEGIN
  IF numElem <= 0 THEN BEGIN
    WriteLn('Stack is empty.');
    HALT;
  END;

  Pop := arrPtr^[numElem];
  Dec(numElem);
END;

FUNCTION ArrayStackObj.IsEmpty : BOOLEAN;
BEGIN
  IsEmpty := numElem = 0;
END;

DESTRUCTOR ArrayStackObj.Done;
BEGIN
  FreeMem(arrPtr, max * SIZEOF(INTEGER));
  arrPtr := NIL;
  numElem := 0;
END;

(********************** DynArrayStack ***************************)
CONSTRUCTOR DynArrayStackObj.Init;
BEGIN
  INHERITED Init;
  capacity := max;
END;

PROCEDURE DynArrayStackObj.Push(val : INTEGER);
BEGIN
  IF numElem = capacity THEN 
    Reallocate(2 * capacity);
  
  Inc(numElem);
  {$R-}
  arrPtr^[numElem] := val;
  {$R+}
END;

FUNCTION DynArrayStackObj.Pop : INTEGER;
BEGIN
  IF numElem = 0 THEN BEGIN
    WriteLn('Stack is empty');
    HALT;
  END;
  {$R-}
  Pop := arrPtr^[numElem];
  {$R+}
  Dec(numElem);

  (* half memory when less then 33% filled *)
  IF numElem < (capacity DIV 3) THEN 
    Reallocate(capacity DIV 2);
END;

PROCEDURE DynArrayStackObj.Reallocate(newCapacity : INTEGER);
VAR newArrPtr : ^IntArray;
    i : INTEGER;
BEGIN
  (* for debugging *)
  (* WriteLn('reallocate new capacity: ', newCapacity); *)
  GetMem(newArrPtr, SIZEOF(INTEGER) * newCapacity);
  FOR i := 1 TO numElem DO BEGIN
    {$R-}
    newArrPtr^[i] := arrPtr^[i];
    {$R+}
  END;
  FreeMem(arrPtr, SIZEOF(INTEGER) * capacity);
  arrPtr := newArrPtr;
  capacity := newCapacity;
END;

DESTRUCTOR DynArrayStackObj.Done;
BEGIN
  FreeMem(arrPtr, capacity * SIZEOF(INTEGER));
  arrPtr := NIL;
  numElem := 0;
  capacity := 0;
END;

BEGIN
END.