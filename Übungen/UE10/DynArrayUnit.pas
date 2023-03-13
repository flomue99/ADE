(* DynArrayUnit:                                          Author, 2022-12-14 *)
(* ------                                                                    *)
(* Arrays of dynmaic size.                                                   *)
(* ========================================================================= *)
UNIT DynArrayUnit;

INTERFACE
  TYPE
    DynArrayPtr = ^DynArray;
    DynArray = RECORD
      size: INTEGER;
      data: Array[1..1] OF INTEGER;
    END; (* DynArray *)
  FUNCTION NewDynArray(size: INTEGER): DynArrayPtr;
  PROCEDURE DisposeDynArray(VAR a: DynArrayPtr);
  FUNCTION GetValueAt(a: DynArrayPtr; i: INTEGER): INTEGER ;
  PROCEDURE SetValueAt(a: DynArrayPtr; i: INTEGER; v: INTEGER);
  PROCEDURE Resize(VAR a: DynArrayPtr; newSize: INTEGER);

IMPLEMENTATION

  FUNCTION NewDynArray(size: INTEGER): DynArrayPtr;
    VAR
      a: DynArrayPtr;
  BEGIN (* NewDynArray *)
    GetMem(a, size * SizeOf(INTEGER) + SizeOf(INTEGER));
    a^.size := size;
    NewDynArray := a;
  END; (* NewDynArray *) 

  PROCEDURE DisposeDynArray(Var a: DynArrayPtr);
  BEGIN (* DisposeDynArray *)
    FreeMem(a, a^.size * SizeOf(INTEGER) + SizeOf(INTEGER));
    a := NIL;
  END; (* DisposeDynArray *) 

  FUNCTION GetValueAt(a: DynArrayPtr; i: INTEGER): INTEGER;
  BEGIN (* GetValueAt *)
    IF ((i < 1) OR (i > a^.size)) THEN BEGIN
      WriteLn('ERROR: Invalid index!');
      HALT;
    END; (* IF *)
    {$R-}
    GetValueAt := a^.data[i];
    {$R+}
  END; (* GetValueAt *)

  PROCEDURE SetValueAt(a: DynArrayPtr; i: INTEGER; v: INTEGER);
  BEGIN (* SetValueAt *)
    IF ((i < 1) OR (i > a^.size)) THEN BEGIN
      WriteLn('ERROR: Invalid index!');
      HALT;
    END; (* IF *)
    {$R-}
    a^.data[i] := v;
    {$R+}
  END; (* SetValueAt *)

  FUNCTION Min(a,b: INTEGER): INTEGER;
  BEGIN (* Min *)
    IF (a < b) THEN BEGIN
      Min := a;
    END ELSE BEGIN
      Min := b;
    END; (* IF *)
  END; (* Min *)

  PROCEDURE Resize(VAR a: DynArrayPtr; newSize: INTEGER);
    VAR
      newArray: DynArrayPtr;
      i: INTEGER;
  BEGIN (* Resize *)
    newArray := NewDynArray(newSize);
    FOR i := 1 TO Min(a^.size, newSize) DO BEGIN
      SetValueAt(newArray, i, GetValueAt(a,i));
    END; (* FOR *)
    DisPoseDynArray(a);
    a := newArray;
  END; (* Resize *)
BEGIN (* DynArrayUnit *)
  
END. (* DynArrayUnit *)
