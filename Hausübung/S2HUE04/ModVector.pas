(* ModVector:                                                MFL, 2023-04-13 *)
(* ------                                                                    *)
(* Vector as ADT                                                             *)
(* ========================================================================= *)
UNIT ModVector;

INTERFACE
  TYPE
    Vector = POINTER;

  PROCEDURE InitVector(VAR v: Vector);
  PROCEDURE DisposeVector(VAR v: Vector);
  PROCEDURE Add(VAR v: Vector; val: INTEGER);
  PROCEDURE SetElementAt(VAR v: Vector; pos: INTEGER; val: INTEGER);
  FUNCTION ElementAt(v: Vector; pos: INTEGER): INTEGER;
  PROCEDURE RemoveElementAt(VAR v: Vector; pos: INTEGER);
  FUNCTION Size(v: Vector): INTEGER;
  FUNCTION Capacity(v: Vector): INTEGER;

IMPLEMENTATION
   
  TYPE
    IntArray = ARRAY [1..1] OF INTEGER;

    InternalVector = ^VectorRec;
    VectorRec = RECORD
      arrPtr: ^IntArray;
      numElem : INTEGER;
      capacity : INTEGER;
    END; (* VectorRec *)

  PROCEDURE IncreaseCapacity(v: InternalVector); FORWARD;

  PROCEDURE InitVector(VAR v: Vector); (* init vector with size 10 *)
    VAR
      internalV: InternalVector;
  BEGIN (* InitVector *)
    new(internalV);
    GetMem(internalV^.arrPtr, 10 * SIZEOF(INTEGER));
    internalV^.capacity := 10;
    internalV^.numElem := 0;
    v := internalV;
  END; (* InitVector *)

  PROCEDURE DisposeVector(VAR v: Vector); (* Dispose vector *)
    VAR
      internalV: InternalVector;
  BEGIN (* DisposeVector *)
    internalV := InternalVector(v);
    FreeMem(internalV^.arrPtr, internalV^.capacity * SIZEOF(INTEGER));
    Dispose(internalV);
    v := NIL;
  END; (* DisposeVector *)

  PROCEDURE Add(VAR v: Vector; val: INTEGER);
    VAR
      internalV: InternalVector;
  BEGIN (* Add *)
    internalV := InternalVector(v);
    IF (internalV^.numElem >= internalV^.capacity) THEN BEGIN
      IncreaseCapacity(internalV);
    END; (* IF *)
    Inc(internalV^.numElem);
    {$R-}
    internalV^.arrPtr^[internalV^.numElem] := val;
    {$R+}
  END; (* Add *)

  PROCEDURE SetElementAt(VAR v: Vector; pos: INTEGER; val: INTEGER);
    VAR
      internalV: internalVector;
  BEGIN (* SetElementAt *)
    internalV := InternalVector(v);
    IF (internalV = NIL) THEN BEGIN
       WriteLn('ERROR: v = NIL, cant set element at pos ', pos,'.');
    END ELSE IF(pos <= 0) OR (pos > internalV^.numElem) THEN BEGIN
      WriteLn('ERROR: Pos outside numElem range, cant set element at pos ', pos ,'.');
    END ELSE BEGIN
       {$R-}
       internalV^.arrPtr^[pos] := val;
       {$R+}
    END; (* IF *)
  END; (* SetElementAt *)

  FUNCTION ElementAt(v: Vector; pos: INTEGER): INTEGER;
    VAR
      internalV: internalVector;
  BEGIN (* ElementAt *)
    internalV := InternalVector(v);
    IF (internalV = NIL) THEN BEGIN
       WriteLn('ERROR: v = NIL, cant return element at pos ', pos,'.');
    END ELSE IF (pos <= 0) OR (pos > internalV^.numElem) THEN BEGIN
      WriteLn('ERROR: Pos outside numElem range, cant return element at pos ', pos,'.');
      ElementAt := 0;
    END ELSE BEGIN
      {$R-}
      ElementAt :=  internalV^.arrPtr^[pos];
      {$R+}
    END; (* IF *)
  END; (* ElementAt *)

  PROCEDURE RemoveElementAt(VAR v: Vector; pos: INTEGER);
    VAR
      internalV: internalVector;
      i: INTEGER;
  BEGIN (* RemoveElementAt *)
    internalV := InternalVector(v);
    IF (internalV = NIL) THEN BEGIN
       WriteLn('ERROR: v = NIL, cant remove element at pos ', pos,'.');
    END ELSE IF (pos <= 0) OR (pos > internalV^.numElem) THEN BEGIN
      WriteLn('ERROR: Pos outside numElem range, cant remove element at pos ', pos,'.');
    END ELSE BEGIN
      Dec(internalV^.numElem);
      FOR i := pos TO internalV^.numElem DO BEGIN
        {$R-}
        internalV^.arrPtr^[i] := internalV^.arrPtr^[i+1];
        {$R+}
      END; (* FOR *)
    END; (* IF *)
  END; (* RemoveElementAt *)

  FUNCTION Size(v: Vector): INTEGER;
    VAR
      internalV: internalVector;
  BEGIN (* Size *)
    internalV := InternalVector(v);
    IF (internalV = NIL) THEN BEGIN
      Size := 0;
    END ELSE BEGIN
      Size := internalV^.numElem;
    END; (* IF *)
  END; (* Size *)

  FUNCTION Capacity(v: Vector): INTEGER;
    VAR
      internalV: internalVector;
  BEGIN (* Capacity *)
    internalV := InternalVector(v);
    IF (internalV = NIL) THEN BEGIN
      Capacity := 0;
    END ELSE BEGIN
      Capacity := internalV^.capacity;
    END; (* IF *)
  END; (* Capacity *)

  PROCEDURE ChangeCapacity(v: internalVector; newCapacity: INTEGER);
    VAR
      newPtr: ^IntArray;
      i: INTEGER;
  BEGIN (* ChangeCapacity *)
    GetMem(newPtr, newCapacity * SIZEOF(INTEGER));
    FOR i := 1 TO v^.numElem DO BEGIN
      {$R-}
      newPtr^[i] := v^.arrPtr^[i];
      {$R+}
    END; (* FOR *)
    FreeMem(v^.arrPtr, v^.capacity * SIZEOF(INTEGER));
    v^.arrPtr := newPtr;
    v^.capacity := newCapacity;
  END; (* ChangeCapacity *)

  PROCEDURE IncreaseCapacity(v: InternalVector);
  BEGIN (* IncreaseCapacity *)
    ChangeCapacity(v, 2* v^.capacity);
  END; (* IncreaseCapacity *)

BEGIN (* ModVector *)  
END. (* ModVector *)
