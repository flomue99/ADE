(* Title:                                                 Author, 2023-04-13 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
UNIT ModMultiset;

INTERFACE
  TYPE
    StrMSet = POINTER;

  PROCEDURE InitStrMSet(VAR ms: StrMSet);
  PROCEDURE DisposeStrMSet(VAR ms: StrMSet);
  PROCEDURE Insert(VAR ms: StrMSet; element: STRING);
  PROCEDURE Remove(VAR ms: StrMSet; element: STRING);
  FUNCTION IsEmpty(ms: StrMSet): BOOLEAN;
  FUNCTION Contains(ms: StrMSet; element: STRING): BOOLEAN;
  FUNCTION Count(ms: StrMSet; element: STRING): INTEGER;
  FUNCTION Cardinality(ms: StrMSet): INTEGER;
  FUNCTION CountUnique(ms: StrMSet): INTEGER;
  
  PROCEDURE WriteTree(ms: StrMSet);

IMPLEMENTATION

    TYPE
      MsNode = ^Node;
      Node = RECORD
        left, right: MsNode;
        elem: STRING;
        elemCount: INTEGER;
      END; (* Node *)
      InternalMultiset = MsNode;

  FUNCTION NewNode(element: STRING): MsNode;
    VAR
      n: MsNode;
  BEGIN (* NewNode *)
    New(n);
    n^.elem := element;
    n^.elemCount := 1;
    n^.left := NIL;
    n^.right := NIL;
    NewNode := n;
  END; (* NewNode *)

  PROCEDURE InitStrMSet(VAR ms: StrMSet);
    VAR
      internalMs: InternalMultiset;
  BEGIN (* InitStrMSet *)
    internalMs := NIL;
    ms := internalMs;
  END; (* InitStrMSet*)

  FUNCTION IsEmpty(ms: StrMSet): BOOLEAN;
    VAR
      internalMs: InternalMultiset;
  BEGIN (* IsEmpty *)
     internalMs := InternalMultiset(ms);
     IsEmpty := internalMs = NIL;
  END; (* IsEmpty *)
  
  FUNCTION Cardinality(ms: StrMSet): INTEGER;
    VAR
      internalMs: InternalMultiset;
  BEGIN (* Cardinality *)
    internalMs := InternalMultiset(ms);
    IF (internalMs = NIL) THEN BEGIN
      Cardinality := 0;
    END; (* IF *)
    IF (internalMs <> NIL) THEN BEGIN
      Cardinality := internalMs^.elemCount + Cardinality(internalMs^.left) + Cardinality(internalMs^.right);
    END; (* IF *)
  END; (* Cardinality*)

  FUNCTION CountUnique(ms: StrMSet): INTEGER;
    VAR
      internalMs: InternalMultiset;
  BEGIN (* CountUnique *)
    internalMs := InternalMultiset(ms);
    IF (internalMs = NIL) THEN BEGIN
      CountUnique := 0;
    END; (* IF *)
    IF (internalMs <> NIL) THEN BEGIN
      CountUnique := 1 + CountUnique(internalMs^.left) + CountUnique(internalMs^.right);
    END; (* IF *)
  END; (* CountUnique*)

  FUNCTION Count(ms: StrMSet; element: STRING): INTEGER;
    VAR
      internalMs: InternalMultiset;
  BEGIN (* Count *)
    internalMs := InternalMultiset(ms);
    IF (internalMs = NIL) THEN BEGIN
      Count := 0;
    END; (* IF *)
    IF (internalMs <> NIL) THEN BEGIN
      IF (internalMs^.elem = element) THEN BEGIN
        Count := internalMs^.elemCount;
      END ELSE BEGIN
        Count := Count(internalMs^.left, element) + Count(internalMs^.right, element);
      END; (* IF *)
    END; (* IF *)
  END; (* Count*)

  PROCEDURE DisposeStrMSet(VAR ms: StrMSet);
    VAR
      internalMs: InternalMultiset;
  BEGIN (* DisposeStrMSet *)
    internalMs := InternalMultiset(ms);
    IF(ms <> NIL) THEN BEGIN
      WriteLn('dis');
      DisposeStrMSet(internalMs^.left);
      DisposeStrMSet(internalMs^.right);
      Dispose(internalMs);
      ms := NIL;
    END;
  END; (* DisposeStrMSet *)

  FUNCTION Contains(ms: StrMSet; element: STRING): BOOLEAN;
    VAR
      internalMs: InternalMultiset;
  BEGIN (* Contains *)
    internalMs := InternalMultiset(ms);
    IF(ms = NIL ) THEN BEGIN
      Contains := FALSE;
    END ELSE IF (element = internalMs^.elem) THEN BEGIN
      IF (internalMs^.elemCount = 0) THEN BEGIN
        Contains := FALSE;
      END ELSE BEGIN
        Contains := TRUE;
      END; (* IF *)
    END ELSE IF(element < internalMs^.elem) THEN BEGIN
        Contains := Contains(internalMs^.left, element);
    END ELSE BEGIN
        Contains := Contains(internalMs^.right, element);
    END;
  END; (* Contains *)
  
  PROCEDURE ChangeElemcount(VAR ms: InternalMultiset; element: STRING; i: INTEGER);
  BEGIN (* ChangeElemcount *)
    IF(ms = NIL) THEN BEGIN
      WriteLn('String not in Multiset!, cant remove it');
    END ELSE IF (element = ms^.elem) THEN BEGIN
      IF (ms^.elemCount = 0) THEN BEGIN
        WriteLn('String not in Multiset!, cant remove it');
      END ELSE BEGIN
        ms^.elemCount := ms^.elemCount + i;
      END; (* IF *)
    END ELSE IF(element < ms^.elem) THEN BEGIN
        ChangeElemcount(ms^.left, element, i);
    END ELSE BEGIN
        ChangeElemcount(ms^.right, element, i);
    END;
  END; (* ChangeElemcount *)
 
  PROCEDURE Insert(VAR ms: StrMSet; element: STRING);
    VAR 
      n: MsNode;
      internalMs: InternalMultiset;
  BEGIN
    internalMs := InternalMultiset(ms);
    n := NIL;
    IF (Not Contains(internalMs, element)) THEN BEGIN
      IF(internalMs = NIL ) THEN BEGIN
        n := NewNode(element); 
        internalMs := n;
      END ELSE IF(element < internalMs^.elem) THEN BEGIN
        Insert(internalMs^.left, element);
      END ELSE BEGIN
        Insert(internalMs^.right, element);
      END;
      ms := internalMs;
    END ELSE BEGIN
      ChangeElemcount(internalMs, element, 1);
      ms := internalMs;
    END; (* IF *)
  END;
  PROCEDURE Remove(VAR ms: StrMSet; element: STRING);
    VAR
      internalMs: InternalMultiset;
  BEGIN (* Remove *)
    internalMs := InternalMultiset(ms);
    ChangeElemcount(internalMs, element, -1);
  END; (* Remove *)
 

  PROCEDURE WriteTreeGraphically(ms: InternalMultiset);
    PROCEDURE WTGRec(ms: InternalMultiset; l: INTEGER);
    BEGIN (* WTGRec *)
      IF(ms <> NIL) THEN BEGIN
        WTGRec(ms^.right,  l + 1); 
        WriteLn('  ':l * 6, '( ', ms^.elem,'|',ms^.elemCount ,' ) <');
        WTGRec(ms^.left, l + 1);
      END;
    END; (* WTGRec *)
  BEGIN (* WriteTreeGraphically *)
    WTGRec(ms, 0);
  END; (* WriteTreeGraphically *)
  
  PROCEDURE WriteTree(ms: StrMSet);
    VAR
      internalMs: InternalMultiset;
  BEGIN (* WriteTree *)
    internalMs := InternalMultiset(ms);
    WriteTreeGraphically(internalMs);
  END; (* WriteTree *)

BEGIN (* ModMultiset *)
  
END. (* ModMultiset *)
