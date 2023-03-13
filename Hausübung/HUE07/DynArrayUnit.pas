(* DynArrayUnit:                                             MFL, 2022-12-14 *)
(* ------                                                                    *)
(* DynArrayUnit                                                              *)
(* ========================================================================= *)
UNIT DynArrayUnit;

INTERFACE

  TYPE
    TreeNodePtr = ^TreeNode;
    TreeNode = RECORD
      left, right: TreeNodePtr;
      data: INTEGER;
    END; (* TreeNode *)
    TreePtr = TreeNodePtr;

    DynArrayPtr = ^DynArray;
    DynArray = RECORD
      size: INTEGER;
      data: Array[1..1] OF TreeNodePtr;
    END; (* DynArray *)

  FUNCTION NewDynArray(size: INTEGER): DynArrayPtr;
  PROCEDURE DisposeDynArray(VAR a: DynArrayPtr);
  FUNCTION GetValueAt(a: DynArrayPtr; i: INTEGER): TreeNodePtr ;
  PROCEDURE SetValueAt(a: DynArrayPtr; i: INTEGER; node: TreeNodePtr);

IMPLEMENTATION

  FUNCTION NewDynArray(size: INTEGER): DynArrayPtr;
    VAR
      a: DynArrayPtr;
  BEGIN (* NewDynArray *)
    GetMem(a, size * SizeOf(TreeNodePtr) + SizeOf(TreeNodePtr));
    a^.size := size;
    NewDynArray := a;
  END; (* NewDynArray *) 

  PROCEDURE DisposeDynArray(Var a: DynArrayPtr);
  BEGIN (* DisposeDynArray *)
    FreeMem(a, a^.size * SizeOf(TreeNodePtr) + SizeOf(TreeNodePtr));
    a := NIL;
  END; (* DisposeDynArray *) 

  FUNCTION GetValueAt(a: DynArrayPtr; i: INTEGER): TreeNodePtr;
  BEGIN (* GetValueAt *)
    IF ((i < 1) OR (i > a^.size)) THEN BEGIN
      WriteLn('ERROR: Invalid index!');
      HALT;
    END; (* IF *)
    {$R-}GetValueAt := a^.data[i];{$R+}
  END; (* GetValueAt *)

  PROCEDURE SetValueAt(a: DynArrayPtr; i: INTEGER; node: TreeNodePtr);
  BEGIN (* SetValueAt *)
    IF ((i < 1) OR (i > a^.size)) THEN BEGIN
      WriteLn('ERROR: Invalid index!');
      HALT;
    END; (* IF *)
    {$R-}
    a^.data[i] := node;
    {$R+}
  END; (* SetValueAt *)

BEGIN (* DynArrayUnit *) 
END. (* DynArrayUnit *)
