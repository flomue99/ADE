(* CodeGen:                                         HDO, 2004-02-06
   -------
   Byte code generator for the MiniPascal compiler.
===================================================================*)
UNIT CodeGen;

(*$I Chooser.inc*)

INTERFACE

  USES
    CodeDef, SymTab, TreeUnit;

  PROCEDURE InitCodeGenerator;

  PROCEDURE Emit1(opc: OpCode);
  PROCEDURE Emit2(opc: OpCode; opd: INTEGER);
  PROCEDURE EmitCodeForExprTree(t: TreePtr);

(*$IFDEF Midi*)
  FUNCTION  CurAddr: INTEGER;
  PROCEDURE FixUp(addr: INTEGER; opd: INTEGER);
(*$ENDIF*)

  PROCEDURE GetCode(VAR ca: CodeArray);


IMPLEMENTATION

  VAR
    ca: CodeArray; (*array of opCodes and opderands*)
    n: INTEGER;    (*index of next free byte in c*)


  PROCEDURE InitCodeGenerator;
(*-----------------------------------------------------------------*)
    VAR
      i: INTEGER;
  BEGIN
    n := 1;
    FOR i := 1 TO maxCodeLen DO BEGIN
      ca[i] := 0;
    END; (*FOR*)
  END; (*InitCodeGenerator*)


  PROCEDURE EmitByte(b: BYTE);
  BEGIN
    IF n = maxCodeLen THEN BEGIN
      WriteLn('*** Error: overflow in code array');
      HALT;
    END; (*IF*)
    ca[n] := b;
    n := n + 1;
  END; (*EmitByte*)

  PROCEDURE EmitWord(w: INTEGER);
  BEGIN
    EmitByte(w DIV 256);
    EmitByte(w MOD 256);
  END; (*EmitWord*)


  PROCEDURE Emit1(opc: OpCode);
(*-----------------------------------------------------------------*)
  BEGIN
    EmitByte(Ord(opc));
  END; (*Emit1*)

  PROCEDURE Emit2(opc: OpCode; opd: INTEGER);
(*-----------------------------------------------------------------*)
  BEGIN
    EmitByte(Ord(opc));
    EmitWord(opd);
  END; (*Emit1*)

  PROCEDURE EmitCodeForExprTree(t: TreePtr); (*Emit Tree*)
    VAR
      numberVal: INTEGER;
  BEGIN (* EmitCodeForExprTree *)
    IF (t <> NIL) THEN BEGIN
      EmitCodeForExprTree(t^.left);
      EmitCodeForExprTree(t^.right);
      IF (t^.valType = 1) THEN BEGIN
        IF (t^.val = '+') THEN BEGIN
          Emit1(AddOpc);
        END ELSE IF (t^.val = '-') THEN BEGIN
          Emit1(SubOpc);
        END ELSE IF(t^.val = '/') THEN BEGIN
          Emit1(DivOpc);
        END ELSE IF(t^.val = '*') THEN BEGIN
          Emit1(MulOpc);
        END;
      END ELSE IF (t^.valType = 2) THEN BEGIN
        Val(t^.val, numberVal);
        Emit2(LoadConstOpc, numberVal);
      END ELSE IF (t^.valType = 3) THEN BEGIN
        Emit2(LoadValOpc, AddrOf(t^.val));
      END;
    END; (* IF *)
  END; (* EmitCodeForExprTree *)


(*$IFDEF Midi*)
  FUNCTION CurADdr: INTEGER;
(*-----------------------------------------------------------------*)
  BEGIN
    CurAddr := n;
  END; (*CurAddr*)

  PROCEDURE FixUp(addr: INTEGER; opd: INTEGER);
(*-----------------------------------------------------------------*)
  BEGIN
    ca[addr    ] := opd DIV 256;
    ca[addr + 1] := opd MOD 256;
  END; (*FixUp*)
(*$ENDIF*)


  PROCEDURE GetCode(VAR ca: CodeArray);
(*-----------------------------------------------------------------*)
  BEGIN
    ca := CodeGen.ca;
  END; (*GetCode*)


END. (*CodeGen*)