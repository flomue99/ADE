PROGRAM MPVM;
USES CodeDef, CodeInt, CodeDis;

VAR ca : CodeArray;
    inFileName : STRING;
    ok : BOOLEAN;

BEGIN
  IF ParamCount <> 1 THEN BEGIN
    WriteLn('Usage: MPVM.exe <file.mpc>');
    HALT;
  END;
  inFileName := ParamStr(1);
  
  LoadCode(inFileName, ca, ok);

  IF ok THEN BEGIN
    DisassembleCode(ca); (* for debugging *)  
    InterpretCode(ca);
  END;
END.