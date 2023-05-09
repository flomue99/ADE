program MPVM;
USES CodeDef, CodeInt, CodeDis;

VAR ca: CodeArray;
    inFileName: STRING;
    ok: BOOLEAN;
BEGIN (* MPVM *)
  IF (ParamCount <> 1) THEN BEGIN
    WriteLn('Usage: MPVM.exe <filename>');
    HALT;
  END; (* IF *) 
  inFileName := ParamStr(1);
  LoadCode(inFileName, ca, ok);
  IF (ok) THEN BEGIN
    DisassembleCode(ca);
    InterpretCode(ca);
  END; (* IF *)
END. (* MPVM *)