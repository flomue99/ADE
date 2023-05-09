(* Main program for MiniPascal interpeter *)
PROGRAM MPI;
USES MP_Lex, MP_SEM;

VAR inFileName : STRING;
BEGIN
  IF ParamCount <> 1 THEN BEGIN
    WriteLn('Usage: MPI.exe <file.mp>');
    HALT;
  END;
  inFileName := ParamStr(1);
  InitLex(inFileName);

  S; (* Start symbol of MP grammar *)
  IF success THEN
    WriteLn('Parsed successfully.')
  ELSE 
    WriteLn('Syntax error in line ', lineNr);
END.