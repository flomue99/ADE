(* Main program for MiniPascal compiler *)
PROGRAM MPC;
USES MP_Lex, MP_SemComp, CodeGen, CodeDef, CodeInt;

VAR inFileName : STRING;
    ca : CodeArray;
BEGIN
  IF ParamCount <> 1 THEN BEGIN
    WriteLn('Usage: MPC.exe <file.mp>');
    HALT;
  END;
  inFileName := ParamStr(1);
  InitLex(inFileName);

  S; (* Start symbol of MP grammar *)
  IF success THEN BEGIN
    WriteLn('Parsed successfully.');
    GetCode(ca);
    StoreCode(inFileName + 'c', ca);
  END ELSE 
    WriteLn('Syntax error in line ', lineNr);
END.