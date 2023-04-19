(* Title:                                                 Author, 2023-04-19 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM CalcMain;
USES CalcLex, CalcSyn;

VAR
  s: STRING;
BEGIN (* CalcMain *)
  WriteLn('Expr >');
  ReadLn(s);
  InitLex(s);
  Start;
  WriteLn();
  IF success THEN WriteLn('Parsed succesfully')
  ELSE WriteLn('Syntax Error');
END. (* CalcMain *)
