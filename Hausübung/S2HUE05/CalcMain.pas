(* CalcMain:                                                 MFL, 2023-05-01 *)
(* ------                                                                    *)
(* Input, Output, Syntax Error                                               *)
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
