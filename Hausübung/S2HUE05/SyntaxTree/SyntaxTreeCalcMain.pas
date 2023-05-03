(* SyntaxTreeCalcMain:                                       MFL, 2023-04-19 *)
(* ------                                                                    *)
(* SyntaxTreeCalcMain                                                        *)
(* ========================================================================= *)
PROGRAM SyntaxTreeCalcMain;
USES SyntaxTreeCalcLex, SyntaxTreeCalcSyn;
VAR
  s: STRING;
BEGIN (* SyntaxTreeCalcMain *) 
  WriteLn('Expr >');
  ReadLn(s);
  InitLex(s);
  Start;
  WriteLn();
  IF success THEN WriteLn('Parsed succesfully')
  ELSE WriteLn('Syntax Error');
END. (* SyntaxTreeCalcMain *)
