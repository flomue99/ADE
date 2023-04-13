(* StackADTTest:                                                 Author, 2023-04-12 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM StackADTTest;
USES ModStackADTv1;

VAR
  i: INTEGER;
  s1, s2: Stack;
BEGIN (* StackADTTest *)
  Init(s1); Init(s2);
  FOR i := 1 TO 20 DO BEGIN
    Push(s1, i);
  END; (* FOR *)

  WHILE (NOT IsEmpty(s1)) DO BEGIN
    WriteLn(Pop(s1));
  END; (* WHILE *)
  DisposeStack(s1); DisposeStack(s2);

END. (* StackADTTest *)
