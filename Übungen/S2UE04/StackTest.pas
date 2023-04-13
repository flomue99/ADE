(* Stacktest:                                                 Author, 2023-04-12 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
//Uses abstract data structure stack
// Push Pop IsEmpty from ModStackADS
PROGRAM StackTest;
USES ModStackADS;

VAR
  i: INTEGER;
BEGIN (* StackTest *)
  FOR i := 1 TO 20 DO BEGIN
    Push(i);
  END; (* FOR *)
  WHILE (NOT IsEmpty) DO BEGIN
    WriteLn(Pop);
  END; (* WHILE *)
END. (* StackTest *)
