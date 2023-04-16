(* Title:                                                 Author, 2023-04-13 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM VectorPrg;
USES
  ModVector;

VAR 
  i: INTEGER;
  v1, v2: Vector;
BEGIN (* VectorPrg *)
  InitVector(v1);
  InitVector(v2);
  WriteLn(Capacity(v1));
  WriteLn(Size(v1));
  FOR i := 1 TO 20 DO BEGIN
    Add(v1, i);
  END; (* FOR *)
  FOR i := 10 TO 30 DO BEGIN
    Add(v2, i);
  END; (* FOR *)
  
  FOR i := 1 TO 20 DO BEGIN
    Write(ElementAt(v1, i), ' ');
  END; (* FOR *)
  WriteLn();
  SetElementAt(v1, 3,0);
  SetElementAt(v1, 4,0);
  WriteLn(Size(v1));
  WriteLn(Capacity(v1));
  RemoveElementAt(v1, 3);
  RemoveElementAt(v1, 4);
  RemoveElementAt(v1, 20);
  SetElementAt(v1, 21, 0);
  WriteLn(ElementAt(v1, 21));
  WriteLn(Size(v1));
  FOR i := 1 TO 20 DO BEGIN
    Write(ElementAt(v1, i), ' ');
  END; (* FOR *)
  DisposeVector(v1);
  DisposeVector(v2);
END. (* VectorPrg *)
