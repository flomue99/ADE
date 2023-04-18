(* VectorPrg:                                                MFL, 2023-04-13 *)
(* ------                                                                    *)
(* Uses ADT Vector                                                           *)
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
  (* test 1 _________________________________ *)
  //Add(v1, 10);
  //Add(v1, 3);
  //Add(v1, 5);
  //WriteLn('Pos 1 ', ElementAt(v1, 1));
  //WriteLn('Pos 2 ',ElementAt(v1, 2));
  //WriteLn('Pos 3 ',ElementAt(v1, 3));
  //SetElementAt(v1, 1, 3);
  //SetElementAt(v1, 2, 2);
  //SetElementAt(v1, 3, 1);
  //WriteLn('All positions changed with SetElementAt.');
  //WriteLn('Pos 1 ',ElementAt(v1, 1));
  //WriteLn('Pos 2 ',ElementAt(v1, 2));
  //WriteLn('Pos 3 ',ElementAt(v1, 3));
  (* test 1 _________________________________ *)

  (* test 2 _________________________________ *)
  //Add(v1, 10);
  //Add(v1, 3);
  //Add(v1, 5);
  //WriteLn('Size v1: ',Size(v1));
  //SetElementAt(v1, -1, 3);
  //SetElementAt(v1, 4, 2);
  //SetElementAt(v1, 3, 1);
  //WriteLn('ElementAt -1:', ElementAt(v1, -1));
  //WriteLn('ElementAt 2: ',ElementAt(v1, 2));
  //WriteLn('ElementAt 4:',ElementAt(v1, 4));
  (* test 2 _________________________________ *)

  (* test 3 _________________________________ *)
  //FOR i := 1 TO 20 DO BEGIN
  //  Add(v1, i);
  //END; (* FOR *)
  //FOR i := 1 TO 30 DO BEGIN
  //  Add(v2, i);
  //END; (* FOR *)
  //WriteLn('Capacity v1: ',Capacity(v1));
  //WriteLn('Size v2: ',Size(v1));
  //WriteLn('Capacity v1: ',Capacity(v2));
  //WriteLn('Size v2: ',Size(v2));
  //FOR i := 1 TO Size(v1) DO BEGIN
  //  Write(ElementAt(v1, i), ' ');
  //END; (* FOR *)
  //WriteLn();
  //FOR i := 1 TO Size(v2) DO BEGIN
  //  Write(ElementAt(v2, i), ' ');
  //END; (* FOR *)
  (* test 3 _________________________________ *)

  (* test 4 _________________________________ *)
  FOR i := 1 TO 20 DO BEGIN
    Add(v1, i);
  END; (* FOR *)
  FOR i := 1 TO 30 DO BEGIN
    Add(v2, i);
  END; (* FOR *)
  FOR i := 1 TO Size(v1) DO BEGIN
    Write(ElementAt(v1, i), ' ');
  END; (* FOR *)
  WriteLn();
  FOR i := 1 TO Size(v2) DO BEGIN
    Write(ElementAt(v2, i), ' ');
  END; (* FOR *)
  RemoveElementAt(v1, 2);
  RemoveElementAt(v1, 15);
  RemoveElementAt(v2, 1);
  RemoveElementAt(v2, 30);
  WriteLn('Capacity v1: ',Capacity(v1));
  WriteLn('Size v2: ',Size(v1));
  WriteLn('Capacity v1: ',Capacity(v2));
  WriteLn('Size v2: ',Size(v2));
  FOR i := 1 TO Size(v1) DO BEGIN
    Write(ElementAt(v1, i), ' ');
  END; (* FOR *)
  WriteLn();
  FOR i := 1 TO Size(v2) DO BEGIN
    Write(ElementAt(v2, i), ' ');
  END; (* FOR *)
  (* test 4 _________________________________ *)

  DisposeVector(v1);
  DisposeVector(v2);
  WriteLn();
END. (* VectorPrg *)
