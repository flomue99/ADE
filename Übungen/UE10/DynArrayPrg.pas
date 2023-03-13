(* Title:                                                 Author, 2022-12-14 *)
(* ------                                                                    *)
(* Fun with arrays of dynamic size                                           *)
(* ========================================================================= *)
PROGRAM DynArrayPrg;
  USES
    DynArrayUnit;
  
    VAR
      n, i: INTEGER;
      a: DynArrayPtr;

BEGIN (* DynArrayPrg *)
  Write('n > ');
  ReadLn(n);
  a := NewDynArray(n);
  FOR i := 1 TO n DO BEGIN
    SetValueAt(a, i, i);
  END; (* FOR *)

  FOR i := 1 TO n DO BEGIN
    WriteLn(GetValueAt(a,i));
  END; (* FOR *)
  DisPoseDynArray(a);

  Write('n > ');
  ReadLn(n);
  Resize(a,n);
  FOR i := 1 TO a^.size DO BEGIN
    WriteLn(GetValueAt(a,i));
  END; (* FOR *)
  DisPoseDynArray(a);

  //Irgendwo do is a fehler!!

END. (* DynArrayPrg *)
