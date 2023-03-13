(* PointersPRG:                                              MFL, 2022-11-30 *)
(* ------                                                                    *)
(* Fun with pointers                                                         *)
(* ========================================================================= *)
PROGRAM PointersPrg;

  TYPE
    IntPtr = ^INTEGER;

  PROCEDURE Swap(VAR a, b: INTEGER); // ohne zeiger
    VAR
      h: INTEGER;
  BEGIN (* Swap *)
    h := a;
    a := b;
    b := h;
  END; (* Swap *)

  PROCEDURE SwapWithoutVar(a, b: IntPtr); //Mit zeigern
    VAR
      h: INTEGER;
  BEGIN (* SwapWithoutVar *)
    h := a^;
    a^ := b^;
    b^ := h;
  END; (* SwapWithoutVar *)
   
  PROCEDURE TestSwap;
    VAR
      a, b: INTEGER;
  BEGIN (* TestSwap *)
    a := 17;
    b := 18;
    WriteLn(a, ' ' ,b);
    //Swap(a,b);
    SwapWithoutVar(@a, @b);
    WriteLn(a, ' ' ,b);
  END; (* TestSwap *)

  VAR
    i: INTEGER;
    p: ^INTEGER;
    q: ^INTEGER;
  
BEGIN (* PointersPrg *)
  i := 17;  
  p := @i;
  WriteLn(p^);
  WriteLn(LONGINT(p));
  WriteLn(LONGINT(@p));
  WriteLn(LONGINT(@q));
  q := p;
  q^ := 18;
  WriteLn(i);
  p := NIL;
  //WriteLn(p^);
  WriteLn(q^);

  New(q);
  q^ := 19;
  WriteLn(q^);
  WriteLn(LONGINT(q));
  Dispose(q);
  q := NIL;
  New(q);
  WriteLn(LONGINT(q));
  WriteLn(q^);
  Dispose(q);
  q := NIL;
  WriteLn('-------------------');
  WriteLn('Swap');
  TestSwap();
END. (* PointersPrg *)