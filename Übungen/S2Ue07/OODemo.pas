PROGRAM OODemo;

TYPE
  A = ^AObj;
  AObj = OBJECT
          a: INTEGER;
          ptr: ^INTEGER;
          CONSTRUCTOR Init(a: INTEGER);
          PROCEDURE Proc; VIRTUAL;  (* dynamische Bindung T*)
          DESTRUCTOR Done; VIRTUAL; (* necessary to dispose ptr *)
        END;

  B = ^BObj;
  BObj = OBJECT(AObj)
          b: INTEGER;
          ptr2: ^REAL;
          CONSTRUCTOR Init(a, b: INTEGER);
          PROCEDURE Proc; VIRTUAL;
          FUNCTION Func: BOOLEAN;
          DESTRUCTOR Done; VIRTUAL;
        END;

  ABase = ^ABaseObj;
  ABaseObj = OBJECT
              a: INTEGER;
              CONSTRUCTOR Init;
              PROCEDURE Proc; VIRTUAL ABSTRACT;
             END;

  C = ^CObj;
  CObj = OBJECT(ABaseObj)
          PROCEDURE Proc; VIRTUAL;
         END;

CONSTRUCTOR AObj.Init(a: INTEGER);
BEGIN
  SELF.a := a; (* self represents the object to which the message was sent *)
  New(ptr); (* allocate memory for ptr --> must be disposed in destructor *)
END;

PROCEDURE AObj.Proc;
BEGIN (* AObj.Proc *)
  WriteLn('In AObj.Proc');
  WriteLn('a=', a);        (* aObj^.Proc() == Proc(aObj) *)
END; (* AObj.Proc *)

DESTRUCTOR AObj.Done;
BEGIN
  Dispose(ptr);
END;

CONSTRUCTOR BObj.Init(a, b: INTEGER);
BEGIN
  INHERITED Init(a); (* call ctor of base class *)
  SELF.b := b;
  New(ptr2);
END;

FUNCTION BObj.Func: BOOLEAN;
BEGIN (* BObj.Func *)
  WriteLn('In BObj.Func');
  Func := TRUE;
END; (* BObj.Func *)

PROCEDURE BObj.Proc;
BEGIN (* BObj.Proc *)
  WriteLn('In BObj.Proc');
  WriteLn('a=', a, ' b=', b)
END; (* BObj.Proc *)

DESTRUCTOR BObj.Done;
BEGIN
  Dispose(ptr2);
  INHERITED Done;
END;

CONSTRUCTOR ABaseObj.Init;
BEGIN
  a := 0;
END;

PROCEDURE CObj.Proc;
BEGIN
  WriteLn('In CObj.Proc');
END;

VAR x: A;
    y: B;
    p: A; (* helper to show polymorphism / dynamic binding *)

    (* abstract class demo: *)
    o: ABase;
    z: C;
BEGIN (* OODemo *)
  New(x, Init(0)); (* allocate memory and call ctor *)
  New(y, Init(0,0));

  x^.a := 17;
  y^.a := 4;
  y^.b := 21;

  x^.Proc;
  y^.Proc;
  WriteLn(y^.Func);

  p := x;
  p^.Proc;
  p := y; (* p: A; y: B *)
  p^.Proc; (* -> BObj.Proc*)


  Randomize;
  IF (Random > 0.5) THEN BEGIN
    p := x;
  END ELSE BEGIN
    p := y;
  END; (* IF *)

  p^.Proc;
  Dispose(x, Done);
  Dispose(y, Done);
  
END. (* OODemo *)