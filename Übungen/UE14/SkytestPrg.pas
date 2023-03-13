PROGRAM SkyTestPrg;

  USES
    Windows, WinGraph;

 CONST
    m = 32768;
    c = 1;
    a = 3421;

  VAR
    x: INTEGER;
  FUNCTION IntRand: INTEGER;
  BEGIN (* IntRand *)
    x := (x * a + c) MOD m;
    IntRand := x;
  END; (* IntRand *)

  FUNCTION RangeRand(n: INTEGER): INTEGER;
    VAR
      r, range: INTEGER;
  BEGIN (* RangeRand *)
    range := (m DIV n) * n;
    REPEAT   
      r := IntRand;
    UNTIL (r < range); (* REPEAT *)
    RangeRand := r MOD n;
  END; (* RangeRand *)

  PROCEDURE SkyTest(dc: HDC; wnd: HWnd; r: TRect); FAR;
    VAR
      i: LONGINT;
      x, y: INTEGER;
  BEGIN
    FOR i := 1 TO 200000 DO BEGIN
      x := Random(r.Width);
      y := Random(r.Height);
      Rectangle(dc, x-1, y-1, x+1, y+1);
    END; (* FOR *)
  END; (*SkyTest*)

BEGIN (*SkyTestPrg*)
  RandSeed := 1234;
  x := 0;
  redrawProc := SkyTest;
  WGMain;
END. (*SkyTestPrg*)
