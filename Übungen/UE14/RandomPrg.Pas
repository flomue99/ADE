(* Title:                                                 Author, 2023-01-25 *)
(* ------                                                                    *)
(* Fun with random number generators (PRNG)                                  *)
(* ========================================================================= *)
PROGRAM RandomPrg;
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
  
    FUNCTION ApproximatePi(n: LONGINT): REAL;
    VAR
      i, hits :LONGINT;
      x, y: REAL;
  BEGIN (* ApproximatePi *)
    hits := 0;
    FOR i := 1 TO n DO BEGIN
      x := RealRand;
      y := RealRand;
      IF ((x * x + y * y) <= 1) THEN BEGIN
        hits := hits + 1;
      END; (* IF *)
    END; (* FOR *)
    ApproximatePi := (hits / n) * 4;
  END; (* ApproximatePi *)

  VAR 
    i: LONGINT;
    histogram: ARRAY[0..19] OF LONGINT;
BEGIN (* RandomPrg *)
  x := 0;
  FOR i := 1 TO 100 DO BEGIN
    Write(RangeRand(6) + 1, '   ');
  END; (* FOR *)
  WriteLn;

  FOR i := Low(histogram) TO High(Histogram) DO BEGIN
    histogram[i] := 0;
  END; (* FOR *)

  FOR i := 1 TO 100000 DO BEGIN
    Inc(histogram[RangeRand(19000) DIV 1000]);
  END; (* FOR *)

  FOR i := Low(histogram) TO High(Histogram) DO BEGIN
    WriteLn(i,':', histogram[i]);
  END; (* FOR *)
  
END.(* RandomPrg *)