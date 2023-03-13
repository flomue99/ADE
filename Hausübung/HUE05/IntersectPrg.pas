(* IntersectPrg:                                             MFL, 2022-11-19 *)
(* ------                                                                    *)
(* Intersect two integer arrays                                              *)
(* ========================================================================= *)
PROGRAM IntersectPrg;

  CONST
    max = 5;

  FUNCTION IsSorted(a: ARRAY OF INTEGER; n: INTEGER): BOOLEAN;
    VAR
      i: INTEGER;
  BEGIN (* IsSorted *)
    i := 0;
      WHILE ((i < n-1) AND (a[i+1] >= a[i])) DO BEGIN
        i := i + 1;
      END; (* WHILE *)
    IsSorted := (i = (n - 1));
  END; (* IsSorted *)

  PROCEDURE Intersect(a1: ARRAY OF INTEGER; n1: INTEGER; a2: ARRAY OF INTEGER; n2: INTEGER; VAR a3: ARRAY OF INTEGER; VAR n3: INTEGER);
    VAR
      i, j, a: INTEGER;
      found: BOOLEAN;
  BEGIN (* Intersect *)
    IF ((NOT IsSorted(a1, n1)) OR (NOT IsSorted(a2, n2)) OR (n1 <= 0) OR (n2 <= 0)) THEN BEGIN
      n3 := -1;
      Exit;
    END; (* IF *)
    j := 0;
    i := 0;
    a := 0; (* used to skip the already checked values in a2*)
    found := FALSE;
    WHILE ((i <= (n1 - 1)) AND (n3 <= (max - 1))) DO BEGIN 
      WHILE ((j <= (n2 - 1)) AND (NOT found)) DO BEGIN
        IF (a1[i] = a2[j]) THEN BEGIN
          a3[n3] := a1[i];
          n3 := n3 + 1;
          found := TRUE;
          a := j + 1;
        END ELSE BEGIN
          j := j + 1;  
        END; (* IF *)
      END; (* WHILE *)
      j := a;
      i := i + 1;
      found := FALSE;
    END; (* WHILE *)
    IF (i < (n1- 1)) THEN BEGIN (*overflow*)
      n3 := -1;
    END; (* IF *)    
  END; (* Intersect *)

  PROCEDURE ResultToString(a: ARRAY OF INTEGER; n: INTEGER);
    VAR
      i: INTEGER;
      result,x: STRING;
  BEGIN (* ResultToString *)
    result := '';
    IF (n = -1) THEN BEGIN
      result := 'n = -1, overflow or arrays are not sorted';
      WriteLn(result);
    END ELSE BEGIN
      Str(a[0], x);
      result := x;
      FOR i := 1 TO (n - 1) DO BEGIN
        Str(a[i], x);
        result := result + ', ' + x; 
      END; (* FOR *)
      WriteLn('a3 = ', result, '  n3 = ',n);
    END; (* IF *)
  END; (* ResultToString *)

  CONST
    a1 : ARRAY OF INTEGER = (1, 2, 2, 5, 6);
    a2 : ARRAY OF INTEGER = (1, 2, 3, 4);
  VAR
    n3: INTEGER;
    a3: ARRAY [1..max] OF INTEGER;
BEGIN (* IntersectPrg *)
  Intersect(a1, 5, a2, 4, a3, n3);
  ResultToString(a3, n3);
END. (* IntersectPrg *)
