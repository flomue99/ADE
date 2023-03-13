



FOR i := 1 TO m DO BEGIN
  FOR j := 1 TO n DO BEGIN
    (* process matrix[i, j] *)
  END; (* FOR *)
END; (* FOR *)

i := 1;
WHILE (i <= m) DO BEGIN
  j := 1;
  WHILE (k <= n) DO BEGIN
    (* process matrix[i, j] *)
    j := j +1;
  END; (* WHILE *)
  i := i + 1;
END; (* WHILE *)



r := 1;
c := 1; +
i := 1;
prod := m * n;
WHILE (i <= prod) DO BEGIN
  (* process matrix[r, c] *)
  IF (c = n) THEN BEGIN
    r := r + 1;
    c := 1;
  END ELSE BEGIN
    c := c + 1;
  END; (* IF *)
  i := i + 1;
END; (* FOR *)