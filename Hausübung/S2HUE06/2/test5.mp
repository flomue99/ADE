PROGRAM Test5;
VAR 
  a: INTEGER;
BEGIN
  Read(a);
  Write((5 + 0) * a);
  Write((0 + 5) * a);
  Write((5 / 0) * a);
  Write((5 * 0) * a);
  Write((0 * 5) * a);
  WRITE((0 - 5) * a);
  Write((5 - 0) * a);
END.