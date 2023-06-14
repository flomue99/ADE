PROGRAM TestOOStack;
USES OOStack;

VAR s1, s2 : Stack;
    i : INTEGER;
BEGIN
  s1 := CreateDynArrayStack;
  s2 := CreateDynArrayStack;

  FOR i := 1 TO 100 DO BEGIN
    s1^.Push(i);
    s2^.Push(i);
  END;

  WHILE NOT s1^.IsEmpty DO BEGIN
    WriteLn(s1^.Pop);
  END;
  WHILE NOT s2^.IsEmpty DO BEGIN
    WriteLn(s2^.Pop);
  END;

  Dispose(s1, Done);
  Dispose(s2, Done);
END.