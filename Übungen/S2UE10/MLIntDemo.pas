PROGRAM MLIntDemo;
USES MetaInfo, MLObj, MLInt;

VAR i1, i2 : MLInteger;
BEGIN
  i1 := NewMLInteger(17);
  i2 := NewMLInteger(21);

  WriteLn(i1^.AsString);
  WriteLn(i2^.AsString);

  WriteLn(i1^.Class);
  WriteLn(i2^.Class);

  WriteLn(i1^.IsEqualTo(i2));
  WriteLn(i1^.IsLessThan(i2));

  Dispose(i1, Done);
  (* Dispose(i2, Done); *)
  WriteMetaInfo;
END.