PROGRAM TestMLIterator;

USES MetaInfo, MLObj, MLStr, MLInt, MLColl, MLVect;

VAR
  c : MLCollection;
  it : MLIterator;
  obj : MLObject;
  v : MLVector;

BEGIN
  c := NewMLVector;
  c^.Add(NewMLInteger(1));
  c^.Add(NewMLInteger(2));
  c^.Add(NewMLInteger(3));
  c^.Add(NewMLString('abc'));
  c^.Add(NewMLString('xyz'));

  it := c^.NewIterator;
  obj := it^.Next;
  WHILE obj <> NIL DO BEGIN
    WriteLn(obj^.AsString);
    obj := it^.Next;
  END;
  Dispose(it, Done);

  c^.WriteAsString;

  c^.DisposeElements;
  Dispose(c, Done);
  WriteMetaInfo;


  (* demo of MLVector *)
  v := NewMLVector;
  v^.Add(NewMLString('Software Engineering'));
  v^.Add(NewMLString('Hagenberg'));
  v^.Sort;
  v^.WriteAsString;

(* error: sorting elements with different types 
  v^.Add(NewMLInteger(17));
  v^.Sort;
  *)

  obj := v^.GetAt(1);
  v^.SetAt(1, NIL);
  Dispose(obj, Done);

  v^.WriteAsString;

  it := v^.NewIterator;
  obj := it^.Next;
  WHILE obj <> NIL DO BEGIN
    WriteLn(obj^.AsString);
    obj := it^.Next;
  END;
  WriteLn('Size: ', v^.Size);
  Dispose(it, Done);

  v^.DisposeElements;
  Dispose(v, Done);

  WriteMetaInfo;
END.