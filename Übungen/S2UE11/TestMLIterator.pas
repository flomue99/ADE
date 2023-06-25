PROGRAM TestMLIterator;

USES MetaInfo, MLObj, MLStr, MLInt, MLColl, MLLi;

VAR
  c : MLCollection;
  it : MLIterator;
  obj : MLObject;
  v : MLList;

BEGIN
  c := NewMLList;
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
  Dispose(c, Done);
  WriteMetaInfo;


  (* demo of MLVector *)
  v := NewMLList;
  v^.Add(NewMLString('Software Engineering'));
  v^.Add(NewMLString('Hagenberg'));
  v^.WriteAsString;

(* error: sorting elements with different types 
  v^.Add(NewMLInteger(17));
  v^.Sort;
  *)

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