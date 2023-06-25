(* MLListTest:                                               MFL, 2023-06-20 *)
(* ------                                                                    *)
(*                                                                           *)
(* ========================================================================= *)
PROGRAM MLListTest;

USES MetaInfo, MLObj, MLStr, MLInt, MLColl, MLLi;

VAR
  l : MLList;
  obj, deletedObj, floObj: MLObject;
  it : MLIterator;
BEGIN (* MLListTest *)
  
  l := NewMLList;
  floObj := NewMLString('Muehleder');
  l^.Add(floObj);
  l^.Add(NewMLString('Florian'));
  l^.Add(NewMLString('Suedhang'));
  l^.Prepend(NewMLString('4183 Traberg'));
  l^.Prepend(NewMLInteger(9));
  l^.WriteAsString;

  it := l^.NewIterator;
  obj := it^.Next;
  WHILE obj <> NIL DO BEGIN
    WriteLn(obj^.AsString);
    obj := it^.Next;
  END;
  l^.WriteAsString;
  WriteLn('Size: ', l^.Size);
  Dispose(it, Done);

  WriteLn(l^.Contains(floObj));
  deletedObj := l^.Remove(floObj);
  WriteLn(deletedObj^.AsString);
  WriteLn(l^.Contains(floObj));
  Dispose(deletedObj, Done);
  Dispose(l, Done);
  WriteMetaInfo;
END. (* MLListTest *)