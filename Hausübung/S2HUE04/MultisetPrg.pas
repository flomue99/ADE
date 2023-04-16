(* Title:                                                 Author, 2023-04-13 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM MultisetPrg;
USES
  ModMultiset;

VAR
  ms1: StrMSet;
BEGIN (* MultisetPrg *)
  InitStrMSet(ms1);
  WriteLn(IsEmpty(ms1));
  WriteLn('cardinal   ',Cardinality(ms1));
  WriteLn('unique   ',CountUnique(ms1));
  Insert(ms1, 'e');
  Insert(ms1, 'a');
  Insert(ms1, 'b');
  Insert(ms1, 'c');
  Insert(ms1, 'g');
  Insert(ms1, 'f');
  Insert(ms1, 'g');
  WriteLn('Count   ',Count(ms1, 'g'));
  WriteLn('Count   ',Count(ms1, 'a'));
  WriteLn('Count   ',Count(ms1, 'e'));
  WriteLn('Count   ',Count(ms1, 'z'));
  WriteLn('cardinal   ',Cardinality(ms1));
  WriteLn('unique   ',CountUnique(ms1));
  WriteTree(ms1);
  WriteLn(Contains(ms1, 'g'));
  Remove(ms1, 'g');
  WriteTree(ms1);
  WriteLn(Contains(ms1, 'g'));
  Remove(ms1, 'g');
  Remove(ms1, 'g');
  WriteTree(ms1);
  WriteLn(IsEmpty(ms1));
  DisposeStrMSet(ms1);
  WriteLn(IsEmpty(ms1));
  Remove(ms1, 'g');
END. (* MultisetPrg *)
