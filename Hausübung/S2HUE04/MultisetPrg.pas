(* MultisetPrg:                                              MFL, 2023-04-13 *)
(* ------                                                                    *)
(* Multiset test programm                                                    *)
(* ========================================================================= *)
PROGRAM MultisetPrg;
USES
  ModMultiset;

VAR
  ms1: StrMSet;
BEGIN (* MultisetPrg *)
  InitStrMSet(ms1);
  Insert(ms1, 'b');
  Insert(ms1, 'c');
  Insert(ms1, 'd');
  DisposeStrMSet(ms1);
  //Insert(ms1, 'a');
  //Insert(ms1, 'v');
  //Remove(ms1, 'b');
  //Remove(ms1, 'a');
  //WriteLn('Cardinality:',Cardinality(ms1));
  //WriteLn('CountUnique: ',CountUnique(ms1));
  //WriteLn('Count v: ',Count(ms1, 'v'));
  //WriteLn('Count a: ',Count(ms1, 'a'));
  //WriteLn('Count b: ',Count(ms1, 'b'));
  //Insert(ms1, 'c');
  //Insert(ms1, 'c');
  //Insert(ms1, 'c');
  //Insert(ms1, 'c');
  //WriteLn('Contains c: ',Contains(ms1, 'c'));
  //WriteLn('Contains e: ',Contains(ms1, 'e'));
  //WriteLn('IsEmpty: ',IsEmpty(ms1));
  //WriteLn('IsEmpty: ',IsEmpty(ms1));
  //Insert(ms1, 'a');
  //Insert(ms1, 'a');
  //Insert(ms1, 'c');
  //Insert(ms1, 'g');
  //Insert(ms1, 'a');
  //Insert(ms1, 'a');
  //Insert(ms1, 'a');
  //Insert(ms1, 'f');
  //Insert(ms1, 'g');
  //WriteLn('Count   ',Count(ms1, 'g'));
  //WriteLn('Count   ',Count(ms1, 'a'));
  //WriteLn('Count   ',Count(ms1, 'e'));
  //WriteLn('Count   ',Count(ms1, 'z'));
  //WriteLn('cardinal   ',Cardinality(ms1));
  //WriteLn('unique   ',CountUnique(ms1));
  //Remove(ms1, 'g');
  //WriteLn(Contains(ms1, 'g'));
  //Remove(ms1, 'g');
  //Remove(ms1, 'g');
  //WriteLn(IsEmpty(ms1));
  //WriteLn(IsEmpty(ms1));
  //Remove(ms1, 'g');

END. (* MultisetPrg *)
