(* Title:                                                 Author, 2023-03-08 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM HashTableTest;
USES
  ModHashProbing;

VAR
  s: STRING;
BEGIN (* HashTableTest *)
  //Insert('Hagenberg');
  //Insert('Software Engineering');
  //Insert('Stefan Wagner');
  //Insert('Gabriel Kronberger');
  //Insert('Dagmar Auer');
  //Insert('Stephan Winkler');
  ReadLn(s);
  WHILE (s <> '') DO BEGIN
    Insert(s);
    ReadLn(s);
  END; (* WHILE *)
  WriteHashtable;
END. (* HashTableTest *)
