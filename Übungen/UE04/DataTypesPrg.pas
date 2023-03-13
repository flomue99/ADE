(* DataTypesPrg:                                             MFL, 2022-10-28 *)
(* ------                                                                    *)
(* Func with scalared and structured data types                               *)
(* ========================================================================= *)
PROGRAM DataTypesPrg;
  CONST
    numberOfExercises = 8;
    maxGroupSize = 22;
    max = 100;
  TYPE 
    Points = 0..24;

    Student = RECORD
      name: STRING;
      studyPrg: STRING;
      year: WORD;
      points: ARRAY [1..numberOfExercises] OF Points;
    END; (* Student *)

    Group = ARRAY [1..maxGroupSize] OF Student;

  PROCEDURE WriteStudent(s: Student);
    VAR
      i: INTEGER;
      sum: INTEGER;
  BEGIN (* WriteStudent *)
    sum := 0;
    FOR i := Low(s.points) TO High(s.points) DO BEGIN
      sum := sum + s.points[i];
    END; (* FOR *)

    WriteLN('Name:        ', s.name);
    WriteLN('Studiengang: ', s.studyPrg);
    WriteLN('Jahrgang:    ', s.year);
    WriteLN('Punkte:      ', (sum / Length(s.points)):0:2);
  END; (* WriteStudent *)
  
  TYPE
    IntArray = ARRAY [1..max] OF INTEGER;
    IntArrayArray = ARRAY [1..2 * max] OF IntArray;
    IntMatrix = ARRAY [1..2 * max, 1..max] OF INTEGER;
  VAR
    g3: Group;
    i: INTEGER;
    s, t: STRING;
    arrayArray : IntArrayArray;
    matrix : IntMatrix;
BEGIN (* DataTypesPrg *)
  g3[1].name := 'Blaise Pascal';
  g3[1].studyPrg := 'SE';
  g3[1].year := 2022;
  g3[1].points[1] := 24;
  
  WriteStudent(g3[1]);

  WriteLN('------------------------');
  WriteLN('Low(INTEGER)      ', Low(INTEGER));
  WriteLN('High(INTEGER)     ', High(INTEGER));
  i := 17;

  WriteLN('Low(i)            ', Low(i));
  WriteLN('High(i)           ', High(i)); 
  WriteLN('Pred(i)           ', Pred(i));  // vorstehender Wert
  WriteLN('Succ(i)           ', Succ(i));  // nachstehender Wert
  WriteLN('Ord(i)            ', Ord(i));
  WriteLN('Ord(''a'')          ', Ord('a'));
  WriteLN('Ord(''A'')          ', Ord('a'));
  WriteLN('Succ(''a'')         ', Succ('a'));
  WriteLN('Pred(''a'')         ', Pred('a'));

  s := 'Hello';
  t := 'World';
  s := s + ' ' + t;
  WriteLN(s);
  WriteLN(Length(s));
  WriteLN(ORD(s[0]));
  WriteLN('----------------------------');
  WriteLN(High(STRING));
  arrayArray[1][2] := 17; // felder von feldern
  matrix[1,2] := 17;  // mehr dimensional
  WriteLN('Length(arrayArray)        ', Length(arrayArray));
  WriteLN('Length(matrix)            ', Length(matrix));
  WriteLN('Length(arrayArray[1])     ', Length(arrayArray[1])); // zweite dimension
  WriteLN('Length(matrix[1])         ', Length(matrix[1]));     // zweite dimension
END. (* DataTypesPrg *)
