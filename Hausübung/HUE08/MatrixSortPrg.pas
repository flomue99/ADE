(* MatrixSortPrg:                                            MFL, 2023-01-19 *)
(* ------                                                                    *)
(* sort matrix by lines                                                      *)
(* ========================================================================= *)
PROGRAM MatrixSortPrg;
  CONST
    rows = 5;
    columns = 5;
  
  TYPE
    Row = ARRAY [1..columns] OF INTEGER;
    Matrix = ARRAY [1..rows] OF Row;
  
  CONST
    m1: Matrix = ((2,1,2,1,1),
                 (1,2,3,7,2),
                 (2,1,3,1,1),
                 (1,1,3,4,5),
                 (1,1,1,1,1));

    m2: Matrix = ((0,0,1,1,1),
                  (1,1,1,1,1),
                  (0,0,0,0,1),
                  (0,0,0,1,1),
                  (0,1,1,1,1));

    m3: Matrix = ((22,1,12,13,31),
                  (22,2,3,7,25),
                  (21,12,345,1,1),
                  (20,41,3,4,5),
                  (11,1,14,1,1));

  FUNCTION RowIsSmaller(row1, row2: row): BOOLEAN;
    VAR
      i: INTEGER;
  BEGIN (* RowIsSmaller *)
    i := 1;
    WHILE ((row1[i] = row2[i]) AND (i <= columns)) DO BEGIN
      i := i + 1;
    END; (* WHILE *)
    RowIsSmaller := row1[i] < row2[i];
  END; (* RowIsSmaller *)
  
  PROCEDURE WriteMatrix(m: Matrix);
    VAR
      i, j: INTEGER;
  BEGIN (* WriteMatrix *)
    FOR i := 1 TO rows DO BEGIN
      FOR j := 1 TO columns DO BEGIN
        Write(m[i][j]:3,' ');
      END; (* FOR *)
      WriteLn();
    END; (* FOR *)
  END; (* WriteMatrix *)

  PROCEDURE SortLinesByColumns(VAR m: Matrix);
    VAR
      i, j: INTEGER;
      h: ROW;
  BEGIN (* SortLinesByColumns *)
    FOR i := 1 TO rows-1 DO BEGIN
      h := m[i + 1];
      j := i;
      WHILE ((j >= 1) AND (RowIsSmaller(h, m[j]))) DO BEGIN
        m[j + 1] := m[j];
        j := j -1;
      END; (* WHILE *)
      m[j + 1] := h;
    END; (* FOR *)   
  END; (* SortLinesByColumns *)

  PROCEDURE WriteInputOutPut(VAR m: Matrix);
  BEGIN (* WriteInputOutPut *)
    WriteLn('Input:');
    WriteMatrix(m);
    SortLinesByColumns(m);
    WriteLn('Output:');
    WriteMatrix(m);
  END; (* WriteInputOutPut *)

BEGIN (* MatrixSortPrg *)
  WriteInputOutPut(m1);
  WriteLn('-------------------------');
  WriteInputOutPut(m2);
  WriteLn('-------------------------');
  WriteInputOutPut(m3);
END. (* MatrixSortPrg *)
