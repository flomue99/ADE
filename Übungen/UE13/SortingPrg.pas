(* Title:                                                 Author, 2023-01-18 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
{$ASSERTIONS ON}
PROGRAM SortingPrg;

  CONST 
    small = 100;
    medium = 1000;
    large = 10000;
  
  TYPE
    ElementType = INTEGER;

    SmallArray = ARRAY [0..small - 1] OF ElementType;
    MediumArray = ARRAY [0..medium - 1] OF ElementType;
    LargeArray = ARRAY [0..large - 1] OF ElementType;

    SortProc = PROCEDURE(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    InitProc = SortProc;

    SortStatistics = RECORD
      comparisons: LONGINT;
      assignements: LONGINT;
    END; (* RecordName *)
  VAR
    stats: SortStatistics;

  PROCEDURE InitSortStatistics;
  BEGIN (* InitSortStatistics *)
    stats.comparisons := 0;
    stats.assignements := 0;
  END; (* InitSortStatistics *)

  PROCEDURE InitRandom(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i: INTEGER;
  BEGIN (* InitRandom *)
    randSeed := 1234;
    FOR i := left TO right DO BEGIN
      arr[i] := Random(High(arr));
    END; (* FOR *)
  END; (* InitRandom *)
  
  PROCEDURE InitAscending(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i: INTEGER;
  BEGIN (* InitAscending *)
    FOR i := left TO right DO BEGIN
      arr[i] := i;
    END; (* FOR *)
  END; (* InitAscending *)

  PROCEDURE InitDescending(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i: INTEGER;
  BEGIN (* InitDescending *)
    FOR i := left TO right DO BEGIN
      arr[i] := right - i;
    END; (* FOR *)
  END; (* InitDescending *)
  
  FUNCTION IsSorted(arr: ARRAY OF ElementType; left, right: INTEGER): BOOLEAN;
    VAR
      i: INTEGER;
  BEGIN (* IsSorted *)
    i := left;
    WHILE ((i < right) AND (arr[i] <= arr[i + 1])) DO BEGIN
      i := i + 1;
    END; (* WHILE *)
    IsSorted := i = right;
  END; (* IsSorted *)

  PROCEDURE Swap(VAR x, y: ElementType);
    VAR
      h: ElementType;
  BEGIN (* Swap *)
    h := x;
    x := y;
    y := h;
    stats.assignements := stats.assignements + 3;
  END; (* Swap *)

  PROCEDURE Assign(VAR target: ElementType; source: ElementType);
  BEGIN (* Assign *)
    target := source;
    stats.assignements := stats.assignements + 1;
  END; (* Assign *)

  FUNCTION LessThan(x, y: ElementType): BOOLEAN;
  BEGIN (* LessThan *)
    LessThan := x < y;
    stats.comparisons := stats.comparisons + 1;
  END; (* LessThan *)

  PROCEDURE SelctionSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i, j, minPos: INTEGER;
      minVal: ElementType;
  BEGIN (* SelctionSort *)
    FOR i := left TO right - 1 DO BEGIN
      minPos := i;
      Assign(minVal, arr[i]);
      FOR j := i + 1 TO right DO BEGIN
        IF (LessThan(arr[j], minVal)) THEN BEGIN
          Assign(minVal, arr[j]);
          minPos := j;
        END; (* IF *)
      END; (* FOR *)
      Swap(arr[i], arr[minPos])
    END; (* FOR *)
  END; (* SelctionSort *)
  
  PROCEDURE InsertionSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i, j: INTEGER;
      h : ElementType; 
  BEGIN (* InsertionSort *)
    FOR i := left TO right-1 DO BEGIN
      Assign(h, arr[i+1]);
      j := i;
      WHILE ((j >= left) AND LessThan(h, arr[j])) DO BEGIN
        Assign(arr[j+1], arr[j]);
        j := j -1;;
      END; (* WHILE *)
      Assign(arr[j+1], h);
    END; (* FOR *)
  END; (* InsertionSort *)

  PROCEDURE ShellSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i, j: INTEGER;
      h : ElementType;
      m: INTEGER; 
  BEGIN (* ShellSort *)
    m := right - left +1;
    m := m DIV 2;
    WHILE (m > 0) DO BEGIN
      FOR i := left TO right-m DO BEGIN
        Assign(h, arr[i + m]);
        j := i;
        WHILE ((j >= left) AND LessThan(h, arr[j])) DO BEGIN
          Assign(arr[j+m], arr[j]);
          j := j -m;;
        END; (* WHILE *)
        Assign(arr[j+m], h);
      END; (* FOR *)
      m := m DIV 2;
    END; (* WHILE *)
  END; (* ShellSort *)

  PROCEDURE QuickSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    
    PROCEDURE Partition(l, r: INTEGER);
      VAR
        m: ElementType;
        i, j: INTEGER;
    BEGIN (* Partition *)
      i := l;
      j := r;
      Assign(m, arr[(l + r) DIV 2]);
      REPEAT
        WHILE (LessThan(arr[i], m)) DO BEGIN
          i := i + 1;
        END; (* WHILE *)
        WHILE (LessThan(m, arr[j])) DO BEGIN
          j := j - 1;
        END; (* WHILE *)

        IF (i <= j) THEN BEGIN
          IF (i <> J) THEN BEGIN
            Swap(arr[i], arr[j]);
          END; (* IF *)
          i := i + 1;
          j := j - 1;
        END; (* IF *)
      UNTIL (i > j); (* REPEAT *)

      IF (l < j) THEN BEGIN
        Partition(l, j);
      END; (* IF *)
      IF (i < r) THEN BEGIN
        Partition(i, r);
      END; (* IF *)
    END; (* Partition *)
  BEGIN (* QuickSort *)
    Partition(left, right);
  END; (* QuickSort *)

  PROCEDURE CombSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i, gap: INTEGER;
      noSwaps: BOOLEAN;
  BEGIN (* CombSort *)
    gap := right - left + 1;
    REPEAT
      gap := (gap * 10) DIV 13; (* approx: gap / 1,3 *)
      IF (gap = 0) THEN BEGIN
        gap := 1;
      END; (* IF *)

      noSwaps := TRUE;
      FOR i := left TO right - gap DO BEGIN
        IF (LessThan(arr[i+gap], arr[i])) THEN BEGIN
          Swap(arr[i], arr[i+gap]);
          noSwaps := FALSE;
        END; (* IF *)
      END; (* FOR *)
    UNTIL noSwaps AND (gap = 1); (* REPEAT *)
  END; (* CombSort *)

  PROCEDURE BubbleSort(VAR arr: ARRAY OF ElementType; left, right: INTEGER);
    VAR
      i: INTEGER;
      noSwaps: BOOLEAN;
  BEGIN (* BubbleSort *)
    REPEAT
      noSwaps := TRUE;
      FOR i := left TO right - 1 DO BEGIN
        IF (LessThan(arr[i+1], arr[i])) THEN BEGIN
          Swap(arr[i], arr[i+1]);
          noSwaps := FALSE;
        END; (* IF *)
      END; (* FOR *)
    UNTIL noSwaps; (* REPEAT *)
  END; (* BubbleSort *)

  PROCEDURE TestSorter(sorter: SortProc; sorterName: STRING; initializer: InitProc; arr: ARRAY OF ElementType; left, right: INTEGER);
  BEGIN (* TestSorter *)
    initializer(arr, left, right);
    InitSortStatistics;
    sorter(arr, left, right);
    Assert(IsSorted(arr, left, right), 'ERROR: array not sorted');
    Write(stats.comparisons:8, ' ', stats.assignements:8, '  | ');
    
  END; (* TestSorter *)

  PROCEDURE TestSorterAllCases(sorter: SortProc; sorterName: STRING);
    VAR
      smallArr: SmallArray;
      mediumArr: MediumArray;
      largeArr: LargeArray;
  BEGIN (* TestSorter *)
    Write(sorterName:20, ' | ');
    TestSorter(sorter, sorterName, InitRandom, smallArr, 0, small-1);
    TestSorter(sorter, sorterName, InitRandom, mediumArr, 0, medium-1);
    TestSorter(sorter, sorterName, InitRandom, largeArr,0, large-1);
    WriteLn();
    Write(sorterName:20, ' | ');
    TestSorter(sorter, sorterName, InitAscending, smallArr, 0, small-1);
    TestSorter(sorter, sorterName, InitAscending, mediumArr, 0, medium-1);
    TestSorter(sorter, sorterName, InitAscending, largeArr,0, large-1);
    WriteLn();
    Write(sorterName:20, ' | ');
    TestSorter(sorter, sorterName, InitDescending, smallArr, 0, small-1);
    TestSorter(sorter, sorterName, InitDescending, mediumArr, 0, medium-1);
    TestSorter(sorter, sorterName, InitDescending, largeArr,0, large-1);
    WriteLn();
  END; (* TestSorter *)

BEGIN (* SortingPrg *)
  TestSorterAllCases(SelctionSort, 'SelectionSort');
  WriteLn();
  TestSorterAllCases(InsertionSort, 'InsertionSort');
  WriteLn();
  TestSorterAllCases(ShellSort, 'ShellSort');
  WriteLn();
  TestSorterAllCases(QuickSort, 'QuickSort');
  WriteLn();
  TestSorterAllCases(BubbleSort, 'BubbleSort');
  WriteLn();
  TestSorterAllCases(CombSort, 'CombSort');
END. (* SortingPrg *)
