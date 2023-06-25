PROGRAM MLChart;

USES
	MetaInfo, OSBridge, MLVect, MLColl,
  MLObj, MLWin, MLAppl; 

TYPE
  Dot = ^DotObj;
  DotObj = OBJECT(MLObjectObj)
             x, y: INTEGER;
             CONSTRUCTOR Init(x, y: INTEGER);
           END;

  ChartWindow = ^ChartWindowObj;
  ChartWindowObj = OBJECT(MLWindowObj)
                     maxVal: INTEGER;
                     chartTitle: STRING;
                     numData: INTEGER;
                     showData: BOOLEAN;
                     data: MLVector;
                     CONSTRUCTOR Init(title: STRING);
                     PROCEDURE Redraw; VIRTUAL;
                     PROCEDURE DrawData;
                     PROCEDURE DrawYAxisLabel;
                     PROCEDURE DrawXAxisLabel;
                     PROCEDURE DrawDataValues;
                     PROCEDURE OnMouseMove(mousePos : Point); VIRTUAL;
                     PROCEDURE OnMouseMove
                     PROCEDURE OnCommand(commandNr: INTEGER); VIRTUAL;
                     PROCEDURE Open; VIRTUAL;
                     DESTRUCTOR Done; VIRTUAL;
                   END;

  HLinesChartWindow = ^HLinesChartWindowObj;
  HLinesChartWindowObj = OBJECT(ChartWindowObj)
                           CONSTRUCTOR Init(title: STRING);
                           PROCEDURE DrawHLines;
                           PROCEDURE Redraw; VIRTUAL;
                           PROCEDURE Open; VIRTUAL;
                         END;

  VLinesChartWindow = ^VLinesChartWindowObj;
  VLinesChartWindowObj = OBJECT(ChartWindowObj)
                           CONSTRUCTOR Init(title: STRING);
                           PROCEDURE DrawVLines;
                           PROCEDURE Redraw; VIRTUAL;
                           PROCEDURE Open; VIRTUAL;
                         END;

HVLinesChartWindow = ^HVLinesChartWindowObj;
HVLinesChartWindowObj = OBJECT(ChartWindowObj)
                           CONSTRUCTOR Init(title: STRING);
                           PROCEDURE DrawVLines;
                           PROCEDURE DrawHLines;
                           PROCEDURE Redraw; VIRTUAL;
                           PROCEDURE Open; VIRTUAL;
                         END;
  
  ChartApplication = ^ChartApplicationObj;
  ChartApplicationObj = OBJECT(MLApplicationObj)
                            CONSTRUCTOR Init(name: STRING);
                            PROCEDURE OpenNewWindow; VIRTUAL;
                            PROCEDURE BuildMenus; VIRTUAL;
                          END;
VAR
  chart1, chart2, chart3, chart4: INTEGER;
  showValues, noValues: INTEGER;
  baseChartTitle: STRING;
  baseChartData: ARRAY [1..100] OF STRING;
  baseNumData: INTEGER;

(* -------------------DotObj----------------------- *)
FUNCTION NewDot(x, y : INTEGER) : Dot;
VAR d : Dot;
BEGIN
  New(d, Init(x, y));
	NewDot := d;
END;

CONSTRUCTOR DotObj.Init(x, y : INTEGER);
BEGIN
  INHERITED Init;
	Register('Dot', 'MLObject');
	SELF.x := x;
	SELF.y := y;
END;

(* -------------------ChartObj----------------------- *)
FUNCTION NewChartWindow(title: STRING): ChartWindow;
  VAR
    w: ChartWindow;
BEGIN (* NewChartWindow: *)
  New(w, Init(title));
  NewChartWindow := w;
END; (* NewChartWindow: *)

CONSTRUCTOR ChartWindowObj.Init(title: STRING);
  VAR
    i: INTEGER;
    strVal, dotX: INTEGER;
    d: Dot;
BEGIN
  INHERITED Init(title);
  Register('ChartWindow', 'MLWindow');
  data := NewMLVector;
  numData := baseNumData;
  chartTitle := baseChartTitle;
  maxVal := 0;
  showData := FALSE;
  dotx := 50;
  FOR i := 1 TO baseNumData DO BEGIN
    Val(baseChartData[i], strVal);
    d := NewDot(dotx, -((strVal DIV 2) - 50)); (* 50 offset *)
    data^.Add(d);
    IF(strVal > maxVal) THEN BEGIN
      maxVal := strVal;
    END;
  dotX := dotX + 50;
  END; (* FOR *)
  maxVal := ((maxVal DIV 100) + 1) * 100;
END;

DESTRUCTOR ChartWindowObj.Done;
BEGIN
  data^.DisposeElements;
	Dispose(data, Done);
	INHERITED Done;
END;

PROCEDURE ChartWindowObj.Redraw;
BEGIN
	DrawXAxisLabel;
  DrawYAxisLabel;
  DrawData;
  IF (showData) THEN BEGIN
    DrawDataValues;
  END;
  END; (*BouncerWindowObj.Redraw*)

PROCEDURE ChartWindowObj.OnMouseMove(mousePos : Point);
  VAR
    isOnPoint: BOOLEAN;
    i: INTEGER;
    it: MLIterator;
    d: Dot;
BEGIN
  it := data^.NewIterator;
  d := Dot(it^.Next);
  isOnPoint := FALSE;
  DrawData;
  WriteLn(mousePos.x,'   ', mousePos.y);
  WHILE (d <> NIL) AND (NOT isOnPoint) DO BEGIN
    IF ((d^.y + (mousePos.y + maxVal) <= 10) OR (d^.y + (mousePos.y + maxVal) >= -10) AND (d^.x + mousePos.x <= 10) OR (d^.x + mousePos.x >= -10)) THEN BEGIN
      d^.y := -mousePos.y - maxVal;
      isOnPoint := TRUE;
    END; (* IF *)
    d := Dot(it^.Next);
  END; (* WHILE *)
  Dispose(it, Done);
  
	IF (isOnPoint) THEN BEGIN
    DrawData;
  END; (* IF *)
  INHERITED OnMouseMove(mousePos);
END;

PROCEDURE ChartWindowObj.DrawXAxisLabel;
  VAR
    p1, p2: Point;
    strP: Point;
    i: INTEGER;
    strVal: STRING;
BEGIN (* ChartWindowObj.DrawXAxisLabel *)
  p1.x := 50;
  p1.y := (maxVal DIV 2) + 50;
  p2.x := 50 + numData * 50;
  p2.y := (maxVal DIV 2) + 50;
  strP.x := 50;
  strP.y := 65 + maxVal DIV 2;
  DrawLine(p1, p2, 1);

  FOR i := 1 TO data^.Size DO BEGIN
    Str(i , strVal);
    DrawString(strP, strVal, 10);
    strP.x := strP.x + 50;
  END; (* FOR *)
END; (* ChartWindowObj.DrawXAxisLabel *)

PROCEDURE ChartWindowObj.DrawYAxisLabel;
  VAR
    p1,p2,strP: Point;
    strVal: STRING;
    i: INTEGER;
BEGIN (* ChartWindowObj.DrawYAxisLabel *)
  p1.x := 50;
  p1.y := 50;
  p2.x := 50;
  p2.y := 50 + maxVal DIV 2;
  strP.x := 20;
  strP.y := 50 + maxVal DIV 2;

  DrawLine(p1, p2, 1);
  FOR i := 0 TO maxVal DIV 100 DO BEGIN
    Str(i * 100, strVal);
    DrawString(strP, strVal, 10);
    strP.y := strP.y - ((maxVal DIV 2) DIV (maxVal DIV 100));
  END; (* FOR *)
END; (* ChartWindowObj.DrawYAxisLabel *)

PROCEDURE ChartWindowObj.DrawData;
  VAR
    it: MLIterator;
    d1, d2: Dot;
    p1, p2: Point;
BEGIN (* ChartWindowObj.DrawData *)
  it := data^.NewIterator;
  d1 := Dot(it^.Next);
  d2 := NIL;
  WHILE (d1 <> NIL) DO BEGIN
    p1.x := d1^.x;
    p1.y := d1^.y + (maxVal DIV 2);

    IF (d2 <> NIL)THEN BEGIN (* Draw Connection *)
      p2.x := d2^.x;
      p2.y := d2^.y + (maxVal DIV 2);
      DrawLine(p1, p2, 1);
    END; (* IF *)

    DrawFilledRectangle(p1, 8, 8);
    d2 := d1;
    d1 := Dot(it^.next);
  END; (* WHILE *)
  Dispose(it, Done);
END; (* ChartWindowObj.DrawData *)

PROCEDURE ChartWindowObj.DrawDataValues;
 VAR
    it: MLIterator;
    d: Dot;
    p: Point;
    strVal: STRING;
BEGIN (* ChartWindowObj.DrawDataValues *)
  it := data^.NewIterator;
  d := Dot(it^.Next);
  WHILE (d <> NIL) DO BEGIN
    p.x := d^.x -10;
    p.y := d^.y + (maxVal DIV 2) - 15;
    Str((d^.y - 50) * -2, strVal);
    DrawString(p, strVal, 10);
    d := Dot(it^.next);
  END; (* WHILE *)
  Dispose(it, Done);
END; (* ChartWindowObj.DrawDataValues *)


PROCEDURE ChartWindowObj.Open;
BEGIN
	INHERITED Open;
  DrawXAxisLabel;
  DrawYAxisLabel;
  DrawData;
  IF (showData) THEN BEGIN
    DrawDataValues;
  END;
END; (*ChartWindowObj.Open*)

(*--------------Vertical lines--------------------*)
 
(* Vertical lines CHART *)
FUNCTION NewVLinesChartWindow(title: STRING): VLinesChartWindow;
  VAR
    w: VLinesChartWindow;
BEGIN (* NewVLinesChartWindow: *)
  New(w, Init(title));
  NewVLinesChartWindow := w;
END; (* NewVLinesChartWindow: *)


(* Horizonzal lines CHART *)
FUNCTION NewHLinesChartWindow(title: STRING): HLinesChartWindow;
  VAR
    w: HLinesChartWindow;
BEGIN (* NewHLinesChartWindow: *)
  New(w, Init(title));
  NewHLinesChartWindow := w;
END; (* NewHLinesChartWindow: *)

(* Horizonzal checked CHART *)
FUNCTION NewHVLinesChartWindow(title: STRING): HVLinesChartWindow;
  VAR
    w: HVLinesChartWindow;
BEGIN (* NewHVLinesChartWindow: *)
  New(w, Init(title));
  NewHVLinesChartWindow := w;
END; (* NewHVLinesChartWindow: *)

PROCEDURE ChartWindowObj.OnCommand(commandNr: INTEGER);
	BEGIN
    IF commandNr = chart1 THEN
			NewChartWindow(chartTitle)^.Open
		ELSE IF commandNr = chart2 THEN
			NewVLinesChartWindow(chartTitle)^.Open
		ELSE IF commandNr = chart3 THEN
			NewHLinesChartWindow(chartTitle)^.Open
		ELSE IF commandNr = chart4 THEN
			NewHVLinesChartWindow(chartTitle)^.Open
    ELSE IF commandNr = showValues THEN BEGIN
			showData := TRUE;
      DrawDataValues;
    END ELSE IF commandNr = noValues THEN BEGIN
      showData := FALSE;
      DrawDataValues;
		END ELSE
			INHERITED OnCommand(commandNr);
	END; (*BouncerWindowObj.OnCommand*)
  
(* -----------------------------Hlines--------------------------------------- *)
CONSTRUCTOR HLinesChartWindowObj.Init(title: STRING);
BEGIN
  INHERITED Init(title);
END;

PROCEDURE HLinesChartWindowObj.DrawHLines;
  VAR
    p1, p2: Point;
    i: INTEGER;
BEGIN (* CHLinesChartWindowObj.DrawHLines *)
  p1.x := 50;
  p1.y := 50 + maxVal DIV 2;
  p2.x := 50 + numData * 50;
  p2.y := 50 + maxVal DIV 2;

  FOR i := 1 TO maxVal DIV 100 DO BEGIN
    p1.y := p1.y - 50;
    p2.y := p2.y - 50;
    DrawLine(p1, p2, 1);
  END; (* FOR *) 
END; (* HLinesChartWindowObj.DrawHLines *)

PROCEDURE HLinesChartWindowObj.Open;
BEGIN
	INHERITED Open;
  DrawHLines;
END; (*ChartWindowObj.Open*)

PROCEDURE HLinesChartWindowObj.Redraw;
BEGIN
  INHERITED Redraw;
	DrawHLines;
END; (*BouncerWindowObj.Redraw*)

(* -----------------------------VLines--------------------------------------- *)
CONSTRUCTOR VLinesChartWindowObj.Init(title: STRING);
BEGIN
  INHERITED Init(title);
END;
PROCEDURE VLinesChartWindowObj.DrawVLines;
  VAR
    p1, p2: Point;
    i: INTEGER;
BEGIN (* VLinesChartWindowObj.DrawVLines *)
  p1.x := 50;
  p1.y := 50;
  p2.x := 50;
  p2.y := 50 + maxVal DIV 2;

  FOR i := 1 TO numData DO BEGIN
    p1.x := p1.x + 50;
    p2.x := p2.x + 50;
    DrawLine(p1, p2, 1);
  END; (* FOR *)
END; (* VLinesChartWindowObj.DrawVLines *)


PROCEDURE VLinesChartWindowObj.Open;
BEGIN
	INHERITED Open;
  DrawVLines;
END; (*VLinesChartWindowObj.Open*)

PROCEDURE VLinesChartWindowObj.Redraw;
BEGIN
  INHERITED Redraw;
	DrawVLines;
END; (*VLinesChartWindowObj.Redraw*)

(* -----------------------------HVLines--------------------------------------- *)
CONSTRUCTOR HVLinesChartWindowObj.Init(title: STRING);
BEGIN
  INHERITED Init(title);
END;

PROCEDURE HVLinesChartWindowObj.DrawVLines;
  VAR
    p1, p2: Point;
    i: INTEGER;
BEGIN (* HVLinesChartWindowObj.DrawVLines *)
  p1.x := 50;
  p1.y := 50;
  p2.x := 50;
  p2.y := 50 + maxVal DIV 2;

  FOR i := 1 TO numData DO BEGIN
    p1.x := p1.x + 50;
    p2.x := p2.x + 50;
    DrawLine(p1, p2, 1);
  END; (* FOR *)
END; (* HVLinesChartWindowObj.DrawVLines *)

PROCEDURE HVLinesChartWindowObj.DrawHLines;
  VAR
    p1, p2: Point;
    i: INTEGER;
BEGIN (* HVLinesChartWindowObj.DrawHLines *)
  p1.x := 50;
  p1.y := 50 + maxVal DIV 2;
  p2.x := 50 + numData * 50;
  p2.y := 50 + maxVal DIV 2;

  FOR i := 1 TO maxVal DIV 100 DO BEGIN
    p1.y := p1.y - 50;
    p2.y := p2.y - 50;
    DrawLine(p1, p2, 1);
  END; (* FOR *) 
END; (* HVLinesChartWindowObj.DrawHLines *)

PROCEDURE HVLinesChartWindowObj.Open;
BEGIN
	INHERITED Open;
  DrawVLines;
  DrawHLines;
END; (*HVLinesChartWindowObj.Open*)

PROCEDURE HVLinesChartWindowObj.Redraw;
BEGIN
  INHERITED Redraw;
	DrawVLines;
  DrawHLines;
END; (*HVLinesChartWindowObj.Redraw*)




(* -----------------------------ChartApplicationObj--------------------------------------- *)
FUNCTION NewChartApplication: ChartApplication;
  VAR
    a: ChartApplication;
BEGIN (* NewChartApplication: *)
  New(a, Init('MiniChart'));
  NewChartApplication := a;
END; (* NewChartApplication: *)


CONSTRUCTOR ChartApplicationObj.Init(name: STRING);
BEGIN
  INHERITED Init(name);
  Register('ChartApplication', 'MLApplication');
END;

PROCEDURE ChartApplicationObj.OpenNewWindow;
BEGIN
	NewChartWindow(baseChartTitle)^.Open;
END; (*ChartApplicationObj.OpenNewWindow*)

PROCEDURE ChartApplicationObj.BuildMenus;
BEGIN
(*size menu:*)
	chart1  := NewMenuCommand('Chart', 'Ohne Raster',  '1');
	chart2   := NewMenuCommand('Chart', 'Vertikales Raster',   '2');
	chart3    := NewMenuCommand('Chart', 'Horizontales Raster',    '3');
	chart4    := NewMenuCommand('Chart', 'Karriertes Raster',    '4');
  showValues    := NewMenuCommand('Values', 'Anzeigen',    '5');
	noValues    := NewMenuCommand('Values', 'Verbergen',    '6');
END; (*BouncerApplicationObj.BuildMenus*)

PROCEDURE ReadData;
  VAR
    t: TEXT;
    str: STRING;
BEGIN (* ReadData *)
  (* read data *)
  Assign(t, 'C:\Temp\Data.txt');
  Reset(t); (* open file *)
  
  ReadLn(t, str);
  baseChartTitle := str;
  WHILE (not EOF(t)) DO BEGIN
    Inc(baseNumData);
    ReadLn(t, str);
    baseChartData[baseNumData] := str;
  END; (* WHILE *)
  //Close(t);
END; (* ReadData *)

VAR
	a: MLApplication;
BEGIN (* MLChart *)
  ReadData;
  a := NewChartApplication;
	a^.Run;
	Dispose(a, Done);
	WriteMetaInfo;
END. (* MLChart *)