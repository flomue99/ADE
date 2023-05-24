PROGRAM OOShapes;
CONST max = 100;
TYPE
  Shape = ^ShapeObj;
  ShapeObj = OBJECT
               borderColor : STRING;
               borderWidth : INTEGER;
               CONSTRUCTOR Init;
               CONSTRUCTOR InitCopy(original : Shape);
               PROCEDURE Draw; VIRTUAL; ABSTRACT; 
               PROCEDURE Move(dx, dy : INTEGER); VIRTUAL; ABSTRACT; 
               FUNCTION DeepCopy : Shape; VIRTUAL; ABSTRACT;
               DESTRUCTOR Done; VIRTUAL;
             END;

  Line = ^LineObj;
  LineObj = OBJECT(ShapeObj)
              x1, y1, x2, y2 : INTEGER;
              CONSTRUCTOR Init(x1, y1, x2, y2 : INTEGER);
              CONSTRUCTOR InitCopy(original : Line);
              PROCEDURE Draw; VIRTUAL;
              PROCEDURE Move(dx, dy : INTEGER); VIRTUAL;
              FUNCTION DeepCopy : Shape; VIRTUAL;
            END;

  Rectangle = ^RectangleObj;
  RectangleObj = OBJECT(ShapeObj)
                   x, y, width, height : INTEGER;
                   CONSTRUCTOR Init(x, y, w, h : INTEGER);
                   CONSTRUCTOR InitCopy(original : Rectangle);
                   PROCEDURE Draw; VIRTUAL;
                   PROCEDURE Move(dx, dy : INTEGER); VIRTUAL;
                   FUNCTION DeepCopy : Shape; VIRTUAL;
                 END;

  Circle = ^CircleObj;
  CircleObj = OBJECT(ShapeObj)
                x, y, r : INTEGER;
                CONSTRUCTOR Init(x, y, r : INTEGER);
                CONSTRUCTOR InitCopy(original : Circle);
                PROCEDURE Draw; VIRTUAL;
                PROCEDURE Move(dx, dy : INTEGER); VIRTUAL;
                FUNCTION DeepCopy : Shape; VIRTUAL;
              END;

  Composite = ^CompositeObj;
  CompositeObj = OBJECT(ShapeObj)
                   numShapes : INTEGER;
                   shapes : ARRAY [1..max] OF Shape;
                   CONSTRUCTOR Init;
                   CONSTRUCTOR InitCopy(original : Composite);
                   DESTRUCTOR Done; VIRTUAL;
                   PROCEDURE Move(dx, dy : INTEGER); VIRTUAL;
                   PROCEDURE Draw; VIRTUAL;
                   PROCEDURE Add(s : Shape);
                  FUNCTION DeepCopy : Shape; VIRTUAL;
                 END; 

(************ Shape **************)
CONSTRUCTOR ShapeObj.Init;
BEGIN
  borderWidth := 1;
  borderColor := 'black';
END;

CONSTRUCTOR ShapeObj.InitCopy(original : Shape);
BEGIN
  SELF.borderWidth := original^.borderWidth;
  SELF.borderColor := original^.borderColor;
END;

DESTRUCTOR ShapeObj.Done;
BEGIN
  (* empty *)
END;

(*********** LineObj *************)
CONSTRUCTOR LineObj.Init(x1, y1, x2, y2 : INTEGER);
BEGIN
  INHERITED Init;
  SELF.x1 := x1; SELF.y1 := y1;
  SELF.x2 := x2; SELF.y2 := y2;
END;

CONSTRUCTOR LineObj.InitCopy(original : Line);
BEGIN
  INHERITED InitCopy(original);
  SELF.x1 := original^.x1;
  SELF.y1 := original^.y1;
  SELF.x2 := original^.x2;
  SELF.y2 := original^.y2;
END;

PROCEDURE LineObj.Move(dx, dy : INTEGER);
BEGIN
  x1 := x1 + dx; y1 := y1 + dy;
  x2 := x2 + dx; y2 := y2 + dy;
END;

PROCEDURE LineObj.Draw;
BEGIN
  WriteLn('<line x1="',x1,'" y1="',y1,'" x2="',x2,'" y2="', y2,'" stroke="',
          borderColor,'"/>');
END;

FUNCTION LineObj.DeepCopy : Shape;
VAR l : Line;
BEGIN
  New(l, InitCopy(@SELF));
  DeepCopy := l;
END;

(******************* Rectangle ********************)
CONSTRUCTOR RectangleObj.Init(x, y, w, h : INTEGER);
BEGIN
  INHERITED Init;
  SELF.x := x;
  SELF.y := y;
  SELF.width := w;
  SELF.height := h;
END;

CONSTRUCTOR RectangleObj.InitCopy(original : Rectangle);
BEGIN
  INHERITED InitCopy(original);
  x := original^.x;
  y := original^.y;
  width := original^.width;
  height := original^.height;
END;

PROCEDURE RectangleObj.Move(dx, dy : INTEGER);
BEGIN
  x := x + dx;
  y := y + dy;
END;

PROCEDURE RectangleObj.Draw;
BEGIN
  WriteLn('<rect x="',x ,'" y="',y ,'" width="', width
          ,'" height="',height ,'" stroke="',borderColor ,'" />');
END;

FUNCTION RectangleObj.DeepCopy : Shape;
VAR r : Rectangle;
BEGIN
  New(r, InitCopy(@SELF));
  DeepCopy := r;
END;

(*********************** Circle ********************)
CONSTRUCTOR CircleObj.Init(x, y, r : INTEGER);
BEGIN
  INHERITED Init;
  SELF.x := x;
  SELF.y := y;
  SELF.r := r;
END;

CONSTRUCTOR CircleObj.InitCopy(original : Circle);
BEGIN
  INHERITED InitCopy(original);
  x := original^.x;
  y := original^.y;
  r := original^.r;
END;

FUNCTION CircleObj.DeepCopy : Shape;
VAR c : Circle;
BEGIN
  New(c, InitCopy(@SELF));
  DeepCopy := c;
END;

PROCEDURE CircleObj.Move(dx, dy : INTEGER);
BEGIN
  x := x + dx;
  y := y + dy;
END;

PROCEDURE CircleObj.Draw;
BEGIN
  WriteLn('<circle cx="',x ,'" cy="', y, '" r="', r, '" stroke="',borderColor,'"/>');
END;

(******************* Composite *******************)
CONSTRUCTOR CompositeObj.Init;
VAR i : INTEGER;
BEGIN
  INHERITED Init;
  numShapes := 0;
  FOR i := Low(shapes) TO High(shapes) DO 
    shapes[i] := NIL;  
END;

CONSTRUCTOR CompositeObj.InitCopy(original : Composite);
VAR i : INTEGER;
BEGIN
  INHERITED InitCopy(original);
  numShapes := original^.numShapes;
  FOR i := 1 TO numShapes DO
    shapes[i] := original^.shapes[i]^.DeepCopy;
END;

FUNCTION CompositeObj.DeepCopy : Shape;
VAR c : Composite;
BEGIN
  New(c, InitCopy(@SELF));
  DeepCopy := c;
END;

PROCEDURE CompositeObj.Add(s : Shape);
BEGIN
  Inc(numShapes);
  IF numShapes <= max THEN BEGIN
    shapes[numShapes] := s;
  END ELSE BEGIN
    WriteLn('Error');
    HALT;
  END;
END;

PROCEDURE CompositeObj.Move(dx, dy : INTEGER);
VAR i : INTEGER;
BEGIN
  FOR i := 1 TO numShapes DO
    shapes[i]^.Move(dx, dy);
END;

PROCEDURE CompositeObj.Draw;
VAR i : INTEGER;
BEGIN
  WriteLn('<g stroke="', borderColor, '" stroke-width="', borderWidth, '" >');
  FOR i := 1 TO numShapes DO
    shapes[i]^.Draw;
  WriteLn('</g>');
END;

DESTRUCTOR CompositeObj.Done;
VAR i : INTEGER;
BEGIN
  FOR i := 1 to numShapes DO
    Dispose(shapes[i], Done);
  
  INHERITED Done;
END;


VAR l : Line;
    c : Circle;
    r : Rectangle;
    g : Composite;
    gCopy : Shape;
    
BEGIN
  New(g, Init);

  New(c, Init(100, 100, 100));
  g^.Add(c);

  New(l, Init(100, 200, 100, 600));
  g^.Add(l);


  New(l, Init(100, 300, 0, 200));
  g^.Add(l);
  New(l, Init(100, 300, 200, 200));
  g^.Add(l);

  New(l, Init(100, 600, 0, 800));
  g^.Add(l);
  New(l, Init(100, 600, 200, 800));
  g^.Add(l);

  New(r, Init(0,0, 200, 800));
  g^.Add(r);
  
  gCopy := g^.DeepCopy;
  gCopy^.Move(400, 0);

  WriteLn('<svg viewBox="0 0 1000 1000" fill="none" xmlns="http://www.w3.org/2000/svg">');
  g^.Draw;
  gCopy^.Draw;
  WriteLn('</svg>');

  Dispose(g, Done);
  Dispose(gCopy, Done);
END.