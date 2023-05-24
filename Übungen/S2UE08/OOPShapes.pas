(* Title:                                                 Author, 2023-05-24 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM OOPShapes;

CONST
  max  = 100;

TYPE
  Shape = ^ShapeObj;
  ShapeObj = OBJECT
              borderColor: STRING;
              borderWidth: INTEGER;
              CONSTRUCTOR Init;
              PROCEDURE Draw; VIRTUAL; ABSTRACT;
              PROCEDURE Move(dx, dy : INTEGER); VIRTUAL; ABSTRACT;
              DESTRUCTOR Done; VIRTUAL;
             END;

  Line = ^LineObj;
  LineObj = OBJECT(ShapeObj)
              x1, y1, x2, y2: INTEGER;
              CONSTRUCTOR Init(x1, y1, x2, y2: INTEGER);
              PROCEDURE Draw; VIRTUAL;
              PROCEDURE Move(dx, dy : INTEGER); VIRTUAL;
            END;
  
  Rectangle = ^RectangleObj;
  RectangleObj = OBJECT(ShapeObj)
                  x, y, width, height: INTEGER;
                  CONSTRUCTOR Init(x, y, width, height: INTEGER);
                  PROCEDURE Draw; VIRTUAL;
                  PROCEDURE Move(dx, dy: INTEGER); VIRTUAL;
                 END;

  Circle = ^CircleObj;
  CircleObj = OBJECT(ShapeObj)
               x, y, r: INTEGER;
               CONSTRUCTOR Init(x, y, r: INTEGER);
               PROCEDURE Draw; VIRTUAL;
               PROCEDURE Move(dx, dy: INTEGER); VIRTUAL;
              END;

  Composite = ^CompositeObj;
  CompositeObj = OBJECT(ShapeObj)
                  numShapes: INTEGER;
                  shapes: ARRAY [1..max] OF Shape;
                  CONSTRUCTOR Init;
                  DESTRUCTOR Done; VIRTUAL;
                  PROCEDURE Move(dx, dy: INTEGER); VIRTUAL;
                  PROCEDURE Draw; VIRTUAL;
                  PROCEDURE Add(s: Shape);
                 END;            
  
(*-----------------SHAPE----------------*)
CONSTRUCTOR ShapeObj.Init;
BEGIN
  borderWidth := 1;
  borderColor := 'black';
END;

DESTRUCTOR ShapeObj.Done;
BEGIN
  (* empty*)
END;

(*-----------------LineObj----------------*)
CONSTRUCTOR LineObj.Init(x1, y1, x2, y2: INTEGER);
BEGIN
  INHERITED Init;
  SELF.x1 := x1; SELF.y1 := y1;
  SELF.x2 := x2; SELF.y2 := y2;
END;

PROCEDURE LineObj.Move(dx, dy: INTEGER);
BEGIN (* LineObj.Move *)
  x1 := x1 + dx; y1 := y1 + dy;
  x2 := x2 + dx; y2 := y2 + dy;
END; (* LineObj.Move *)

PROCEDURE LineObj.Draw;
BEGIN (* LineObj.Draw *)
  WriteLn('<line x1="',x1,'" y1="',y1,'" x2="',x2,'" y2="', y2, '" stroke="', borderColor,'"/>');
END; (* LineObj.Draw *)

(*-----------------RectangleObj----------------*)

CONSTRUCTOR RectangleObj.Init(x, y, width, height: INTEGER);
BEGIN
  INHERITED Init;
  SELF.x := x;
  SELF.y := y;
  SELF.width := width;
  SELF.height := height;
END;

PROCEDURE RectangleObj.Move(dx, dy: INTEGER);
BEGIN (* RectangleObj.Move *)
  x := x + dx;
  y := y + dy;
END; (* RectangleObj.Move *)

PROCEDURE RectangleObj.Draw;
BEGIN (* RectangleObj.Draw *)
  WriteLn('<rect x="', x ,'" y="', y ,'" width="', width ,'" height="', height ,'" stroke="', borderColor ,'" />');
END; (* RectangleObj.Draw *)

(*-----------------CircleObj----------------*)
CONSTRUCTOR CircleObj.Init(x, y, r: INTEGER);
BEGIN
  INHERITED Init;
  SELF.x := x;
  SELF.y := y;
  SELF.r := r;
END;

PROCEDURE CircleObj.Move(dx, dy: INTEGER);
BEGIN (* CircleObj.Move *)
  x := x + dx;
  y := y + dy;
END; (* CircleObj.Move *)

PROCEDURE CircleObj.Draw;
BEGIN (* CircleObj.Draw *)
  WriteLn('<circle cx="', x ,'" cy="', y ,'" r="', r ,'" stroke="',borderColor, '" />');
END; (* CircleObj.Draw *)

(*-----------------Composite----------------*)
CONSTRUCTOR CompositeObj.Init;
  VAR
    i: INTEGER;
BEGIN
  INHERITED Init;
  numShapes := 0;
  FOR i := Low(shapes) TO High(shapes) DO BEGIN
    shapes[i] := NIL;
  END; (* FOR *)
END;

PROCEDURE CompositeObj.Add(s: Shape);
BEGIN (* CompositeObj.Add *)
  Inc(numShapes);
  IF numShapes <= max THEN BEGIN
    shapes[numShapes] := s;
  END ELSE BEGIN
    WriteLn('ERROR');
    HALT;
  END;
END; (* CompositeObj.Add *)

PROCEDURE CompositeObj.Move(dx, dy: INTEGER);
  VAR
    i: INTEGER;
BEGIN (* CompositeObj.Move *)
  FOR i := 1 TO numShapes DO BEGIN
    shapes[i]^.Move(dx, dy);
  END; (* FOR *)
END; (* CompositeObj.Move *)

PROCEDURE CompositeObj.Draw;
  VAR
    i: INTEGER;
BEGIN (* CompositeObj.Draw *)
  WriteLn('<g stroke="', borderColor,'" stroke-width="', borderWidth,'"> ');
  FOR i := 1 TO numShapes DO BEGIN
    shapes[i]^.Draw;
  END; (* FOR *)
  WriteLn('</g>');
END; (* CompositeObj.Draw *)

DESTRUCTOR CompositeObj.Done;
  VAR
    i: INTEGER;
BEGIN
  FOR i := 1 TO numShapes DO BEGIN
    Dispose(shapes[i], Done);
  END; (* FOR *)
  INHERITED Done;
END;



VAR
  l: Line;
  c: Circle;
  r: Rectangle;
  g: Composite;
BEGIN (* OOPShapes *)
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

  New(r, Init(0, 0, 200, 800));
  g^.Add(r);
  
  WriteLn('<svg viewBox="0 0 1000 1000" fill="none">');
  g^.Move(50,50);
  g^.Draw;
  WriteLn('</svg>');

  Dispose(g, Done);
END. (* OOPShapes *)