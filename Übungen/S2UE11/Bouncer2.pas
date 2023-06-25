(*Bouncer1:                                            MiniLib V.4, 2004
  --------
  Bouncing ball application.
  Version 2: DX Ball
========================================================================*)
PROGRAM Bouncer2;

	USES
		MetaInfo, OSBridge,
    MLObj, MLWin, MLAppl, MLColl, MLVect;

	TYPE
    Block = ^BlockObj;
		BlockObj = OBJECT(MLObjectObj)
		             x, y : INTEGER;
								 w, h : INTEGER;
								 CONSTRUCTOR Init(x, y : INTEGER);
								 FUNCTION Contains(px, py : INTEGER) : BOOLEAN;
		           END;
	 
		BouncerWindow = ^BouncerWindowObj;
		BouncerWindowObj = OBJECT(MLWindowObj)
		    CONST 
				  paddelWidth = 100;
		      paddelHeight = 20;
		      gap = 20;
				VAR 
				  pos: Point;
          dx, dy, size, speed: INTEGER;
				  paddelPos : Point;
					blocks : MLVector;
					gameOver : BOOLEAN;
				CONSTRUCTOR Init(title: STRING);
		  (*overridden methods*)
        PROCEDURE Open; VIRTUAL;
				PROCEDURE Redraw; VIRTUAL;
				PROCEDURE OnIdle; VIRTUAL;
				PROCEDURE OnCommand(commandNr: INTEGER); VIRTUAL;
				PROCEDURE OnMouseMove(mousePos : Point); VIRTUAL;
				PROCEDURE OnResize; VIRTUAL;
				DESTRUCTOR Done; VIRTUAL;
		  (*new methods*)
				PROCEDURE InvertBall; VIRTUAL;
				PROCEDURE InvertPaddel; VIRTUAL;
				PROCEDURE InvertBlocks; VIRTUAL;
				PROCEDURE ChangeSize(newSize: INTEGER); VIRTUAL;
				PROCEDURE ChangeSpeed(newSpeed: INTEGER); VIRTUAL;
				FUNCTION IsCollision : BOOLEAN; VIRTUAL;
				FUNCTION IsCollisionWithBlock(b : Block) : BOOLEAN; VIRTUAL;
			END; (*OBJECT*)

		BouncerApplication= ^BouncerApplicationObj;
		BouncerApplicationObj = OBJECT(MLApplicationObj)
				CONSTRUCTOR Init(name: STRING);
	    (*overridden methods*)
				PROCEDURE OpenNewWindow; VIRTUAL;
				PROCEDURE BuildMenus; VIRTUAL;
			END; (*OBJECT*)

	VAR
  (*size menu:*)
		smallCommand, mediumCommand, largeCommand: INTEGER;
  (*speed menu:*)
		crawlCommand, walkCommand, runCommand, flyCommand: INTEGER;

(*=== Block ===*)

FUNCTION NewBlock(x, y : INTEGER) : Block;
VAR b : Block;
BEGIN
  New(b, Init(x, y));
	NewBlock := b;
END;

CONSTRUCTOR BlockObj.Init(x, y : INTEGER);
BEGIN
  INHERITED Init;
	Register('Block', 'MLObject');
	SELF.x := x;
	SELF.y := y;
	SELF.w := 100;
	SELF.h := 20;
END;

FUNCTION BlockObj.Contains(px, py : INTEGER) : BOOLEAN;
BEGIN
  Contains := (px >= x) 
	            AND (px <= x + w) 
	            AND (py >= y) 
							AND (py <= y + h);
END;


(*=== BouncerWindow ===*)

	FUNCTION NewBouncerWindow: BouncerWindow;
		VAR
			w: BouncerWindow;
	BEGIN
		New(w, Init('Bouncer Window'));
		NewBouncerWindow := w;
	END; (*NewBouncerWindow*)

	CONSTRUCTOR BouncerWindowObj.Init(title: STRING);
	BEGIN
		INHERITED Init(title);
    Register('BouncerWindow', 'MLWindow');
		pos.x :=  0;
		pos.y :=  0;
		size  := 20;
		speed := 5;
		dx    := speed;
		dy    := speed;
		paddelPos.x := 0;
		paddelPos.y := 0;
		blocks := NewMLVector;
		gameOver := FALSE;
	END; (*BouncerWindowObj.Init*)

  DESTRUCTOR BouncerWindowObj.Done;
	BEGIN
	  blocks^.DisposeElements;
		Dispose(blocks, Done);
		INHERITED Done;
	END;

	PROCEDURE BouncerWindowObj.Open;
	VAR lvl, curX, curY : INTEGER;
	    b : Block;
	BEGIN
		INHERITED Open;

    (* Create blocks *)
		curY := 10;
		FOR lvl := 1 TO 4 DO BEGIN
		  curX := 10;
		  WHILE curX < Width DO BEGIN
			  b := NewBlock(curX, curY);
		    blocks^.Add(b);
				curX := curX + b^.w + 10; (* gap: 10 px *) 
			END; 
			curY := curY + b^.h + 10; (* gap: 10 px *)
		END;

		paddelPos.x := Width DIV 2; (* width is not defined in ctor *)
		paddelPos.y := Height - 40; (* paddelHeight = 20, gap = 20 *)
    InvertBall;
		InvertPaddel;
		InvertBlocks;
	END; (*BouncerWindowObj.Open*)

	PROCEDURE BouncerWindowObj.Redraw;
	VAR center : Point;
	BEGIN
		InvertBall;
		InvertPaddel;
		InvertBlocks;

		IF gameOver THEN BEGIN
		  center.x := Width DIV 2;
			center.y := Height DIV 2;
		  DrawString(center, 'Game over.', 20);
		END;
	END; (*BouncerWindowObj.Redraw*)

	PROCEDURE BouncerWindowObj.OnIdle;
	VAR it : MLIterator;
	    b, collisionBlock : Block;
      center : Point;

		PROCEDURE Move(VAR val, delta: INTEGER; max: INTEGER);
		BEGIN
			val := val + delta;
			IF val < 0 THEN BEGIN
				val   := 0;
				delta := + speed;
			END (*THEN*)
			ELSE IF val + size > max THEN BEGIN
				val   := max - size;
				delta := -speed;
			END; (*ELSE*)
		END; (*Move*)

	BEGIN (*BouncerWindowObj.OnIdle*)
	  InvertBall;
		(* game over? *)
		IF pos.y + size + dy >= Height THEN BEGIN
		  dx := 0;
			dy := 0;
			gameOver := TRUE;
			Redraw;
		END ELSE BEGIN
		  Move(pos.x, dx, Width);
		  Move(pos.y, dy, Height);
  
      IF IsCollision THEN BEGIN
		    dy := -dy; (* reflect *)
		  END;
  
      collisionBlock := NIL;
		  it := blocks^.NewIterator;
		  b := Block(it^.Next);
		  WHILE b <> NIL DO BEGIN
		    IF IsCollisionWithBlock(b) THEN BEGIN
		  	  collisionBlock := b
		  	END;
		    b := Block(it^.Next);
		  END;
		  Dispose(it, Done);
      
		  IF collisionBlock <> NIL THEN BEGIN		
		    InvertBlocks; (* clear all blocks *)  
		    blocks^.Remove(collisionBlock);
		    InvertBlocks; (* draw remaining blocks *)  
  
        
		  	(* reflect ball *)
		  	center.x := pos.x + size DIV 2;
		  	center.y := pos.y + size DIV 2;
		  	IF (center.x >= collisionBlock^.x) 
		  	   AND (center.x <= collisionBlock^.x + collisionBlock^.w) THEN BEGIN
		  		 dy := -dy;
		  	END;
		  	IF (center.y >= collisionBlock^.y) 
		  	   AND (center.y <= collisionBlock^.y + collisionBlock^.h) THEN BEGIN
		  		 dx := -dx;
		  	END;
		  END;
  
		  InvertBall;		
		END;
	END; (*BouncerWindowObj.OnIdle*)

  PROCEDURE BouncerWindowObj.OnMouseMove(mousePos : Point);
	BEGIN
	  IF NOT gameOver THEN BEGIN
	    InvertPaddel;
	    paddelPos.x := mousePos.x;
		  InvertPaddel;
		END;
		INHERITED OnMouseMove(mousePos);
	END;

  PROCEDURE BouncerWindowObj.OnResize;
	BEGIN
	  INHERITED OnResize;
	  paddelPos.y := Height - paddelHeight - gap;
		InvertBlocks;
	END;

	PROCEDURE BouncerWindowObj.OnCommand(commandNr: INTEGER);
	BEGIN
    IF commandNr = smallCommand THEN
			ChangeSize(10)
		ELSE IF commandNr = mediumCommand THEN
			ChangeSize(20)
		ELSE IF commandNr = largeCommand THEN
			ChangeSize(40)
		ELSE IF commandNr = crawlCommand THEN
			ChangeSpeed(1)
		ELSE IF commandNr = walkCommand THEN
			ChangeSpeed(5)
		ELSE IF commandNr = runCommand THEN
			ChangeSpeed(10)
		ELSE IF commandNr = flyCommand THEN
			ChangeSpeed(20)
		ELSE
			INHERITED OnCommand(commandNr);
	END; (*BouncerWindowObj.OnCommand*)

	PROCEDURE BouncerWindowObj.InvertBall;
	BEGIN
		DrawFilledOval(pos, size, size);
	END; (*BouncerWindowObj.InvertBall*)

	PROCEDURE BouncerWindowObj.InvertPaddel;
	VAR topLeft : Point;
	BEGIN
		topLeft.x := paddelPos.x - paddelWidth DIV 2; (* left *)
		topLeft.y := paddelPos.y;
		DrawFilledRectangle(topLeft, paddelWidth, paddelHeight);
	END; (*BouncerWindowObj.InvertPaddel*)

  PROCEDURE BouncerWindowObj.InvertBlocks;
	VAR it : MLIterator;
	    b : Block;
			bPos : Point;
	BEGIN
	  it := blocks^.NewIterator;
		b := Block(it^.Next);
		WHILE b <> NIL DO BEGIN
		  bPos.x := b^.x;
			bPos.y := b^.y;
		  DrawFilledRectangle(bPos, b^.w, b^.h);
			b := Block(it^.Next);
		END;
		Dispose(it, Done);
	END;

  FUNCTION BouncerWindowObj.IsCollision : BOOLEAN;
	VAR c : BOOLEAN;
	BEGIN
	  c := (paddelPos.y - pos.y < size) AND
		     (pos.x > paddelPos.x - paddelWidth DIV 2) AND
		   	 (pos.x < paddelPos.x + paddelWidth DIV 2);
		IsCollision := c;
	END;

	FUNCTION BouncerWindowObj.IsCollisionWithBlock(b : Block) : BOOLEAN;
	BEGIN
	  IsCollisionWithBlock := b^.Contains(pos.x, pos.y) 
		                        OR b^.Contains(pos.x + size, pos.y)
		                        OR b^.Contains(pos.x, pos.y + size)
		                        OR b^.Contains(pos.x + size, pos.y + size);
	END;

	PROCEDURE BouncerWindowObj.ChangeSize(newSize: INTEGER);
	BEGIN
		InvertBall;
		size := newSize;
		InvertBall;
	END; (*BouncerWindowObj.ChangeSize*)

	PROCEDURE BouncerWindowObj.ChangeSpeed(newSpeed: INTEGER);
	BEGIN
		speed := newSpeed;
		IF dx < 0 THEN
			dx := -speed
		ELSE
			dx := speed;
		IF dy < 0 THEN
			dy := -speed
		ELSE
			dy := speed;
	END; (*BouncerWindowObj.ChangeSpeed*)


(*=== BouncerApplication ===*)

	FUNCTION NewBouncerApplication: BouncerApplication;
		VAR
			a: BouncerApplication;
	BEGIN
		New(a, Init('Bouncer Application V.1'));
		NewBouncerApplication := a;
	END; (*NewBouncerApplication*)

	CONSTRUCTOR BouncerApplicationObj.Init(name: STRING);
	BEGIN
		INHERITED Init(name);
		Register('BouncerApplication', 'MLApplication');
	END; (*BouncerApplicationObj.Init*)

	PROCEDURE BouncerApplicationObj.OpenNewWindow;
	BEGIN
		NewBouncerWindow^.Open;
	END; (*BouncerApplicationObj.OpenNewWindow*)

	PROCEDURE BouncerApplicationObj.BuildMenus;
	BEGIN
  (*size menu:*)
		smallCommand  := NewMenuCommand('Size',  'Small',  'S');
		mediumCommand := NewMenuCommand('Size',  'Medium', 'M');
		largeCommand  := NewMenuCommand('Size',  'Large',  'L');
  (*speed menu:*)
		crawlCommand  := NewMenuCommand('Speed', 'Crawl',  '1');
		walkCommand   := NewMenuCommand('Speed', 'Walk',   '2');
		runCommand    := NewMenuCommand('Speed', 'Run',    '3');
		flyCommand    := NewMenuCommand('Speed', 'Fly',    '4');
	END; (*BouncerApplicationObj.BuildMenus*)


(*=== main program ===*)

	VAR
		a: MLApplication;

BEGIN (*Bouncer1*)
	a := NewBouncerApplication;
	a^.Run;
	Dispose(a, Done);
	WriteMetaInfo;
END. (*Bouncer1*)
