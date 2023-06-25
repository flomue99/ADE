UNIT MLInt;

INTERFACE
USES MLObj;

TYPE
  MLInteger = ^MLIntegerObj;
  MLIntegerObj = OBJECT(MLObjectObj)
                   PRIVATE
                     val : INTEGER;
                   PUBLIC
                     CONSTRUCTOR Init(val : INTEGER);
                     FUNCTION AsString : STRING; VIRTUAL;
                     FUNCTION IsEqualTo(o : MLObject) : BOOLEAN; VIRTUAL;
                     FUNCTION IsLessThan(o : MLObject) : BOOLEAN; VIRTUAL;
                 END;

FUNCTION NewMLInteger(val : INTEGER) : MLInteger;

IMPLEMENTATION
USES MetaInfo;

FUNCTION NewMLInteger(val : INTEGER) : MLInteger;
VAR i : MLInteger;
BEGIN
  New(i, Init(val));
  NewMLInteger := i;
END;

(******************* MLInteger **********************)
CONSTRUCTOR MLIntegerObj.Init(val : INTEGER);
BEGIN
  INHERITED Init;
  Register('MLInteger', 'MLObject');
  SELF.val := val;
END;

FUNCTION MLIntegerObj.AsString : STRING;
VAR s : STRING;
BEGIN
  Str(val, s);
  AsString := s;
END;

FUNCTION MLIntegerObj.IsEqualTo(o : MLObject) : BOOLEAN;
VAR other : MLInteger;
BEGIN
  IF o^.IsA('MLInteger') THEN BEGIN
    other := MLInteger(o);
    IsEqualTo := SELF.val = other^.val;
  END ELSE 
    IsEqualTo := FALSE;
END;

FUNCTION MLIntegerObj.IsLessThan(o : MLObject) : BOOLEAN;
VAR other : MLInteger;
BEGIN
  IF o^.IsA('MLInteger') THEN BEGIN
    other := MLInteger(o);
    IsLessThan := SELF.val < other^.val;
  END ELSE BEGIN
    IsLessThan := INHERITED IsLessThan(o);
  END;  
END;

BEGIN
END.
