PROGRAM Test;

PROCEDURE FindLongestMatch(s1, s2: STRING; VAR sub: STRING; VAR start1, start2: INTEGER);
VAR
  i, j, len, maxLen, p: INTEGER;
  prefix: ARRAY[1..MAX_LENGTH] OF INTEGER;
BEGIN
  sub := '';
  start1 := 0;
  start2 := 0;
  maxLen := 0;
  
  FOR i := 1 TO LENGTH(s2) DO
    prefix[i] := 0;
  p := 0;
  
  FOR i := 2 TO LENGTH(s2) DO
    BEGIN
      WHILE (p > 0) AND (s2[p+1] <> s2[i]) DO
        p := prefix[p];
        
      IF s2[p+1] = s2[i] THEN
        INC(p);
        
      prefix[i] := p;
    END;
    
  i := 1;
  j := 1;
  WHILE (i <= LENGTH(s1)) AND (j <= LENGTH(s2)) DO
    BEGIN
      WHILE (j > 0) AND (s1[i] <> s2[j]) DO
        j := prefix[j];
        
      IF s1[i] = s2[j] THEN
        BEGIN
          INC(i);
          INC(j);
          INC(len);
          
          IF len > maxLen THEN
            BEGIN
              maxLen := len;
              sub := COPY(s1, i-len, len);
              start1 := i-len+1;
              start2 := j-len+1;
            END;
        END
      ELSE
        BEGIN
          len := 0;
          INC(i);
        END;
    END;
END;

BEGIN (* Test *)
  
END. (* Test *)