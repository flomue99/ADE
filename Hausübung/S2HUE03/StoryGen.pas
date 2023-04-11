(* StoryGen:                                                 MFL, 2023-04-08 *)
(* ------                                                                    *)
(* Replace words in a story                                                  *)
(* ========================================================================= *)
PROGRAM StoryGen;
CONST
  M = 100;

TYPE 
  replacement = RECORD
    newWord: STRING;
    oldWord: STRING;
  END; (* replacement *)

VAR
  replacements: ARRAY [1..M] OF replacement;
  numOfRepls: INTEGER;

PROCEDURE ReadReplacements(replsFileName: STRING);
  VAR
    repls: TEXT;
    delimiterPos: INTEGER;
    line: STRING;
BEGIN (* ReadReplacements *)
  delimiterPos := 0;
  Assign(repls, replsFileName);
  {$i-}
  Reset(repls); 
  {$i+}
  IF (IOResult <> 0) THEN BEGIN
    WriteLn('ERROR: File ', replsFileName, ' does not exist!');
  END ELSE BEGIN
    WHILE (not EOF(repls)) DO BEGIN
      ReadLn(repls, line);
      delimiterPos := Pos(' ', line);
      replacements[numOfRepls].oldWord := Copy(line, 1, delimiterPos - 1);
      replacements[numOfRepls].newWord := Copy(line, delimiterPos + 1, Length(line));
      Inc(numOfRepls);
    END; (* WHILE *)
    Close(repls);
  END; (* IF *)
END; (* ReadReplacements *)

PROCEDURE CreateNewStory(oldStoryFileName, newStoryFileName: STRING);
  VAR
    oldStory, newStory: TEXT;
    line: STRING;
    i, replPos: INTEGER;
BEGIN (* CreateNewStory *)
  replPos := 0;
  Assign(oldStory, oldStoryFileName);
  {$i-}
  Reset(oldStory); 
  {$i+}
  IF (IOResult <> 0) THEN BEGIN
    WriteLn('ERROR: File ', oldStoryFileName, ' does not exist!');
  END ELSE BEGIN
    Assign(newStory, newStoryFileName);
    Rewrite(newStory);
    REPEAT
      ReadLn(oldStory, line);
      FOR i := 1 TO numOfRepls DO BEGIN
        replPos := Pos(replacements[i].oldWord, line);
        WHILE (replPos <> 0) DO BEGIN
          Delete(line, replPos, Length(replacements[i].oldWord));
          Insert(replacements[i].newWord, line, replPos);
          replPos := Pos(replacements[i].oldWord, line);
        END; (* WHILE *)
      END; (* FOR *)
      replPos := 0;
      WriteLn(newStory, line);
    UNTIL (EOF(oldStory)); (* REPEAT *)
    Close(oldStory);
    Close(newStory);
  END;
END; (* CreateNewStory *)

VAR
  replsFileName, oldStoryFileName, newStoryFileName: STRING;
BEGIN (* StoryGen *)
  numOfRepls := 1;
  replsFileName := ParamStr(1);
  oldStoryFileName := ParamStr(2);
  newStoryFileName := ParamStr(3);
  ReadReplacements(replsFileName);
  CreateNewStory(oldStoryFileName, newStoryFileName);
END. (* StoryGen *)
