(* Florian:                                                  MFL, 2023-05-28 *)
(* ------                                                                    *)
(* OOP filesystem                                                            *)
(* ========================================================================= *)
PROGRAM OOPFileSystem;
CONST
  max = 100;
TYPE
  TFileSystem = ^FileSystemObj;
  FileSystemObj = OBJECT
                    name: STRING;
                    objectType: STRING;
                    dateModified: STRING;
                    CONSTRUCTOR Init(name, objectType, dateModified: STRING);
                    FUNCTION AsString: STRING; VIRTUAL; ABSTRACT;
                    PROCEDURE WriteStructure(l: INTEGER); VIRTUAL; ABSTRACT;
                    FUNCTION Size: LONGINT; VIRTUAL; ABSTRACT;
                    DESTRUCTOR Done; VIRTUAL;
                  END;

  TFile = ^FileObj;
  FileObj = OBJECT(FileSystemObj)
              fileSize: LONGINT;
              CONSTRUCTOR Init(name, objectType, dateModified: STRING; size: LONGINT);
              FUNCTION AsString: STRING; VIRTUAL;
              PROCEDURE WriteStructure(l: INTEGER); VIRTUAL;
              FUNCTION Size: LONGINT; VIRTUAL;
            END;


  TFolder = ^FolderObj;
  FolderObj = OBJECT(FileSystemObj)
                numObjects: INTEGER;
                fileSystemObjects: ARRAY [1..max] OF TFileSystem;
                CONSTRUCTOR Init(name, objectType, dateModified: STRING);
                PROCEDURE Add(fileSystemObject: TFileSystem);
                FUNCTION AsString: STRING; VIRTUAL;
                FUNCTION Remove(name: STRING): TFileSystem;
                PROCEDURE Delete(name: STRING);
                PROCEDURE WriteStructure(l: INTEGER); VIRTUAL;
                FUNCTION Size: LONGINT; VIRTUAL;
                DESTRUCTOR Done; VIRTUAL;
              END;

(*--------------------TFileSystemObj---------------------*)
CONSTRUCTOR FileSystemObj.Init(name, objectType, dateModified: STRING);
BEGIN (* FileSystemObj.Init *)
  SELF.name := name;
  SELF.objectType := objectType;
  SELF.dateModified := dateModified;
END; (* FileSystemObj.Init *)

DESTRUCTOR FileSystemObj.Done;
BEGIN
  (* empty *)
END;

(*--------------------FileObj---------------------*)
CONSTRUCTOR FileObj.Init(name, objectType, dateModified: STRING; size: LONGINT);
BEGIN (* FileObj.Init *)
  INHERITED Init(name, objectType, dateModified);
  SELF.fileSize := size;
END; (* FileObj.Init *)

FUNCTION FileObj.AsString: STRING;
  VAR
    sizeStr: STRING;
BEGIN (* FileObj.AsString *)
  Str(fileSize, sizeStr);
  AsString := 'Name: '+ name + ' Type: ' + objectType + ' DateModified: ' + dateModified + '  Size: ' + sizeStr;
END; (* FileObj.AsString *)

PROCEDURE FileObj.WriteStructure(l: INTEGER);
BEGIN (* FileObj.WriteStructure *)
    WriteLn('|__':l, name);
END; (* FileObj.WriteStructure *)

FUNCTION FileObj.Size: LONGINT;
BEGIN (* FileSystemObj.Size *)
  Size := fileSize;
END; (* FileSystemObj.Size *)

(*--------------------TFolder---------------------*)
CONSTRUCTOR FolderObj.Init(name, objectType, dateModified: STRING);
  VAR 
    i: INTEGER;
BEGIN (* FileObj.Init *)
  SELF.numObjects := 0;
  INHERITED Init(name, objectType, dateModified);
  FOR i := Low(fileSystemObjects) TO High(fileSystemObjects) DO BEGIN
    fileSystemObjects[i] := NIL;
  END; (* FOR *)
END; (* FileObj.Init *)

PROCEDURE FolderObj.Add(fileSystemObject: TFileSystem);
BEGIN (* FolderObj.Add *)
  Inc(numObjects);
  IF numObjects <= max THEN BEGIN
    fileSystemObjects[numObjects] := fileSystemObject;
  END ELSE BEGIN
    WriteLn('ERROR: numObjects > max.');
    HALT;
  END;
END; (* FolderObj.Add *)

FUNCTION FolderObj.AsString: STRING;
BEGIN (* FolderObj.AsString *)
  AsString := 'Name: ' + name + ' Type: ' + objectType + ' DateModified: ' + dateModified;
END; (* FolderObj.AsString *)

FUNCTION FolderObj.Size: LONGINT;
  VAR
    i: INTEGER;
    sizeSum: LONGINT;
BEGIN (* FolderObj.Size *)
  sizeSum := 0;
  FOR i := 1 TO numObjects DO BEGIN
    sizeSum := sizeSum + fileSystemObjects[i]^.Size;
  END; (* FOR *)
  Size := sizeSum;
END; (* FolderObj.Size *)

PROCEDURE FolderObj.Delete(name: STRING);
  VAR
    res: TFileSystem;
BEGIN (* FolderObj.Delete *)
   res := Remove(name);
   IF (res = NIL) THEN BEGIN
      WriteLn('Object with name [', name, '] does not exist!');
   END ELSE BEGIN
      Dispose(res, Done );
   END; (* IF *)
END; (* FolderObj.Delete *)

FUNCTION FolderObj.Remove(name: STRING): TFileSystem;
  VAR
    j, i : INTEGER;
    res: TFileSystem;
BEGIN
  j := 1;
  res := NIL;
  WHILE (j <= numObjects) AND (name <> fileSystemObjects[j]^.name) DO BEGIN
    Inc(j);
  END; (* WHILE *)
  IF (j <= numObjects) THEN BEGIN
    res := fileSystemObjects[j];
    Dec(numObjects);
  END; (* IF *)
    FOR i := j TO numObjects DO BEGIN
      fileSystemObjects[i] := fileSystemObjects[i + 1];
    END; (* FOR *)
  Remove := res;
END; (* FolderObj.Remove *)

PROCEDURE FolderObj.WriteStructure(l: INTEGER);
VAR
  i: INTEGER;
BEGIN
  IF (SELF.objectType ='Ordner') THEN BEGIN
    WriteLn('/':l, name);
  END;
  FOR i := 1 TO numObjects DO BEGIN
    IF (fileSystemObjects[i]^.objectType = 'Ordner') THEN BEGIN
      fileSystemObjects[i]^.WriteStructure(l*2);
    END ELSE BEGIN
      fileSystemObjects[i]^.WriteStructure(l+3);
    END;
  END;
  Write(' ')
END;

DESTRUCTOR FolderObj.Done;
  VAR 
    i: INTEGER;
BEGIN
  FOR i := 1 TO numObjects DO BEGIN
    Dispose(fileSystemObjects[i], Done);
  END; (* FOR *)
  INHERITED Done;
END;

VAR
  benutzer: TFolder;
  florian: TFolder;
  pascalprgs: TFolder;
  file1: TFile;
  file2: TFile;
  file3: TFile;
  file4: TFile;
  file5: TFile;
  file6: TFile;
  test: TFileSystem;
BEGIN (* OOPFileSystem *)
  New(benutzer, Init('Benutzer', 'Ordner', '09.09.2001'));
  New(florian, Init('Florian', 'Ordner', '09.09.2011'));
  New(pascalprgs, Init('PascalProgramms', 'Ordner', '09.09.2011'));
  New(file1, Init('ToDo.txt', 'Datei', '30.05.2023', 99));
  New(file2, Init('test.pas', 'Datei', '03.07.2022', 123));
  New(file3, Init('Hue1.pas', 'Datei', '03.07.2022', 30));
  New(file4, Init('Hue2.pas', 'Datei', '03.07.2022', 40));
  New(file5, Init('Hue3.pas', 'Datei', '03.07.2022', 20));
  New(file6, Init('Hue4.pas', 'Datei', '03.07.2022', 70));
  florian^.Add(file2);
  pascalprgs^.Add(file3);
  pascalprgs^.Add(file4);
  pascalprgs^.Add(file5);
  pascalprgs^.Add(file6);
  florian^.Add(pascalprgs);
  benutzer^.Add(florian);
  benutzer^.Add(file1);
  WriteLn(benutzer^.AsString);
  WriteLn(file1^.AsString);
  benutzer^.WriteStructure(4);
  WriteLn('Ordner Benutzer ist ',benutzer^.Size,' kB gross.');
  test := benutzer^.Remove('Florian');
  test^.WriteStructure(4);
  WriteLn('Ordner Florian ist ',test^.Size,' kB gross.');
  WriteLn('-----------------------');
  benutzer^.WriteStructure(4);
  WriteLn('Ordner Benutzer ist ',benutzer^.Size,' kB gross.');
  benutzer^.Delete('Florian');
  WriteLn('Ordner Benutzer ist ',benutzer^.Size,' kB gross.');
  benutzer^.Delete('ToDo.txt');
  WriteLn('Ordner Benutzer ist ',benutzer^.Size,' kB gross.');
  Dispose(test, Done);
  Dispose(benutzer, Done);
END. (* OOPFileSystem *)