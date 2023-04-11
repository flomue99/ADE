(* Title:                                                 Author, 2023-03-29 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
(*
3 Dateitypen in Pascal
 1. Textdatei               VAR textFile: TEXT;
 2. Binärdatei              VAR binFile: FILE;
 3. Typisierte Binärdatei   
*)
PROGRAM FileIO;

VAR 
  t: TEXT;
  ch: CHAR;
  r1, r2: REAL;
BEGIN (* FileIO *)
  Assign(t, 'C:\Temp\myfile.txt');
  Rewrite(t);
  WriteLn(t, 'Hallo Florian!');
  Close(t);

  Assign(t, 'C:\Temp\myfile.txt');
  Append(t);
  WriteLn(t, 'Append new Line');
  Close(t);

  (* read pascal file and output to console*)
  Assign(t, 'FileIO.pas');
  Reset(t); (* open file *)
  WHILE (not EOF(t)) DO BEGIN
    Read(t, ch);
    Write(ch);
  END; (* WHILE *)
  Close(t);

  Assign(t, 'C:\temp\numbers.txt');
  Rewrite(t);
  WriteLn(t, 3.12341234, 22/7);
  Reset(t);
  ReadLn(t, r1, r2);
  WriteLn(r1);
  WriteLn(r2);
  Close(t);

  (*Fehlerbehandlung*)
  Assign(t, 'c:\temp\num.txt');
  {$I-}  
  //IO Fehler deaktivieren
  Reset(t);
  IF (IOResult <> 0) THEN BEGIN
    WriteLn('file does not exist.');
    Halt;
  END; (* IF *)
  {$I+}  
  //IO Fehler aktivieren
  Read(t, r1);
END. (* FileIO *)