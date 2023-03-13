(* WorktimeCalculater:                          Florian Mühleder, 2022-10-14 *)
(* ------                                                                    *)
(* Read two values, minutes and hours and calculate the worktime             *)
(* ========================================================================= *)
PROGRAM WorktimeCalculater;
VAR
  hours,minutes: INTEGER;
  worktime,result:REAL;
BEGIN (* WorktimeCalculater *)
  Write('Stunden: ');
  ReadLN(hours);
 
  Write('Minuten: ');
  ReadLN(minutes);
  worktime:= hours+ (minutes/60);

IF (hours>12) THEN BEGIN
   WriteLN('Taegliche Hoechstarbeitszeit ueberschritten');
END ELSE BEGIN
  IF ((hours>=8) AND (hours<10) AND (minutes>0)) THEN BEGIN
    result:= worktime-8;
    WriteLN('Anspruch auf Zeitausgleich:',result:5:2,' Stunden');
  END ELSE IF ((hours>=10) AND (hours<12)) THEN BEGIN
     result:= ((worktime-10)*1.5)+2;
    WriteLN('Anspruch auf Zeitausgleich:',result:5:2,' Stunden');
    END ELSE BEGIN
      WriteLN('Kein Zeitausgleich moeglich');
    End
END; (* IF *)
 
END. (* WorktimeCalculater *)