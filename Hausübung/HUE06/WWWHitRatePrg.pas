(* WWWHitRatePrg:                                            MFL, 2022-12-01 *)
(* ------                                                                    *)
(* Calculate amount of ips with more then one access                         *)
(* ========================================================================= *)
PROGRAM WWWHitRatePrg;

  CONST
    max = 4;

  TYPE
    StringArray = ARRAY OF STRING;
    ByteArray = ARRAY[1..max] OF BYTE;
    IPaddrNodePtr = ^IPaddrNode;
    IPaddrNode = RECORD
      next: IPaddrNodePtr;
      addr: ARRAY [1..max] OF BYTE;
      n: INTEGER; (* number of acceses from addr *)
    END; (* IPaddrNode *)
    ListPtr = IPaddrNodePtr;
  
  FUNCTION IpStringToByteArray(ipString: STRING): ByteArray;
    VAR
      byteArr: ByteArray;
      value: BYTE;
      i, j: INTEGER;
      s: STRING;
  BEGIN (* IpStringToByteArray *)
    s := '';
    j := 1;
    FOR i := 1 TO max DO BEGIN
      WHILE ((ipString[j] <> '.') AND (j <= Length(ipString))) DO BEGIN
        s := s + ipString[j];
        j := j + 1;
      END; (* WHILE *)
      Val(s, value);
      byteArr[i] := value;
      s := '';
      j := j + 1;
    END; (* FOR *)
    IpStringToByteArray := byteArr;
  END; (* IpStringToByteArray *)

  FUNCTION NewList: ListPtr;
  BEGIN (* NewList *)
    NewList := NIL;
  END; (* NewList *)

  FUNCTION NewNode(a: ARRAY OF BYTE): IPaddrNodePtr;
    VAR
      ipAddr: IPaddrNodePtr;
  BEGIN (* NewNode *)
    New(ipAddr);
    ipAddr^.n := 1;
    ipAddr^.addr := a;
    ipAddr^.next := NIL;
    NewNode := ipAddr;
  END; (* NewNode *)

  FUNCTION IsSameAddress(addr1, addr2: Array OF BYTE): BOOLEAN;
  BEGIN (* IsSameAddress *)
    IsSameAddress := ((addr1[0] = addr2[0]) AND (addr1[1] = addr2[1]) AND (addr1[2] = addr2[2]) AND (addr1[3] = addr2[3]));
  END; (* IsSameAddress *)

  FUNCTION IpAddrExists(VAR list: ListPtr; ipAddr: IPaddrNodePtr): BOOLEAN;
    VAR
      addr1: IPaddrNodePtr;
      res: BOOLEAN;
  BEGIN (* IpAddrExists *)
    addr1 := list;
    res := FALSE;
    WHILE ((addr1 <> NIL) AND (res <> TRUE)) DO BEGIN
      res := IsSameAddress(addr1^.addr, ipAddr^.addr);
      IF (res) THEN BEGIN
        addr1^.n := addr1^.n + 1;
      END; (* IF *)
      addr1 := addr1^.next;
    END; (* WHILE *) 
    IpAddrExists := res;
  END; (* IpAddrExists *)

  FUNCTION IsLexicographicSmaller(ip1, ip2: ARRAY OF BYTE): BOOLEAN;
  BEGIN (* LexicographicComparison *)
    IF (ip1[0] > ip2[0]) THEN BEGIN 
      Exit(FALSE);
    END; (* IF *)
    IF ((ip1[0] = ip2[0]) AND (ip1[1] > ip2[1])) THEN BEGIN
      Exit(FALSE);
    END; (* IF *)
    IF ((ip1[0] = ip2[0]) AND (ip1[1] = ip2[1]) AND (ip1[2] > ip2[2])) THEN BEGIN
      Exit(FALSE);
    END; (* IF *)
    IF ((ip1[0] = ip2[0]) AND (ip1[1] = ip2[1]) AND (ip1[2] = ip2[2]) AND (ip1[3] > ip2[3])) THEN BEGIN
      Exit(FALSE);
    END; (* IF *)
    IsLexicographicSmaller := TRUE;
  END; (* LexicographicComparison *)
  
  PROCEDURE WriteList(list: ListPtr); (* test Procedure to diplay the sorted list.*)
    VAR
      ipAddr: IPaddrNodePtr;
      i: INTEGER;
  BEGIN (* WriteList *)
    ipAddr := list;
    WHILE (ipAddr <> NIL) DO BEGIN
      FOR i := 1 TO max DO BEGIN
        IF (i = 1) THEN BEGIN
          Write(ipAddr^.addr[i]);  
        END ELSE BEGIN
          Write('.', ipAddr^.addr[i]);  
        END; (* IF *)
      END; (* FOR *)
      Write(' Anzahl: (', ipAddr^.n, ')  --> ');
      WriteLn();
      ipAddr := ipAddr^.next;
      i := 1;
    END; (* WHILE *)
    Write('|');
    WriteLn();
  END; (* WriteList *)

  FUNCTION CountIpDots(ipString: STRING): INTEGER; (* for validation *)
    VAR
      i, count: INTEGER;
  BEGIN (* IpDotCount *)
    count := 0;
    FOR i := 1 TO Length(ipString) DO BEGIN
      IF (ipString[i] = '.') THEN BEGIN
        count := count + 1;
      END; (* IF *)
    END; (* FOR *)
    CountIpDots := count;
  END; (* IpDotCount *)

  FUNCTION DotValidation(ipString: STRING): BOOLEAN;
    VAR
      i, j, count: INTEGER;
  BEGIN (* DotValidation *)
    i := 1;
    j := 1;
    count := 0;
    IF ((ipString[1] = '.') OR (ipString[Length(ipString)] = '.')) THEN BEGIN
      Exit(FALSE);
    END; (* IF *)
    WHILE (i <> Length(ipString)) DO BEGIN
      IF (ipString[i] <> '.') THEN BEGIN
        count := count + 1;
        IF (count > 3) THEN BEGIN //to big for byte
          Exit(FALSE);
        END; (* IF *)
      END ELSE BEGIN
        count := 0;
      END;
      i := i + 1;
    END; (* WHILE *)
    WHILE (j < Length(ipString)) DO BEGIN
      IF ((ipString[j] = '.') AND (ipString[j + 1] = '.')) THEN BEGIN (* two dots side by side*)
        Exit(FALSE);
      END;
      j := j + 1;
    END; (* WHILE *)
    DotValidation := TRUE;
  END; (* DotValidation *)

  FUNCTION ByteValidation(ipString: STRING): BOOLEAN;
    VAR
      i, j, value: INTEGER;
      s: STRING;
  BEGIN (* ByteValidation *)
    s := '';
    j := 1;
    FOR i := 1 TO max DO BEGIN
      WHILE ((ipString[j] <> '.') AND (j <= Length(ipString))) DO BEGIN
        s := s + ipString[j];
        j := j + 1;
      END; (* WHILE *)
      Val(s, value);
      IF (value > 255) THEN BEGIN
        Exit(FALSE);
      END; (* IF *)
      s := '';
      j := j + 1;
    END; (* FOR *)
    ByteValidation := TRUE;
  END; (* ByteValidation *)

  FUNCTION IsIpWithoutChars(ipString: STRING): BOOLEAN;
    VAR
      tempOrd, i: INTEGER;
      
  BEGIN (* IsIpWithoutChars *)
    FOR i := 1 TO Length(ipString) DO BEGIN
       tempOrd := Ord(ipString[i]);
      IF (((tempOrd <> 46) AND (tempOrd < 48)) OR ((tempOrd <> 46) AND (tempOrd > 59))) THEN BEGIN
        Exit(FALSE);
      END; (* IF *)
    END; (* FOR *)
    IsIpWithoutChars := TRUE;
  END; (* IsIpWithoutChars *)

  FUNCTION IsValidIpAddr(ipString: STRING): BOOLEAN;
  BEGIN (* IsValidIpAddr *)
    IF ((Length(ipString) < 7) OR (Length(ipString) > 15) OR (CountIpDots(ipString) <> 3) OR (NOT DotValidation(ipString)) OR (NOT IsIpWithoutChars(ipString)) OR (NOT ByteValidation(ipString))) THEN BEGIN 
      Exit(FALSE);
    END; (* IF *)
    IsValidIpAddr := TRUE;
  END; (* IsValidIpAddr *)

  PROCEDURE InsertIp(Var list: ListPtr; ipString: STRING);
    VAR
      newIpAddr, ipAddr, predIpAddr: IPaddrNodePtr;     
  BEGIN(* InsertIp *)
    IF (NOT IsValidIpAddr(ipString)) THEN BEGIN
      WriteLn('ERROR: ipaddress was incorrect! ', ipString);
      Exit;
    END; (* IF *)
    ipAddr := list;
    predIpAddr := NIL;
    newIpAddr := NewNode(IpStringToByteArray(ipString));
    IF (list = NIL) THEN BEGIN
      list := newIpAddr;
    END ELSE BEGIN
      IF (NOT IpAddrExists(list, newIpAddr)) THEN BEGIN
        WHILE ((ipAddr <> NIL) AND (IsLexicographicSmaller(ipAddr^.addr, newIpAddr^.addr))) DO BEGIN
          predIpAddr := ipAddr;
          ipAddr := ipAddr^.next;
        END; (* WHILE *)
        IF (predIpAddr = NIL) THEN BEGIN
          newIpAddr^.next := list;
          list := newIpAddr;
        END ELSE BEGIN
          predIpAddr^.next := newIpAddr;
          newIpAddr^.next := ipAddr;
        END; (* IF *)
      END ELSE BEGIN
        Dispose(newIpAddr);
      END;
    END;
  END; (* InsertIp *)

  PROCEDURE InsertIpsToList(VAR list: ListPtr); (* to read ips from a .txt file *)
    VAR
      s: STRING;
  BEGIN (* ReadIps *)
    ReadLn(s); (* it always reads a new line in the .txt file *)
    InsertIp(list, s);
    WHILE(s <> '') DO BEGIN
      ReadLn(s); (* it always reads a new line in the .txt file *)
      IF (s <> '') THEN BEGIN
        InsertIp(list, s);  
      END; (* IF *)
    END; (* WHILE *)
  END; (* ReadIps *)

  PROCEDURE DisposeList(VAR list: ListPtr);
    VAR
      ipAddr: IPaddrNodePtr;
  BEGIN (* DisposeList *)
    WHILE (list <> NIL) DO BEGIN
      ipAddr := list^.next;
      Dispose(list); 
      list := ipAddr;
    END; (* WHILE *)
  END; (* DisposeList *)

  FUNCTION AmountOfIpsWithMoreThenOneAccess(list: ListPtr): INTEGER;
    VAR
      count: INTEGER;
      ipAddr: IPaddrNodePtr;
  BEGIN (* AmountOfIpsWithMoreThenOneAccess *)
    ipAddr := list;
    count := 0;
    WHILE (ipAddr <> NIL) DO BEGIN
      IF (ipAddr^.n > 1) THEN BEGIN
        count := count + 1;
      END; (* IF *)
      ipAddr := ipAddr^.next;
    END; (* WHILE *)
    AmountOfIpsWithMoreThenOneAccess := count;
  END; (* AmountOfIpsWithMoreThenOneAccess *)
VAR
  list: ListPtr;
BEGIN (* WWWHitRatePrg *)
  list := NewList();
  InsertIpsToList(List);
  WriteList(list);
  WriteLn('Amount of ips with more then one access: ', AmountOfIpsWithMoreThenOneAccess(list));
  DisposeList(list);
END. (* WWWHitRatePrg *)