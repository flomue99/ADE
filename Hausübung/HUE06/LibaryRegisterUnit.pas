(* LibaryRegisterUnitPrg:                                    MFL, 2022-12-09 *)
(* ------                                                                    *)
(* Libary administration backend                                             *)
(* ========================================================================= *)
UNIT LibaryRegisterUnit;

INTERFACE
  PROCEDURE PrintAll;
  PROCEDURE InsertBook(title: STRING; author: STRING);
  PROCEDURE DisposeALL;
  FUNCTION NrOfBooksOF(author: STRING): INTEGER;

IMPLEMENTATION

  TYPE
    AuthorNodePtr = ^AuthorNode;
    AuthorNode = RECORD
    next: AuthorNodePtr;
    name: STRING;
    END; (* AuthorNode *)

  TYPE
    BookNodePtr = ^BookNode;
    BookNode = RECORD
    prev, next: BookNodePtr;
    title: STRING;
    authors: AuthorNodePtr;
    END; (* BookNode *) 

  VAR
    l: BookNodePtr;

  FUNCTION NewBookNode(title: STRING): BookNodePtr;
    VAR
      n: BookNodePtr;
  BEGIN (* NewBookNode *)
    New(n);
    n^.authors := NIL;
    n^.title := title;
    n^.prev := n;   
    n^.next := n;
    NewBookNode := n;
  END; (* NewBookNode *)

  FUNCTION NewBookList: BookNodePtr;
    VAR
    l: BookNodePtr;
  BEGIN (* NewBookList *)
    l := NewBookNode('DCList'); (* dummy *)
    NewBookList := l;
  END; (* NewBookList *)
  
  PROCEDURE InitList;
  BEGIN (* InitList *)
    l := NewBookList;
  END; (* InitList *)

  FUNCTION NewAuthorNode(author: STRING): AuthorNodePtr;
    VAR
      n: AuthorNodePtr;
  BEGIN (* NewAuthorNode *)
    New(n);
    n^.name := author;   
    n^.next := NIL;
    NewAuthorNode := n;
  END; (* NewAuthorNode *)

  PROCEDURE WriteAuthorsList(l: AuthorNodePtr);
    VAR
      n: AuthorNodePtr;
  BEGIN (* WriteAuthorsList *)
    n := l;
    Write('  ');
    IF (n <> NIL) THEN BEGIN
      Write('Authors: ', n^.name);
      n := n^.next;
    END; (* IF *)
    WHILE (n <> NIL) DO BEGIN
      Write(', ', n^.name);
      n := n^.next; (*set next node*)
    END; (* WHILE *)
  END; (* WriteAuthorsList *)
  
  PROCEDURE PrintAll;
    VAR
      n: BookNodePtr;
  BEGIN (* PrintAll *)
    IF (l^.next = l) THEN BEGIN
      WriteLn('ERROR: List is NIL');
    END ELSE BEGIN
      n := l^.next;
      WHILE (n <> l) DO BEGIN
        WriteLn(n^.title, '');
        WriteAuthorsList(n^.authors);
        WriteLn();
        WriteLn('--------------------------------------------------------');
        n := n^.next; (*set next node*)
      END; (* WHILE *)
    END;
  END; (* PrintAll *)

  PROCEDURE InsertAuthor(VAR l: AuthorNodePtr; author: STRING);
    VAR
      newNode: AuthorNodePtr;
      succ, pred: AuthorNodePtr;
  BEGIN (* InsertAuthor *)
    newNode := NewAuthorNode(author);
    IF (l = NIL) THEN BEGIN
      l := newNode;
    END ELSE BEGIN
      succ := l;
      pred := NIL;
      WHILE ((succ <> NIL) AND (succ^.name <= newNode^.name)) DO BEGIN
        pred := succ;
        succ := succ^.next;
      END; (* WHILE *)
      IF ((pred <> NIL) AND (newNode^.name = pred^.name)) THEN BEGIN (* Author already exists *)
        Dispose(newNode);
      END ELSE BEGIN
        newNode^.next := succ;
        IF (pred = NIL) THEN BEGIN
          l := newNode;
        END ELSE BEGIN
          pred^.next := newNode;
        END; (* IF *)
      END; (* IF *)
    END; (* IF *)
  END; (* InsertAuthor *)

  PROCEDURE InsertBook(title: STRING; author: STRING);
    VAR
      newNode: BookNodePtr;
      succ: BookNodePtr;
  BEGIN (* Insert *)
    IF ((author = '') OR (title = '')) THEN BEGIN
      WriteLn('ERROR: title or author string is empty');
      Exit;
    END; (* IF *)
    newNode := NewBookNode(title);
    succ := l^.next;
    WHILE ((succ <> l) AND (succ^.title <= newNode^.title)) DO BEGIN
      succ := succ^.next;
    END; (* WHILE *)
    IF (newNode^.title = succ^.prev^.title) THEN BEGIN (* book already exists *)
      Dispose(newNode);
      InsertAuthor(succ^.prev^.authors, author);
    END ELSE BEGIN
      InsertAuthor(newNode^.authors, author);
      newNode^.prev := succ^.prev;
      newNode^.next := succ;
      succ^.prev^.next := newNode;
      succ^.prev := newNode;
    END; (* IF *)
  END; (* Insert *)

  FUNCTION NrOfBooksOF(author: STRING): INTEGER;
    VAR
      n1: BookNodePtr;
      n2: AuthorNodePtr;
      count: INTEGER;
  BEGIN (* NrOfBooksOF *)
    IF (author = '') THEN BEGIN
      WriteLn('ERROR: author is empty');
      Exit;
    END; (* IF *)
    count := 0;
    n1 := l^.next;
    n2 := NIL;
    WHILE (n1 <> l) DO BEGIN
      n2 := n1^.authors;
      WHILE (n2 <> NIL) DO BEGIN
        IF (n2^.name = author) THEN BEGIN
          count := count + 1;
        END; (* IF *)
        n2 := n2^.next; (*set next authornode*)
      END; (* WHILE *)
      n1 := n1^.next; (*set next booknode*)
    END; (* WHILE *)
    NrOfBooksOF := count;
  END; (* NrOfBooksOF *)

  PROCEDURE DisposeSinglyLinkedList(VAR l2: AuthorNodePtr);
    VAR 
      n: AuthorNodePtr;
  BEGIN (* DisposeSinglyLinkedList *)
    WHILE (l2 <> NIL) DO BEGIN
      n := l2^.next;
      Dispose(l2);
      l2 := n;
    END; (* WHILE *)
  END;(* DisposeSinglyLinkedList *)

  PROCEDURE DisposeALL;
    VAR
      n1: BookNodePtr;
  BEGIN (* DisposeALL *)
    l^.prev^.next := NIL;
    WHILE (l <> NIL) DO BEGIN
      n1 := l^.next;  
      DisposeSinglyLinkedList(l^.authors);
      Dispose(l); 
      l := n1; 
    END; (* WHILE *)
  END; (* DisposeALL *)

BEGIN (* LibaryRegisterUnit *)
  InitList();
END. (* LibaryRegisterUnit *)