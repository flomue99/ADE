(* OOPerson:                                                 Author, 2023-05-24 *)
(* ------                                                                    *)
(* Description                                                               *)
(* ========================================================================= *)
PROGRAM OOPerson;

TYPE
  Person = ^PersonObj;
  PersonObj = OBJECT
                name: STRING;
                birthYear: INTEGER;
                favDrink: STRING; 
                CONSTRUCTOR Init(name: STRING; birthYear: INTEGER;
                                 favDrink: STRING); 
                CONSTRUCTOR InitRandom;
                PROCEDURE WriteFavDrink; VIRTUAL; (* darf überschreiben werden*)
              END;
  
  Student = ^StudentObj;
  StudentObj = OBJECT(PersonObj)
                 matNr: STRING;
                 course: STRING;
                 CONSTRUCTOR Init(name: STRING;
                             birthYear: INTEGER; 
                             favDrink : STRING; 
                             matNr: STRING; 
                             course: STRING);
                 CONSTRUCTOR InitRandom;
               END;
  
  IncomingStudent = ^IncomingStudentObj;
  IncomingStudentObj = OBJECT(StudentObj)
                          homeUni: STRING;
                          homeCourse: STRING;
                          CONSTRUCTOR Init(name: STRING;
                                           birthYear: INTEGER;
                                           favDrink : STRING;
                                           matNr: STRING; 
                                           course: STRING);
                          CONSTRUCTOR InitRandom;
                        END;

(*---------------------PERSON-------------------------*)
CONSTRUCTOR PersonObj.Init(name: STRING; birthYear: INTEGER; favDrink: STRING); 
BEGIN
  SELF.name := name;
  SELF.birthYear := birthYear;
  SELF.favDrink := favDrink;
END;

CONSTRUCTOR PersonObj.InitRandom;
BEGIN
  CASE Random(5) OF 
    0: SELF.name := 'Mayr';
    1: SELF.name := 'Huber';
    2: SELF.name := 'Groß';
    3: SELF.name := 'Kurz';
    4: SELF.name := 'Berger';
  END; (* CASE *)
  SELF.birthYear := 1900 + Random(123);
  CASE Random(5) OF
    0: SELF.favDrink := 'water';
    1: SELF.favDrink := 'coffee';
    2: SELF.favDrink := 'wine';
    3: SELF.favDrink := 'beer';
    4: SELF.favDrink := 'gintonic';
  END; (* CASE *)
END;

PROCEDURE PersonObj.WriteFavDrink;
BEGIN (* PersonObj.WriteFavDrinkk *)
  WriteLn('My name is ', name ,' and my favorite drink is ', favDrink);
END; (* PersonObj.WriteFavDrink *)
                              
(*---------------------Student-------------------------*)
CONSTRUCTOR StudentObj.Init(name: STRING;
                             birthYear: INTEGER; 
                             favDrink : STRING; 
                             matNr: STRING; 
                             course: STRING);
BEGIN
  INHERITED Init(name, birthYear, favDrink);
  SELF.matNr := matNr;
  SELF.course := course;
END;

CONSTRUCTOR StudentObj.InitRandom;
  VAR
    s: STRING;
BEGIN
  INHERITED InitRandom;
  CASE Random(5) OF
    0: course := 'Software Enigneering';
    1: course := 'Mobile Computing';
    2: course := 'Design of Digital Products';
    3: course := 'Media and Design';
    4: course := 'Medicine and Bioinformatics';
  END; (* CASE *)
  Str(Random(1000000), s);
  matNr := 'S' + s;
END;

(*---------------------IncommingStudent-------------------------*)
CONSTRUCTOR IncomingStudentObj.Init(name: STRING;
                                           birthYear: INTEGER;
                                           favDrink : STRING;
                                           matNr: STRING; 
                                           course: STRING);
BEGIN
  INHERITED Init(name, birthYear, favDrink, matNr, course);
  SELF.homeUni := homeUni;
  SELF.homeCourse := homeCourse;
END;

CONSTRUCTOR IncomingStudentObj.InitRandom;
BEGIN
  INHERITED InitRandom;
  homeUni := '....';
  homeCourse := '....';
END;

(*---------------------Programm-------------------------*)

FUNCTION NewRandomPerson: Person;
  VAR
    p: Person;
    s: Student;
    t: IncomingStudent;
BEGIN (* NewRandomPerson *)
  CASE Random(100) OF
    0..60  : BEGIN
              New(p, InitRandom);
              NewRandomPerson := p;
             END;
    61..85 : BEGIN
              New(s, InitRandom);
              NewRandomPerson := s;
             END;
    86..99 : BEGIN
              New(t, InitRandom);
              NewRandomPerson := t;
             END;
    ELSE BEGIN WriteLn('ERROR'); HALT; END;
  END; (*CASE*)
END; (* NewRandomPerson *)

CONST
  max = 100;

VAR
  personArray: ARRAY[1..max] OF PERSON;
  i: INTEGER;
BEGIN (* OOPerso *)
  FOR i := 1 TO 20 DO BEGIN
    personArray[i] := NewRandomPerson;
  END; (* FOR *)
  FOR i := 1 TO 20 DO BEGIN
    personArray[i]^.WriteFavDrink;
  END; (* FOR *)
  
END. (* OOPerson *)