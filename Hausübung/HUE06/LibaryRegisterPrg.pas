(* LibaryRegisterPrg:                                        MFL, 2022-12-11 *)
(* ------                                                                    *)
(* Libary administation                                                      *)
(* ========================================================================= *)
PROGRAM LibaryRegisterPrg;
  USES
    LibaryRegisterUnit;
  
BEGIN (* LibaryRegisterPrg *)
  PrintAll;
  InsertBook('Der kleine Hobbit', 'J. R. R. Tolkien');
  InsertBook('Anatomy', 'Dana Schwartz');
  InsertBook('Roloff/Matek Maschinenelemente', 'Herbert Wittel');
  InsertBook('Roloff/Matek Maschinenelemente', 'Dieter Muhs');
  InsertBook('Roloff/Matek Maschinenelemente', 'Dieter Jannasch');
  InsertBook('Roloff/Matek Maschinenelemente', 'Joahim Vossiek');
  InsertBook('Mathematik HTL II', 'Fischer A.');
  InsertBook('Mathematik HTL II', 'Tinhof');
  InsertBook('Mathematik HTL II', 'Tordai');
  InsertBook('Mathematik HTL II', 'Wagenleitner');
  InsertBook('Mathematik HTL II', 'Fischer S.');
  InsertBook('Mathematik HTL I', 'Fischer A.');
  InsertBook('Mathematik HTL I', 'Fischer S.');
  InsertBook('Mathematik HTL III', 'Fischer A.');
  InsertBook('Mathematik HTL III', 'Fischer A.');
  InsertBook('Mathematik HTL III', 'Fischer S.');
  InsertBook('Mathematik HTL III', 'Fischer S.');  
  InsertBook('Mathematik HTL IV', 'Fischer A.');
  InsertBook('Mathematik HTL IV', 'Fischer S.');
  InsertBook('Mathematik HTL V', 'Fischer S.');
  InsertBook('Mathematik HTL V', 'Fischer S.');
  InsertBook('', 'Fischer S.');
  InsertBook('Mathematik HTL V', '');
  InsertBook('Klavier spielen lernen', 'Xaver Muster');
  InsertBook('Klavier spielen lernen', 'Albert Mayr');
  InsertBook('Klavier spielen lernen', 'Rudolf Lehner');
  PrintAll;
  WriteLn('Amount of books writen by Fischer S. : ', NrOfBooksOF('Fischer S.'));
  WriteLn('Amount of books writen by Fischer A. : ', NrOfBooksOF('Fischer A.'));
  WriteLn('Amount of books writen by J. R. R. Tolkien : ', NrOfBooksOF('J. R. R. Tolkien'));
  WriteLn('Amount of books writen by Florian : ', NrOfBooksOF(''));
  DisposeALL;
END. (* LibaryRegisterPrg *)