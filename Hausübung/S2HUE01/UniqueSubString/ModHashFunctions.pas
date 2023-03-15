(* Hahsfunctions:                                            MFL, 2023-03-14 *)
(* ------                                                                    *)
(* Create a hash for a string                                                *)
(* ========================================================================= *)
UNIT ModHashFunctions;

INTERFACE
  FUNCTION Hash1(str: String): WORD;
  FUNCTION Hash2(str: String): WORD;
IMPLEMENTATION
  
  FUNCTION Hash1(str: STRING): WORD;
  (* bad hash function *)
  BEGIN (* Hash1 *)
    Hash1 := 17 * Ord(str[1]) + 29 * Ord(str[2]);
  END; (* Hash1 *)

  FUNCTION Hash2(str: STRING): WORD;
  VAR
    h: WORD;
    i: INTEGER;
  BEGIN (* Hash2 *)
    h := 0;
    FOR i := 1 TO Length(str) DO BEGIN
      h := (h * 17 + Ord(str[i])) MOD 65536;
    END; (* FOR *)
    Hash2 := h;
  END; (* Hash2 *)

BEGIN (* ModHashFunctions *)
END. (* ModHashFunctions *)