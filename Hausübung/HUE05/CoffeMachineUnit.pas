(* CoffeMachineUnit:                                        MFL2, 2022-11-29 *)
(* ------                                                                    *)
(* memory for coffemachine                                                   *)
(* ========================================================================= *)
UNIT CoffeMachineUnit;

INTERFACE
  TYPE
    Coins = RECORD
      coin10: INTEGER;
      coin50: INTEGER;
     coin100: INTEGER;
  END;

  PROCEDURE CoffeeButtonPressed(input: Coins; VAR change: Coins);
  FUNCTION GetMemoryCoins: Coins;

IMPLEMENTATION
  CONST
    coffePrice = 40;
  VAR
    coinsMemory: Coins;
    errorMemory: INTEGER;

  PROCEDURE InitMachineMemory;
  BEGIN (* InitMachineMemory *)
    coinsMemory.coin10 := 10;
    coinsMemory.coin50 := 5;
    coinsMemory.coin100 := 0;
    errorMemory := 0;
  END; (* InitMachineMemory *)

  FUNCTION CoinsInCent(input: Coins): INTEGER;
  BEGIN (* CoinsInCent *)
    CoinsInCent := (input.coin10 * 10) + (input.coin50 * 50) + (input.coin100 * 100);
  END; (* CoinsInCent *)

  PROCEDURE AddInputToMemory(change: Coins);
  BEGIN (* AddInputToMemory *)
    coinsMemory.coin10 := coinsMemory.coin10 + change.coin10;
    coinsMemory.coin50 := coinsMemory.coin50 + change.coin50;
    coinsMemory.coin100 := coinsMemory.coin100 + change.coin100;
  END; (* AddInputToMemory *)

  PROCEDURE SubtractChangeFromMemory(change: Coins);
  BEGIN (* SubtractChangeFromMemory *)
    coinsMemory.coin10 := coinsMemory.coin10 - change.coin10;
    coinsMemory.coin50 := coinsMemory.coin50 - change.coin50;
    coinsMemory.coin100 := coinsMemory.coin100 - change.coin100;
  END; (* SubtractChangeFromMemory *)
  
  FUNCTION CalculateChange(inputInCent: INTEGER): Coins;
    VAR
      change: Coins;
  BEGIN (* CalculateChange *)
    change.coin100 := inputInCent DIV 100;
    IF(change.coin100 > coinsMemory.coin100) THEN BEGIN
      change.coin100 := coinsMemory.coin100;
    END; (* IF *)
    inputInCent := inputInCent - (change.coin100 * 100);
    change.coin50 := inputInCent DIV 50;
    IF (change.coin50 > coinsMemory.coin50) THEN BEGIN (* 50Cent = 0 => use five 10Cent coins instead of 50Cent coins *)
      change.coin50 := coinsMemory.coin50;
    END; (* IF *)
    inputInCent := inputInCent - (change.coin50 * 50);
    change.coin10 := inputInCent DIV 10;
    CalculateChange := change;
  END; (* CalculateChange *)

  PROCEDURE CoffeeButtonPressed(input: Coins; VAR change: Coins);
    VAR
      inputInCent: INTEGER;
  BEGIN (* CoffeeButtonPressed *)
    inputInCent := CoinsInCent(input);
    IF (errorMemory = 3) THEN BEGIN
      WriteLn('ERROR: Out of order!');
      Halt;
    END; (* IF *)
    IF (inputInCent < coffePrice) THEN BEGIN (* to less money *)
      WriteLn('ERROR: Not enough money to buy a coffe! coffeeprice = 40 Cent');
      change := input;
      Exit;
    END; (* IF *)
    AddInputToMemory(input); 
    change := CalculateChange((inputInCent - coffePrice));
    IF (change.coin10 <= coinsMemory.coin10) THEN BEGIN (* enough money to change *)
      errorMemory := 0;
      WriteLn('Coffee in progress ');
      SubtractChangeFromMemory(change);
    END ELSE BEGIN (* not enough money to change *)
      change := input;
      SubtractChangeFromMemory(change);
      errorMemory := errorMemory + 1;
      WriteLn('ERROR: Not enough money to change!');
    END; (* IF *)
  END; (* CoffeeButtonPressed *)

  FUNCTION GetMemoryCoins: Coins; (* function for testing *)
  BEGIN (* GetCoinsMemoryInString *)
    GetMemoryCoins := coinsMemory;
  END; (* GetCoinsMemoryInString *)

BEGIN (* CoffeMachineUnit *)
  InitMachineMemory();
END. (* CoffeMachineUnit *)
