(* CoffeeMachineWithUnitPrg:                                 MFL, 2022-11-24 *)
(* ------                                                                    *)
(* coffe machine with unit as memory                                         *)
(* ========================================================================= *)
PROGRAM CoffeeMachineWithUnitPrg;
  USES
    CoffeMachineUnit;

  PROCEDURE DisplayChangeAndMemory(input: Coins);
    VAR
      change, memory: Coins;
  BEGIN (* DisplayChangeAndMemory *)
    CoffeeButtonPressed(input,change);
    memory := GetMemoryCoins();
    WriteLn('Input: coin10 = ', input.coin10, '  coin50 = ', input.coin50, '  coin100 = ' , input.coin100);
    WriteLn('Change: coin10 = ', change.coin10, '  coin50 = ', change.coin50, '  coin100 = ' , change.coin100);
    WriteLn('Memory: coin10 = ', memory.coin10, '  coin50 = ', memory.coin50, '  coin100 = ' , memory.coin100);
    WriteLn('-----------------------------------------------------------------------------------------');
  END; (* DisplayChangeAndMemory *)
  
  VAR
    input, memory: Coins;
BEGIN (* CoffeeMachineWithUnitPrg *)
  memory := GetMemoryCoins();
  WriteLn('Startmemory: coin10 = ', memory.coin10, '  coin50 = ', memory.coin50, '  coin100 = ' , memory.coin100);
  WriteLn('-----------------------------------------------------------------------------------------');  
  input.coin10 := 0;
  input.coin50 := 3;
  input.coin100 := 0;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 3;
  input.coin100 := 0;
  DisplayChangeAndMemory(input);
  input.coin10 := 2;
  input.coin50 := 0;
  input.coin100 := 0;
  DisplayChangeAndMemory(input);
  input.coin10 := 4;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 2;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
  DisplayChangeAndMemory(input);
  input.coin10 := 0;
  input.coin50 := 0;
  input.coin100 := 1;
  DisplayChangeAndMemory(input);
END. (* CoffeeMachinePrg *)