{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor
   Ctrl-Alt-W: Add With View -}
module Main

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

total
loopPrint0 : String -> InfIO
loopPrint0 msg = Do (putStrLn msg) (\_ => loopPrint0 msg)

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S n) = More (tank n)

forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

total
loopPrint1 : String -> InfIO
loopPrint1 msg = do
  putStrLn msg
  loopPrint1 msg

main : IO ()
main = do
  run (tank 5) (loopPrint0 "vroom")
  run (tank 5) (loopPrint1 "broom")
