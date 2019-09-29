module Main

import Data.Primitives.Views
import System

%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S n) = More (tank n)

run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel."
run (More fuel) (Do action cont) = do
  res <- action
  run fuel (cont res)

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  line <- getLine
  putStrLn (action line)
  totalREPL prompt action

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = run forever (totalREPL "\n: " toUpper)
