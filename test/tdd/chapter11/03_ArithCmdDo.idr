module Main

import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine    = getLine
runCommand (Pure x)   = pure x
runCommand (Bind m k) = do
  res <- runCommand m
  runCommand (k res)

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do
  PutStr prompt
  answer <- GetLine
  if toLower answer == "quit"
    then Pure QuitCmd
    else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do
    PutStr "Correct!\n"
    quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans score = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score = do
    PutStr ("Score so far: " ++ show score ++ "\n")
    input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
    case input of
        Answer answer => if answer == num1 * num2
                            then correct nums score
                            else wrong nums (num1 * num2) score
        QuitCmd => Quit score

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S n) = More (tank n)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run _ (Quit a) = pure $ Just a
run (More fuel) (Do c f) = do
  res <- runCommand c
  run fuel (f res)

partial
main : IO ()
main = do
  seed <- time
  Just score <- run (tank 20) (quiz (arithInputs (fromInteger seed)) 0)
    | Nothing => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show score)
