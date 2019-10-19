module Main

import Data.Primitives.Views
import System

%default total

record Score where
  constructor MkScore
  correct   : Nat
  attempted : Nat

record GameState where
  constructor MkGameState
  score       : Score
  difficulty  : Int

initState : GameState
initState = MkGameState (MkScore 0 0) 12

Show GameState where
  show st = show (correct (score st)) ++ "/" ++
            show (attempted (score st)) ++ "\n" ++
            "Difficulty: " ++ show (difficulty st)

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

Functor Command where
  map f x = Bind x (\val => Pure (f val))

Applicative Command where
  pure = Pure
  f <*> x = Bind f $ \f' =>
            Bind x $ \x' =>
            Pure (f' x')

Monad Command where
  (>>=) = Bind

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do   : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

setDifficulty1 : Int -> GameState -> GameState
setDifficulty1 newDiff (MkGameState score _) = MkGameState score newDiff

setDifficulty2 : Int -> GameState -> GameState
setDifficulty2 newDiff state = record { difficulty = newDiff } state

setDifficulty3 : Int -> GameState -> GameState
setDifficulty3 newDiff = record { difficulty = newDiff }

addWrong0 : GameState -> GameState
addWrong0 state = record
  { score -> attempted = attempted (score state) + 1
  } state

addCorrect0 : GameState -> GameState
addCorrect0 state = record
  { score -> correct = correct (score state) + 1
  , score -> attempted = attempted (score state) + 1
  } state

addWrong : GameState -> GameState
addWrong = record { score -> attempted $= (+1) }

addCorrect : GameState -> GameState
addCorrect = record
  { score -> attempted $= (+1)
  , score -> correct   $= (+1)
  }

data Input = Answer Int
           | QuitCmd

mutual
  correct : ConsoleIO GameState
  correct = do
    PutStr "Correct!\n"
    st <- GetGameState
    PutGameState (addCorrect st)
    quiz

  wrong : Int -> ConsoleIO GameState
  wrong ans = do
    PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
    st <- GetGameState
    PutGameState (addWrong st)
    quiz

  readInput : (prompt : String) -> Command Input
  readInput prompt = do
    PutStr prompt
    answer <- GetLine
    if toLower answer == "quit"
      then Pure QuitCmd
      else Pure (Answer (cast answer))

  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st <- GetGameState
            PutStr (show st ++ "\n")
            input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
            case input of
              Answer answer => if answer == num1 * num2
                                then correct
                                else wrong (num1 * num2)
              QuitCmd => Quit st

runCommand : Stream Int
          -> GameState
          -> Command a
          -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do
  putStr x
  pure ((), rnds, state)

runCommand rnds state GetLine = do
  str <- getLine
  pure (str, rnds, state)

runCommand (val :: rnds) state GetRandom = pure (getRandom val (difficulty state), rnds, state)
  where
    getRandom : Int -> Int -> Int
    getRandom val max with (divides val max)
      getRandom val 0 | DivByZero = 1
      getRandom ((max * div) + rem) max | (DivBy prf) = abs rem + 1

runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state (Pure val) = pure (val, rnds, state)
runCommand rnds state (Bind c f) = do
  (res, newRnds, newState) <- runCommand rnds state c
  runCommand newRnds newState (f res)

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S n) = More (tank n)

run : Fuel -> Stream Int -> GameState -> ConsoleIO a -> IO (Maybe a, Stream Int, GameState)
run Dry         rnds state p          = pure (Nothing, rnds, state)
run fuel        rnds state (Quit val) = pure (Just val, rnds, state)
run (More fuel) rnds state (Do c f)   = do
  (res, newRnds, newState) <- runCommand rnds state c
  run fuel newRnds newState (f res)

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

test : IO ()
test = do
  printLn initState
  printLn $ setDifficulty1 10 initState
  printLn $ setDifficulty2 11 initState
  printLn $ setDifficulty3 9  initState
  printLn $ addWrong0 initState
  printLn $ addCorrect0 initState
  printLn $ addWrong initState
  printLn $ addCorrect initState

partial
main : IO ()
main = do
  test
  -- seed <- time
  let seed = 0
  (Just score, _, state) <-
    run forever (randoms (fromInteger seed)) initState quiz
      | _ => putStrLn "Ran out of fuel"
  putStrLn ("Final score: " ++ show state)
