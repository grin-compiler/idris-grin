module Main

import Control.Monad.State

increase : Nat -> State Nat ()
increase inc = do
  current <- get
  put (current + inc)

main : IO ()
main = do
  printLn $ runState (increase 5) 89
  printLn $ evalState (increase 5) 89
  printLn $ execState (increase 5) 89
