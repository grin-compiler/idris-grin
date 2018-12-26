module Main

import Data.Vect
import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do
  Just num1_ok <- readNumber | Nothing => pure Nothing
  Just num2_ok <- readNumber | Nothing => pure Nothing
  pure (Just (num1_ok, num2_ok))

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Left off!"
countdown (S secs) = do
  putStrLn (show (S secs))
  usleep 1000000
  countdown secs

countdowns : IO ()
countdowns = do
  putStr "Enter a starting number: "
  Just startNum <- readNumber
     | Nothing => do putStrLn "Invalid input"
                     countdowns
  countdown startNum
  putStr "Another (y/n)? "
  yn <- getLine
  if yn == "y"
    then countdowns
    else pure ()

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)

anyVect : (n : Nat ** Vect n String)
anyVect = (3 **  ["a", "b", "c"])

readVect : IO (n : Nat ** Vect n String)
readVect = do
  x <- getLine
  if x == ""
    then pure (0 ** [])
    else do
      (k ** xs) <- readVect
      pure (S k ** x :: xs)

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter the first vector (blank line to end): "
  (len1 ** vect1) <- readVect
  putStrLn "Enter the second vector (blank line to end): "
  (len2 ** vect2) <- readVect
  case exactLength len1 vect2 of
    Nothing => putStrLn "Vectors are different lengths"
    Just vect2' => printLn (zip vect1 vect2')

main : IO ()
main = do
  countdowns
  res3 <- readVectLen 3
  print res3
  print anyVect
  zipInputs

