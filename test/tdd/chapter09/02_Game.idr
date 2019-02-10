{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor -}
module Main

import Data.Vect


data WorldState : (guesses_remaning : Nat) -> (letters : Nat) -> Type where
  MkWorldState : (word : String) ->
                 (missing : Vect letters Char) ->
                 WorldState guesses_remaning letters

data Finished : Type where
  Lost : (game : WorldState 0 (S letters)) -> Finished
  Won  : (game : WorldState (S letters) 0) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidMore : ValidInput (x :: (y :: ys)) -> Void
isValidMore (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No isValidNil
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: ys)) = No isValidMore

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do
  putStr "Guess:"
  x <- getLine
  case isValidString (toUpper x) of
    Yes prf => pure (_ ** prf)
    No contra => do putStrLn "Invalid guess"
                    readGuess

processGuess : (letter : Char) ->
               WorldState (S guesses) (S letters) ->
               Either (WorldState guesses (S letters))
                      (WorldState (S guesses) letters)
processGuess letter (MkWorldState word missing) = case isElem letter missing of
  (Yes elem) => Right (MkWorldState word (dropElem missing elem))
  (No contra) => Left (MkWorldState word missing)

game : WorldState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
  (_ ** Letter letter) <- readGuess
  case processGuess letter st of
    Left badGuess => do putStrLn "Wrong!"
                        case guesses of
                          Z => pure (Lost badGuess)
                          S _ => game badGuess
    Right goodGuess => do putStrLn "Right!"
                          case letters of
                            Z => pure (Won goodGuess)
                            S _ => game goodGuess

main : IO ()
main = do
  result <- game {guesses = 2} (MkWorldState "Test" ['T', 'E', 'S'])
  case result of
    Lost (MkWorldState word missing) =>
      putStrLn ("You lose. The word was " ++ word)
    Won (MkWorldState word missing) =>
      putStrLn "You win!"
