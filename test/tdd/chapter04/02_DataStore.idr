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

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData s i) = s

items : (store : DataStore) -> Vect (size store) String
items (MkData s i) = i

addToStore : DataStore -> String -> DataStore
addToStore (MkData s i) n = MkData _ (addToData i)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [n]
    addToData (i :: is) = i :: addToData is

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str    = Just (Add str)
parseCommand "get" val    = case all isDigit (unpack val) of
                              False => Nothing
                              True  => Just (Get (cast val))
parseCommand "quit" ""    = Just Quit
parseCommand "size" ""    = Just Size
parseCommand "search" str = Just $ Search str
parseCommand _      _     = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
  (cmd, args) => parseCommand cmd args

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store
  in case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    Just ix => Just (index ix store_items ++ "\n", store)

getSize : (store : DataStore) -> Maybe (String, DataStore)
getSize store = Just ("Size of store: " ++ cast (size store), store)

search : (store : DataStore) -> (str : String) -> Maybe (String, DataStore)
search store str =
  let store_items  = items store in
  let found = foldrImpl
                (\(ix, msg), acc => if (str `isInfixOf` msg)
                  then (cast $ finToInteger ix) ++ ": " ++ msg ++ "\n" ++ acc
                  else acc)
                ""
                (\acc => acc)
                (zip range store_items)
  in Just (found, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
  Nothing           => Just ("Invalid command\n", store)
  Just (Add item)   => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos)    => getEntry pos store
  Just Size         => getSize store
  Just (Search str) => search store str
  Just Quit         => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
