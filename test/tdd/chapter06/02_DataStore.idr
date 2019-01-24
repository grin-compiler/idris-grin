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

infixr 5 .+.

data Schema
  = SString
  | SInt
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData sch s i) n = MkData sch _ (addToData i)
  where
    addToData : Vect old (SchemaType sch) -> Vect (S old) (SchemaType sch)
    addToData [] = [n]
    addToData (i :: is) = i :: addToData is

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parsePrefix : (sch : Schema) -> String -> Maybe (SchemaType sch, String)
parsePrefix SString x = getQuoted (unpack x)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
      (qouted, '"' :: rest) => Just (pack qouted, ltrim (pack rest))
      _                     => Nothing
    getQuoted _ = Nothing
parsePrefix SInt x = case span isDigit x of
  ("", rest) => Nothing
  (num, rest) => Just (cast num, ltrim rest)
parsePrefix (y .+. z) x = case parsePrefix y x of
  Nothing => Nothing
  Just (lval, x') => case parsePrefix z x' of
    Nothing => Nothing
    Just (rval, x'') => Just ((lval, rval), x'')

parseBySchema : (sch : Schema) -> (str : String) -> Maybe (SchemaType sch)
parseBySchema sch inp = case parsePrefix sch inp of
  Just (res, "") => Just res
  Just _ => Nothing
  Nothing => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
  [] => Just SString
  _ => case parseSchema xs of
    Nothing  => Nothing
    Just xsh => Just (SString .+. xsh)
parseSchema ("Int" :: xs) = case xs of
  [] => Just SInt
  _ => case parseSchema xs of
    Nothing  => Nothing
    Just xsh => Just (SInt .+. xsh)
parseSchema _ = Nothing


parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand sch "add" str    = case parseBySchema sch str of
                                  Nothing => Nothing
                                  Just ok => Just (Add ok)
parseCommand sch "get" val    = case all isDigit (unpack val) of
                                  False => Nothing
                                  True  => Just (Get (cast val))
parseCommand sch "quit" ""    = Just Quit
parseCommand sch "schema" rest = case parseSchema (words rest) of
  Nothing => Nothing
  Just sok => Just (SetSchema sok)
parseCommand _   _      _     = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema s sc = case size s of
  Z => Just (MkData sc _ [])
  S _ => Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse sch input = case span (/= ' ') input of
  (cmd, args) => parseCommand sch cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = cast item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store
  in case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    Just ix => Just (display (index ix store_items) ++ "\n", store)

getSize : (store : DataStore) -> Maybe (String, DataStore)
getSize store = Just ("Size of store: " ++ cast (size store), store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
  Nothing           => Just ("Invalid command\n", store)
  Just (SetSchema sc) => case setSchema store sc of
                          Nothing => Just ("Can't update schema\n", store)
                          Just st => Just ("OK\n", st)
  Just (Add item)   => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos)    => getEntry pos store
  Just Quit         => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
