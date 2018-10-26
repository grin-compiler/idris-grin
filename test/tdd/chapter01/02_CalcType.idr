module Main

StringOrInt : Bool -> Type
StringOrInt x = case x of
  True  => Int
  False => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
  True  => 94
  False => "Ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
  True  => cast val
  False => val

main : IO ()
main = do
  putStrLn (valToString True (getStringOrInt True))
  putStrLn (valToString False (getStringOrInt False))
