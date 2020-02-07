module Main

import Text.PrettyPrint.WL

%default total

myDoc : Doc
myDoc = fold (|//|) $ map text $ words "this is a long sentence."

myString : String
myString = toString 0 15 $ myDoc

main : IO ()
main = putStrLn myString
