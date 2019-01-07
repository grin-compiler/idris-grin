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

-- 6.1.1

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

tri2 : Polygon 3
tri2 = [(0, 0), (0, 0), (0, 0)]

-- 6.1.2

StringOrInt : Bool -> Type
StringOrInt True = Int
StringOrInt False = String

valToString
  :   (isInt : Bool)
  ->  (case isInt of
        True => Int
        False => String)
  -> String
valToString False x = trim x
valToString True x = cast x

-- 6.2.1

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z     t = t
AdderType (S k) t = (next : t) -> AdderType k t

adder : Num numType => (numargs : Nat) -> (acc : numType) -> AdderType numargs numType
adder Z     acc = acc
adder (S k) acc = \next => adder k (next + acc)

adder1 : (numargs : Nat) -> (acc : Int) -> AdderType numargs Int
adder1 = adder

-- 6.2.2

data Format : Type where
  Number : Format -> Format
  Str    : Format -> Format
  Lit    : String -> Format -> Format
  End    : Format

PrintfType : Format -> Type
PrintfType (Number x) = Int -> PrintfType x
PrintfType (Str x) = String -> PrintfType x
PrintfType (Lit x y) = PrintfType y
PrintfType End = String

toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
  Lit l f => Lit (strCons c l) f
  fmt => Lit (strCons c "") fmt

printFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printFmt (Number x) acc = \num => printFmt x (acc ++ show num)
printFmt (Str x) acc = \str => printFmt x (acc ++ str)
printFmt (Lit x y) acc = printFmt y (acc ++ x)
printFmt End acc = acc

printf : (s : String) -> (PrintfType (toFormat (unpack s)))
printf fmt = printFmt (toFormat (unpack fmt)) ""

main : IO ()
main = do
  printLn tri
  printLn tri2
  printLn (valToString False "  string  ")
  printLn (valToString True 42)
  printLn (adder1 0 10)
  printLn (adder1 1 10 15)
  printLn (adder1 2 1 4 7)
  putStrLn (printf "Hello!")
  putStrLn (printf "Answer : %d" 42)
  putStrLn (printf "%s numner %d" "Page" 97)
