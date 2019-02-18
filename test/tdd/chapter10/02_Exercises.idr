{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor
   Ctrl-Alt-W: Add With View -}
module Main

import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

{-
O(n) in time: because we travel the lists twice
O(n) in space: because we build a helper SnocList structures.
-}
total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] input2 | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) =
      case x == y of
        True => (equalSuffix xs ys | xsrec | ysrec) ++ [x]
        False => []

total
mergeSort : (Ord a) => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | (SplitRecPair lrec rrec) =
    merge (mergeSort ys | lrec) (mergeSort zs | rrec)

total
toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (x + x) | (HalfRecEven rec) = (toBinary x | rec) ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd rec) = (toBinary x | rec) ++ "1"

total
palindrome : List Char -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) =
    case x == y of
      False => False
      True => palindrome ys | rec

main : IO ()
main = do
  printLn $ equalSuffix [] [1] -- []
  printLn $ equalSuffix [1,2,4,5] [1..5] -- [4,5]
  printLn $ equalSuffix [1,2,4,5,6] [1..5] -- []
  printLn $ equalSuffix [1,2,4,5,6] [1..6] -- [4,5,6]
  printLn $ mergeSort [1,2,3,8,4,3,6,7,9,7]
  putStrLn $ toBinary 42 -- "101010"
  putStrLn $ toBinary 94 -- "1011110"
  printLn $ palindrome (unpack "abccba") -- True
  printLn $ palindrome (unpack "abcba") -- True
  printLn $ palindrome (unpack "abcb") -- False
