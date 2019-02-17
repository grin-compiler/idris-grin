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


data SnocList0 ty = Empty0 | Snoc0 (SnocList0 ty) ty

reverseSnoc : SnocList0 a -> List a
reverseSnoc Empty0 = []
reverseSnoc (Snoc0 xs y) = y :: reverseSnoc xs

data SnocList1 : List a -> Type where
  Empty1 : SnocList1 []
  Snoc1 : (rec : SnocList1 xs) -> SnocList1 (xs ++ [x])

snocListHelper : (snoc : SnocList1 input) -> (rest : List a) -> SnocList1 (input ++ rest)
snocListHelper {input} snoc [] =
  rewrite appendNilRightNeutral input
  in snoc
snocListHelper {input} snoc (x :: xs) =
  rewrite appendAssociative input [x] xs
  in snocListHelper (Snoc1 snoc {x}) xs

snocList1 : (xs : List a) -> SnocList1 xs
snocList1 = snocListHelper Empty1

myReverseHelper : (input : List a) -> SnocList1 input -> List a
myReverseHelper [] Empty1 = []
myReverseHelper (xs ++ [x]) (Snoc1 rec) = x :: myReverseHelper xs rec

total
myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList1 input)

total
myReverse2 : List a -> List a
myReverse2 xs with (snocList1 xs)
  myReverse2 [] | Empty1 = []
  myReverse2 (ys ++ [x]) | (Snoc1 rec) = x :: myReverse2 ys | rec

total
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList1 input1)
  isSuffix [] ys | Empty1 = True
  isSuffix (xs ++ [x]) input2 | (Snoc1 xsrec) with (snocList1 input2)
    isSuffix (xs ++ [x]) [] | (Snoc1 xsrec) | Empty1 = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc1 xsrec) | (Snoc1 ysrec) =
      case x == y of
        False => False
        True => isSuffix xs ys | xsrec | ysrec

total
mergeSort : Ord a => List a -> List a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) =
    (mergeSort lefts | lrec) `merge` (mergeSort rights | rrec)

main : IO ()
main = do
  printLn $ reverseSnoc (Snoc0 Empty0 1)
  printLn $ myReverse [1]
  printLn $ myReverse [1..10]
  printLn $ myReverse2 [1..10]
  printLn $ isSuffix [7,8,9,10] [1..10]
  printLn $ isSuffix [7,8,9] [1..10]
  printLn $ mergeSort [3,2,1]
  printLn $ mergeSort [5,1,4,3,2,6,8,7,9]
