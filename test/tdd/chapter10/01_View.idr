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

-- The View
data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

-- Covering function for the View
total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
  Empty => NonEmpty [] x
  NonEmpty ys y => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEndWith : List Int -> String
describeListEndWith input with (listLast input)
  describeListEndWith [] | Empty = "Empty"
  describeListEndWith (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs

data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input
  where
    splitListHelp : List a -> (input : List a) -> SplitList input
    splitListHelp _ [] = SplitNil
    splitListHelp _ [x] = SplitOne
    splitListHelp (_ :: _ :: counter) (item :: items) =
      case splitListHelp counter items of
        SplitNil => SplitOne
        SplitOne {x} => SplitPair [item] [x]
        SplitPair lfts rgts => SplitPair (item :: lfts) rgts
    splitListHelp _ items = SplitPair [] items

mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts) (mergeSort rights)

main : IO ()
main = do
  putStrLn $ describeListEnd []
  putStrLn $ describeListEnd [1]
  putStrLn $ describeListEnd [1,2,3]
  putStrLn $ describeListEndWith []
  putStrLn $ show $ myReverse [1,2,3,4,5]
  putStrLn $ show $ merge [1,3,5] [2,4,6]
  printLn $ mergeSort [3,2,1]
  printLn $ mergeSort [5,1,4,3,2,6,8,7,9]
