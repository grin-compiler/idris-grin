module Main

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

total
countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

total
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith xs [] = []
labelWith (x :: xs) (y :: ys) = (x, y) :: labelWith xs ys

label : List a -> List (Int, a)
label = labelWith (iterate (+1) 0)

main : IO ()
main = do
  printLn $ getPrefix 10 $ countFrom 2 -- [2,3,4,5,6,7,8,9,10,11]
  printLn $ label [1..7] -- [(0,1) .. (6, 7)]
  printLn $ labelWith (cycle ["a", "b", "c"]) [1..5] -- [("a",1), ("b".2), ("c",3), ("a",4), ("b",5)]
