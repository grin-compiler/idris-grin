module Main

import Data.Vect

allLengths : List String -> List Nat
allLengths [] = []
allLengths (w :: ws) = length w :: allLengths ws

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven0 : Nat -> Bool
isEven0 Z = True
isEven0 (S k) = not (isEven0 k)

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [4,5,6,7,8,9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

--total
allLengths1 : Vect l String -> Vect l Nat
allLengths1 [] = []
allLengths1 (w :: ws) = length w :: allLengths1 ws -- length w :: allLengths1 ws

insert  : Ord elem
       => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
  False => y :: insert x xs
  True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) =
  let xsSorted = insSort xs
  in (insert x xsSorted)

addVector : Num num => Vect n num -> Vect n num -> Vect n num
addVector [] [] = []
addVector (x :: xs) (y :: ys) = x + y :: addVector xs ys

addMatrix
  : Num n
  => Vect rows (Vect cols n)
  -> Vect rows (Vect cols n)
  -> Vect rows (Vect cols n)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addVector x y :: addMatrix xs ys

createEmpties : Num num => Vect m (Vect 0 num)
createEmpties {m} = replicate m []

transposeHelper
  : Num num
  => (x : Vect n num)
  -> (xsTrans : Vect n (Vect len num))
  -> Vect n (Vect (S len) num)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat
  : Num num
  => Vect m (Vect n num)
  -> Vect n (Vect m num)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let xsTrans = transposeMat xs
  in (transposeHelper x xsTrans)

{-
mulMatrix
  : Num num
  => Vect n (Vect m num)
  -> Vect m (Vect p num)
  -> Vect n (Vect p num)
-}

main : IO ()
main = do
  putStrLn $ show $ allLengths $ words "Hello World! This is nice. I am being an Idris dev."
  putStrLn $ show $ isEven0 100000
  putStrLn $ show $ isEven 100000
  putStrLn $ show $ tenInts
  putStrLn $ show $ insSort [1,8,9,7,5,6,3,2]
  putStrLn $ show $ addMatrix [[1,1], [2,2]] [[2,2], [1,1]]
  putStrLn $ show $ transposeMat [[1,2,3], [4,5,6]]
