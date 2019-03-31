module Main

import Data.Primitives.Views


everyOther : Stream ty -> Stream ty
everyOther (e1 :: e2 :: es) = e2 :: everyOther es

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

Functor InfList where
  map f (x :: xs) = f x :: map f xs

total
countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

total
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

data Face : Type where
  Heads : Face
  Tails : Face

Show Face where
  show Heads = "Heads"
  show Tails = "Tails"

getFace : Int -> Face
getFace n with (divides n 2)
  getFace ((2 * div) + rem) | (DivBy prf) =
    case rem of
      0 => Heads
      _ => Tails

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = map getFace (take count xs)

squareRootApprox : (number : Double) -> (approx : Double) -> Stream Double
squareRootApprox number approx = approx :: squareRootApprox number next
  where
    next = (approx + (number / approx)) / 2

squareRootBound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
squareRootBound Z number bound (val :: xs) = val
squareRootBound (S k) number bound (val :: xs) =
  case (val * val - number <= bound) of
    True  => val
    False => squareRootBound k number bound xs

squareRoot : (numner : Double) -> Double
squareRoot number = squareRootBound 100 number 0.00000000001 (squareRootApprox number number)

main : IO ()
main = do
  printLn $ take 10 (everyOther [1..]) -- [2,4,6,8,10,12,14,16,18,20]
  printLn $ getPrefix 10 (map (*2) (countFrom 1)) -- [2,4 .. 20]
  printLn $ coinFlips 6 (randoms 12345) -- [T,H,T,T,H,T]
  printLn $ take 3 (squareRootApprox 10 10)
  printLn $ take 3 (squareRootApprox 100 25)
  printLn $ squareRoot 6
  printLn $ squareRoot 2500
  printLn $ squareRoot 2501
