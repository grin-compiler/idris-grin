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

data Elem' : a -> Vect k a -> Type where
  Here' : Elem' x (x :: xs)
  There' : (later : Elem' x xs) -> Elem' x (y :: xs)

removeElemPrf :
  (value : a) ->
  (xs : Vect (S n) a) ->
  (prf : Elem value xs) ->
  Vect n a
removeElemPrf value (value :: ys) Here = ys
removeElemPrf {n = Z} value (y :: []) (There later) = absurd later
removeElemPrf {n = (S k)} value (y :: ys) (There later) = y :: removeElemPrf value ys later

removeElem :
  (value : a) ->
  (xs : Vect (S n) a) ->
  {auto prf : Elem value xs} ->
  Vect n a
removeElem x xs {prf} = removeElemPrf x xs prf

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

isYes : Dec a -> Bool
isYes (Yes prf) = True
isYes (No contra) = False

isElem' : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem' value [] = No notInNil
isElem' value (x :: xs) = case decEq value x of
  Yes Refl   => Yes Here
  No notHere => case isElem' value xs of
    Yes prf => Yes (There prf)
    No notThere => No (notInTail notHere notThere)

data LElem : a -> List a -> Type where
  LHere : LElem a (a :: as)
  LThere : LElem a as -> LElem a (b :: as)

data Last : List a -> a -> Type where
  LOne : Last [value] value
  LCons : (prf : Last xs value) -> Last (x :: xs) value

noLast1 : (notLast : (x = value) -> Void) -> Last [x] value -> Void
noLast1 notLast LOne = notLast Refl
noLast1 _ (LCons LOne) impossible
noLast1 _ (LCons (LCons _)) impossible

noLast2 : (noLast : Last (y :: ys) value -> Void) -> Last (x :: (y :: ys)) value -> Void
noLast2 noLast (LCons prf) = noLast prf

noLast0 : Last [] value -> Void
noLast0 LOne impossible
noLast0 (LCons _) impossible

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No noLast0
isLast (x :: []) value = case decEq x value of
  Yes Refl => Yes LOne
  No notLast => No (noLast1 notLast)
isLast (x :: y :: ys) value = case isLast (y :: ys) value of
  Yes prf => Yes (LCons prf)
  No noLast => No (noLast2 noLast)

main : IO ()
main = do
  printLn $ removeElem 3 [1,2,3]
  printLn $ isYes $ isElem' 3 [1,2,3]
  printLn $ isYes $ isElem' 10 [1,2,3,4,5,6,7,8,9]
--  printLn $ isYes $ isLast 10 [1,2,3,4,5,6,7,8,9] -- Idris bug: Infinite loop
