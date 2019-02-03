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

reverseProof : Vect (k + 1) e -> Vect (S k) e
reverseProof {k} result = rewrite plusCommutative 1 k in result

myReverse : Vect n e -> Vect n e
myReverse [] = []
myReverse {n = S k} (x :: xs) = reverseProof (myReverse xs ++ [x])

appNilProof : Vect m e -> Vect (plus m 0) e
appNilProof {m} result = rewrite plusZeroRightNeutral m in result

appConsProof : Vect (S (m + k)) e -> Vect (plus m (S k)) e
appConsProof {m} {k} result =
  rewrite (sym (plusSuccRightSucc m k))
  in result

app : Vect n e -> Vect m e -> Vect (m + n) e
app [] ys = appNilProof ys
app (x :: xs) ys = appConsProof (x :: app xs ys)

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m     = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k

reverseProofNil : Vect n a -> Vect (plus n 0) a
reverseProofNil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProofCons : Vect ((S n) + l) a -> Vect (plus n (S l)) a
reverseProofCons {n} {l} res =
  rewrite sym (plusSuccRightSucc n l) in
  res

myReverse2 : Vect n a -> Vect n a
myReverse2 xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = reverseProofNil acc
    reverse' acc (x :: xs) = reverseProofCons (reverse' (x :: acc) xs)

data Dec' : (prop : Type) -> Type where
  Yes' : (prf : prop)            -> Dec' prop
  No'  : (contra : prop -> Void) -> Dec' prop

zeroNotSucc : (0 = S k) -> Void
zeroNotSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSucc
checkEqNat (S k) Z = No succNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
  Yes prf => Yes (cong prf)
  No contra => No (noRec contra)

isYes : Dec (a = b) -> Bool
isYes (Yes prf) = True
isYes (No contra) = False

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case decEq m len of
  Yes Refl => Just input
  No contra => Nothing

headEqual : DecEq a
  => {xs : Vect n a} -> {ys : Vect n a}
  -> (contra : (x = y) -> Void)
  -> ((x :: xs = y :: ys) -> Void)
headEqual contra Refl = contra Refl

tailEqual : DecEq a
  => {xs : Vect n a} -> {ys : Vect n a}
  -> (contra : (xs = ys) -> Void)
  -> ((x :: xs = y :: ys) -> Void)
tailEqual {xs = ys} {ys = ys} contra Refl = contra Refl

decEqVect : (DecEq a) => (xs : Vect n a) -> (ys : Vect n a) -> Dec (xs = ys)
decEqVect [] [] = Yes Refl
decEqVect (x :: xs) (y :: ys) = case decEq x y of
  (No contra) => No $ headEqual contra
  (Yes Refl) => case decEqVect xs ys of
    (Yes Refl) => Yes Refl
    (No contra) => No $ tailEqual contra

main : IO ()
main = do
  printLn $ myReverse [1,2,3,4]
  printLn $ app [1,2,3] [4,5,6,7]
  printLn $ myReverse2 [1,2,3,4,5]
  printLn $ isYes $ checkEqNat 100 99
  printLn $ Main.exactLength 2 [1,2,3]
  printLn $ isYes $ decEqVect [1,2,3] [1,2,3]
