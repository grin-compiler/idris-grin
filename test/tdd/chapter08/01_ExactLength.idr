{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor -}
module Main

data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  (::) : a -> Vect n a -> Vect (S n) a

data EqNat : (n1 : Nat) -> (n2 : Nat) -> Type where
  Same : (n : Nat) -> EqNat n n

Show (EqNat n m) where
  show (Same n) = "Same " ++ show n

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (n1 : Nat) -> (n2 : Nat) -> Maybe (EqNat n1 n2)
checkEqNat Z Z = Just $ Same 0
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
  Nothing => Nothing
  Just eq => Just (sameS _ _ eq)

exactLength : (len : Nat) -> (input : Vect n a) -> Maybe (Vect len a)
exactLength {n} len input = case checkEqNat n len of
  Nothing => Nothing
  Just (Same len0) => Just input

checkEqNat2 : (n1 : Nat) -> (n2 : Nat) -> Maybe (n1 = n2)
checkEqNat2 Z Z = Just Refl
checkEqNat2 Z (S k) = Nothing
checkEqNat2 (S k) Z = Nothing
checkEqNat2 (S k) (S j) = map cong (checkEqNat2 k j)

sameCons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
sameCons prf = cong prf

sameList : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
sameList Refl prf2 = cong prf2

main : IO ()
main = do
  printLn $ Same 4
  printLn $ Same 5
  printLn $ the (EqNat 3 3) (Same _)
  printLn $ Same (2 + 2)
  printLn $ isJust $ exactLength 2 (0 :: 1 :: Nil)
  printLn $ isJust $ checkEqNat2 10 10
  printLn $ isNothing $ checkEqNat2 10 9
