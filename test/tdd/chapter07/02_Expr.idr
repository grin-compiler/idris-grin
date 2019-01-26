{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor -}
module Main

data Expr num
  = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs (eval x)

data Expr' : Type -> Type where
  Val' : num -> Expr' num
  Add' : Num num => Expr' num -> Expr' num -> Expr' num
  Mul' : Num num => Expr' num -> Expr' num -> Expr' num
  Sub' : Neg neg => Expr' neg -> Expr' neg -> Expr' neg
  Div' : Integral int => Expr' int -> Expr' int -> Expr' int
  Abs' : Abs abs => Expr' abs -> Expr' abs

eval' : Expr' num -> num
eval' (Val' x) = x
eval' (Add' x y) = eval' x + eval' y
eval' (Mul' x y) = eval' x * eval' y
eval' (Sub' x y) = eval' x - eval' y
eval' (Div' x y) = eval' x `div` eval' y
eval' (Abs' x) = abs $ eval' x

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

Show ty => Show (Expr ty) where
  show e = case e of
    (Val x) => show x
    (Add x y) => "(" ++ show x ++ " + " ++ show y ++ ")"
    (Sub x y) => "(" ++ show x ++ " - " ++ show y ++ ")"
    (Mul x y) => "(" ++ show x ++ " * " ++ show y ++ ")"
    (Div x y) => "(" ++ show x ++ " / " ++ show y ++ ")"
    (Abs x) => "|" ++ show x ++ "|"

(Eq ty, Integral ty, Neg ty, Abs ty) => Eq (Expr ty) where
  e1 == e2 = eval e1 == eval e2

(Integral ty, Neg ty, Abs ty) => Cast (Expr ty) ty where
  cast e = eval e

totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs

main : IO ()
main = do
  putStrLn "Expr"
  printLn $ eval $ the (Expr Integer) (Add (Val 6) (Mul (Val 3) (Val 12)))
  printLn $ eval $ the (Expr Integer) (6 + 3 * 12)
  printLn $ the (Expr Integer) (6 + 3 * 12)
  printLn $ eval' $ the (Expr' Integer) (Add' (Val' 6) (Mul' (Val' 3) (Val' 12)))
  printLn $ map (*2) [1,2,3,4]
  printLn $ foldr (+) 0 [1,2,3,4]
  printLn $ foldr (*) 1 [1,2,3,4]
  printLn $ totalLen ["One", "Two", "Three"]
