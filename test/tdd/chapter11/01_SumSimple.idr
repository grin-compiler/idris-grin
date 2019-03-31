module Main

data LazyList : Type -> Type where
  Nil : LazyList ty
  (::) : (value : ty) -> Inf (LazyList ty) -> LazyList ty

%name LazyList xs, ys, zs

upto : Integer -> Integer -> LazyList Integer
upto n m = case n > m of
  True => Nil
  False => n :: upto (n + 1) m

sum : LazyList Integer -> Integer
sum [] = 0
sum (x :: xs) = x + sum xs

main : IO ()
main = do
  printLn (sum (upto 1 10000))
