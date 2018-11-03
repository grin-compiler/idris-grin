module Main

double : Num ty => ty -> ty
double x = x + x

identity : ty -> ty
identity x = x

the2 : (ty : Type) -> ty -> ty
the2 ty x = x

twice : (a -> a) -> a -> a
twice f x = f (f x)

-- Shape : Type
-- rotate : Shape -> Shape

longer : String -> String -> Nat
longer w1 w2
  = let l1 = length w1
        l2 = length w2
    in if l1 > l2 then l1 else l2

||| Calculates the value of the pythagoras equasion.
||| @x a value for the first side of a triangle
||| @y a value for the second side of a triangle
pythagoras : (x : Double) -> (y : Double) -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x

main : IO ()
main = do
  putStrLn $ cast $ twice double $ the2 Int $ identity 3
  putStrLn $ cast $ longer "Hello" "World!"
  putStrLn $ cast $ pythagoras 1 3
