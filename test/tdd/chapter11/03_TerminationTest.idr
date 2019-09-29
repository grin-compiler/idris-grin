module Main

%default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do
  putStrLn "Enter your name: "
  name <- getLine
  if name == ""
    then do putStrLn "Bye bye!"
            Quit ()
    else do putStrLn ("Hello " ++ name)
            greet

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> RunIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run _ (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do
  res <- c
  run fuel (f res)

partial
forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S n) = More (tank n)

partial
main : IO ()
main = do
  run (tank 10) greet
