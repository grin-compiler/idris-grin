module Main

import Data.Primitives.Views


quiz : Stream Int -> (score : Nat) -> IO ()
quiz (n1 :: n2 :: ns) score = do
  putStrLn $ "Score so far: " ++ show score
  putStrLn (show n1 ++ " * " ++ show n2 ++ "?")
  answer <- getLine
  case answer of
    "quit" => do putStrLn "Bye!"
                 pure ()
    _ => if cast answer == n1 * n2
           then do putStrLn "Correct!"
                   quiz ns (score + 1)
           else do putStrLn ("Wrong, the answer is " ++ show (n1 * n2))
                   quiz ns score

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = rem + 1

main : IO ()
main = quiz (arithInputs 52816) 0
