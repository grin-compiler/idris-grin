module Main

randoms : Int -> Stream Int
randoms seed =
  let seed' = 1664525 * seed + 1013904223
  in (seed' `shiftR` 2) :: randoms seed'

main : IO ()
main = do
  printLn $ take 100 $ randoms 52816
