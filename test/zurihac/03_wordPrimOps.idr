module Main

import Data.Bits
import Data.Fin

main : IO ()
main = do
  let a = the (Bits 64) (bitAt 3)
  let b = the (Bits 64) (bitAt 4)
  print $ a < b
