module Main

import Data.Bits
import Data.Fin

main : IO ()
main = print $ the (Bits 64) (bitAt 3)
