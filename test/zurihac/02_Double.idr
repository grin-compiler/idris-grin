module Main

main : IO ()
main = do
  putStrLn $ show $ cos 0.0
  putStrLn $ show $ cos 1.0
  putStrLn $ show $ cos (-1.0)
  putStrLn $ show $ sin 0.0
  putStrLn $ show $ sin 1.0
  putStrLn $ show $ sin (-1.0)
  putStrLn $ show $ tan 0.0
  putStrLn $ show $ tan 1.0
  putStrLn $ show $ tan (-1.0)
  putStrLn $ show $ log 0.0
  putStrLn $ show $ log 1.0
--  print (log (-1.0)) -- Fix pipeline, Float and Double representation
  putStrLn $ show $ exp 0.0
  putStrLn $ show $ exp 1.0
  putStrLn $ show $ exp (-1.0)
  putStrLn $ show $ acos 0.0
  putStrLn $ show $ acos 1.0
  putStrLn $ show $ acos (-1.0)
  putStrLn $ show $ asin 0.0
  putStrLn $ show $ asin 1.0
  putStrLn $ show $ asin (-1.0)
  putStrLn $ show $ atan 0.0
  putStrLn $ show $ atan 1.0
  putStrLn $ show $ atan (-1.0)
  putStrLn $ show $ atan2 0.0 1.0
  putStrLn $ show $ atan2 1.0 0.0
  putStrLn $ show $ atan2 (-1.0) 0.5
  putStrLn $ show $ floor 0.0
  putStrLn $ show $ floor 1.0
  putStrLn $ show $ floor (-1.0)
  putStrLn $ show $ ceiling 0.0
  putStrLn $ show $ ceiling 1.0
  putStrLn $ show $ ceiling (-1.0)
  putStrLn $ show $ negate 0.0
  putStrLn $ show $ negate 1.0
  putStrLn $ show $ negate (-1.0)
