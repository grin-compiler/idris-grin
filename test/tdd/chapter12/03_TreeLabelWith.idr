module Main

import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

Show a => Show (Tree a) where
  show Empty = "Empty"
  show (Node left val right) = "(Node " ++ show left ++ " " ++ show val ++ " " ++ show right ++ ")"

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty)
                      "Fred"
                      (Node Empty "Sheila" Empty))
                "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right) = do
  leftLabelled <- treeLabelWith left
  (this :: rest) <- get
  put rest
  rightLabelled <- treeLabelWith right
  pure (Node leftLabelled (this, val) rightLabelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = evalState (treeLabelWith tree) [1..]

main : IO ()
main = do
  printLn $ flatten (treeLabel testTree)
