module Main

import Data.List


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

treeLabelWith
  : Stream labelType
  -> Tree a
  -> (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node left val right)
  = let (lblThis :: lblsLeft, leftLabelled) = treeLabelWith lbls left
        (lblRight, rightLabelled) = treeLabelWith lblsLeft right
    in (lblRight, Node leftLabelled (lblThis, val) rightLabelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)

main : IO ()
main = do
  printLn $ flatten testTree
  printLn $ flatten $ treeLabel testTree
