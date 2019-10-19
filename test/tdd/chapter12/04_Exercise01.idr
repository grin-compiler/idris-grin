module Main

import Control.Monad.State

update : (stateType -> stateType) -> State stateType ()
update f = do
  x <- get
  put (f x)

increase : Nat -> State Nat ()
increase n = update (+n)

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty)
                      "Fred"
                      (Node Empty "Sheila" Empty))
                "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left val right) = do
  countEmpty left
  countEmpty right

foundEmpty : State (Nat, Nat) ()
foundEmpty = do
  (e,n) <- get
  put (e + 1, n)

foundNode : State (Nat, Nat) ()
foundNode = do
  (e,n) <- get
  put (e, n + 1)

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = foundEmpty
countEmptyNode (Node left var right) = do
  foundNode
  countEmptyNode left
  countEmptyNode right

main : IO ()
main = do
  printLn $ runState (increase 5) 89
  printLn $ runState (countEmpty testTree) 0
  printLn $ runState (countEmptyNode testTree) (0,0)
