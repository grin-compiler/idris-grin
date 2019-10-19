module Main

data State : (stateType : Type) -> Type -> Type where
  Get : State stateType stateType
  Put : stateType -> State stateType ()

  Pure : ty -> State stateType ty
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
(>>=) = Bind

runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get              st = (st, st)
runState (Put newState)   st = ((), newState)
runState (Pure x)         st = (x, st)
runState (Bind cmd prog)  st =
  let (val, nextState) = runState cmd st
  in runState (prog val) nextState

Functor (State stateType) where
  map func x = Bind x (\val => Pure (func val))

Applicative (State stateType) where
  pure      = Pure
  (<*>) f a =
    Bind f (\f' =>
    Bind a (\a' =>
    Pure (f' a')))

Monad (State stateType) where
  (>>=) = Bind

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

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = Pure Empty
treeLabelWith (Node left val right) = do
  leftLabelled <- treeLabelWith left
  (this :: rest) <- Get
  Put rest
  rightLabelled <- treeLabelWith right
  Pure (Node leftLabelled (this, val) rightLabelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = fst (runState (treeLabelWith tree) [1..])

main : IO ()
main = do
  printLn $ treeLabel testTree
