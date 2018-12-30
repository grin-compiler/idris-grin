module Main

import Data.Vect

-- Enumerated types - Types defined by giving the possible values directly.

data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

-- Union types - Enumerated types that carries additional data at each values.

||| Shape represent objects from trigonometry.
data Shape = ||| A triangle,  with its base length and height
             Triangle Double Double
           | ||| A rectabgle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Shape2 : Type where
  Triangle2 : Double -> Double -> Shape2
  Rectagle2 : Double -> Double -> Shape2
  Circle2   : Double -> Shape2

-- Recursive types - Union types that defined on terms of themselves.

data Picture : Type where
  Primitive : Shape -> Picture
  Combine : Picture -> Picture -> Picture
  Rotate : Double -> Picture -> Picture
  Translate : Double -> Double -> Picture -> Picture

%name Picture pic, pic1, pic2

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture =
    Combine (Translate 5 5 rectangle)
    (Combine (Translate 35 5 circle)
    (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

-- Generic types - Types that are parametrized over some type

data Tree e
  = Empty
  | Node (Tree e) e (Tree e)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x node@(Node left val right) = case compare x val of
  LT => Node (insert x left) val right
  EQ => node
  GT => Node left val (insert x right)

data STree : Type -> Type where
  SEmpty : Ord elem => STree elem
  SNode : Ord elem
    => (left : STree elem)
    -> (val : elem)
    -> (right : STree elem)
    -> STree elem

insertS : elem -> STree elem -> STree elem
insertS x SEmpty = SNode SEmpty x SEmpty
insertS x node@(SNode left val right)
  = case compare x val of
      LT => SNode (insertS x left) val right
      EQ => node
      GT => SNode left val (insertS x right)

listTree : Ord a => List a -> Tree a
listTree [] = Empty
listTree (x :: xs) = insert x $ listTree xs

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

-- Dependent types - Types are computed from some other values

data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

renderVehicle : Vehicle power -> String
renderVehicle Bicycle = "Bicycle"
renderVehicle (Car fuel) = "Car " ++ show fuel
renderVehicle (Bus fuel) = "Bus " ++ show fuel

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible

zip2 : Vect n a -> Vect n b -> Vect n (a,b)
zip2 [] [] = []
zip2 (x :: xs) (y :: ys) = (x, y) :: zip2 xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
  Nothing  => Nothing
  Just idx => Just (index idx xs)

fin : Fin n -> Nat
fin FZ     = 0
fin (FS x) = 1 + fin x

total
vectTake : (fn : Fin n) -> Vect n a -> Vect (fin fn) a
vectTake FZ xs = []
vectTake (FS n) (x :: xs) = x :: vectTake n xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} i xs ys = case integerToFin i n of
  Nothing => Nothing
  Just idx => Just $ (index idx xs) + (index idx ys)

main : IO ()
main = do
  printLn "Data types"
  printLn $ pictureArea testPicture
  printLn $ treeToList $ listTree [1,4,3,5,2]
  printLn $ wheels Bicycle
  printLn $ renderVehicle (Bus 10)
  printLn $ zip2 [1,2,3] ['a', 'b', 'c']
  printLn $ Vect.index 3 [1,2,3,4,5]
  printLn $ tryIndex 3 [1,2,3,4,5]
  printLn $ sumEntries 2 [1,2,3,4] [5,6,7,8]
