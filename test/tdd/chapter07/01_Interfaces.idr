{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor -}
module Main

occurences : (Eq ty) => (item : ty) -> (values : List ty) -> Nat
occurences item [] = 0
occurences item (x :: xs) = case item == x of
  False => occurences item xs
  True => 1 + occurences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

Eq Album where
  (==) (MkAlbum artist1 title1 year1) (MkAlbum artist2 title2 year2)
    = artist1 == artist2 && title1 == title2 && year1 == year2

Ord Album where
  compare (MkAlbum artist1 title1 year1) (MkAlbum artist2 title2 year2)
    = case compare artist1 artist2 of
        EQ => case compare year1 year2 of
          EQ => compare title1 title2
          diff_year => diff_year
        diff_artist => diff_artist

Show Album where
  show (MkAlbum artist title year)
    = title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"

main : IO ()
main = do
  printLn "Main"
  -- Triggers a compiler like bug: https://github.com/typedefs/typedefs/issues/72
  -- print $ occurences 'b' ['a', 'a', 'b', 'b', 'b', 'c']
  printLn $ occurences 'b' ['a', 'a', 'b', 'b', 'b', 'c']
  printLn $ occurences 100 [50, 100, 100, 50]
  printLn $ occurences Liquid [Solid, Liquid, Liquid, Gas, Gas]
  printLn $ heroes > clouds
  printLn $ help <= rubbersoul
  printLn $ map title (sort collection)
  printLn $ clouds
