{- Ctrl-Alt-A: Add definition
   Ctrl-Alt-C: Case split
   Ctrl-Alt-D: Documentation
   Ctrl-Alt-L: Lift hole: Lifts a hole to the top level as a new function definition
   Ctrl-Alt-M: Match: Replaces a hole with a case expression that matches on an intermediate result
   Ctrl-Alt-R: Reloads and typechecks the current buffer
   Ctrl-Alt-S: Search
   Ctrl-Alt-T: Type-check: Displays the type under cursor -}
module Main

import Data.SortedMap
import Data.SortedSet
import Control.Monad.State
import Control.Monad.Reader
import Prelude.Traversable
import Text.PrettyPrint.WL

interface Pretty p where
  doc : p -> Doc

(Pretty p1, Pretty p2) => Pretty (Pair p1 p2) where
  doc (p1, p2) = brackets (doc p1 |++| text "," |++| doc p2)

Pretty p => Pretty (List p) where
  doc ps = vsep (map doc ps)

Pretty Nat where
  doc = text . show

-- Arity
data Arity = MkArity Nat
Eq Arity where
  (MkArity a1) == (MkArity a2) = a1 == a2
Show Arity where
  show (MkArity a) = show a
Pretty Arity where
  doc (MkArity a) = text $ show a

-- Var
data Var = MkVar String
Eq Var where
  (MkVar v1) == (MkVar v2) = v1 == v2
Ord Var where
  compare (MkVar v1) (MkVar v2) = compare v1 v2
Pretty Var where
  doc (MkVar v) = text v

--  Constructor
data Cnst
  = MkCnst String
  | PCnst String
Eq Cnst where
  (MkCnst v1) == (MkCnst v2) = v1 == v2
  (PCnst c1) == (PCnst c2) = c1 == c2
  _ == _ = False
Show Cnst where
  show (MkCnst v) = show v
  show (PCnst v) = show v
Ord Cnst where
  compare (MkCnst c1) (MkCnst c2) = compare c1 c2
  compare (PCnst c1) (PCnst c2) = compare c1 c2
  compare (MkCnst c1) (PCnst c2) = LT
  compare (PCnst c1) (MkCnst c2) = GT
Pretty Cnst where
  doc (MkCnst v) = text v
  doc (PCnst c) = text c

-- Expr
data Expr
  = Call Cnst (List Var)
  | Pap Cnst (List Var)
  | App Var Var
  | Ctor Int (List Var)
  | Proj Int Var
  | Reset Var
  | Reuse Var Int (List Var)
Eq Expr where
  (Call c1 vs1)     == (Call c2 vs2)      = c1 == c2 && vs1 == vs2
  (Pap c1 vs1)      == (Pap c2 vs2)       = c1 == c2 && vs1 == vs2
  (App v11 v12)     == (App v21 v22)      = v11 == v21 && v21 == v22
  (Ctor i1 v1)      == (Ctor i2 v2)       = i1 == i2 && v1 == v2
  (Proj i1 v1)      == (Proj i2 v2)       = i1 == i2 && v1 == v2
  (Reset v1)        == (Reset v2)         = v1 == v2
  (Reuse v1 i1 vs1) == (Reuse v2 i2 vs2)  = v1 == v2 && i1 == i2 && vs1 == vs2
  _ == _ = False
Pretty Expr where
  doc e = case e of
    Call c vs     => hsep (doc c :: map doc vs)
    Pap c vs      => hsep (text "pap" :: doc c :: map doc vs)
    App x y       => hsep [doc x, doc y]
    Ctor i vs     => hsep (text "ctor" :: text (show i) :: map doc vs)
    Proj i v      => hsep [text "proj", text (show i), doc v]
    Reset v       => hsep [text "reset", doc v]
    Reuse v i vs  => hsep (text "reuse" :: doc v :: text "in" :: text "ctor" :: text (show i) :: map doc vs)

data Phase
  = Pure
  | RC -- Reset/Reuse
  | OC -- Owned Const

data FnBody : Phase -> Type where
  Ret  : Var                            -> FnBody p
  Let  : Var -> Expr -> FnBody p        -> FnBody p
  Case : Var -> List (Arity, FnBody p)  -> FnBody p
  Inc  : Var -> FnBody p                -> FnBody p
  Dec  : Var -> FnBody p                -> FnBody p
  LetC : Cnst -> Cnst -> FnBody p       -> FnBody p -- let cO := c
Pretty (FnBody p) where
  doc e = case e of
    Ret  v      => text "ret" |++| doc v
    Let  v e b  => text "let" |++| doc v |++| text "=" |++| doc e |+| text ";" |$$| doc b
    Case v alts => text "case" |++| doc v |$$| indent 2 (vsep (map (\(a,b) => parens (indent 1 (doc b) |+| text " ")) alts))
    Inc  v b    => text "inc" |++| doc v |++| text ";" |$$| doc b
    Dec  v b    => text "dec" |++| doc v |++| text ";" |$$| doc b
    LetC o c b  => text "let" |++| doc o |++| text ":=" |$$| doc b

eqFnBody : FnBody p -> FnBody p -> Bool
eqFnBody b1 b2 = case (b1, b2) of
  (Ret v1, Ret v2)              => v1 == v2
  (Let v1 e1 b1, Let v2 e2 b2)  => v1 == v2 && e1 == e2 && eqFnBody b1 b2
  (Case v1 as1, Case v2 as2)    => v1 == v2 && length as1 == length as2 && and (zipWith (\(a1,c1) , (a2,c2) => a1 == a2 && eqFnBody c1 c2) as1 as2)
  (Inc v1 b1, Inc v2 b2)        => v1 == v2 && eqFnBody b1 b2
  (Dec v1 b1, Dec v2 b2)        => v1 == v2 && eqFnBody b1 b2
  (LetC o1 c1 b1, LetC o2 c2 b2) => o1 == o2 && c1 == c2 && eqFnBody b1 b2
  (_, _)                        => False

data Fun : Phase -> Type where
  MkFun : List Var -> FnBody p -> Fun p
Pretty (Fun p) where
  doc (MkFun ps b) = hsep (map doc ps) |++| text "=" |$$| indent 2 (doc b)

data Program : Phase -> Type where
  MkProgram : List (Cnst, Fun p) -> Program p
Pretty (Program p) where
  doc (MkProgram fs) = vsep $ map (\(n,f) => doc n |++| doc f) fs

swapExample : Program Pure
swapExample = MkProgram
  [ ( swap
    , MkFun [xs]
      $ Case xs
        [ (MkArity 0, Ret xs)
        , (MkArity 2, Let t1 (Proj 2 xs)
                    $ Case t1
                      [ (MkArity 0, Ret xs)
                      , (MkArity 2, Let h1 (Proj 1 xs)
                                  $ Let h2 (Proj 2 t1)
                                  $ Let t2 (Proj 2 t1)
                                  $ Let r1 (Ctor 2 [h1, t2])
                                  $ Let r2 (Ctor 2 [h2, r1])
                                  $ Ret r2
  )])])]
  where
    swap = MkCnst "swap"
    xs = MkVar "xs"
    t1 = MkVar "t1"
    t2 = MkVar "t2"
    h1 = MkVar "h1"
    h2 = MkVar "h2"
    r1 = MkVar "r1"
    r2 = MkVar "r2"

mapExample : Program Pure
mapExample = MkProgram
  [ ( map
    , MkFun [f, xs]
        $ Case xs
          [ (MkArity 0, Ret xs)
          , (MkArity 2, Let x  (Proj 1 xs)
                      $ Let s  (Proj 2 xs)
                      $ Let y  (App f x)
                      $ Let ys (Call map [f, s])
                      $ Let r  (Ctor 2 [y, ys])
                      $ the (FnBody Pure) (Ret r))
          ]
    )
  ]
  where
    map = MkCnst "map"
    f = MkVar "f"
    x = MkVar "x"
    s = MkVar "s"
    r = MkVar "r"
    y = MkVar "y"
    ys = MkVar "ys"
    xs = MkVar "xs"

-- Originally S
insertReuse : Var -> Arity -> FnBody RC -> FnBody RC
insertReuse w n f = case f of
  Ret x     => Ret x
  Case x fs => Case x (map (\(a, f) => (a, insertReuse w n f)) fs)
  Let x e g => case e of
    Ctor i ys => if MkArity (List.length ys) == n
                    then Let x (Reuse w i ys) g
                    else Let x (Ctor i ys) (insertReuse w n g)
    e' => Let x e (insertReuse w n g)
  Dec x g => Dec x g
  Inc x g => Inc x g
  LetC c1 c2 g => LetC c1 c2 g

-- NewName
data NewNameT : (f : Type -> Type) -> (a : Type) -> Type where
  NewName : StateT Nat m a -> NewNameT m a

unNewName : NewNameT m a -> StateT Nat m a
unNewName (NewName m) = m

runNewNameT : Monad m => NewNameT m a -> m a
runNewNameT (NewName s) = fst <$> runStateT s 0

runNewName : NewNameT Identity a -> a
runNewName = runIdentity . runNewNameT

Functor f => Functor (NewNameT f) where
  map f (NewName s) = NewName (map f s)

(Functor (NewNameT f), Applicative (StateT Nat f)) => Applicative (NewNameT f) where
  pure x                      = NewName (pure x)
  (NewName f) <*> (NewName x) = NewName (f <*> x)

(Monad (StateT Nat f), Applicative (NewNameT f)) => Monad (NewNameT f) where
   (NewName m) >>= k = NewName $ do
     x <- m
     unNewName (k x)

freshVar : Monad m => NewNameT m Var
freshVar = NewName $ do
  n <- get
  put (S n) -- TODO: How to increment nat??? :)
  pure $ MkVar $ "w" ++ show n

freshCnst : Monad m => NewNameT m Cnst
freshCnst = NewName $ do
  n <- get
  put (S n)
  pure $ PCnst ("c" ++ show n)

total
inExpr : Var -> Expr -> Bool
inExpr v e = case e of
  (Call c vs)     => v `elem` vs
  (Pap c vs)      => v `elem` vs
  (App x y)       => x == v || y == v
  (Ctor i vs)     => v `elem` vs
  (Proj i x)      => v == x
  (Reuse x i vs)  => (v `elem` vs) || (v == x)
  (Reset x)       => v == x

-- total : TODO: List is not strong enough
inFBody : Var -> FnBody p -> Bool
inFBody v f = case f of
  Ret  x      => v == x
  Let  x e b  => v == x || (inExpr v e) || (inFBody v b)
  Case x alts => v == x || (foldl (\x,y => x && y) True (map (inFBody v . snd) alts)) -- TODO: How to use and here?
  Inc  x b    => v == x || (inFBody v b)
  Dec  x b    => v == x || (inFBody v b)
  LetC _ _ b  => inFBody v b

insertResetIns : Monad m => Var -> Arity -> FnBody RC -> NewNameT m (FnBody RC)
insertResetIns z n f = do
  w <- freshVar
  let g = insertReuse w n f
  pure $ if not $ eqFnBody g f
            then Let w (Reset z) g
            else f

-- Originally D
insertReset : Monad m => Var -> Arity -> FnBody RC -> NewNameT m (FnBody RC)
insertReset z n f = case f of
  Dec x g   => pure $ Dec x g
  Inc x g   => pure $ Inc x g
  LetC o c g => LetC o c <$> insertReset z n g
  Ret x     => pure $ Ret x
  Case x gs => Case x <$> traverse (\(a, g) => (\v => (a,v)) <$> insertReset z n g) gs
  Let x e g =>
    case ((inExpr z e) || (inFBody z g)) of
      True  => Let x e <$> insertReset z n g
      False => insertResetIns z n f

-- Originally R
resetReuse : Monad m => FnBody Pure -> NewNameT m (FnBody RC)
resetReuse b = case b of
  Ret x     => pure $ Ret x
  Let x e f => Let x e <$> resetReuse f
  LetC o c f => LetC o c <$> resetReuse f
  Case x bs => Case x <$> traverse (\(arity, body) => do
                                        body' <- resetReuse body
                                        (MkPair arity) <$> (insertReset x arity body'))
                                   bs

resetReuseFun : Monad m => Fun Pure -> NewNameT m (Fun RC)
resetReuseFun (MkFun ps body) = MkFun ps <$> resetReuse body

resetReuseProgram : Monad m => Program Pure -> NewNameT m (Program RC)
resetReuseProgram (MkProgram defs) = MkProgram <$> traverse (\(n,f) => (MkPair n <$> resetReuseFun f)) defs

||| Inserts local constant
insertLetCFnBody : Monad m => FnBody p -> NewNameT m (FnBody p) -- TODO: Right phase parameter
insertLetCFnBody b = case b of
  Ret  v => pure $ Ret v
  Let  v e g => case e of
                  (Pap c xs) => do
                    c2 <- freshCnst
                    (LetC c2 c . Let v (Pap c2 xs)) <$> insertLetCFnBody g
                  e0 => Let v e0 <$> insertLetCFnBody g
  Case v alts => Case v <$> traverse (\(a,g) => MkPair a <$> insertLetCFnBody g) alts
  Inc  v g => Inc v <$> insertLetCFnBody g
  Dec  v g => Dec v <$> insertLetCFnBody g
  LetC c1 c2 g => LetC c1 c2 <$> insertLetCFnBody g

insertLetCFun : Monad m => Fun p -> NewNameT m (Fun p)
insertLetCFun (MkFun ps body) = MkFun ps <$> insertLetCFnBody body

insertLetCProgram : Monad m => Program p -> NewNameT m (Program p)
insertLetCProgram (MkProgram defs) = MkProgram <$> traverse (\(n,f) => (MkPair n <$> insertLetCFun f)) defs

{-
Inferring borrowing signatures:
Every function parameter is annotated with Owned or Borrowed

Our heruristing pcollects which parameters and variables should be owned.
We say a parameter should be owned
  - if x or one of its projections is used in a reset
  - is passed to a function that takes an owned reference.
    (This is heuristic and not required for correctness)
-}

data Borrowing
  = Owned
  | Borrowed
Eq Borrowing where
  Owned     == Owned    = True
  Borrowed  == Borrowed = True
  _         == _        = False
Show Borrowing where
  show Owned = "Owned"
  show Borrowing = "Borrowing"
Pretty Borrowing where
  doc Owned     = text "Owned"
  doc Borrowed  = text "Borrowed"

BorrowingMap : Type
BorrowingMap = SortedMap (Cnst, Nat) Borrowing

borrowed : BorrowingMap -> (Cnst, Nat) -> Maybe Borrowing
borrowed b (PCnst _, _) = Just Owned
borrowed b (c,n)        = Data.SortedMap.lookup (c,n) b

constBorrowingParams : BorrowingMap -> Cnst -> List Borrowing
constBorrowingParams bm c
  = map snd
  $ sortBy (\((_, e1), _) , ((_, e2), _) => compare e1 e2)
  $ filter ((c ==) . fst . fst) $ Data.SortedMap.toList bm

number : List a -> List (Nat, a)
number = snd . foldl (\(n, ns), a => (S n, (n,a) :: ns)) (Z, [])

||| Collects the owned variables in a function body
collectOwnedVars : BorrowingMap -> FnBody rc -> SortedSet Var
collectOwnedVars beta b = case b of -- TODO: Unicode Beta
  Let z (Ctor i xs)     f => collectOwnedVars beta f
  Let z (Reuse x i xs)  f => collectOwnedVars beta f
  Let z (Call c xs)     f
    => union (collectOwnedVars beta f)
             (fromList
              (concatMap (\(n,x) => maybe [x]
                                          (\b => if b == Owned then [x] else [])
                                          (borrowed beta (c,n)))
                         (number xs)))
  Let z (Reset x)       f => insert x (collectOwnedVars beta f)
  Let z (App x y)       f => union (collectOwnedVars beta f) (fromList [x, y])
  Let z (Pap cO xs)     f => union (collectOwnedVars beta f) (fromList xs)
  -- TODO: Define a wrapper constant transformation for partial application
  -- as we assume that partially applied functions always have Owned parameters.
  Let z (Proj i x)      f => let c = collectOwnedVars beta f
                             in if contains z c
                                  then insert x c
                                  else c
  Ret x => empty
  Case z alts => foldl1 union $ map (collectOwnedVars beta . snd) alts
  Inc z f => collectOwnedVars beta f
  Dec z f => collectOwnedVars beta f
  LetC o c f => collectOwnedVars beta f

ownedFunctionVar : BorrowingMap -> Fun p -> List (Nat, Borrowing)
ownedFunctionVar beta (MkFun ps body) = snd $ foldl borrowed (0, []) ps
  where
    c : SortedSet Var
    c = collectOwnedVars beta body

    borrowed : (Nat, List (Nat, Borrowing)) -> Var -> (Nat, List (Nat, Borrowing))
    borrowed (n, vs) v = (n + 1, (n, if contains v c then Owned else Borrowed) :: vs)

ownedParameters : BorrowingMap -> Program p -> BorrowingMap
ownedParameters beta (MkProgram funs) = foldl insertFun empty funs
  where
    insertFun : BorrowingMap -> (Cnst, Fun p) -> BorrowingMap
    insertFun s (name, fun) = foldl (\s1, (n, b) => insert (name, n) b s1) s (ownedFunctionVar beta fun)

startBorrowingMap : Program p -> BorrowingMap
startBorrowingMap (MkProgram funs) = foldl insertFunSignature empty funs
  where
    insertFunSignature : BorrowingMap -> (Cnst, Fun p) -> BorrowingMap
    insertFunSignature beta (cnst, MkFun ps _) = fromList
      [ ((cnst, n), Borrowed) | (n,_) <- number ps ]

Eq BorrowingMap where
  m1 == m2 = toList m1 == toList m2

calculateOwnedParameters : Program p -> BorrowingMap
calculateOwnedParameters program = go empty (startBorrowingMap program)
  where
    go : BorrowingMap -> BorrowingMap -> BorrowingMap
    go previous actual = if previous == actual
                            then actual
                            else ownedParameters previous program

BorrowVarMap : Type
BorrowVarMap = Var -> Borrowing

varsInExp : Expr -> SortedSet Var
varsInExp e = case e of
  Call c  vs    => SortedSet.fromList vs
  Pap  c  vs    => SortedSet.fromList vs
  App  v1 v2    => SortedSet.fromList [v2] -- ???
  Ctor i  vs    => SortedSet.fromList vs
  Proj i  v     => SortedSet.fromList [v]
  Reset v       => SortedSet.fromList [v]
  Reuse v i vs  => SortedSet.fromList $ v :: vs

freeVars : FnBody p -> SortedSet Var
freeVars f = case f of
  Ret  x        => fromList [x]
  Let  x e b    => delete x $ union (varsInExp e) (freeVars b)
  Case x alts   => insert x $ foldl1 union $ map (freeVars . snd) alts
  -- Special cases
  Inc  x b      => insert x $ freeVars b
  Dec  x b      => insert x $ freeVars b
  LetC c1 c2 b  => freeVars b

ownPlus : Var -> SortedSet Var -> BorrowVarMap -> FnBody p -> FnBody p
ownPlus x vars beta f = if (not (contains x vars)) && (beta x == Owned)
  then f
  else Inc x f

ownMinus_ : Var -> SortedSet Var -> BorrowVarMap -> FnBody p -> FnBody p
ownMinus_ x freeVars beta f = if (not (contains x freeVars)) && (beta x == Owned)
  then Dec x f
  else f

ownMinuses_ : List Var -> SortedSet Var -> BorrowVarMap -> FnBody p -> FnBody p
ownMinuses_ []        freeVars beta f = f
ownMinuses_ (x :: xs) freeVars beta f = ownMinuses_ xs freeVars beta (ownMinus_ x freeVars beta f)

ownMinus : Var -> BorrowVarMap -> FnBody p -> FnBody p
ownMinus x beta f = ownMinus_ x (freeVars f) beta f

ownMinuses : List Var -> BorrowVarMap -> FnBody p -> FnBody p
ownMinuses xs beta f = ownMinuses_ xs (freeVars f) beta f

assignBorrowMap : Var -> Borrowing -> BorrowVarMap -> BorrowVarMap
assignBorrowMap x b f v = if x == v then b else f v

insertIncDecApp : Monad m => List Var -> List Borrowing -> BorrowVarMap -> FnBody p -> m (FnBody p)
insertIncDecApp (y :: ys) (Owned    :: bs) beta (Let z e f)
  = ownPlus y (union (freeVars f) (fromList ys)) beta <$> insertIncDecApp ys bs beta (Let z e f)
insertIncDecApp (y :: ys) (Borrowed :: bs) beta (Let z e f)
  = insertIncDecApp ys bs beta (Let z e (ownMinus y beta f))
insertIncDecApp ys os beta f = pure f

--insertIncDec1 : (Monad m, MonadReader BorrowingMap m) => BorrowVarMap -> FnBody p -> m (FnBody p)
--insertIncDec1 beta (Ret v)        = pure (ownPlus v empty beta (Ret v))
--insertIncDec1 beta (Inc v b)      = Inc v <$> insertIncDec1 beta b
--insertIncDec1 beta (Dec v b)      = Dec v <$> insertIncDec1 beta b
--insertIncDec1 beta (LetC c1 c2 b) = LetC c1 c2 <$> insertIncDec1 beta b
--insertIncDec1 beta f@(Case v alts)
--  = Case v <$> traverse
--      (\(arity, b0) => map (MkPair arity . ownMinuses (SortedSet.toList (freeVars f)) beta) $ insertIncDec1 beta b0)
--      alts
--insertIncDec1 beta (Let w e b) with (e)
--  insertIncDec1 beta (Let w e b) | Proj i x     = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | Reset x      = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | Call c ys    = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | Pap c ys     = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | App x y      = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | Ctor i ys    = insertIncDec1 beta b
--  insertIncDec1 beta (Let w e b) | Reuse x i ys = insertIncDec1 beta b

insertIncDec : (Monad m, MonadReader BorrowingMap m) => BorrowVarMap -> FnBody p -> m (FnBody p)
insertIncDec beta f = case f of
  Ret  v     => pure $ ownPlus v empty beta (Ret v)
  Let  w e b => case e of
                  (Proj i x)     => case beta x of
                                      Owned  => Let w (Proj i x) . Inc w . ownMinus x beta <$> insertIncDec beta b
                                      Borrowed => Let w (Proj i x) <$> insertIncDec (assignBorrowMap w Borrowed beta) b
                  (Reset x)      => Let w (Reset x) <$> insertIncDec beta b
                  (Call c ys)    => do
                    bm <- ask
                    b1 <- insertIncDec beta b
                    insertIncDecApp ys (constBorrowingParams bm c) beta $ Let w (Call c ys) b1
                  (Pap c ys)     => do
                    bm <- ask
                    b1 <- insertIncDec beta b
                    insertIncDecApp ys (constBorrowingParams bm c) beta $ Let w (Pap c ys) b1
                  (App x y)      => do
                    b1 <- insertIncDec beta b
                    insertIncDecApp [x,y] [Owned, Owned] beta $ Let w (App x y) b1
                  (Ctor i ys)    => do
                    b1 <- insertIncDec beta b
                    insertIncDecApp ys (const Owned <$> ys) beta $ Let w (Ctor i ys) b1
                  (Reuse x i ys) => do
                    b1 <- insertIncDec beta b
                    insertIncDecApp ys (const Owned <$> ys) beta $ Let w (Reuse x i ys) b1
  Case v alts => let fv = SortedSet.toList (freeVars f)
                 in Case v <$> traverse
                                (\(arity, b0) => do
                                    b1 <- insertIncDec beta b0
                                    let b2 = ownMinuses fv beta b1
                                    pure (arity, b2))
                                alts
  Inc  v     b  => Inc  v      <$> insertIncDec beta b
  Dec  v     b  => Dec  v      <$> insertIncDec beta b
  LetC c1 c2 b  => LetC c1 c2  <$> insertIncDec beta b

funInsertIncDec : (Monad m, MonadReader BorrowingMap m) => Cnst -> Fun p -> m (Fun p)
funInsertIncDec funName (MkFun args body) = do
  bm <- ask
  let argNum = \x => findIndex (x==) args
  let beta = \x => case argNum x of
                    Nothing => Owned
                    Just n => case Data.SortedMap.lookup (funName, n) bm of
                      Nothing => Owned
                      Just r  => r
  (MkFun args . ownMinuses args beta) <$> insertIncDec beta body

programInsertIncDec : (Monad m, MonadReader BorrowingMap m) => Program p -> m (Program p)
programInsertIncDec (MkProgram funDefs) = MkProgram <$> traverse (\(c, f) => MkPair c <$> funInsertIncDec c f) funDefs

prettyPutStrLn : (Pretty p) => p -> IO ()
prettyPutStrLn = putStrLn . toString 1.0 80 . doc

main : IO ()
main = do
  let mapExampleRC = runNewName $ do
        p1 <- resetReuseProgram mapExample
        p2 <- insertLetCProgram p1
        pure p2
  let swapExampleRC = runNewName $ do
        p1 <- resetReuseProgram swapExample
        p2 <- insertLetCProgram p1
        pure p2
  putStrLn ""
  prettyPutStrLn mapExample
  prettyPutStrLn mapExampleRC
  putStrLn ""
  prettyPutStrLn swapExample
  prettyPutStrLn swapExampleRC
  putStrLn "Owned parameters: Map example"
  printLn $ ownedParameters empty mapExampleRC
  prettyPutStrLn $ toList $ ownedParameters empty mapExampleRC
  putStrLn "Owned parameters: Swap example"
  printLn $ ownedParameters empty swapExampleRC
  prettyPutStrLn $ toList $ ownedParameters empty swapExampleRC
  printLn "Start Borrowing Maps"
  let mco = calculateOwnedParameters mapExampleRC
  let sco = calculateOwnedParameters swapExampleRC
  printLn mco
  printLn sco
  let mapExampleRC2 = runIdentity $ runReaderT (programInsertIncDec mapExampleRC) sco
  prettyPutStrLn (the (Program RC) mapExampleRC2)
  let swapExampleRC2 = runIdentity $ runReaderT (programInsertIncDec swapExampleRC) sco
  prettyPutStrLn (the (Program RC) swapExampleRC2)

{-
Implementation of the Counting Immutable Beans (CIB) for the GRIN compiler.

CIB uses instrumentation of the original program. There are four new instructions in the syntax
that are inserted via instrumentation. This can be categorized into two;
- reference counter instructions:
  - inc
  - dec
- heap location reuse:
  - reset
  - reuse

In the CIB approach every heap location has a reference counter associated with it.
Inc increments the counter for the location, and also increments all the referred locations transitively.
Dec decrements the counter for the location, and also decrements all the referred locations transitively.

Reset, reuse:
From the CIB paper:
let y = reset x.
If x is a shared value, than y is set to a special pointer value BOX, otherwise to the heap location associated with x.
If x is not shared than reset decrements the reference counters of the components of x, and y is set to x.

let z = reuse y in ctor_i w.
If y is BOX reuse allocates a new heap for the constructor.
If y is not Box the runtime reuses the heap location for storing the constructor.

Application of the same idea for GRIN:
Differences: meanwhile Lean's IR put every variable on the heap, GRIN uses variables as were registers
and only a subset of the registers are associated with heap locations. A register is associated
with heap location if its type is Loc. This means the GRIN implementation of the CIB approach needs
a type environment which tells which variables can be affected by the CIB operations.

In GRIN:
 * The CIB instrumentation should happen after the optimization steps.
 * Special interpreter should be implemented which handles the CIB instructions.
 * Probably it should have its own LLVM code generator and LLVM implemented runtime, preferably a plugin
   for the existing one.

We need to add 4 new instructions:
 * `x <- reset y;`   where y is a heap location, x can be a special heap location, which can be BOX too.
 * `z <- reuse x y;` where x is a special heap location created by reset, and y is a Node value.
 * `z <- inc x;`     where x is a heap location, it transitively increments the reference counters in the locations.
                     cycle detection should happen. The increment operation computes unit as its return value.
 * `z <- dec x;`     where x is a heap location, it transitively decrements the reference counters in the locations.
                     cycle detection should happen. When the counter reaches zero, the runtime must deallocate
                     the location. The decrement operation computes unit as its return value.

The GRIN nodes store primitive values, but the runtime makes the difference between location values and
primitive values, thus it is able to create the transitive closure of the reachability relation of a location
and manipulate its reference counters.

Every of the four instructions needs to be implemented in the GRIN runtime/interpreter.

In the original paper reuse of the constructors could happen only of the arity of the constructors
are the same. But in GRIN as the runtime needs to allocate heaps based on the type of the heap location.
This means every heap location can have its own arity, and reuse if the heap location is possible
only if the new node does not exceeds the arity of the heap node. Otherwise a new node needs to
be allocated, with the maximum arity.

The most important change is the reuse construction. It changes certain instances of the
store operation to the reuse operation.

```
Before:
x <- store y;

After:
z <- reset w;
...
x <- reuse z y;
```

In this case we need to decide to reuse the heap location associated with w only if w can accommodate
all the possible values of x. This means the max-arity(w) >= max-arity(x). Meanwhile Lean's
approach uses the arity of the constructors in the alternatives, we can use the abstract information
of all the possible runs.

Implementation steps:
[ ] Import abstracting the definitional interpreters from the mini-grin repo
[ ] Change the implementation to use base functors instead of Expr datatype
[ ] Implement reference statistics with the new interpreter, as a warm-up exercise
[ ] Implement CIB program instrumentation for GRIN producing ExprF :+: CibF AST
[ ] Implement interpreter for CIB extended GRIN program
[ ] Extra: Implement LLVM codegen plugin for CIB instructions
-}
