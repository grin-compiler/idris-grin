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
import Control.Monad.State
import Prelude.Traversable
import Text.PrettyPrint.WL

interface Pretty p where
  doc : p -> Doc

-- Arity
data Arity = MkArity Nat
Eq Arity where
  (MkArity a1) == (MkArity a2) = a1 == a2
Pretty Arity where
  doc (MkArity a) = text $ show a

-- Var
data Var = MkVar String
Eq Var where
  (MkVar v1) == (MkVar v2) = v1 == v2
Pretty Var where
  doc (MkVar v) = text v

--  Constructor
data Cnst  = MkCnst String
Eq Cnst where
  (MkCnst v1) == (MkCnst v2) = v1 == v2
Pretty Cnst where
  doc (MkCnst v) = text v

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
  | RC

data FnBody : Phase -> Type where
  Ret  : Var                            -> FnBody p
  Let  : Var -> Expr -> FnBody p        -> FnBody p
  Case : Var -> List (Arity, FnBody p)  -> FnBody p
  Inc  : Var -> FnBody p                -> FnBody p
  Dec  : Var -> FnBody p                -> FnBody p
Pretty (FnBody p) where
  doc e = case e of
    Ret  v      => text "ret" |++| doc v
    Let  v e b  => text "let" |++| doc v |++| text "=" |++| doc e |+| text ";" |$$| doc b
    Case v alts => text "case" |++| doc v |$$| indent 2 (vsep (map (\(a,b) => parens (indent 1 (doc b) |+| text " ")) alts))
    Inc  v b    => text "inc" |++| doc v |++| text ";" |$$| doc b
    Dec  v b    => text "dec" |++| doc v |++| text ";" |$$| doc b

eqFnBody : FnBody p -> FnBody p -> Bool
eqFnBody b1 b2 = case (b1, b2) of
  (Ret v1, Ret v2)              => v1 == v2
  (Let v1 e1 b1, Let v2 e2 b2)  => v1 == v2 && e1 == e2 && eqFnBody b1 b2
  (Case v1 as1, Case v2 as2)    => v1 == v2 && length as1 == length as2 && and (zipWith (\(a1,c1) , (a2,c2) => a1 == a2 && eqFnBody c1 c2) as1 as2)
  (Inc v1 b1, Inc v2 b2)        => v1 == v2 && eqFnBody b1 b2
  (Dec v1 b1, Dec v2 b2)        => v1 == v2 && eqFnBody b1 b2
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
                      , (MkArity 2, Let h1 (Proj 1 t1)
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

-- data FnBodyF r
--   = RetF Var
--   | LetF Var Expr r
--   | CaseF Var (List r)
--   | IncF Var r
--   | DecF Var r
--
-- Functor FnBodyF where
--   map f x = case x of
--     RetF v     => RetF v
--     LetF v e a => LetF v e (f a)
--     CaseF v as => CaseF v (map f as)
--     IncF v a   => IncF v (f a)
--     DecF v a   => DecF v (f a)
--
-- -- Least fix-point
-- data LFix : (Type -> Type) -> Type where
--   MkLFix : f (LFix f) -> LFix f
--
-- unLFix : LFix f -> f (LFix f)
-- unLFix (MkLFix f) = f
--
-- cata : Functor f => (f a -> a) -> LFix f -> a
-- cata alg = alg . (map (cata alg)) . unLFix
--
-- codata GFix : (Type -> Type) -> Type where
--   MkGFix : f (GFix f) -> GFix f
--
-- ana : Functor f => (a -> f a) -> a -> GFix f
-- ana coAlg = MkGFix . map (ana coAlg) . coAlg
--

-- data Expr
--   = Call Cnst (List Var)
--   | Pap Cnst (List Var)
--   | App Var Var
--   | Ctor Int (List Var)
--   | Proj Int Var
--   | Reset Var
--   | Reuse Var Int (List Var)

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
  Case x bs => Case x <$> traverse (\(arity, body) => do
                                        body' <- resetReuse body
                                        (MkPair arity) <$> (insertReset x arity body'))
                                   bs

resetReuseFun : Monad m => Fun Pure -> NewNameT m (Fun RC)
resetReuseFun (MkFun ps body) = MkFun ps <$> resetReuse body

resetReuseProgram : Monad m => Program Pure -> NewNameT m (Program RC)
resetReuseProgram (MkProgram defs) = MkProgram <$> traverse (\(n,f) => (MkPair n <$> resetReuseFun f)) defs

main : IO ()
main = do
  putStrLn ""
  putStrLn $ toString 1.0 80 $ doc mapExample
  putStrLn $ toString 1.0 80 $ doc $ runNewName $ resetReuseProgram mapExample
  putStrLn ""
  putStrLn $ toString 1.0 80 $ doc swapExample
  putStrLn $ toString 1.0 80 $ doc $ runNewName $ resetReuseProgram swapExample
