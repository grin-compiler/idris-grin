{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, ConstraintKinds #-}
module Idris.CodegenGrin
  ( Options(..)
  , defaultOptions
  , codegenGrin
  ) where

import Control.Exception
import Control.Monad
import Data.Char (ord)
import Data.Functor.Foldable
import Data.List
import Data.String (fromString)
import Debug.Trace
import Grin.ExtendedSyntax.Grin as Grin
import Grin.ExtendedSyntax.Pretty
import IRTS.CodegenCommon
import IRTS.Lang as Idris
import IRTS.Simplified as Idris
import Idris.EvalPrimOp
import Idris.PrimOps
import Pipeline.Pipeline
import Reducer.Pure (EvalPlugin(..))
import System.Directory (removeFile)
import System.Environment
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.Process (callCommand)
import Text.PrettyPrint.ANSI.Leijen (ondullblack)
import Text.Printf
import Text.Show.Pretty hiding (Name)
import Transformations.ExtendedSyntax.BindNormalisation
import Transformations.ExtendedSyntax.StaticSingleAssignment
import Transformations.ExtendedSyntax.Conversion (convert)
import Transformations.ExtendedSyntax.Names

import qualified Data.Text as Text
import qualified Idris.Core.TT as Idris


{-
PLAN
[x] Implement Recursive call only dead param elimination
[ ] Check of duplicate function names
[ ] Implement primitive operations
    [ ] Use Double instead of Float
    [ ] Arithmetic for Integer types
    [ ] Handle BIG Integers
    [ ] Implement IORef with parametric primitives
    [ ] Implement Idris VM and multithreading
    [ ] VMs are handles as pointers to
[ ] Implement C FFI
    [ ] Mapping C types to Idris Tagged types
    [ ] Compilation of C files
    [ ] Linking libraries
    [ ] Re-implement parts of the Idris runtime
[ ] Extend GRIN with LLVM primitives
[ ] Implement appropriate String handling
[ ] Extend Idris-GRIN LLVM codegen
[ ] Memory management
    [ ] Counting immutable beans
    [ ] Simple Garbage Collector
[ ] Compile Idris2
[ ] Explore Idris related transformations
[ ] Threading should be a GRIN primitive
-}

data Options = Options
  { inputs :: [FilePath]
  , output :: FilePath
  , outputGrin :: Bool
  , evalGrin :: Bool
  , evalProgName :: String
  , evalArgs :: [String]
  , optimise :: Bool
  , quiet :: Bool
  , help :: Bool
  , lint :: Bool
  , outputDir :: String
  , deadCodeElim :: Bool -- Interprocedural dead code elimination
  , saveInBinary :: Bool
  , debugSymbols :: Bool
  }

defaultOptions = Options
  { inputs = []
  , output = "a.out"
  , outputGrin = False
  , evalGrin = False
  , evalProgName = "eval"
  , evalArgs = []
  , optimise = True
  , quiet = False
  , help = False
  , lint = True
  , outputDir = ".idris"
  , deadCodeElim = False
  , saveInBinary = False
  , debugSymbols = True
  }

codegenGrin :: Options -> CodegenInfo -> IO ()
codegenGrin o@Options{..} CodegenInfo{..} = do
  hSetBuffering stdout NoBuffering
  postProcessing <- createPostProcessing
  void $ withProgName "IdrisGRIN" $ withArgs evalArgs $ do
    optimizeWith
      (defaultOpts
        { _poOutputDir = outputDir
        , _poFailOnLint = False
        , _poSaveTypeEnv = True
        , _poStatistics = True
        , _poLogging = not quiet
        , _poLintOnChange = lint
        , _poSaveBinary = saveInBinary
        , _poCFiles = ["prim_ops.c", "runtime.c"]
        })
      (convert (program simpleDecls))
      preparation
      (idrisOptimizations o)
      (postProcessing o)

program :: [(Idris.Name, SDecl)] -> Exp
program defs =
  bindNormalisation $
  staticSingleAssignment $
  Program exts $ primOps ++ runEvalNameM (mapM (function . snd) defs)
 where Program exts primOps = idrisPrimOps

function :: SDecl -> NameM Exp
function (SFun fname params _int body) =
  Def (name fname)
      (map (\(p, i) -> packName $ unpackName (name fname) ++ show i) (params `zip` [0..]))
    <$> sexp (name fname) body

loc :: Name -> LVar -> Val
loc fname (Idris.Loc i) = Var $ packName $ unpackName fname ++ show i
loc _ _ = error "loc"

locVal :: Name -> LVar -> Val
locVal fname (Idris.Loc i) = Var $ packName $ unpackName fname ++ show i ++ "_val"
locVal _ _ = error "locVal"

lvar :: Name -> LVar -> Name
lvar fname = \case
  Idris.Loc loc -> packName $ unpackName fname ++ show loc
  Glob nm       -> name nm

lvarVal :: Name -> LVar -> Val
lvarVal fname = Var . \case
  Idris.Loc loc -> packName $ unpackName fname ++ show loc ++ "_val"
  Glob nm       -> packName $ unpackName (name nm) ++ "_val"

sexp :: Name -> SExp -> NameM Exp
sexp fname = \case

  SLet loc0@(Idris.Loc i) v sc -> do
    lhsVal <- deriveNewName "lhs-val"
    EBind
      <$> (SBlock <$> sexp fname v) -- calculate
      <*> (pure (VarPat lhsVal))
      <*> (EBind (SStore lhsVal) (VarPat (var loc0)) -- store
            <$> sexp fname sc)

  Idris.SApp bool nm lvars -> pure $ Grin.SApp (name nm) (map var lvars)

  -- Update is used in eval like functions, where the computed value must be the value
  -- of the expression
  Idris.SUpdate loc0 sexp0 -> do
    updateVal <- deriveNewName "update"
    updateUnitPat <- deriveNewName "update-unit"
    EBind
      <$> (SBlock <$> sexp fname sexp0)
      <*> (pure (VarPat updateVal))
      <*> (pure (EBind
            (Grin.SUpdate (var loc0) updateVal)
            (VarPat updateUnitPat)
            (SReturn (Var updateVal))))

  SCase caseType lvar0 salts -> do
    fetchVal <- deriveNewName "fetch"
    EBind (SFetch (var lvar0)) (VarPat fetchVal)
      <$> ECase fetchVal <$> alts fname salts
  SChkCase lvar0 salts -> do
    fetchVal <- deriveNewName "fetch"
    EBind (SFetch (var lvar0)) (VarPat fetchVal)
      <$> ECase fetchVal <$> alts fname salts

  --SProj lvar0 int -> SFetchI (lvar fname lvar0) (Just int)

  -- All the primitive operations must be part of the runtime, and
  -- they must fetch values, as wrappers
  SOp f lvars -> pure $ primFn f (map var lvars)

  -- Constanst contains only tags and variables, which are locations, thus
  -- it can be the returned as the last
  scon@(SCon maybeLVar int name lvars) -> returnVal fname scon
  sconst@(SConst cnst) -> returnVal fname sconst

  SV lvar0@(Idris.Loc i)  -> pure $ SFetch $ var lvar0
  SV lvar0@(Idris.Glob n) -> traceShow "Global call" $ pure $ Grin.SApp (var lvar0) []

  -- SForeign fdesc1 fdesc2 fdescLVars -> undefined
  -- TODO: Foreign function calls must handle pointers or they must be wrapped.
  SForeign t fun args -> pure $ foreignFun fname t fun args

  SNothing -> pure $ SReturn (ConstTagNode (Tag C "Erased") [])
  SError msg -> do
    idrisErrorVar <- deriveNewName "idrisError"
    pure $
      EBind (SReturn (Lit $ LString $ Text.pack msg)) (VarPat idrisErrorVar) $
            (Grin.SApp "idris_error" [idrisErrorVar])
  e -> error $ printf "unsupported %s" (show e)
  where
    var  = lvar fname
    varV = lvarVal fname

variableName :: Val -> Name
variableName (Var n) = n
variableName _ = error "variableName"

foreignFun fname _ (FStr "idris_int_print") [(_, arg)]
  = Grin.SApp "idris_int_print" [lvar fname $ arg]
foreignFun fname _ (FStr "fileEOF") [(_,lvar0)]
  = Grin.SApp "idris_ffi_file_eof" [lvar fname $ lvar0]
foreignFun fname _ (FStr "idris_usleep") [(_,lvar0)]
  = Grin.SApp "idris_usleep" [lvar fname $ lvar0]
foreignFun fname _ (FStr "idris_time") []
  = Grin.SApp "idris_time" []
foreignFun fname _ (FStr "idris_errno") []
  = Grin.SApp "idris_errno" []
foreignFun fname _ (FStr "fileSize") [(_FCon_C_Ptr, lvar0)]
  = Grin.SApp "idris_fileSize" [lvar fname $ lvar0]
foreignFun fname _ (FStr "fileOpen") [(_FCon_C_Str0, lvar0), (_FCon_C_Str1, lvar1)]
  = Grin.SApp "idris_fileOpen" $ map (lvar fname) [lvar0, lvar1]
foreignFun fname _ (FStr "fileError") [(_FCon_C_Ptr, lvar0)]
  = Grin.SApp "fileError" [lvar fname $ lvar0]
foreignFun fname _ (FStr "fileClose") [(_FCon_C_Ptr, lvar0)]
  = Grin.SApp "idris_fileClose" [lvar fname $ lvar0]
foreignFun fname _ (FStr "idris_addToString") [(_FCon_C_Ptr, lvar0), (_FCon_C_Str, lvar1)]
  = Grin.SApp "idris_addToString" $ map (lvar fname) [lvar0, lvar1]
foreignFun fname _ (FStr "idris_mkFileError") [(_FCon_C_Ptr_VM, lvar0)]
  = Grin.SApp "idris_mkFileError" [lvar fname $ lvar0]
foreignFun fname _ (FStr "idris_getString") [(_FCon_C_Ptr0VM, lvar0), (_FCon_C_Ptr1StrBuffer, lvar1)]
  = Grin.SApp "idris_getString" $ map (lvar fname) [lvar0, lvar1]
foreignFun fname _ (FStr "idris_makeStringBuffer") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative,lvar0)]
  = Grin.SApp "idris_makeStringBuffer" [lvar fname $ lvar0]
foreignFun fname _ (FStr "isNull") [(_FCon_C_Ptr,lvar0)]
  = Grin.SApp "isNull" [lvar fname $ lvar0]

foreignFun fname _ (FStr "idris_newBuffer") [(_FCon_C_Ptr,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative,lvar2)]
  = Grin.SApp "idris_newBuffer" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_newRef") [(_FApp_C_Any_FCon1,lvar1)]
  = Grin.SApp "idris_newRef" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_writeRef") [(_FApp_C_Any_FCon1,lvar1),(_FApp_C_Any_FCon2,lvar2)]
  = Grin.SApp "idris_writeRef" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_copyBuffer") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3),(_FCon_C_MPtr4,lvar4),(_FApp_C_IntT_FUnknown_FCon_C_IntNative5,lvar5)]
  = Grin.SApp "idris_copyBuffer" (map (lvar fname) [lvar1, lvar2, lvar3, lvar4, lvar5])
foreignFun fname _ (FStr "idris_getBufferByte") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2)]
  = Grin.SApp "idris_getBufferByte" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_getBufferDouble") [(_FCon_C_MPtr,lvar1),(_FApp_C_IntT_FUnknown_FCon, lvar2)]
  = Grin.SApp "idris_getBufferDouble" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "fflush") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "fflush" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "exit") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1)]
  = Grin.SApp "exit" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_freeMsg") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_freeMsg" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "free") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "free" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_showerror") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1)]
  = Grin.SApp "idris_showerror" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_numArgs") []
  = Grin.SApp "idris_numArgs" []
foreignFun fname _ (FStr "fgetc") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "fgetc" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "getchar") []
  = Grin.SApp "getchar" []
foreignFun fname _ (FStr "idris_memmove") [(_FCon_C_Ptr1,lvar1),(_FCon_C_Ptr2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3),(_FApp_C_IntT_FUnknown_FCon_C_IntNative4,lvar4),(_FApp_C_IntT_FUnknown_FCon_C_IntNative5,lvar5)]
  = Grin.SApp "idris_memmove" (map (lvar fname) [lvar1, lvar2, lvar3, lvar4, lvar5])
foreignFun fname _ (FStr "calloc") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1),(_FApp_C_IntT2, lvar2)]
  = Grin.SApp "calloc" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_getSender") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_getSender" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_getArg") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1)]
  = Grin.SApp "idris_getArg" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_readRef") [(_FApp_C_Any_FCon1,lvar1)]
  = Grin.SApp "idris_readRef" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_readBuffer") [(_FCon_C_Ptr1,lvar1),(_FCon_C_MPtr2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3),(_FApp_C_IntT_FUnknown_FCon_C_IntNative4,lvar4)]
  = Grin.SApp "idris_readBuffer" (map (lvar fname) [lvar1, lvar2, lvar3, lvar4])
foreignFun fname _ (FStr "idris_getBufferInt") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2)]
  = Grin.SApp "idris_getBufferInt" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "putchar") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1)]
  = Grin.SApp "putchar" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_memset") [(_FCon_C_Ptr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntBits8_3,lvar3),(_FApp_C_IntT_FUnknown_FCon_C_IntNative4,lvar4)]
  = Grin.SApp "idris_memset" (map (lvar fname) [lvar1, lvar2, lvar3, lvar4])
foreignFun fname _ (FStr "idris_getChannel") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_getChannel" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_setBufferByte") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntBits8_3,lvar3)]
  = Grin.SApp "idris_setBufferByte" (map (lvar fname) [lvar1, lvar2, lvar3])
foreignFun fname _ (FStr "idris_getBufferString") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3)]
  = Grin.SApp "idris_getBufferString" (map (lvar fname) [lvar1, lvar2, lvar3])
foreignFun fname _ (FStr "idris_peek") [(_FCon_C_Ptr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2)]
  = Grin.SApp "idris_peek" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_recvMessage") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_recvMessage" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_checkMessages") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_checkMessages" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_setBufferString") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FCon_C_Str3,lvar3)]
  = Grin.SApp "idris_setBufferString" (map (lvar fname) [lvar1, lvar2, lvar3])
foreignFun fname _ (FStr "idris_setBufferDouble") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FCon_C_Float3,lvar3)]
  = Grin.SApp "idris_setBufferDouble" (map (lvar fname) [lvar1, lvar2, lvar3])
foreignFun fname _ (FStr "idris_poke") [(_FCon_C_Ptr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntBits8_3,lvar3)]
  = Grin.SApp "idris_poke" (map (lvar fname) [lvar1, lvar2, lvar3])
foreignFun fname _ (FStr "idris_checkMessagesTimeout") [(_FCon_C_Ptr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2)]
  = Grin.SApp "idris_checkMessagesTimeout" (map (lvar fname) [lvar1, lvar2])
foreignFun fname _ (FStr "idris_getMsg") [(_FCon_C_Ptr1,lvar1)]
  = Grin.SApp "idris_getMsg" (map (lvar fname) [lvar1])
foreignFun fname _ (FStr "idris_sendMessage") [(_FCon_C_Ptr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FCon_C_Ptr3,lvar3),(_FApp_C_Any_FCon4,lvar4)]
  = Grin.SApp "idris_sendMessage" (map (lvar fname) [lvar1,lvar2,lvar3,lvar4])
foreignFun fname _ (FStr "idris_writeBuffer") [(_FCon_C_Ptr1,lvar1),(_FCon_C_MPtr2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3),(_FApp_C_IntT_FUnknown_FCon_C_IntNative4,lvar4)]
  = Grin.SApp "idris_writeBuffer" (map (lvar fname) [lvar1,lvar2,lvar3,lvar4])
foreignFun fname _ (FStr "idris_setBufferInt") [(_FCon_C_MPtr1,lvar1),(_FApp_C_IntT_FUnknown_FCon_C_IntNative2,lvar2),(_FApp_C_IntT_FUnknown_FCon_C_IntNative3,lvar3)]
  = Grin.SApp "idris_setBufferInt" (map (lvar fname) [lvar1,lvar2,lvar3])
foreignFun fname _ (FStr "malloc") [(_FApp_C_IntT_FUnknown_FCon_C_IntNative1,lvar1)]
  = Grin.SApp "malloc" (map (lvar fname) [lvar1])
foreignFun fname _ rest args = error $ show rest ++ " " ++ show args

alts :: Name -> [SAlt] -> NameM [Exp]
alts fname as = do
  let (con0, cons0, defs0) = groupAlternatives as
  let defs1 = take 1 $ (if (length defs0 > 1) then (traceShow ("More than one def:",defs0)) else id) defs0
  let cons1 = constAlternativesByTag cons0
  con2  <- mapM (constructorAlt fname) con0
  defs2 <- mapM (defaultAlt fname) defs1
  cons2 <- mapM (constantAlts fname defs2) cons1
  pure $ concat [con2, cons2, defs2]

groupAlternatives :: [SAlt] -> ([SAlt], [SAlt], [SAlt])
groupAlternatives = go ([],[],[])
  where
    go (con, cons, def) = \case
      []     -> (reverse con, reverse cons, reverse def)
      (a:as) -> case a of
        SConCase{}      -> go (a:con, cons, def) as
        SConstCase{}    -> go (con, a:cons, def) as
        SDefaultCase{}  -> go (con, cons, a:def) as

constAlternativesByTag :: [SAlt] -> [[SAlt]]
constAlternativesByTag = groupBy sameTag
  where
    sameTag (SConstCase c1 _) (SConstCase c2 _) = literalTag c1 == literalTag c2
    sameTag _                 _                 = error "constAlternativesByTag"

constructorAlt :: Name -> SAlt -> NameM Exp
constructorAlt fname (SConCase startIdx t nm names sexp0) =
  Alt (NodePat (Tag C (name nm))
        (map (\(i,_n) -> packName $ unpackName fname ++ show i) ([startIdx ..] `zip` names)))
      <$> deriveNewName "cnstr-alt"
      <*> sexp fname sexp0
constructorAlt _ _ = error "constructorAlt"

constantAlts :: Name -> [Exp] -> [SAlt] -> NameM Exp
constantAlts fname defs as@(a@(SConstCase cnst _):_) =
  Alt (NodePat tag [cpatVar])
    <$> deriveNewName "cst-alt"
    <*> (ECase cpatVar <$> do-- TODO
          alts <- (forM as $ \(SConstCase c e) -> do
                    let (t, l) = literalTagAndParams c
                    Alt (LitPat l) <$> deriveNewName "cst-alt-pat" <*> sexp fname e)
          pure $ alts ++ defs)
  where
    (tag,lit) = literalTagAndParams cnst
    cpatVar = packName $ unpackName fname ++ "_cpat_" ++ (map (\case { ' ' -> '_'; c -> c}) (show lit))
constantAlts _ _ _ = error "constantAlts"

defaultAlt :: Name -> SAlt -> NameM Exp
defaultAlt fname (SDefaultCase sexp0) = Alt DefaultPat <$> deriveNewName "def-alt" <*> sexp fname sexp0
defaultAlt _ _ = error "defaultAlt"

primFn :: Idris.PrimFn -> [Name] -> Exp
primFn f ps = case f of
  LPlus   (Idris.ATInt Idris.ITChar)                -> Grin.SApp "idris_int_add" ps
  LPlus   (Idris.ATInt Idris.ITBig)                 -> Grin.SApp "idris_int_add" ps
  LPlus   (Idris.ATInt Idris.ITNative)              -> Grin.SApp "idris_int_add" ps
  LPlus   (Idris.ATInt (Idris.ITFixed Idris.IT8))   -> undefined
  LPlus   (Idris.ATInt (Idris.ITFixed Idris.IT16))  -> undefined
  LPlus   (Idris.ATInt (Idris.ITFixed Idris.IT32))  -> undefined
  LPlus   (Idris.ATInt (Idris.ITFixed Idris.IT64))  -> Grin.SApp "idris_bit64_add" ps
  LPlus   Idris.ATFloat                             -> Grin.SApp "idris_float_add" ps

  LMinus  (Idris.ATInt Idris.ITChar)                -> Grin.SApp "idris_int_sub" ps
  LMinus  (Idris.ATInt Idris.ITBig)                 -> Grin.SApp "idris_int_sub" ps
  LMinus  (Idris.ATInt Idris.ITNative)              -> Grin.SApp "idris_int_sub" ps
  LMinus  (Idris.ATInt (Idris.ITFixed Idris.IT8))   -> undefined
  LMinus  (Idris.ATInt (Idris.ITFixed Idris.IT16))  -> undefined
  LMinus  (Idris.ATInt (Idris.ITFixed Idris.IT32))  -> undefined
  LMinus  (Idris.ATInt (Idris.ITFixed Idris.IT64))  -> undefined
  LMinus  Idris.ATFloat                             -> Grin.SApp "idris_float_sub" ps

  LTimes  (Idris.ATInt (Idris.ITChar))              -> Grin.SApp "idris_int_mul" ps
  LTimes  (Idris.ATInt (Idris.ITBig))               -> Grin.SApp "idris_int_mul" ps
  LTimes  (Idris.ATInt (Idris.ITNative))            -> Grin.SApp "idris_int_mul" ps
  LTimes  (Idris.ATInt (Idris.ITFixed Idris.IT8))   -> undefined
  LTimes  (Idris.ATInt (Idris.ITFixed Idris.IT16))  -> undefined
  LTimes  (Idris.ATInt (Idris.ITFixed Idris.IT32))  -> undefined
  LTimes  (Idris.ATInt (Idris.ITFixed Idris.IT64))  -> Grin.SApp "idris_bit64_mul" ps
  LTimes  Idris.ATFloat                             -> Grin.SApp "idris_float_mul" ps

  LSDiv  (Idris.ATInt (Idris.ITChar))               -> Grin.SApp "idris_int_div" ps
  LSDiv  (Idris.ATInt (Idris.ITBig))                -> Grin.SApp "idris_int_div" ps
  LSDiv  (Idris.ATInt (Idris.ITNative))             -> Grin.SApp "idris_int_div" ps
  LSDiv  (Idris.ATInt (Idris.ITFixed Idris.IT8))    -> undefined
  LSDiv  (Idris.ATInt (Idris.ITFixed Idris.IT16))   -> undefined
  LSDiv  (Idris.ATInt (Idris.ITFixed Idris.IT32))   -> undefined
  LSDiv  (Idris.ATInt (Idris.ITFixed Idris.IT64))   -> undefined
  LSDiv  Idris.ATFloat                              -> Grin.SApp "idris_float_div" ps

  LUDiv (Idris.ITChar)                              -> Grin.SApp "idris_int_udiv" ps
  LUDiv (Idris.ITBig)                               -> Grin.SApp "idris_int_udiv" ps
  LUDiv (Idris.ITNative)                            -> Grin.SApp "idris_int_udiv" ps
  LUDiv (Idris.ITFixed Idris.IT8)                   -> Grin.SApp "idris_bit8_udiv" ps
  LUDiv (Idris.ITFixed Idris.IT16)                  -> Grin.SApp "idris_bit16_udiv" ps
  LUDiv (Idris.ITFixed Idris.IT32)                  -> Grin.SApp "idris_bit32_udiv" ps
  LUDiv (Idris.ITFixed Idris.IT64)                  -> Grin.SApp "idris_bit64_udiv" ps

  LURem (Idris.ITChar)                              -> Grin.SApp "idris_int_urem" ps
  LURem (Idris.ITBig)                               -> Grin.SApp "idris_int_urem" ps
  LURem (Idris.ITNative)                            -> Grin.SApp "idris_int_urem" ps
  LURem (Idris.ITFixed Idris.IT8)                   -> Grin.SApp "idris_bit8_urem" ps
  LURem (Idris.ITFixed Idris.IT16)                  -> Grin.SApp "idris_bit16_urem" ps
  LURem (Idris.ITFixed Idris.IT32)                  -> Grin.SApp "idris_bit32_urem" ps
  LURem (Idris.ITFixed Idris.IT64)                  -> Grin.SApp "idris_bit64_urem" ps

  LSRem (Idris.ATInt (Idris.ITChar))                -> Grin.SApp "idris_int_rem" ps
  LSRem (Idris.ATInt (Idris.ITBig))                 -> Grin.SApp "idris_int_rem" ps
  LSRem (Idris.ATInt (Idris.ITNative))              -> Grin.SApp "idris_int_rem" ps
  LSRem (Idris.ATInt (Idris.ITFixed Idris.IT8))     -> undefined
  LSRem (Idris.ATInt (Idris.ITFixed Idris.IT16))    -> undefined
  LSRem (Idris.ATInt (Idris.ITFixed Idris.IT32))    -> undefined
  LSRem (Idris.ATInt (Idris.ITFixed Idris.IT64))    -> undefined
  LSRem Idris.ATFloat                               -> undefined

  LAnd (Idris.ITChar)                               -> Grin.SApp "idris_int_and" ps
  LAnd (Idris.ITBig)                                -> Grin.SApp "idris_int_and" ps
  LAnd (Idris.ITNative)                             -> Grin.SApp "idris_int_and" ps
  LAnd (Idris.ITFixed Idris.IT8)                    -> Grin.SApp "idris_bit8_and" ps
  LAnd (Idris.ITFixed Idris.IT16)                   -> undefined
  LAnd (Idris.ITFixed Idris.IT32)                   -> undefined
  LAnd (Idris.ITFixed Idris.IT64)                   -> Grin.SApp "idris_bit64_and" ps

  LOr (Idris.ITChar)                                -> undefined
  LOr (Idris.ITBig)                                 -> undefined
  LOr (Idris.ITNative)                              -> undefined
  LOr (Idris.ITFixed Idris.IT8)                     -> undefined
  LOr (Idris.ITFixed Idris.IT16)                    -> undefined
  LOr (Idris.ITFixed Idris.IT32)                    -> undefined
  LOr (Idris.ITFixed Idris.IT64)                    -> undefined

  LXOr (Idris.ITChar)                               -> undefined
  LXOr (Idris.ITBig)                                -> undefined
  LXOr (Idris.ITNative)                             -> undefined
  LXOr (Idris.ITFixed Idris.IT8)                    -> undefined
  LXOr (Idris.ITFixed Idris.IT16)                   -> undefined
  LXOr (Idris.ITFixed Idris.IT32)                   -> undefined
  LXOr (Idris.ITFixed Idris.IT64)                   -> undefined

  LCompl (Idris.ITChar)                             -> undefined
  LCompl (Idris.ITBig)                              -> undefined
  LCompl (Idris.ITNative)                           -> undefined
  LCompl (Idris.ITFixed Idris.IT8)                  -> undefined
  LCompl (Idris.ITFixed Idris.IT16)                 -> undefined
  LCompl (Idris.ITFixed Idris.IT32)                 -> undefined
  LCompl (Idris.ITFixed Idris.IT64)                 -> undefined

  LSHL (Idris.ITChar)                           -> Grin.SApp "idris_int_shl" ps
  LSHL (Idris.ITBig)                            -> Grin.SApp "idris_int_shl" ps
  LSHL (Idris.ITNative)                         -> Grin.SApp "idris_int_shl" ps
  LSHL (Idris.ITFixed Idris.IT8)                -> undefined
  LSHL (Idris.ITFixed Idris.IT16)               -> undefined
  LSHL (Idris.ITFixed Idris.IT32)               -> undefined
  LSHL (Idris.ITFixed Idris.IT64)               -> Grin.SApp "idris_bit64_shl" ps

  LLSHR (Idris.ITChar)                          -> Grin.SApp "idris_int_lshr" ps
  LLSHR (Idris.ITBig)                           -> Grin.SApp "idris_int_lshr" ps
  LLSHR (Idris.ITNative)                        -> Grin.SApp "idris_int_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT8)               -> Grin.SApp "idris_bit8_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT16)              -> Grin.SApp "idris_bit16_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT32)              -> Grin.SApp "idris_bit32_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT64)              -> Grin.SApp "idris_bit64_lshr" ps

  LASHR Idris.ITChar                            -> Grin.SApp "idris_lashr_int" ps
  LASHR Idris.ITBig                             -> Grin.SApp "idris_lashr_int" ps
  LASHR Idris.ITNative                          -> Grin.SApp "idris_lashr_int" ps
  LASHR (Idris.ITFixed Idris.IT8)               -> undefined
  LASHR (Idris.ITFixed Idris.IT16)              -> undefined
  LASHR (Idris.ITFixed Idris.IT32)              -> undefined
  LASHR (Idris.ITFixed Idris.IT64)              -> undefined

  LEq (Idris.ATInt Idris.ITChar)                -> Grin.SApp "idris_int_eq" ps
  LEq (Idris.ATInt Idris.ITBig)                 -> Grin.SApp "idris_int_eq" ps
  LEq (Idris.ATInt Idris.ITNative)              -> Grin.SApp "idris_int_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed Idris.IT8))   -> Grin.SApp "idris_bit8_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed Idris.IT16))  -> Grin.SApp "idris_bit16_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed Idris.IT32))  -> Grin.SApp "idris_bit32_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed Idris.IT64))  -> Grin.SApp "idris_bit64_eq" ps
  LEq Idris.ATFloat                             -> Grin.SApp "idris_float_eq" ps

  LSLt (Idris.ATInt Idris.ITChar)               -> Grin.SApp "idris_int_lt" ps
  LSLt (Idris.ATInt Idris.ITBig)                -> Grin.SApp "idris_int_lt" ps
  LSLt (Idris.ATInt Idris.ITNative)             -> Grin.SApp "idris_int_lt" ps
  LSLt (Idris.ATInt (Idris.ITFixed Idris.IT8))  -> undefined
  LSLt (Idris.ATInt (Idris.ITFixed Idris.IT16)) -> undefined
  LSLt (Idris.ATInt (Idris.ITFixed Idris.IT32)) -> undefined
  LSLt (Idris.ATInt (Idris.ITFixed Idris.IT64)) -> undefined
  LSLt Idris.ATFloat                            -> Grin.SApp "idris_float_lt" ps

  LSLe (Idris.ATInt Idris.ITChar)               -> Grin.SApp "idris_int_le" ps
  LSLe (Idris.ATInt Idris.ITBig)                -> Grin.SApp "idris_int_le" ps
  LSLe (Idris.ATInt Idris.ITNative)             -> Grin.SApp "idris_int_le" ps
  LSLe (Idris.ATInt (Idris.ITFixed Idris.IT8))  -> undefined
  LSLe (Idris.ATInt (Idris.ITFixed Idris.IT16)) -> undefined
  LSLe (Idris.ATInt (Idris.ITFixed Idris.IT32)) -> undefined
  LSLe (Idris.ATInt (Idris.ITFixed Idris.IT64)) -> undefined
  LSLe Idris.ATFloat                            -> undefined

  LSGt (Idris.ATInt Idris.ITChar)               -> Grin.SApp "idris_int_gt" ps
  LSGt (Idris.ATInt Idris.ITBig)                -> Grin.SApp "idris_int_gt" ps
  LSGt (Idris.ATInt Idris.ITNative)             -> Grin.SApp "idris_int_gt" ps
  LSGt (Idris.ATInt (Idris.ITFixed Idris.IT8))  -> undefined
  LSGt (Idris.ATInt (Idris.ITFixed Idris.IT16)) -> undefined
  LSGt (Idris.ATInt (Idris.ITFixed Idris.IT32)) -> undefined
  LSGt (Idris.ATInt (Idris.ITFixed Idris.IT64)) -> undefined
  LSGt Idris.ATFloat                            -> undefined

  LSGe (Idris.ATInt Idris.ITChar)               -> Grin.SApp "idris_int_ge" ps
  LSGe (Idris.ATInt Idris.ITBig)                -> Grin.SApp "idris_int_ge" ps
  LSGe (Idris.ATInt Idris.ITNative)             -> Grin.SApp "idris_int_ge" ps
  LSGe (Idris.ATInt (Idris.ITFixed Idris.IT8))  -> undefined
  LSGe (Idris.ATInt (Idris.ITFixed Idris.IT16)) -> undefined
  LSGe (Idris.ATInt (Idris.ITFixed Idris.IT32)) -> undefined
  LSGe (Idris.ATInt (Idris.ITFixed Idris.IT64)) -> undefined
  LSGe Idris.ATFloat                            -> undefined

  LLt Idris.ITChar                              -> Grin.SApp "_prim_int_lt" ps
  LLt Idris.ITBig                               -> Grin.SApp "_prim_int_lt" ps
  LLt Idris.ITNative                            -> Grin.SApp "_prim_int_lt" ps
  LLt (Idris.ITFixed Idris.IT8)                 -> undefined
  LLt (Idris.ITFixed Idris.IT16)                -> undefined
  LLt (Idris.ITFixed Idris.IT32)                -> undefined
  LLt (Idris.ITFixed Idris.IT64)                -> undefined

  LLe Idris.ITChar                              -> Grin.SApp "_prim_int_le" ps
  LLe Idris.ITBig                               -> Grin.SApp "_prim_int_le" ps
  LLe Idris.ITNative                            -> Grin.SApp "_prim_int_le" ps
  LLe (Idris.ITFixed Idris.IT8)                 -> undefined
  LLe (Idris.ITFixed Idris.IT16)                -> undefined
  LLe (Idris.ITFixed Idris.IT32)                -> undefined
  LLe (Idris.ITFixed Idris.IT64)                -> undefined

  LGt Idris.ITChar                              -> Grin.SApp "_prim_int_gt" ps
  LGt Idris.ITBig                               -> Grin.SApp "_prim_int_gt" ps
  LGt Idris.ITNative                            -> Grin.SApp "_prim_int_gt" ps
  LGt (Idris.ITFixed Idris.IT8)                 -> undefined
  LGt (Idris.ITFixed Idris.IT16)                -> undefined
  LGt (Idris.ITFixed Idris.IT32)                -> undefined
  LGt (Idris.ITFixed Idris.IT64)                -> undefined

  LGe Idris.ITChar                              -> Grin.SApp "_prim_int_ge" ps
  LGe Idris.ITBig                               -> Grin.SApp "_prim_int_ge" ps
  LGe Idris.ITNative                            -> Grin.SApp "_prim_int_ge" ps
  LGe (Idris.ITFixed Idris.IT8)                 -> undefined
  LGe (Idris.ITFixed Idris.IT16)                -> undefined
  LGe (Idris.ITFixed Idris.IT32)                -> undefined
  LGe (Idris.ITFixed Idris.IT64)                -> undefined

  LSExt Idris.ITChar Idris.ITChar                           -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITChar Idris.ITBig                            -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITChar Idris.ITNative                         -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITChar (Idris.ITFixed Idris.IT8)              -> undefined
  LSExt Idris.ITChar (Idris.ITFixed Idris.IT16)             -> undefined
  LSExt Idris.ITChar (Idris.ITFixed Idris.IT32)             -> undefined
  LSExt Idris.ITChar (Idris.ITFixed Idris.IT64)             -> undefined

  LSExt Idris.ITBig Idris.ITChar                            -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITBig Idris.ITBig                             -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITBig Idris.ITNative                          -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITBig (Idris.ITFixed Idris.IT8)               -> undefined
  LSExt Idris.ITBig (Idris.ITFixed Idris.IT16)              -> undefined
  LSExt Idris.ITBig (Idris.ITFixed Idris.IT32)              -> undefined
  LSExt Idris.ITBig (Idris.ITFixed Idris.IT64)              -> undefined

  LSExt Idris.ITNative Idris.ITChar                           -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITNative Idris.ITBig                            -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITNative Idris.ITNative                         -> Grin.SApp "idris_ls_ext" ps
  LSExt Idris.ITNative (Idris.ITFixed Idris.IT8)              -> undefined
  LSExt Idris.ITNative (Idris.ITFixed Idris.IT16)             -> undefined
  LSExt Idris.ITNative (Idris.ITFixed Idris.IT32)             -> undefined
  LSExt Idris.ITNative (Idris.ITFixed Idris.IT64)             -> Grin.SApp "idris_lz_ext_int_bit64" ps

  LSExt (Idris.ITFixed Idris.IT8) Idris.ITChar                -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LSExt (Idris.ITFixed Idris.IT8) Idris.ITBig                 -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LSExt (Idris.ITFixed Idris.IT8) Idris.ITNative              -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LSExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT8)   -> undefined
  LSExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT16)  -> undefined
  LSExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT32)  -> undefined
  LSExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT64)  -> Grin.SApp "idris_lz_ext_bit8_bit64" ps

  LSExt (Idris.ITFixed Idris.IT16) Idris.ITChar                -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT16) Idris.ITBig                 -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT16) Idris.ITNative              -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT8)   -> undefined
  LSExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT16)  -> undefined
  LSExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT32)  -> undefined
  LSExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT64)  -> undefined

  LSExt (Idris.ITFixed Idris.IT32) Idris.ITChar                -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT32) Idris.ITBig                 -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT32) Idris.ITNative              -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT8)   -> undefined
  LSExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT16)  -> undefined
  LSExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT32)  -> undefined
  LSExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT64)  -> undefined

  LSExt (Idris.ITFixed Idris.IT64) Idris.ITChar                -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT64) Idris.ITBig                 -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT64) Idris.ITNative              -> Grin.SApp "idris_ls_ext" ps
  LSExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT8)   -> undefined
  LSExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT16)  -> undefined
  LSExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT32)  -> undefined
  LSExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT64)  -> undefined

  LZExt Idris.ITChar Idris.ITChar                           -> undefined
  LZExt Idris.ITChar Idris.ITBig                            -> undefined
  LZExt Idris.ITChar Idris.ITNative                         -> undefined
  LZExt Idris.ITChar (Idris.ITFixed Idris.IT8)              -> undefined
  LZExt Idris.ITChar (Idris.ITFixed Idris.IT16)             -> undefined
  LZExt Idris.ITChar (Idris.ITFixed Idris.IT32)             -> undefined
  LZExt Idris.ITChar (Idris.ITFixed Idris.IT64)             -> undefined

  LZExt Idris.ITBig Idris.ITChar                            -> undefined
  LZExt Idris.ITBig Idris.ITBig                             -> undefined
  LZExt Idris.ITBig Idris.ITNative                          -> undefined
  LZExt Idris.ITBig (Idris.ITFixed Idris.IT8)               -> undefined
  LZExt Idris.ITBig (Idris.ITFixed Idris.IT16)              -> undefined
  LZExt Idris.ITBig (Idris.ITFixed Idris.IT32)              -> undefined
  LZExt Idris.ITBig (Idris.ITFixed Idris.IT64)              -> undefined

  LZExt Idris.ITNative Idris.ITChar                           -> undefined
  LZExt Idris.ITNative Idris.ITBig                            -> Grin.SApp "idris_lz_ext_int_bigint" ps
  LZExt Idris.ITNative Idris.ITNative                         -> undefined
  LZExt Idris.ITNative (Idris.ITFixed Idris.IT8)              -> undefined
  LZExt Idris.ITNative (Idris.ITFixed Idris.IT16)             -> undefined
  LZExt Idris.ITNative (Idris.ITFixed Idris.IT32)             -> undefined
  LZExt Idris.ITNative (Idris.ITFixed Idris.IT64)             -> Grin.SApp "idris_lz_ext_int_bit64" ps

  LZExt (Idris.ITFixed Idris.IT8) Idris.ITChar                -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LZExt (Idris.ITFixed Idris.IT8) Idris.ITBig                 -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LZExt (Idris.ITFixed Idris.IT8) Idris.ITNative              -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LZExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT8)   -> undefined
  LZExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT16)  -> undefined
  LZExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT32)  -> undefined
  LZExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT64)  -> Grin.SApp "idris_lz_ext_bit8_bit64" ps

  LZExt (Idris.ITFixed Idris.IT16) Idris.ITChar                -> undefined
  LZExt (Idris.ITFixed Idris.IT16) Idris.ITBig                 -> undefined
  LZExt (Idris.ITFixed Idris.IT16) Idris.ITNative              -> undefined
  LZExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT8)   -> undefined
  LZExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT16)  -> undefined
  LZExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT32)  -> undefined
  LZExt (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT64)  -> undefined

  LZExt (Idris.ITFixed Idris.IT32) Idris.ITChar                -> undefined
  LZExt (Idris.ITFixed Idris.IT32) Idris.ITBig                 -> undefined
  LZExt (Idris.ITFixed Idris.IT32) Idris.ITNative              -> undefined
  LZExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT8)   -> undefined
  LZExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT16)  -> undefined
  LZExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT32)  -> undefined
  LZExt (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT64)  -> undefined

  LZExt (Idris.ITFixed Idris.IT64) Idris.ITChar                -> undefined
  LZExt (Idris.ITFixed Idris.IT64) Idris.ITBig                 -> undefined
  LZExt (Idris.ITFixed Idris.IT64) Idris.ITNative              -> undefined
  LZExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT8)   -> undefined
  LZExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT16)  -> undefined
  LZExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT32)  -> undefined
  LZExt (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT64)  -> undefined

  LTrunc Idris.ITChar Idris.ITChar                              -> undefined
  LTrunc Idris.ITChar Idris.ITBig                               -> undefined
  LTrunc Idris.ITChar Idris.ITNative                            -> undefined
  LTrunc Idris.ITChar (Idris.ITFixed Idris.IT8)                 -> undefined
  LTrunc Idris.ITChar (Idris.ITFixed Idris.IT16)                -> undefined
  LTrunc Idris.ITChar (Idris.ITFixed Idris.IT32)                -> undefined
  LTrunc Idris.ITChar (Idris.ITFixed Idris.IT64)                -> undefined

  LTrunc Idris.ITBig Idris.ITChar                               -> undefined
  LTrunc Idris.ITBig Idris.ITBig                                -> undefined
  LTrunc Idris.ITBig Idris.ITNative                             -> Grin.SApp "idris_ltrunc_big_int" ps
  LTrunc Idris.ITBig (Idris.ITFixed Idris.IT8)                  -> undefined
  LTrunc Idris.ITBig (Idris.ITFixed Idris.IT16)                 -> undefined
  LTrunc Idris.ITBig (Idris.ITFixed Idris.IT32)                 -> undefined
  LTrunc Idris.ITBig (Idris.ITFixed Idris.IT64)                 -> Grin.SApp "idris_ltrunc_big_bit64" ps

  LTrunc Idris.ITNative Idris.ITChar                            -> undefined
  LTrunc Idris.ITNative Idris.ITBig                             -> undefined
  LTrunc Idris.ITNative Idris.ITNative                          -> undefined
  LTrunc Idris.ITNative (Idris.ITFixed Idris.IT8)               -> Grin.SApp "idris_ltrunc_int_bit8" ps
  LTrunc Idris.ITNative (Idris.ITFixed Idris.IT16)              -> undefined
  LTrunc Idris.ITNative (Idris.ITFixed Idris.IT32)              -> undefined
  LTrunc Idris.ITNative (Idris.ITFixed Idris.IT64)              -> undefined

  LTrunc (Idris.ITFixed Idris.IT8) Idris.ITChar                 -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) Idris.ITBig                  -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) Idris.ITNative               -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT8)    -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT16)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT32)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT64)   -> undefined

  LTrunc (Idris.ITFixed Idris.IT16) Idris.ITChar                 -> undefined
  LTrunc (Idris.ITFixed Idris.IT16) Idris.ITBig                  -> undefined
  LTrunc (Idris.ITFixed Idris.IT16) Idris.ITNative               -> undefined
  LTrunc (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT8)    -> Grin.SApp "idris_ltrunc_bit16_bit8" ps
  LTrunc (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT16)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT32)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT64)   -> undefined

  LTrunc (Idris.ITFixed Idris.IT32) Idris.ITChar                 -> undefined
  LTrunc (Idris.ITFixed Idris.IT32) Idris.ITBig                  -> undefined
  LTrunc (Idris.ITFixed Idris.IT32) Idris.ITNative               -> undefined
  LTrunc (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT8)    -> Grin.SApp "idris_ltrunc_bit32_bit8" ps
  LTrunc (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT16)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT32)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT64)   -> undefined

  LTrunc (Idris.ITFixed Idris.IT64) Idris.ITChar                 -> undefined
  LTrunc (Idris.ITFixed Idris.IT64) Idris.ITBig                  -> undefined
  LTrunc (Idris.ITFixed Idris.IT64) Idris.ITNative               -> undefined
  LTrunc (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT8)    -> Grin.SApp "idris_ltrunc_bit64_bit8" ps
  LTrunc (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT16)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT32)   -> undefined
  LTrunc (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT64)   -> undefined

  LStrConcat                                                      -> Grin.SApp "idris_str_concat" ps
  LStrLt                                                          -> Grin.SApp "idris_str_lt" ps
  LStrEq                                                          -> Grin.SApp "idris_str_eq" ps
  LStrLen                                                         -> Grin.SApp "idris_str_len" ps

  LIntFloat Idris.ITChar                -> Grin.SApp "idris_int_float" ps
  LIntFloat Idris.ITBig                 -> Grin.SApp "idris_int_float" ps
  LIntFloat Idris.ITNative              -> Grin.SApp "idris_int_float" ps
  LIntFloat (Idris.ITFixed Idris.IT8)   -> undefined
  LIntFloat (Idris.ITFixed Idris.IT16)  -> undefined
  LIntFloat (Idris.ITFixed Idris.IT32)  -> undefined
  LIntFloat (Idris.ITFixed Idris.IT64)  -> undefined

  LFloatInt Idris.ITChar                -> Grin.SApp "idris_float_int" ps
  LFloatInt Idris.ITBig                 -> Grin.SApp "idris_float_int" ps
  LFloatInt Idris.ITNative              -> Grin.SApp "idris_float_int" ps
  LFloatInt (Idris.ITFixed Idris.IT8)   -> undefined
  LFloatInt (Idris.ITFixed Idris.IT16)  -> undefined
  LFloatInt (Idris.ITFixed Idris.IT32)  -> undefined
  LFloatInt (Idris.ITFixed Idris.IT64)  -> undefined

  LIntStr Idris.ITChar                -> Grin.SApp "idris_int_str"    ps
  LIntStr Idris.ITBig                 -> Grin.SApp "idris_int_str"    ps
  LIntStr Idris.ITNative              -> Grin.SApp "idris_int_str"    ps
  LIntStr (Idris.ITFixed Idris.IT8)   -> undefined
  LIntStr (Idris.ITFixed Idris.IT16)  -> undefined
  LIntStr (Idris.ITFixed Idris.IT32)  -> undefined
  LIntStr (Idris.ITFixed Idris.IT64)  -> undefined

  LStrInt Idris.ITChar                -> Grin.SApp "idris_str_int"    ps
  LStrInt Idris.ITBig                 -> Grin.SApp "idris_str_int"    ps
  LStrInt Idris.ITNative              -> Grin.SApp "idris_str_int"    ps
  LStrInt (Idris.ITFixed Idris.IT8)   -> undefined
  LStrInt (Idris.ITFixed Idris.IT16)  -> undefined
  LStrInt (Idris.ITFixed Idris.IT32)  -> undefined
  LStrInt (Idris.ITFixed Idris.IT64)  -> undefined

  LFloatStr     -> Grin.SApp "idris_float_str"  ps
  LStrFloat     -> Grin.SApp "idris_str_float"  ps

  LChInt Idris.ITChar                 -> Grin.SApp "idris_ch_int"     ps
  LChInt Idris.ITBig                  -> Grin.SApp "idris_ch_int"     ps
  LChInt Idris.ITNative               -> Grin.SApp "idris_ch_int"     ps
  LChInt (Idris.ITFixed Idris.IT8)    -> undefined
  LChInt (Idris.ITFixed Idris.IT16)   -> undefined
  LChInt (Idris.ITFixed Idris.IT32)   -> undefined
  LChInt (Idris.ITFixed Idris.IT64)   -> undefined

  LIntCh Idris.ITChar                 -> Grin.SApp "idris_int_ch"     ps
  LIntCh Idris.ITBig                  -> Grin.SApp "idris_int_ch"     ps
  LIntCh Idris.ITNative               -> Grin.SApp "idris_int_ch"     ps
  LIntCh (Idris.ITFixed Idris.IT8)    -> undefined
  LIntCh (Idris.ITFixed Idris.IT16)   -> undefined
  LIntCh (Idris.ITFixed Idris.IT32)   -> undefined
  LIntCh (Idris.ITFixed Idris.IT64)   -> undefined

  LBitCast arithTy1 arithTy2 -> undefined -- Only for values of equal width

  LFExp -> undefined
  LFLog -> undefined
  LFSin -> undefined
  LFCos -> undefined
  LFTan -> undefined
  LFASin -> undefined
  LFACos -> undefined
  LFATan -> undefined
  LFATan2 -> Grin.SApp "idris_float_atan2" ps
  LFSqrt -> undefined
  LFFloor -> Grin.SApp "idris_float_floor" ps
  LFCeil  -> Grin.SApp "idris_float_ceil"  ps
  LFNegate -> undefined

  LStrHead  -> Grin.SApp "idris_str_head" ps
  LStrTail  -> Grin.SApp "idris_str_tail" ps
  LStrCons  -> Grin.SApp "idris_str_cons" ps
  LStrIndex -> Grin.SApp "idris_str_idx"  ps
  LStrRev   -> Grin.SApp "idris_str_rev"  ps
  LStrSubstr -> Grin.SApp "idris_str_sub" ps
  LReadStr -> Grin.SApp "idris_read_str" ps
  LWriteStr -> Grin.SApp "idris_write_str" ps

  LExternal name -> Grin.SApp (packName $ show name) ps
  LSystemInfo -> undefined
  LFork -> Grin.SApp "idris_fork" ps
  LPar -> undefined -- evaluate argument anywhere, possibly on another -- core or another machine. 'id' is a valid implementation
  LCrash -> Grin.SApp "idris_crash" ps
  LNoOp -> undefined

-- | Creates an expression
returnVal :: Name -> SExp -> NameM Exp
returnVal fname = \case
  SV lvar0 -> pure $ SReturn $ Var $ lvar fname lvar0
  SCon _ int nm lvars -> pure $ SReturn $ ConstTagNode (Tag C (name nm)) (map (lvar fname) lvars)
  SConst c -> literalReturnConst c
  rest -> error $ "unsupported val:" ++ show rest

name :: Idris.Name -> Name
name n = packName $ "idr_" ++ (Idris.showCG n)

literalTagAndParams :: Idris.Const -> (Tag, Lit)
literalTagAndParams l = case l of
  Idris.I int       -> expr "GrInt"     (LInt64 (fromIntegral int))
  Idris.BI integer  -> expr "GrInt"     (LInt64 (fromIntegral integer))
  Idris.Str string  -> expr "GrString"  (LString (fromString string))
  Idris.Ch char     -> expr "GrInt"     (LInt64 (fromIntegral (ord char)))
  Idris.Fl double   -> expr "GrFloat"   (LFloat (realToFrac double)) -- TODO
  Idris.B8 word8    -> expr "GrBit8"    (LWord64 (fromIntegral word8))
  Idris.B16 word16  -> expr "GrBit16"   (LWord64 (fromIntegral word16))
  Idris.B32 word32  -> expr "GrBit32"   (LWord64 (fromIntegral word32))
  Idris.B64 word64  -> expr "GrBit64"   (LWord64 (fromIntegral word64))
  -- TODO: Represent appropriate bit length 8,16,32,64
  Idris.AType arithTy -> error $ printf "unsupported literal %s" (show l)
  Idris.StrType       -> error $ printf "unsupported literal %s" (show l)
  Idris.WorldType     -> error $ printf "unsupported literal %s" (show l)
  Idris.TheWorld      -> error $ printf "unsupported literal %s" (show l)
  Idris.VoidType      -> error $ printf "unsupported literal %s" (show l)
  Idris.Forgot        -> error $ printf "unsupported literal %s" (show l)
  where
    expr t l = (Tag C t, l)

literalTag :: Idris.Const -> Name
literalTag l = tag
  where
    (Tag _ tag, _) = literalTagAndParams l

-- Creates Nodes with Literals
literalReturnConst :: Idris.Const -> NameM Exp
literalReturnConst l = do
  lrcVar1 <- deriveNewName "lrcv"
  lrcVar2 <- deriveNewName "lrct"
  let block (tag, lit)
        = pure $ SBlock $
            EBind (SReturn (Lit lit)) (VarPat lrcVar1) $
            EBind (SReturn (ConstTagNode tag [lrcVar1])) (VarPat lrcVar2) $
            SReturn (Var lrcVar2)
  block $ literalTagAndParams l

preparation :: [PipelineStep]
preparation =
  [ SaveGrin (Rel "FromIdris")
  , T SimpleDeadFunctionElimination
  , T BindNormalisation
  --, HPTPass
  --, SaveTypeEnv
  , Statistics
  , SaveGrin (Rel "high-level-code.grin")
  ]

idrisOptimizations :: Options -> [Transformation]
idrisOptimizations o | not (optimise o) = []
idrisOptimizations o =
  [ BindNormalisation
  , InlineEval
  , InlineApply
  , TrivialCaseElimination
  , SparseCaseOptimisation
  , EvaluatedCaseElimination
  , UpdateElimination
  , CopyPropagation
  , SimpleDeadFunctionElimination
  , SimpleDeadVariableElimination
  , SimpleDeadParameterElimination
  , CommonSubExpressionElimination
  , CaseCopyPropagation
  , CaseHoisting
  , GeneralizedUnboxing
  , ArityRaising
  , LateInlining
  , NonSharedElimination
  ] ++
  if (deadCodeElim o)
    then
      [ DeadFunctionElimination
      , DeadVariableElimination
      , DeadParameterElimination
      , DeadDataElimination
      ]
    else []

createPostProcessing :: IO (Options -> [PipelineStep])
createPostProcessing = do
  buffer <- createBuffer
  let evalPlugin executableName = EvalPlugin { evalPluginPrimOp = evalPrimOp executableName buffer }
  pure $ \opt -> concat
    [ [ (if (outputGrin opt) then SaveGrin else (SaveExecutable (debugSymbols opt))) $ Abs $ output opt ]
    , [ PureEvalPlugin (evalPlugin (evalProgName opt)) False | evalGrin opt ] -- Do not show statistics
    ]
