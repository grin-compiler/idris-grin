{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, ConstraintKinds #-}
module Idris.CodegenGrin(
    Options(..)
  , defaultOptions
  , codegenGrin
  ) where

import Control.Monad
import Text.Show.Pretty hiding (Name)
import Text.Printf
import Debug.Trace
import qualified Data.Text as Text
import Data.Char (ord)
import Data.List
import Data.String (fromString)
import Control.Exception

import IRTS.CodegenCommon
import IRTS.Simplified as Idris
import IRTS.Lang as Idris
import qualified Idris.Core.TT as Idris
import Data.Functor.Foldable
import Transformations.StaticSingleAssignment
import Transformations.BindNormalisation
import Text.PrettyPrint.ANSI.Leijen (ondullblack)
import System.Process (callCommand)
import System.Directory (removeFile)
import System.IO (BufferMode(..), hSetBuffering, stdout)

import Grin.Pretty
import Grin.Grin as Grin
import Pipeline.Pipeline
import Reducer.Pure (EvalPlugin(..))

import Idris.PrimOps
import Idris.EvalPrimOp

{-
TODO:
 * Implement appropiate primitive ops
 * Optimization transformation that removed empty defaults, like pure ()
 * Implement String primitives
 * Reenable compilation at the end
-}

data Options = Options
  { inputs :: [FilePath]
  , output :: FilePath
  , outputGrin :: Bool
  , evalGrin :: Bool
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
    (program simpleDecls)
    preparation
    (idrisOptimizations o)
    (postProcessing o)
  pure ()

program :: [(Idris.Name, SDecl)] -> Exp
program defs =
  bindNormalisation $
  staticSingleAssignment $
  Program exts $ primOps ++ map (function . snd) defs
 where Program exts primOps = idrisPrimOps

function :: SDecl -> Exp
function (SFun fname params _int body) =
  Def
    (name fname)
    (map (\(p, i) -> packName $ unpackName (name fname) ++ show i) (params `zip` [0..]))
    (sexp (name fname) body)

loc :: Name -> LVar -> Val
loc fname (Idris.Loc i) = Var $ packName $ unpackName fname ++ show i

locVal :: Name -> LVar -> Val
locVal fname (Idris.Loc i) = Var $ packName $ unpackName fname ++ show i ++ "_val"

lvar :: Name -> LVar -> Val
lvar fname = Var . \case
  Idris.Loc loc -> packName $ unpackName fname ++ show loc
  Glob nm       -> name nm

lvarVal :: Name -> LVar -> Val
lvarVal fname = Var . \case
  Idris.Loc loc -> packName $ unpackName fname ++ show loc ++ "_val"
  Glob nm       -> packName $ unpackName (name nm) ++ "_val"

sexp :: Name -> SExp -> Exp
sexp fname = \case

  SLet loc0@(Idris.Loc i) v sc ->
    EBind (SBlock (sexp fname v)) (varV loc0) $ -- calculate
    EBind (SStore (varV loc0)) (var loc0)     $ -- store
    (sexp fname sc)

  Idris.SApp bool nm lvars -> Grin.SApp (name nm) (map var lvars)

  -- Update is used in eval like functions, where the computed value must be the value
  -- of the expression
  Idris.SUpdate loc0 sexp0 ->
    EBind (SBlock (sexp fname sexp0)) (varV loc0) $
    EBind (Grin.SUpdate (variableName $ var loc0) (varV loc0)) Unit $
    SReturn (varV loc0)

  SCase caseType lvar0 salts ->
    EBind (SFetch $ variableName $ var lvar0) (varV lvar0) $
    ECase (varV lvar0) (alts fname salts)
  SChkCase lvar0 salts ->
    EBind (SFetch $ variableName $ var lvar0) (varV lvar0) $
    ECase (varV lvar0) (alts fname salts)

  --SProj lvar0 int -> SFetchI (lvar fname lvar0) (Just int)

  -- All the primitive operations must be part of the runtime, and
  -- they must fetch values, as wrappers
  SOp f lvars -> primFn f (map var lvars)

  -- Constanst contains only tags and variables, which are locations, thus
  -- it can be the returned as the last
  scon@(SCon maybeLVar int name lvars) -> SReturn $ val fname scon
  sconst@(SConst cnst) -> SReturn $ val fname sconst

  SV lvar0@(Idris.Loc i)  -> SFetch $ variableName $ var lvar0
  SV lvar0@(Idris.Glob n) -> traceShow "Global call" $ Grin.SApp (variableName $ var lvar0) []

  -- SForeign fdesc1 fdesc2 fdescLVars -> undefined
  -- TODO: Foreign function calls must handle pointers or they must be wrapped.
  SForeign t fun args -> foreignFun fname t fun args

  SNothing -> SReturn (ConstTagNode (Tag C "Erased") [])
  SError msg -> Grin.SApp "idris_error" [Lit $ LString $ Text.pack msg]
  e -> error $ printf "unsupported %s" (show e)
  where
    var  = lvar fname
    varV = lvarVal fname

variableName :: Val -> Name
variableName (Var n) = n

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
  = Grin.SApp "idris_fileError" [lvar fname $ lvar0]
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

alts :: Name -> [SAlt] -> [Exp]
alts fname as = concat [con2, cons2, defs2]
  where
    (con0, cons0, defs0) = groupAlternatives as
    defs1 = take 1 $ (if (length defs0 > 1) then (traceShow ("More than one def:",defs0)) else id) defs0
    cons1 = constAlternativesByTag cons0
    con2  = map (constructorAlt fname) con0
    defs2 = map (defaultAlt fname) defs1
    cons2 = map (constantAlts fname defs2) cons1

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
    sameTag (SConstCase c1 _) (SConstCase c2 _)
      | ConstTagNode t1 _ <- literal c1
      , ConstTagNode t2 _ <- literal c2
      = t1 == t2

constructorAlt :: Name -> SAlt -> Exp
constructorAlt fname (SConCase startIdx t nm names sexp0) =
  Alt (NodePat (Tag C (name nm)) (map (\(i,_n) -> packName $ unpackName fname ++ show i) ([startIdx ..] `zip` names)))
      (sexp fname sexp0)

constantAlts :: Name -> [Exp] -> [SAlt] -> Exp
constantAlts fname defs as@(a@(SConstCase cnst _):_) =
  Alt (NodePat tag [cpatVar]) $ ECase (Var cpatVar) $
    (flip map as $ \(SConstCase c e) ->
      let (ConstTagNode _ [Lit l]) = literal c
      in (Alt (LitPat l) (sexp fname e)))
    ++ defs
  where
    (ConstTagNode tag [Lit lit]) = literal cnst
    cpatVar = packName $ unpackName fname ++ "_cpat_" ++ (map (\case { ' ' -> '_'; c -> c}) (show lit))

defaultAlt :: Name -> SAlt -> Exp
defaultAlt fname (SDefaultCase sexp0) = Alt DefaultPat (sexp fname sexp0)

primFn :: Idris.PrimFn -> [SimpleVal] -> Exp
primFn f ps = case f of
  LPlus   Idris.ATFloat               -> Grin.SApp "idris_float_add" ps
  LPlus   (Idris.ATInt Idris.ITChar)  -> Grin.SApp "idris_int_add" ps
  LPlus   (Idris.ATInt Idris.ITBig)   -> Grin.SApp "idris_int_add" ps
  LPlus   (Idris.ATInt (Idris.ITFixed Idris.IT64)) -> Grin.SApp "idris_bit64_add" ps
  LPlus   (Idris.ATInt intTy) -> Grin.SApp "idris_int_add" ps
  LMinus  (Idris.ATInt intTy) -> Grin.SApp "idris_int_sub" ps
  LMinus  Idris.ATFloat       -> Grin.SApp "idris_float_sub" ps
  LTimes  (Idris.ATInt (Idris.ITFixed Idris.IT64))  -> Grin.SApp "idris_bit64_mul" ps
  LTimes  (Idris.ATInt intTy)                       -> Grin.SApp "idris_int_mul" ps
  LTimes  Idris.ATFloat       -> Grin.SApp "idris_float_mul" ps
  LSDiv   (Idris.ATInt intTy) -> Grin.SApp "idris_int_div" ps
  LSDiv   Idris.ATFloat       -> Grin.SApp "idris_float_div" ps
  LUDiv   (Idris.ITFixed Idris.IT8) -> Grin.SApp "idris_bit8_udiv" ps
  LUDiv   (Idris.ITFixed Idris.IT16) -> Grin.SApp "idris_bit16_udiv" ps
  LUDiv   (Idris.ITFixed Idris.IT32) -> Grin.SApp "idris_bit32_udiv" ps
  LUDiv   (Idris.ITFixed Idris.IT64) -> Grin.SApp "idris_bit64_udiv" ps
  LUDiv   intTy               -> Grin.SApp "idris_int_udiv" ps
{-
  LURem intTy -> undefined
-}
  LSRem   (Idris.ATInt intTy) -> Grin.SApp "idris_int_rem" ps
{-
  LSRem   arithTy -> undefined
-}
  LAnd (Idris.ITFixed Idris.IT8)  -> Grin.SApp "idris_bit8_and" ps
  LAnd (Idris.ITFixed Idris.IT64) -> Grin.SApp "idris_bit64_and" ps
  LAnd intTy -> Grin.SApp "idris_int_and" ps
{-
  LOr intTy -> undefined
  LXOr intTy -> undefined
  LCompl intTy -> undefined
-}
  LSHL (Idris.ITFixed Idris.IT64) -> Grin.SApp "idris_bit64_shl" ps
  LSHL  intTy -> Grin.SApp "idris_int_shl" ps

  LLSHR (Idris.ITFixed Idris.IT8) -> Grin.SApp "idris_bit8_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT16) -> Grin.SApp "idris_bit16_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT32) -> Grin.SApp "idris_bit32_lshr" ps
  LLSHR (Idris.ITFixed Idris.IT64) -> Grin.SApp "idris_bit64_lshr" ps
  LLSHR intTy -> Grin.SApp "idris_int_lshr" ps

  LASHR Idris.ITNative    -> Grin.SApp "idris_lashr_int" ps
  LEq (Idris.ATInt Idris.ITBig) -> Grin.SApp "idris_int_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed (Idris.IT8))) -> Grin.SApp "idris_bit8_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed (Idris.IT16))) -> Grin.SApp "idris_bit16_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed (Idris.IT32))) -> Grin.SApp "idris_bit32_eq" ps
  LEq (Idris.ATInt (Idris.ITFixed (Idris.IT64))) -> Grin.SApp "idris_bit64_eq" ps
  LEq (Idris.ATInt intTy) -> Grin.SApp "idris_int_eq" ps
  LEq Idris.ATFloat       -> Grin.SApp "idris_float_eq" ps

  LSLt (Idris.ATInt intTy) -> Grin.SApp "idris_int_lt" ps
  LSLt Idris.ATFloat       -> Grin.SApp "idris_float_lt" ps

  LSLe (Idris.ATInt intTy) -> Grin.SApp "idris_int_le" ps
  LSGt (Idris.ATInt intTy) -> Grin.SApp "idris_int_gt" ps
  LSGe (Idris.ATInt intTy) -> Grin.SApp "idris_int_ge" ps
{-
  LLt intTy -> Grin.SApp "_prim_int_lt" ps
  LLe intTy -> Grin.SApp "_prim_int_le" ps
  LGt intTy -> Grin.SApp "_prim_int_gt" ps
  LGe intTy -> Grin.SApp "_prim_int_ge" ps
  --LSLt Idris.ATFloat       -> Grin.SApp "_prim_float_lt" ps
-}
  LSExt intTy1 intTy2 -> Grin.SApp "idris_ls_ext" ps
  LZExt Idris.ITNative (Idris.ITFixed Idris.IT64)
    -> Grin.SApp "idris_lz_ext_int_bit64" ps
  LZExt (Idris.ITFixed Idris.IT8) Idris.ITNative
    -> Grin.SApp "idris_lz_ext_bit8_int" ps
  LZExt (Idris.ITFixed Idris.IT8) (Idris.ITFixed Idris.IT64)
    -> Grin.SApp "idris_lz_ext_bit8_bit64" ps
  LZExt intTy1 intTy2 -> Grin.SApp "idris_lz_ext" ps

  LTrunc Idris.ITBig (Idris.ITFixed Idris.IT64)
    -> Grin.SApp "idris_ltrunc_big_bit64" ps
  LTrunc (Idris.ITFixed Idris.IT16) (Idris.ITFixed Idris.IT8)
    -> Grin.SApp "idris_ltrunc_bit16_bit8" ps
  LTrunc (Idris.ITFixed Idris.IT32) (Idris.ITFixed Idris.IT8)
    -> Grin.SApp "idris_ltrunc_bit32_bit8" ps
  LTrunc (Idris.ITFixed Idris.IT64) (Idris.ITFixed Idris.IT8)
    -> Grin.SApp "idris_ltrunc_bit64_bit8" ps
  LTrunc Idris.ITBig Idris.ITNative
    -> Grin.SApp "idris_ltrunc_big_int" ps
  LTrunc Idris.ITNative (Idris.ITFixed Idris.IT8)
    -> Grin.SApp "idris_ltrunc_int_bit8" ps

  LStrConcat -> Grin.SApp "idris_str_concat" ps
  LStrLt -> Grin.SApp "idris_str_lt" ps
  LStrEq -> Grin.SApp "idris_str_eq" ps
  LStrLen -> Grin.SApp "idris_str_len" ps
  LIntFloat intTy -> Grin.SApp "idris_int_float" ps
  LFloatInt intTy -> Grin.SApp "idris_float_int" ps
  LIntStr intTy -> Grin.SApp "idris_int_str"    ps
  LStrInt intTy -> Grin.SApp "idris_str_int"    ps
  LFloatStr     -> Grin.SApp "idris_float_str"  ps
  LStrFloat     -> Grin.SApp "idris_str_float"  ps
  LChInt intTy  -> Grin.SApp "idris_ch_int"     ps
  LIntCh intTy  -> Grin.SApp "idris_int_ch"     ps
{-
  LBitCast arithTy1 arithTy2 -> undefined -- Only for values of equal width
  LFExp -> undefined
  LFLog -> undefined
  LFSin -> undefined
  LFCos -> undefined
  LFTan -> undefined
  LFASin -> undefined
  LFACos -> undefined
  LFATan -> undefined
-}
  LFATan2 -> Grin.SApp "idris_float_atan2" ps
{-
  LFSqrt -> undefined
-}
  LFFloor -> Grin.SApp "idris_float_floor" ps
  LFCeil  -> Grin.SApp "idris_float_ceil"  ps
{-
  LFNegate -> undefined
-}
  LStrHead  -> Grin.SApp "idris_str_head" ps
  LStrTail  -> Grin.SApp "idris_str_tail" ps
  LStrCons  -> Grin.SApp "idris_str_cons" ps
  LStrIndex -> Grin.SApp "idris_str_idx"  ps
  LStrRev   -> Grin.SApp "idris_str_rev"  ps
{-
  LStrSubstr -> undefined
}
-}
  LReadStr -> Grin.SApp "idris_read_str" ps
  LWriteStr -> Grin.SApp "idris_write_str" ps

  LExternal name -> Grin.SApp (packName $ show name) ps
  {-
  LSystemInfo -> undefined
  -}
  LFork -> Grin.SApp "idris_fork" ps
  {-
  LPar -> undefined -- evaluate argument anywhere, possibly on another -- core or another machine. 'id' is a valid implementation
  -}
  LCrash -> Grin.SApp "idris_crash" ps
  {-
  LNoOp -> undefined
  -}
  x -> error $ printf "unsupported primitive operation %s" (show x)

-- TODO: Check if the Val is reffered and fetched
val :: Name -> SExp -> Val
val fname = \case
  SV lvar0 -> lvar fname lvar0
  SConst c -> literal c
  SCon _ int nm lvars -> ConstTagNode (Tag C (name nm)) (map (lvar fname) lvars)
  rest -> error $ "unsupported val:" ++ show rest

name :: Idris.Name -> Name
name n = packName $ "idr_" ++ (Idris.showCG n)

-- Creates Nodes with Literals
literal :: Idris.Const -> Val
literal l = case l of
  Idris.I int      -> ConstTagNode (Tag C "GrInt") [Lit $ LInt64 (fromIntegral int)]
  Idris.BI integer -> ConstTagNode (Tag C "GrInt") [Lit $ LInt64 (fromIntegral integer)]
  Idris.Str string -> ConstTagNode (Tag C "GrString") [Lit $ LString $ fromString string]
  Idris.Ch char    -> ConstTagNode (Tag C "GrInt") [Lit $ LInt64 (fromIntegral $ ord $ char)]
  Idris.Fl double  -> ConstTagNode (Tag C "GrFloat") [Lit $ LFloat (realToFrac double)] -- TODO
  -- TODO: Represent appropriate bit length 8,16,32,64
  Idris.B8 word8   -> ConstTagNode (Tag C "GrBit8") [Lit $ LWord64 (fromIntegral word8)]
  Idris.B16 word16 -> ConstTagNode (Tag C "GrBit16") [Lit $ LWord64 (fromIntegral word16)]
  Idris.B32 word32 -> ConstTagNode (Tag C "GrBit32") [Lit $ LWord64 (fromIntegral word32)]
  Idris.B64 word64 -> ConstTagNode (Tag C "GrBit64") [Lit $ LWord64 (fromIntegral word64)]
  Idris.AType arithTy -> error $ printf "unsupported literal %s" (show l)
  Idris.StrType -> error $ printf "unsupported literal %s" (show l)
  Idris.WorldType -> error $ printf "unsupported literal %s" (show l)
  Idris.TheWorld -> error $ printf "unsupported literal %s" (show l)
  Idris.VoidType -> error $ printf "unsupported literal %s" (show l)
  Idris.Forgot -> error $ printf "unsupported literal %s" (show l)

preparation :: [PipelineStep]
preparation =
  [ SaveGrin (Rel "FromIdris")
  , T SimpleDeadFunctionElimination
  , T ProducerNameIntroduction
  , T BindNormalisation
--  , PrintGrin ondullblack
--  , HPT PrintHPTResult
--  , PrintTypeEnv
  , Statistics
  , SaveTypeEnv
--  , HPT PrintHPTCode
  , SaveGrin (Rel "high-level-code.grin")
  , HPTPass
  , Lint
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
  let evalPlugin = EvalPlugin { evalPluginPrimOp = evalPrimOp buffer }
  pure $ \opt -> concat
    [ [ (if (outputGrin opt) then SaveGrin else (SaveExecutable (debugSymbols opt))) $ Abs $ output opt ]
    , [ PureEvalPlugin evalPlugin | evalGrin opt ]
    ]
