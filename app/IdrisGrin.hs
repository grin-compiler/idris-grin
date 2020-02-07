{-# LANGUAGE RecordWildCards #-}
module Main where

import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.Main
import Idris.Options

import IRTS.Compiler

import Idris.CodegenGrin

import System.Environment
import System.Exit
import Data.List (isPrefixOf)
import System.Console.GetOpt
import Data.Monoid
import Text.Read (readMaybe)


options :: [OptDescr (Endo Options)]
options =
  [ Option ['o'] ["output"]     (ReqArg (\a -> Endo $ \opts -> opts { output = a }) "FILE") "Executable ELF output FILE."
  , Option ['g'] ["grin"]       (NoArg $ Endo $ \opts -> opts { outputGrin = True }) "Save the GRIN to the FILE."
  , Option ['e'] ["eval"]       (NoArg $ Endo $ \opts -> opts { evalGrin = True }) "Eval the final optimised GRIN."
  , Option ['a'] ["eval-arg"]   (ReqArg (\a -> Endo $ \opts -> opts { evalArgs = (evalArgs opts) ++ [a] }) "ARG") "Argument when evaluating GRIN code."
  , Option []    ["eval-name"]  (ReqArg (\a -> Endo $ \opts -> opts { evalProgName = a }) "PROGNAME") "Simulated executable name for eval."
  , Option ['q'] ["quiet"]      (NoArg $ Endo $ \opts -> opts { quiet = True }) "Do not log to stdout."
  , Option []    ["O0"]         (NoArg $ Endo $ \opts -> opts { optimise = False }) "No optimisation."
  , Option ['h'] ["help"]       (NoArg $ Endo $ \opts -> opts { help = True }) "Print help."
  , Option []    ["no-lint"]    (NoArg $ Endo $ \opts -> opts { lint = False }) "Turn off linting intermediate results."
  , Option []    ["output-dir"] (ReqArg (\a -> Endo $ \opts -> opts { outputDir = a }) "DIR") "Grin output directory."
  , Option []    ["dead-code-elim"] (NoArg $ Endo $ \opts -> opts { deadCodeElim = True }) "Turn on interprocedural dead code elimination."
  , Option []    ["binary-intermed"] (NoArg $ Endo $ \opts -> opts { saveInBinary = True }) "Save intermediate results in binary format too."
  , Option []    ["dbg"]        (NoArg $ Endo $ \opts -> opts { debugSymbols = True }) "Generate executable with debug symbols."
  ]

getOpts :: IO (Maybe Options)
getOpts = do
  argv <- getArgs
  pure $ case getOpt Permute options argv of
    (os, is, []) -> Just $
      appEndo
        (mconcat (os ++ map (\i -> Endo (\opts -> opts { inputs = i:inputs opts })) is))
        Idris.CodegenGrin.defaultOptions
    _ -> Nothing

showUsage :: IO ()
showUsage = do
  name <- getProgName
  let header = unwords [ name, "[OPTION...]", "FILE" ]
  putStrLn $ usageInfo header options

cg_main :: Options -> Idris ()
cg_main opts = do
  elabPrims
  loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "grin") (output opts) (Just mainProg)
  runIO $ codegenGrin opts ir

main :: IO ()
main = do
  opts <- getOpts
  maybe showUsage (\o@Options{..} -> if help then showUsage else runMain $ cg_main o) opts
