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



data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  , optimise :: Bool
  , quiet :: Bool
  , help :: Bool
  }

options :: [OptDescr (Endo Opts)]
options =
  [ Option ['o'] ["output"]   (ReqArg (\a -> Endo $ \opts -> opts { output = a }) "FILE") "Grin output FILE (does not work yet)"
  , Option ['q'] ["quiet"]    (NoArg $ Endo $ \opts -> opts { quiet = True }) "Do not log to stdout"
  , Option []    ["O0"]       (NoArg $ Endo $ \opts -> opts { optimise = False }) "No optimisation"
  , Option ['h'] ["help"]     (NoArg $ Endo $ \opts -> opts { help = True }) "Print help"
  ]

getOpts :: IO (Maybe Opts)
getOpts = do
  argv <- getArgs
  pure $ case getOpt Permute options argv of
    (os, is, []) -> Just $
      appEndo
        (mconcat (os ++ map (\i -> Endo (\opts -> opts { inputs = i:inputs opts })) is))
        (Opts [] "a.out" True False False)
    _ -> Nothing

showUsage :: IO ()
showUsage = do
  name <- getProgName
  let header = unwords [ name, "[OPTION...]", "FILE" ]
  putStrLn $ usageInfo header options

cg_main :: Opts -> Idris ()
cg_main opts = do
  elabPrims
  loadInputs (inputs opts) Nothing
  mainProg <- elabMain
  ir <- compile (Via IBCFormat "grin") (output opts) (Just mainProg)
  runIO $ codegenGrin (Options (optimise opts) (quiet opts)) ir

main :: IO ()
main = do
  opts <- getOpts
  maybe showUsage (\o@Opts{..} -> if help then showUsage else runMain $ cg_main o) opts
