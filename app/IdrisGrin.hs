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

data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  , optimise :: Bool
  }


showUsage = do putStrLn "Usage: idris-codegen-grin <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out" True) xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--no-opt":xs) = process (opts { optimise = False }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cg_main :: Opts -> Idris ()
cg_main opts = do elabPrims
                  loadInputs (inputs opts) Nothing
                  mainProg <- elabMain
                  ir <- compile (Via IBCFormat "grin") (output opts) (Just mainProg)
                  runIO $ codegenGrin (Options (optimise opts)) ir

main :: IO ()
main = do
  opts <- getOpts
  if (null (inputs opts))
    then showUsage
    else runMain (cg_main opts)
