{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.IdrisFrontend where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.Error
import GHC.IO.Handle
import System.Directory (doesFileExist, removeFile)
import System.Exit
import System.Process
import Test.Hspec.Core.Spec
import Text.Printf


instance Error ResultStatus where
  noMsg  = Failure Nothing NoReason
  strMsg = Failure Nothing . Reason

data IdrisCodeGen = IdrisCodeGen { source :: String }

instance Example IdrisCodeGen where
  type Arg IdrisCodeGen = ()
  evaluateExample (IdrisCodeGen{..}) params actionWith progressCallback = do
    let steps = 12
    progressCallback (0, steps)
    doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
    progressCallback (1, steps)
    doesFileExist "test.grin" >>= flip when (removeFile "test.grin")
    progressCallback (2, steps)
    let idris = (shell (printf "stack exec idris -- %s -o test.bin" source))
                { std_in = NoStream, std_out = CreatePipe, std_err = NoStream } -- TODO: Log activity
    let runTest = (shell "./test.bin")
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
    let idrisGrin = (shell (printf "stack exec idris -- %s --codegen grin -o test.grin --cg-opt --O0 --cg-opt --quiet" source))
                    { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
    let runGrin = (shell "stack exec grin -- eval test.grin")
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }

    fmap (Result "" . either id (const Success)) $ runErrorT $ do
      lift $ progressCallback (3, steps)
      (_in, Just out, _err, idrisPh) <- lift $ createProcess_ "Idris" idris
      lift $ progressCallback (4, steps)
      idrisExitCode <- lift $ waitForProcess idrisPh
      lift $ progressCallback (5, steps)
      when (idrisExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Idris process exited with: " ++ show idrisExitCode
      lift (hGetContents out >>= putStrLn)

      lift $ progressCallback (6, steps)
      (_in, Just out, _err, runTestPh) <- lift $ createProcess_ "Test" runTest
      lift $ progressCallback (7, steps)
      runTestExitCode <- lift $ waitForProcess runTestPh
      lift $ progressCallback (8, steps)
      lift $ removeFile "test.bin"
      when (runTestExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode
      testOut <- lift $ hGetContents out

      lift $ progressCallback (9, steps)
      (_in, Just out, _err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
      lift $ progressCallback (10, steps)
      idrisGrinExitCode <- lift $ waitForProcess idrisGrinPh
      lift $ progressCallback (11, steps)
      lift $ removeFile "test.grin"
      when (idrisGrinExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode
      grinOut <- lift $ hGetContents out

      lift $ progressCallback (12, steps)
      when (grinOut /= testOut) $
        throwError $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut

idris :: String -> IdrisCodeGen
idris = IdrisCodeGen
