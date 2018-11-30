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
    doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
    doesFileExist "test.grin" >>= flip when (removeFile "test.grin")
    let idris = (shell (printf "stack exec idris -- %s -o test.bin" source))
                { std_in = NoStream, std_out = CreatePipe, std_err = NoStream } -- TODO: Log activity
    let runTest = (shell "./test.bin")
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
    let idrisGrin = (shell (printf "stack exec idris -- %s --codegen grin -o test.grin" source))
                    { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
    let runGrin = (shell "stack exec grin -- eval test.grin")
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream }

    fmap (Result "" . either id (const Success)) $ runErrorT $ do
      (_in, Just out, _err, idrisPh) <- lift $ createProcess_ "Idris" idris
      idrisExitCode <- lift $ waitForProcess idrisPh
      when (idrisExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Idris process exited with: " ++ show idrisExitCode
      lift (hGetContents out >>= putStrLn)

      (_in, Just out, _err, runTestPh) <- lift $ createProcess_ "Test" runTest
      runTestExitCode <- lift $ waitForProcess runTestPh
      lift $ removeFile "test.bin"
      when (runTestExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode
      testOut <- lift $ hGetContents out

      (_in, Just out, _err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
      idrisGrinExitCode <- lift $ waitForProcess idrisGrinPh
      lift $ removeFile "test.grin"
      when (idrisGrinExitCode /= ExitSuccess) $ do
        lift $ hClose out
        throwError $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode
      grinOut <- lift $ hGetContents out

      when (grinOut /= testOut) $
        throwError $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut

idris :: String -> IdrisCodeGen
idris = IdrisCodeGen
