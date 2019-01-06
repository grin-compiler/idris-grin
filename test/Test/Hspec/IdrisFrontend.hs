{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.IdrisFrontend where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Data.Maybe (fromMaybe)
import Data.IORef
import GHC.IO.Handle
import System.Directory (doesFileExist, removeFile)
import System.Exit
import System.Process
import Test.Hspec.Core.Spec
import Text.Printf


instance Error ResultStatus where
  noMsg  = Failure Nothing NoReason
  strMsg = Failure Nothing . Reason

data OptMode
  = Optimised
  | NonOptimised
  deriving Show

data IdrisCodeGen
  = IdrisCodeGen
    { source :: String
    , input  :: Maybe String
    , optimised :: OptMode
    }

idris :: OptMode -> String -> IdrisCodeGen
idris o fp = IdrisCodeGen fp Nothing o

idrisWithStdin :: OptMode -> String -> String -> IdrisCodeGen
idrisWithStdin o fp inp = IdrisCodeGen fp (Just inp) o

-- TODO: Handle xit. The test case runs even if xit is the runner,
instance Example IdrisCodeGen where
  type Arg IdrisCodeGen = ()
  evaluateExample (IdrisCodeGen{..}) params actionWith progressCallback = do
    result <- newIORef $ Result "" Success
    actionWith $ \() -> do
      let steps = 12
      progressCallback (0, steps)
      doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
      progressCallback (1, steps)
      doesFileExist "test.grin" >>= flip when (removeFile "test.grin")
      progressCallback (2, steps)
      let stdInCreate = maybe NoStream (const CreatePipe) input
      let enterInput mh = fromMaybe (pure ()) $ do -- Maybe
                            h <- mh
                            i <- input
                            pure $ lift $ do -- IO
                              hPutStr h i
                              -- hPutChar h '\x04'
                              hFlushAll h
                              hClose h
      let idris = (shell (printf "stack exec idris -- %s -o test.bin" source))
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream } -- TODO: Log activity
      let runTest = (shell "./test.bin")
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }
      let idrisGrinCmd = case optimised of
            Optimised     -> "stack exec idris -- %s --codegen grin -o test.grin --cg-opt --quiet"
            NonOptimised  -> "stack exec idris -- %s --codegen grin -o test.grin --cg-opt --O0 --cg-opt --quiet"
      let idrisGrin = (shell (printf idrisGrinCmd source))
                      { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }
      let runGrin = (shell "stack exec grin -- eval test.grin")
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }

      res <- fmap (Result "" . either id (const Success)) $ runErrorT $ do
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
        (mIn, Just out, _err, runTestPh) <- lift $ createProcess_ "Test" runTest
        lift $ progressCallback (7, steps)
        enterInput mIn
        runTestExitCode <- lift $ waitForProcess runTestPh
        lift $ progressCallback (8, steps)
        lift $ removeFile "test.bin"
        when (runTestExitCode /= ExitSuccess) $ do
          lift $ hClose out
          throwError $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode
        testOut <- lift $ hGetContents out

        lift $ progressCallback (9, steps)
        (mIn, Just out, _err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
        lift $ progressCallback (10, steps)
        enterInput mIn
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
      writeIORef result res
    readIORef result
