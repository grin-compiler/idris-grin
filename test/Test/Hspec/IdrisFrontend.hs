{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.Hspec.IdrisFrontend where

import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.Error
import Data.Maybe (fromMaybe, fromJust)
import Data.IORef
import GHC.IO.Handle
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Exit
import System.Process
import Test.Hspec.Core.Spec
import Text.Printf

import System.Directory
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Time.Clock


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
    , timeoutInSecs :: Int
    }


idris :: OptMode -> Int -> String -> IdrisCodeGen
idris o t fp = IdrisCodeGen fp Nothing o t

idrisWithStdin :: OptMode -> Int -> String -> String -> IdrisCodeGen
idrisWithStdin o t fp inp = IdrisCodeGen fp (Just inp) o t


instance Example IdrisCodeGen where
  type Arg IdrisCodeGen = ()
  evaluateExample (IdrisCodeGen{..}) params actionWith progressCallback = do
    result <- newIORef $ Result "" Success
    actionWith $ \() -> do
      doesDirectoryExist ".idris" >>= flip when (removeDirectoryRecursive ".idris")
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
        lift $ doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
        when (runTestExitCode /= ExitSuccess) $ do
          lift $ hClose out
          throwError $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode
        testOut <- lift $ hGetContents out

        lift $ progressCallback (9, steps)
        (mIn, Just out, _err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
        lift $ progressCallback (10, steps)
        enterInput mIn
        idrisGrinExitCode <- lift $ timeout (timeoutInSecs * 1000) idrisGrinPh
        lift $ progressCallback (11, steps)
        lift $ doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
        case idrisGrinExitCode of
          ExitSuccess       -> pure ()
          ExitFailure (-15) -> pure () -- Timeout, killed, etc
          _ -> do
            lift $ hClose out
            throwError $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode
        grinOut <- lift $ hGetContents out

        lift $ progressCallback (12, steps)
        when (grinOut /= testOut) $
          case optimised of
            NonOptimised -> throwError $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            Optimised    -> bisect enterInput timeoutInSecs ".idris" stdInCreate testOut
      writeIORef result res
    readIORef result

bisect enterInput timeoutInSecs directory stdInCreate output = do
  files <- fmap (filter isGrinFile) $ lift $ listDirectory directory
  let fileMap = createFileMap files
  let range = findRange fileMap
  let loop lout !mn !mx | mn >= mx     = throwError $ Failure Nothing $ Reason "Range search exhausted: No clue where the error happens."
                        | mn + 1 == mx = throwError $ Failure Nothing $ ExpectedButGot Nothing output lout
      loop lout !mn !mx = do
        let md = (mx - mn) `div` 2
        mout <- run $ printf "stack exec grin -- %s --quiet --eval" (fromJust $ Map.lookup md fileMap)
        maybe
          (throwError $ Failure Nothing $ ExpectedButGot (Just "Evaluation error, Shrinking has stopped.") output lout)
          (\out -> uncurry (loop out) $ if (out == output) then (md, mx) else (mn, md))
          mout

  uncurry (loop "") range
  where
    run (cmd :: String) = do
      lift $ print cmd
      let runGrin = (shell cmd) { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
      (mIn, Just out, Just err, cmdPh) <- lift $ createProcess_ cmd runGrin
      enterInput mIn
      cmdExitCode <- lift $ timeout (timeoutInSecs * 1000) cmdPh
      case cmdExitCode of
        ExitSuccess       -> fmap Just $ lift $ hGetContents out
        ExitFailure (-15) -> pure $ Just "Timeout!" -- TODO: Improve
        _ -> do lift $ hClose out
                err <- lift $ hGetContents err
                (throwError $ Failure Nothing $ ExpectedButGot (Just "Evaluation error, shrinking has stopped.") output err)
                pure Nothing

    noOfDigits = 3
    isGrinFile name = (all isDigit (take noOfDigits name)) && ".grin" `isSuffixOf` name
    createFileMap files = Map.fromList $
      [ (itr, directory </> name)
      | name <- files
      , let itr = read @Int (take noOfDigits name)
      ]
    findRange = (minimum &&& maximum) . Map.keys

{- There is no garantee that the process will be killed. -}
timeout :: Int -> ProcessHandle -> IO ExitCode
timeout ms h | ms <= 0 = do
  terminateProcess h
  waitForProcess h
timeout ms h = do
  threadDelay (100 * 1000)
  mec <- getProcessExitCode h
  maybe (timeout (ms - 100) h) pure mec

-- * Timed examples

timed :: a -> Timed a
timed = Timed

data Timed a = Timed a

instance Example a => Example (Timed a) where
  type Arg (Timed a) = Arg a
  evaluateExample (Timed a) params actionWith progressCallback = do
    start  <- getCurrentTime
    result <- safeEvaluateExample a params actionWith progressCallback
    end    <- getCurrentTime
    let diff = (showMS $ toRational $ diffUTCTime end start)
    pure $ result { resultInfo = resultInfo result ++ " " ++ diff }
    where
      showMS :: Rational -> String
      showMS t = printf "%.6f ms" (realToFrac $ 1E3 * t :: Double)
