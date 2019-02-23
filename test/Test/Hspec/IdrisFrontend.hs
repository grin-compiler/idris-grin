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
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.IORef
import GHC.IO.Handle
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>), takeDirectory)
import System.Exit
import System.Process
import Test.Hspec.Core.Spec
import Text.Printf

import System.Directory
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Time.Clock




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
    , withInclude :: Bool
    }


idris :: OptMode -> Int -> String -> IdrisCodeGen
idris o t fp = IdrisCodeGen fp Nothing o t False

idrisWithIncludeDir :: OptMode -> Int -> String -> IdrisCodeGen
idrisWithIncludeDir o t fp = IdrisCodeGen fp Nothing o t True

idrisWithStdin :: OptMode -> Int -> String -> String -> IdrisCodeGen
idrisWithStdin o t fp inp = IdrisCodeGen fp (Just inp) o t False


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
      let includeDir = if withInclude then "-i " ++ takeDirectory source else ""
      let idris = (shell (printf "stack exec idris -- %s %s -o test.bin" source includeDir))
                  { std_in = NoStream, std_out = CreatePipe, std_err = NoStream } -- TODO: Log activity
      let runTest = (shell "./test.bin")
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }
      let idrisGrinCmd = case optimised of
            Optimised     -> "stack exec idris -- %s %s --codegen grin -o test.grin --cg-opt --quiet --cg-opt --binary-intermed"
            NonOptimised  -> "stack exec idris -- %s %s --codegen grin -o test.grin --cg-opt --O0 --cg-opt --quiet"
      let idrisGrin = (shell (printf idrisGrinCmd source includeDir))
                      { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }
      let runGrin = (shell "stack exec grin -- eval test.grin")
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = NoStream }

      logs <- newIORef []
      let readFinalLogs = fmap (unlines . reverse) $ readIORef logs
      let attachLogs r = do
            logLines <- readFinalLogs
            pure $ (Result logLines . either id (const Success)) r

      res <- (attachLogs =<<) $ runExceptT $ do
        lift $ progressCallback (3, steps)
        (_in, Just out, _err, idrisPh) <- lift $ createProcess_ "Idris" idris
        lift $ progressCallback (4, steps)
        idrisExitCode <- lift $ waitForProcess idrisPh
        lift $ progressCallback (5, steps)
        when (idrisExitCode /= ExitSuccess) $ do
          lift $ hClose out
          throwE $ Failure Nothing $ Reason $ "Idris process exited with: " ++ show idrisExitCode
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
          throwE $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode
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
            throwE $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode
        grinOut <- lift $ hGetContents out

        lift $ progressCallback (12, steps)
        when (grinOut /= testOut) $
          case optimised of
            NonOptimised -> throwE $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            Optimised    -> bisect logs enterInput timeoutInSecs ".idris" stdInCreate testOut
      writeIORef result res
    readIORef result

bisect logs enterInput timeoutInSecs directory stdInCreate expectedOutput = do
  files <- fmap (filter isGrinFile) $ lift $ listDirectory directory
  let fileMap = createFileMap files
  let range = findRange fileMap

  let run idx = do
        let cmd = printf "stack exec grin -- %s --load-binary --quiet --eval" (fromJust $ Map.lookup idx fileMap)
        lift $ modifyIORef logs ((fromJust $ Map.lookup idx fileMap):)
        let runGrin = (shell cmd) { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
        (mIn, Just out, Just err, cmdPh) <- lift $ createProcess_ cmd runGrin
        enterInput mIn
        cmdExitCode <- lift $ timeout (timeoutInSecs * 1000) cmdPh
        case cmdExitCode of
          ExitSuccess       -> fmap Just $ lift $ hGetContents out
          ExitFailure (-15) -> pure $ Just "Timeout!" -- TODO: Improve
          _ -> do lift $ hClose out
                  err <- lift $ hGetContents err
                  (throwE $ Failure Nothing $ ExpectedButGot (Just "Evaluation error, shrinking has stopped.") expectedOutput err)
                  pure Nothing

  let loop lout !mn !mx
        | mn >= mx     = throwE $ Failure Nothing $ Reason "Range search exhausted: No clue where the error happens."
        | mn + 1 == mx = do
            mnout <- run mn
            when (mnout /= Just expectedOutput) $
              throwE $ Failure Nothing $ Reason $ fromMaybe "Empty output" mnout
            mxout <- run mx
            when (mxout /= Just expectedOutput) $
              throwE $ Failure Nothing $ Reason $ fromMaybe "Empty output" mxout
            when (Just expectedOutput == mxout && Just expectedOutput == mnout) $
              throwE $ Failure Nothing $ Reason "The problematic step did not produce a binary grin. Thus error was in the last step."
            when (isNothing mnout && isNothing mxout) $
              throwE $ Failure Nothing $ Reason $ "Something went wrong. After pinpointing the problem, both ends have failed."
      loop lout !mn !mx = do
        let md = ((mx - mn) `div` 2) + mn
        mout <- run md
        maybe
          (throwE $ Failure Nothing $ ExpectedButGot (Just "Evaluation error, Shrinking has stopped.") expectedOutput lout)
          (\out -> uncurry (loop out) $ if (out == expectedOutput) then (md, mx) else (mn, md))
          mout

  uncurry (loop "") range
  where

    noOfDigits = 3
    isGrinFile name = (all isDigit (take noOfDigits name)) && ".binary" `isSuffixOf` name
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
    pure $ result { resultInfo = resultInfo result ++ diff }
    where
      showMS :: Rational -> String
      showMS t = printf "%.6f ms" (realToFrac $ 1E3 * t :: Double)
