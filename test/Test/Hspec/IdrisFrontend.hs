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




data CompileMode
  = OptimisedEval
  | NonOptimisedEval
  | Compiled
  deriving Show

data IdrisCodeGen
  = IdrisCodeGen
    { source :: String
    , input  :: Maybe String
    , compiled :: CompileMode
    , timeoutInSecs :: Int
    , withInclude :: Bool
    }


idris :: CompileMode -> Int -> String -> IdrisCodeGen
idris c t fp = IdrisCodeGen fp Nothing c t False

idrisWithIncludeDir :: CompileMode -> Int -> String -> IdrisCodeGen
idrisWithIncludeDir c t fp = IdrisCodeGen fp Nothing c t True

idrisWithStdin :: CompileMode -> Int -> String -> String -> IdrisCodeGen
idrisWithStdin c t fp inp = IdrisCodeGen fp (Just inp) c t False


instance Example IdrisCodeGen where
  type Arg IdrisCodeGen = ()
  evaluateExample (IdrisCodeGen{..}) params actionWith progressCallback = do
    result <- newIORef $ Result "" Success
    actionWith $ \() -> do
      doesDirectoryExist ".idris" >>= flip when (removeDirectoryRecursive ".idris")
      let steps = 12
      progressCallback (0, steps)
      doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
      doesFileExist "test.grin" >>= flip when (removeFile "test.grin")
      doesFileExist "test.grin.bin" >>= flip when (removeFile "test.grin.bin")
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
                  { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe } -- TODO: Log activity
      let runTest = (shell "./test.bin")
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
      let runGrinTest = (shell "./test.grin.bin")
                        { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
      let idrisGrinCmd = case compiled of
            OptimisedEval    -> "stack exec idris -- %s %s --codegen grin -o test.grin --cg-opt --grin --cg-opt --quiet --cg-opt --binary-intermed --cg-opt --eval"
            NonOptimisedEval -> "stack exec idris -- %s %s --codegen grin -o test.grin --cg-opt --grin --cg-opt --O0 --cg-opt --quiet --cg-opt --eval"
            Compiled         -> "stack exec idris -- %s %s --codegen grin -o test.grin.bin --cg-opt --quiet"
      let idrisGrin = (shell (printf idrisGrinCmd source includeDir))
                      { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }

      logs <- newIORef []
      let addLogLines lines = modifyIORef logs (++lines)
      let readFinalLogs = fmap unlines $ readIORef logs
      let attachLogs r = do
            logLines <- readFinalLogs
            pure $ (either (Result logLines) (const (Result [] Success))) r

      res <- (attachLogs =<<) $ runExceptT $ do
        lift $ progressCallback (3, steps)
        (_in, Just out, Just err, idrisPh) <- lift $ createProcess_ "Idris" idris
        lift $ progressCallback (4, steps)
        idrisExitCode <- lift $ waitForProcess idrisPh
        lift $ progressCallback (5, steps)
        lift $ addLogLines ["Compile idris"]
        lift (hGetContents out >>= (addLogLines . lines))
        lift (hGetContents err >>= (addLogLines . lines))
        when (idrisExitCode /= ExitSuccess) $ do
          lift $ hClose out
          throwE $ Failure Nothing $ Reason $ "Idris process exited with: " ++ show idrisExitCode
        lift $ progressCallback (6, steps)
        (mIn, Just out, Just err, runTestPh) <- lift $ createProcess_ "Test" runTest
        lift $ progressCallback (7, steps)
        enterInput mIn
        runTestExitCode <- lift $ waitForProcess runTestPh
        lift $ progressCallback (8, steps)
        lift $ doesFileExist "test.bin" >>= flip when (removeFile "test.bin")
        testOut <- lift $ hGetContents out
        lift $ addLogLines ["Run idris generated binary"]
        lift $ addLogLines $ lines testOut
        lift $ (hGetContents err >>= (addLogLines . lines))
        when (runTestExitCode /= ExitSuccess) $ do
          lift $ hClose out
          throwE $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode

        grinOut <- case compiled of
          Compiled -> do
            lift $ progressCallback (9, steps)
            (mIn, Just out, Just err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
            compOut <- lift $ hGetContents out
            compErr <- lift $ hGetContents err
            lift $ addLogLines $ lines compOut
            lift $ addLogLines $ lines compErr

            lift $ progressCallback (10, steps)
            idrisGrinExitCode <- lift $ waitForProcess idrisGrinPh
            lift $ progressCallback (11, steps)
            (mIn, Just out, Just err, runGrinTestPh) <- lift $ createProcess_ "Test grin.bin" runGrinTest
            enterInput mIn
            runGrinTestExitCode <- lift $ waitForProcess runGrinTestPh

            lift $ doesFileExist "test.bin.grin" >>= flip when (removeFile "test.bin.grin")
            grinOut <- lift $ hGetContents out
            lift $ addLogLines ["Compile and run grin."]
            lift $ addLogLines $ lines grinOut
            lift (hGetContents err >>= (addLogLines . lines))
            when (runGrinTestExitCode /= ExitSuccess) $ do
              throwE $ Failure Nothing $ Reason $ "Compiled Grin Test process exited with: " ++ show runGrinTestExitCode

            pure grinOut

          _ -> do
            lift $ progressCallback (9, steps)
            (mIn, Just out, Just err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
            lift $ progressCallback (10, steps)
            enterInput mIn
            idrisGrinExitCode <- lift $ timeout (timeoutInSecs * 1000) idrisGrinPh
            lift $ progressCallback (11, steps)
            lift $ doesFileExist "test.grin" >>= flip when (removeFile "test.grin")
            grinOut <- lift $ hGetContents out
            lift $ addLogLines ["Compile and run grin."]
            lift $ addLogLines $ lines grinOut
            lift (hGetContents err >>= (addLogLines . lines))
            case idrisGrinExitCode of
              ExitSuccess       -> pure grinOut
              ExitFailure (-15) -> pure grinOut -- Timeout, killed, etc
              _ -> do
                lift $ hClose out
                throwE $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode

        lift $ progressCallback (12, steps)
        when (grinOut /= testOut) $
          case compiled of
            NonOptimisedEval -> throwE $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            Compiled         -> throwE $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            OptimisedEval    -> bisect logs enterInput timeoutInSecs ".idris" stdInCreate testOut
      writeIORef result res
    readIORef result

bisect logs enterInput timeoutInSecs directory stdInCreate expectedOutput = do
  files <- fmap (filter isGrinFile) $ lift $ listDirectory directory
  let fileMap = createFileMap files
  let range = findRange fileMap
  let addLogLines lines = modifyIORef logs (++lines)

  let run idx = do
        let cmd = printf "stack exec grin -- %s --load-binary --quiet --eval" (fromJust $ Map.lookup idx fileMap)
        lift $ addLogLines ["", "", fromJust $ Map.lookup idx fileMap]
        let runGrin = (shell cmd) { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
        (mIn, Just out, Just err, cmdPh) <- lift $ createProcess_ cmd runGrin
        enterInput mIn
        cmdExitCode <- lift $ timeout (timeoutInSecs * 1000) cmdPh
        output <- lift $ hGetContents out
        errors <- lift $ hGetContents err
        lift $ addLogLines $ lines output
        lift $ addLogLines $ lines errors
        case cmdExitCode of
          ExitSuccess       -> pure $ Just output
          ExitFailure (-15) -> pure $ Just "Timeout!" -- TODO: Improve
          _ -> do (throwE $ Failure Nothing $ ExpectedButGot (Just "Evaluation error, shrinking has stopped.") expectedOutput errors)
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
