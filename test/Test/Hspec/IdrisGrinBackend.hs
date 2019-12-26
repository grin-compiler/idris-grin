{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.Hspec.IdrisGrinBackend where

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
import System.FilePath

import Control.Exception
import Control.DeepSeq
import Control.Concurrent (threadDelay)


data CompileMode
  = OptimisedEval
  | NonOptimisedEval
  | Compiled
  deriving Show

data IdrisCodeGen
  = IdrisCodeGen
    { source        :: String
    , input         :: Maybe String
    , compiled      :: CompileMode
    , timeoutInSecs :: Int
    , withInclude   :: Bool
    , package       :: Maybe String
    }

idris :: CompileMode -> Int -> String -> IdrisCodeGen
idris c t fp = IdrisCodeGen fp Nothing c t False Nothing

idrisWithIncludeDir :: CompileMode -> Int -> String -> IdrisCodeGen
idrisWithIncludeDir c t fp = IdrisCodeGen fp Nothing c t True Nothing

idrisWithStdin :: CompileMode -> Int -> String -> String -> IdrisCodeGen
idrisWithStdin c t fp inp = IdrisCodeGen fp (Just inp) c t False Nothing

testBinaryName :: IdrisCodeGen -> String
testBinaryName icg = takeFileName (source icg)  ++ ".bin"

testGrinName :: IdrisCodeGen -> String
testGrinName icg = takeFileName (source icg)  ++ ".grin"

testGrinBinaryName :: IdrisCodeGen -> String
testGrinBinaryName icg = takeFileName (source icg) ++ ".grin.bin"

tryExcept :: IO a -> ExceptT ResultStatus IO a
tryExcept io = (lift (try io)) >>= either (throwE . Failure Nothing . Reason . show @SomeException) pure

-- TODO: Generate the test binary name from the idr file
-- TODO: Improve readability
instance Example IdrisCodeGen where
  type Arg IdrisCodeGen = ()
  evaluateExample icg@(IdrisCodeGen{..}) params actionWith progressCallback = do
    progressRef <- newIORef 0
    let progress = do
          p <- readIORef progressRef
          progressCallback (p,p)
          writeIORef progressRef (p + 1)
    let waitProcess 0 proc = do
          terminateProcess proc
          waitForProcess proc
        waitProcess n proc = do
          progress
          mCode <- getProcessExitCode proc
          threadDelay 1000000 -- microsec == 1sec
          maybe (waitProcess (n-1) proc) pure mCode
    result <- newIORef $ Result "" Success
    actionWith $ \() -> do
      doesDirectoryExist ".idris" >>= flip when (removeDirectoryRecursive ".idris")
      let steps = 12
      progress
      let testBin = testBinaryName icg
      let testGrin = testGrinName icg
      let testGrinBin = testGrinBinaryName icg
      doesFileExist testBin >>= flip when (removeFile testBin)
      doesFileExist testGrin >>= flip when (removeFile testGrin)
      doesFileExist testGrinBin >>= flip when (removeFile testGrinBin)
      progress
      let stdInCreate = maybe NoStream (const CreatePipe) input
      let enterInput mh = fromMaybe (pure ()) $ do -- Maybe
                            h <- mh
                            i <- input
                            pure $ lift $ do -- IO
                              hPutStr h i
                              -- hPutChar h '\x04'
                              hFlushAll h
                              -- hClose h
      let includeDir = if withInclude then "-i " ++ takeDirectory source else ""
      let idris = (shell (printf "stack exec idris -- %s %s -o %s %s" source includeDir testBin (maybe "" ("-p "++) package)))
                  { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe } -- TODO: Log activity
      let runTest = (shell $ "./" ++ testBin)
                    { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
      let runGrinTest = (shell $ "./" ++ testGrinBin)
                        { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
      let idrisGrinCmd = case compiled of
            OptimisedEval    -> "stack exec idris -- %s %s --codegen grin -o %s %s --cg-opt --grin --cg-opt --quiet --cg-opt --binary-intermed --cg-opt --eval"
            NonOptimisedEval -> "stack exec idris -- %s %s --codegen grin -o %s %s --cg-opt --grin --cg-opt --O0 --cg-opt --quiet --cg-opt --eval"
            Compiled         -> "stack exec idris -- %s %s --codegen grin -o %s %s --cg-opt --quiet"
      let idrisGrinOutputFile = case compiled of
            Compiled -> testGrinBin
            _        -> testGrin
      let idrisGrin = (shell (printf idrisGrinCmd source includeDir idrisGrinOutputFile (maybe "" ("-p "++) package)))
                      { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }

      logs <- newIORef []
      let addLogLines lines = modifyIORef logs (force . (++ lines))
      let readFinalLogs = fmap unlines $ readIORef logs
      let attachLogs r = do
            logLines <- readFinalLogs
            pure $ (either (Result logLines) (const (Result [] Success))) r

      res <- (attachLogs =<<) $ runExceptT $ do
        lift $ progress
        (_in, Just out, Just err, idrisPh) <- lift $ createProcess_ "Idris" idris
        lift $ progress
        idrisExitCode <- lift $ waitProcess timeoutInSecs idrisPh
        lift $ progress
        lift $ addLogLines ["Compile idris", "============="]
        tryExcept (hGetContents out >>= (addLogLines . lines . force))
        tryExcept (hGetContents err >>= (addLogLines . lines . force))
        when (idrisExitCode /= ExitSuccess) $ do
          -- lift $ hClose out
          throwE $ Failure Nothing $ Reason $ "Idris process exited with: " ++ show idrisExitCode
        lift $ progress
        (mIn, Just out, Just err, runTestPh) <- lift $ createProcess_ "Test" runTest
        lift $ progress
        enterInput mIn
        runTestExitCode <- lift $ waitProcess timeoutInSecs runTestPh
        lift $ progress
        lift $ doesFileExist testBin >>= flip when (removeFile testBin)
        testOut <- tryExcept $ fmap force $ hGetContents out
        lift $ addLogLines ["Run idris generated binary", "=========================="]
        lift $ addLogLines $ lines testOut
        tryExcept (hGetContents err >>= (addLogLines . lines . force))
        when (runTestExitCode /= ExitSuccess) $ do
          -- lift $ hClose out
          throwE $ Failure Nothing $ Reason $ "Test process exited with: " ++ show runTestExitCode

        grinOut <- case compiled of
          Compiled -> do
            lift $ progress
            (mIn, Just out, Just err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
            lift $ progress
            idrisGrinExitCode <- lift $ waitProcess timeoutInSecs idrisGrinPh
            compOut <- tryExcept $ fmap force $ hGetContents out
            lift $ addLogLines $ lines compOut
            when (idrisGrinExitCode /= ExitSuccess) $ do
              compErr <- tryExcept $ fmap force $ hGetContents err
              lift $ addLogLines $ lines compErr

            lift $ progress
            (mIn, Just out, Just err, runGrinTestPh) <- lift $ createProcess_ "Test grin.bin" runGrinTest
            enterInput mIn
            runGrinTestExitCode <- lift $ waitProcess timeoutInSecs runGrinTestPh

            lift $ doesFileExist testGrinBin >>= flip when (removeFile testGrinBin)
            grinOut <- tryExcept $ fmap force $ hGetContents out
            lift $ addLogLines ["Compile and run grin", "===================="]
            lift $ addLogLines $ lines grinOut
            tryExcept (hGetContents err >>= (addLogLines . lines . force))
            when (runGrinTestExitCode /= ExitSuccess) $ do
              throwE $ Failure Nothing $ Reason $ "Compiled Grin Test process exited with: " ++ show runGrinTestExitCode

            pure grinOut

          _ -> do
            lift $ progress
            (mIn, Just out, Just err, idrisGrinPh) <- lift $ createProcess_ "IdrisGrin" idrisGrin
            lift $ progress
            enterInput mIn
            idrisGrinExitCode <- lift $ waitProcess timeoutInSecs idrisGrinPh
            lift $ progress
            lift $ doesFileExist testGrin >>= flip when (removeFile testGrin)
            grinOut <- tryExcept $ fmap force $ hGetContents out
            lift $ addLogLines ["Compile and run grin", "===================="]
            lift $ addLogLines $ lines grinOut
            tryExcept (hGetContents err >>= (addLogLines . lines . force))
            case idrisGrinExitCode of
              ExitSuccess       -> pure grinOut
              ExitFailure (-15) -> pure grinOut -- Timeout, killed, etc
              _ -> do
                -- lift $ hClose out
                throwE $ Failure Nothing $ Reason $ "Idris-Grin process exited with: " ++ show idrisGrinExitCode

        lift $ progress
        when (grinOut /= testOut) $
          case compiled of
            NonOptimisedEval -> throwE $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            Compiled         -> throwE $ Failure Nothing $ ExpectedButGot Nothing testOut grinOut
            OptimisedEval    -> bisect waitProcess logs enterInput timeoutInSecs ".idris" stdInCreate testOut
      writeIORef result res
    readIORef result

bisect waitProcess logs enterInput timeoutInSecs directory stdInCreate expectedOutput = do
  files <- fmap (filter isGrinFile) $ lift $ listDirectory directory
  let fileMap = createFileMap files
  let range = findRange fileMap
  let addLogLines lines = modifyIORef logs (force . (++ lines))

  let run idx = do
        let cmd = printf "stack exec grin -- %s --load-binary --quiet --eval" (fromJust $ Map.lookup idx fileMap)
        lift $ addLogLines ["", "", fromJust $ Map.lookup idx fileMap]
        let runGrin = (shell cmd) { std_in = stdInCreate, std_out = CreatePipe, std_err = CreatePipe }
        (mIn, Just out, Just err, cmdPh) <- lift $ createProcess_ cmd runGrin
        enterInput mIn
        cmdExitCode <- lift $ waitProcess timeoutInSecs cmdPh
        output <- tryExcept $ fmap force $ hGetContents out
        errors <- tryExcept $ fmap force $ hGetContents err
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
