module IdrisDevSpec where

import Control.Monad (forM_, when)
import Test.Hspec
import Test.Hspec.IdrisGrinBackend
import System.Environment (lookupEnv)

{-
All the failing test cases are marked as pending. A reason is provided what goes wrong
when the test runs. In the long term these quests will be unlocked. :)
-}

spec :: Spec
spec = do
  let mode = NonOptimisedEval
  let testBackend idrisFile =
        IdrisCodeGen
          { source        = idrisFile :: String
          , input         = Nothing   :: Maybe String
          , compiled      = mode      :: CompileMode
          , timeoutInSecs = 600       :: Int
          , withInclude   = True      :: Bool
          , package       = Nothing   :: Maybe String
          , pendingReason = Nothing   :: Maybe String
          , arguments     = Nothing   :: Maybe [String]
          }

  describe "In focus" $ do
    pure ()

  notOnCI <- runIO $ do
    env <- lookupEnv "IDRIS_GRIN_CI"
    pure $ maybe True (const False) env

  when notOnCI $ describe "Idris and Grin matches for" $ do
    it "test/idris-dev/effects003/hangman.idr" $ (testBackend "test/idris-dev/effects003/hangman.idr")
      { package = Just "effects"
      , input   = Just $ unlines ["a", "e", "i", "o", "j", "v"]
      }
    it "test/idris-dev/effects004/effects004.idr" $ (testBackend "test/idris-dev/effects004/effects004.idr")
      { package = Just "effects"
      , input   = Just $ unlines ["", "", "", "", "", "", "", "", "done", "", ""]
      }
    it "test/idris-dev/dsl001/test001.idr" $ testBackend "test/idris-dev/dsl001/test001.idr"
    it "test/idris-dev/basic024/basic024.idr" $ testBackend "test/idris-dev/basic024/basic024.idr"
    it "test/idris-dev/primitives001/substring.idr" $ (testBackend "test/idris-dev/primitives001/substring.idr")
      { input = Just $ unlines
          [ "Idris 是一个通用的依赖类型纯函数式编程语言，其类型系统与 Agda 以及 Epigram 相似。"
          , "Idris is a general-purpose purely functional programming language with dependent types."
          ]
      }
    it "test/idris-dev/basic001/basic001a.idr" $ testBackend "test/idris-dev/basic001/basic001a.idr"
    it "test/idris-dev/basic001/reg005.idr" $ testBackend "test/idris-dev/basic001/reg005.idr"
    it "test/idris-dev/basic003/test027.idr" $ testBackend "test/idris-dev/basic003/test027.idr"
    it "test/idris-dev/basic004/test016.idr" $ testBackend "test/idris-dev/basic004/test016.idr"
    it "test/idris-dev/basic005/test019.lidr" $ testBackend "test/idris-dev/basic005/test019.lidr"
    it "test/idris-dev/basic007/test033.idr" $ testBackend "test/idris-dev/basic007/test033.idr"
    it "test/idris-dev/basic008/test036.idr" $ testBackend "test/idris-dev/basic008/test036.idr"
    it "test/idris-dev/basic010/Main.idr" $ testBackend "test/idris-dev/basic010/Main.idr"
    it "test/idris-dev/basic011/basic011.idr" $ (testBackend "test/idris-dev/basic011/basic011.idr")
      { package = Just "contrib" }
    it "test/idris-dev/basic012/basic012.idr" $ testBackend "test/idris-dev/basic012/basic012.idr"
    it "test/idris-dev/basic013/basic013.idr" $ testBackend "test/idris-dev/basic013/basic013.idr"
    it "test/idris-dev/basic015/basic015.idr" $ testBackend "test/idris-dev/basic015/basic015.idr"
    it "test/idris-dev/basic019/basic019.idr" $ testBackend "test/idris-dev/basic019/basic019.idr"
    it "test/idris-dev/basic020/basic020.idr" $ testBackend "test/idris-dev/basic020/basic020.idr"
    it "test/idris-dev/basic021/basic021.idr" $ (testBackend "test/idris-dev/basic021/basic021.idr")
      { pendingReason = Just "Need to implement IORef." }
    it "test/idris-dev/basic021/basic021_2.idr" $ (testBackend "test/idris-dev/basic021/basic021_2.idr")
      { pendingReason = Just "Need to implement IORef." }
    it "test/idris-dev/basic023/sections.idr" $ testBackend "test/idris-dev/basic023/sections.idr"
    it "test/idris-dev/basic025/basic025.idr" $ testBackend "test/idris-dev/basic025/basic025.idr"
    it "test/idris-dev/basic026/basic026.idr" $ testBackend "test/idris-dev/basic026/basic026.idr"
    it "test/idris-dev/bignum001/bignum001.idr" $ testBackend "test/idris-dev/bignum001/bignum001.idr"
    it "test/idris-dev/bignum002/bignum002.idr" $ testBackend "test/idris-dev/bignum002/bignum002.idr"
    it "test/idris-dev/bignum003/bignum003.idr" $ testBackend "test/idris-dev/bignum003/bignum003.idr"
    it "test/idris-dev/bounded001/bounded001.idr" $ testBackend "test/idris-dev/bounded001/bounded001.idr"
    it "test/idris-dev/buffer001/buffer001.idr" $ testBackend "test/idris-dev/buffer001/buffer001.idr"
    it "test/idris-dev/buffer002/buffer002.idr" $ testBackend "test/idris-dev/buffer002/buffer002.idr"
    it "test/idris-dev/contrib001/contrib001.idr" $ (testBackend "test/idris-dev/contrib001/contrib001.idr")
      { package = Just "contrib" }
    it "test/idris-dev/corecords001/corecords001.idr" $ testBackend "test/idris-dev/corecords001/corecords001.idr"
    it "test/idris-dev/corecords002/corecords002.idr" $ testBackend "test/idris-dev/corecords002/corecords002.idr"
    it "test/idris-dev/dsl002/test014.idr" $ (testBackend "test/idris-dev/dsl002/test014.idr")
      { pendingReason = Just "Implement prim__readFile" }
    it "test/idris-dev/effects001/test021.idr" $ (testBackend "test/idris-dev/effects001/test021.idr")
      { package = Just "effects" }
    it "test/idris-dev/effects001/test021a.idr" $ (testBackend "test/idris-dev/effects001/test021a.idr")
      { package = Just "effects" }
    it "test/idris-dev/effects002/test025.idr" $ (testBackend "test/idris-dev/effects002/test025.idr")
      { package = Just "effects" }
    it "test/idris-dev/effects005/categoryLogger.idr" $ (testBackend "test/idris-dev/effects005/categoryLogger.idr") { package = Just "effects" }
    it "test/idris-dev/effects005/defaultLogger.idr" $ (testBackend "test/idris-dev/effects005/defaultLogger.idr") { package = Just "effects" }
    it "test/idris-dev/ffi003/test024.idr" $ (testBackend "test/idris-dev/ffi003/test024.idr")
      { input = Just "test\n" }
    it "test/idris-dev/folding001/folding001.idr" $ testBackend "test/idris-dev/folding001/folding001.idr"
    it "test/idris-dev/interfaces002/interfaces002.idr" $ testBackend "test/idris-dev/interfaces002/interfaces002.idr"
    it "test/idris-dev/interfaces003/interfaces003.idr" $ testBackend "test/idris-dev/interfaces003/interfaces003.idr"
    it "test/idris-dev/interfaces004/interfaces004.idr" $ testBackend "test/idris-dev/interfaces004/interfaces004.idr"
    it "test/idris-dev/interfaces007/interfaces007.idr" $ testBackend "test/idris-dev/interfaces007/interfaces007.idr"
    it "test/idris-dev/io001/test004.idr" $ testBackend "test/idris-dev/io001/test004.idr" -- Remove test file?
    it "test/idris-dev/io002/test008.idr" $ testBackend "test/idris-dev/io002/test008.idr"
    it "test/idris-dev/io003/test018.idr" $ testBackend "test/idris-dev/io003/test018.idr"
    it "test/idris-dev/io003/test018a.idr" $ (testBackend "test/idris-dev/io003/test018a.idr")
      { package = Just "contrib" }
    it "test/idris-dev/literate001/test003.lidr" $ testBackend "test/idris-dev/literate001/test003.lidr"
    it "test/idris-dev/pkg001/Main.idr" $ (testBackend "test/idris-dev/pkg001/Main.idr")
      { package = Just "effects" }
    it "test/idris-dev/pkg002/Main.idr" $ testBackend "test/idris-dev/pkg002/Main.idr"
    it "test/idris-dev/pkg003/Main.idr" $ testBackend "test/idris-dev/pkg003/Main.idr"
    it "test/idris-dev/pkg004/Main.idr" $ testBackend "test/idris-dev/pkg004/Main.idr"
    it "test/idris-dev/prelude001/prelude001.idr" $ testBackend "test/idris-dev/prelude001/prelude001.idr"
    it "test/idris-dev/primitives001/test005.idr" $ testBackend "test/idris-dev/primitives001/test005.idr"
    it "test/idris-dev/primitives003/test038.idr" $ testBackend "test/idris-dev/primitives003/test038.idr"
    it "test/idris-dev/primitives005/primitives005.idr" $ testBackend "test/idris-dev/primitives005/primitives005.idr"
    it "test/idris-dev/primitives006/load-test.idr" $ (testBackend "test/idris-dev/primitives006/load-test.idr")
      { pendingReason = Just "Needs support for  linking." }
    it "test/idris-dev/proof010/proof010.idr" $ testBackend "test/idris-dev/proof010/proof010.idr"
    it "test/idris-dev/proofsearch001/proofsearch001.idr" $ testBackend "test/idris-dev/proofsearch001/proofsearch001.idr"
    it "test/idris-dev/proofsearch002/proofsearch002.idr" $ (testBackend "test/idris-dev/proofsearch002/proofsearch002.idr")
      { pendingReason = Just "Infinite loop." }
    it "test/idris-dev/quasiquote002/GoalQQuote.idr" $ testBackend "test/idris-dev/quasiquote002/GoalQQuote.idr"
    it "test/idris-dev/records001/test011.idr" $ testBackend "test/idris-dev/records001/test011.idr"
    it "test/idris-dev/records002/record002.idr" $ testBackend "test/idris-dev/records002/record002.idr"
    it "test/idris-dev/records003/records003.idr" $ testBackend "test/idris-dev/records003/records003.idr"
    it "test/idris-dev/records004/records004.idr" $ testBackend "test/idris-dev/records004/records004.idr"
    it "test/idris-dev/records005/records005.idr" $ testBackend "test/idris-dev/records005/records005.idr"
    it "test/idris-dev/reg001/Area.idr" $ testBackend "test/idris-dev/reg001/Area.idr"
    it "test/idris-dev/reg002/reg012.idr" $ testBackend "test/idris-dev/reg002/reg012.idr"
    it "test/idris-dev/reg004/reg004.idr" $ testBackend "test/idris-dev/reg004/reg004.idr"
    it "test/idris-dev/reg005/reg005.idr" $ testBackend "test/idris-dev/reg005/reg005.idr"
    it "test/idris-dev/reg013/reg013.idr" $ testBackend "test/idris-dev/reg013/reg013.idr"
    it "test/idris-dev/reg016/reg016.idr" $ testBackend "test/idris-dev/reg016/reg016.idr"
    it "test/idris-dev/reg020/reg020.idr" $ testBackend "test/idris-dev/reg020/reg020.idr"
    it "test/idris-dev/reg024/reg024.idr" $ testBackend "test/idris-dev/reg024/reg024.idr"
    it "test/idris-dev/reg025/reg025.idr" $ testBackend "test/idris-dev/reg025/reg025.idr"
    it "test/idris-dev/reg031/reg031.idr" $ testBackend "test/idris-dev/reg031/reg031.idr"
    it "test/idris-dev/reg032/test028.idr" $ testBackend "test/idris-dev/reg032/test028.idr"
    it "test/idris-dev/reg040/reg040.idr" $ testBackend "test/idris-dev/reg040/reg040.idr"
    it "test/idris-dev/reg041/showu.idr" $ testBackend "test/idris-dev/reg041/showu.idr"
    it "test/idris-dev/reg042/reg042.idr" $ testBackend "test/idris-dev/reg042/reg042.idr"
    it "test/idris-dev/reg045/reg045.idr" $ testBackend "test/idris-dev/reg045/reg045.idr"
    it "test/idris-dev/reg048/reg048.idr" $ (testBackend "test/idris-dev/reg048/reg048.idr")
      { package = Just "contrib" }
    it "test/idris-dev/reg052/reg052.idr" $ testBackend "test/idris-dev/reg052/reg052.idr"
    it "test/idris-dev/reg067/reg067.idr" $ testBackend "test/idris-dev/reg067/reg067.idr"
    it "test/idris-dev/reg076/reg076.idr" $ testBackend "test/idris-dev/reg076/reg076.idr"
    it "test/idris-dev/regression003/regression003.idr" $ testBackend "test/idris-dev/regression003/regression003.idr"
    it "test/idris-dev/sourceLocation001/SourceLoc.idr" $ testBackend "test/idris-dev/sourceLocation001/SourceLoc.idr"
    it "test/idris-dev/st001/test001.idr" $ (testBackend "test/idris-dev/st001/test001.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st002/test002.idr" $ (testBackend "test/idris-dev/st002/test002.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st003/test003.idr" $ (testBackend "test/idris-dev/st003/test003.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st004/test004.idr" $ (testBackend "test/idris-dev/st004/test004.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st005/test005.idr" $ (testBackend "test/idris-dev/st005/test005.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st006/test006.idr" $ (testBackend "test/idris-dev/st006/test006.idr")
      { package = Just "contrib" }
    it "test/idris-dev/st007/test007.idr" $ (testBackend "test/idris-dev/st007/test007.idr")
      { package = Just "contrib" }
    it "test/idris-dev/sugar001/test007.idr" $ testBackend "test/idris-dev/sugar001/test007.idr"
    it "test/idris-dev/sugar002/test009.idr" $ testBackend "test/idris-dev/sugar002/test009.idr"
    it "test/idris-dev/sugar003/test013.idr" $ testBackend "test/idris-dev/sugar003/test013.idr"
    it "test/idris-dev/sugar004/sugar004.idr" $ testBackend "test/idris-dev/sugar004/sugar004.idr"
    it "test/idris-dev/sugar005/As.idr" $ testBackend "test/idris-dev/sugar005/As.idr"
    it "test/idris-dev/syntax002/syntax002.idr" $ testBackend "test/idris-dev/syntax002/syntax002.idr"
    it "test/idris-dev/totality005/totality005.idr" $ testBackend "test/idris-dev/totality005/totality005.idr"
    it "test/idris-dev/unique001/unique001.idr" $ testBackend "test/idris-dev/unique001/unique001.idr"
    it "test/idris-dev/views001/views001.idr" $ testBackend "test/idris-dev/views001/views001.idr"
    it "test/idris-dev/views001/views001a.idr" $ testBackend "test/idris-dev/views001/views001a.idr"
    it "test/idris-dev/views002/views002.idr" $ testBackend "test/idris-dev/views002/views002.idr"
    it "test/idris-dev/views003/views003.idr" $ (testBackend "test/idris-dev/views003/views003.idr")
      { arguments = Just ["10"] }

    -- TODO: Move to IdrisExperiments
    it "test/experiments/counting-immutable-beans/Main.idr" $ (testBackend "test/experiments/counting-immutable-beans/Main.idr")
      { package = Just "contrib"
      }
