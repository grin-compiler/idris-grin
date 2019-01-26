module IdrisSpec where

import Control.Monad (forM_)
import Test.Hspec
import Test.Hspec.IdrisFrontend

{-
We keep the failing test cases for reference and regression detecting.
-}

spec :: Spec
spec = describe "Idris and Grin matches for:" $ forM_ [NonOptimised, Optimised] $ \mode ->
  describe (show mode) $ do
    it "TDD 01 - 01 Hello World" $ timed $ idris mode 60 "test/tdd/chapter01/01_HelloWorld.idr"
    it "TDD 01 - 02 CalcType" $ timed $ idris mode 60 "test/tdd/chapter01/02_CalcType.idr"
    it "TDD 02 - 01 Average" $ timed $ idris mode 60 "test/tdd/chapter02/01_Average.idr"
    it "TDD 02 - 02 Average" $ timed $ idrisWithStdin
      mode 60
      "test/tdd/chapter02/02_Average.idr"
      $ unlines
        [ "This is a test sentence."
        , "This is a another test sentence."
        ]
    it "TDD 02 - 03 Prelude" $ timed $ idris mode 60 "test/tdd/chapter02/03_Prelude.idr"
    it "TDD 03 - 01 Matrix" $ timed $ idris mode 60 "test/tdd/chapter03/01_Matrix.idr"
    it "TDD 04 - 01 Data Types" $ timed $ idris mode 120 "test/tdd/chapter04/01_DataTypes.idr"
    it "TDD 04 - 02 Data Store" $ timed $ idrisWithStdin
      mode 60
      "test/tdd/chapter04/02_DataStore.idr"
      $ unlines
        [ "help"
        , "add 1"
        , "add 2"
        -- , "get 1" -- ????
        , "size"
        , "search 1"
        , "quit"
        ]
    it "TDD 05 - 01 IO Intro" $ timed $ idrisWithStdin
      mode 60
      "test/tdd/chapter05/01_IOIntro.idr"
      $ unlines
        [ "3"
        , "y"
        , "1"
        , "n"
        , "1", "2", "3"
        , "1", "2", ""
        , "1", "2", ""
        ]
    it "TDD 06 - 01 Type Level Functions" $ timed $ idris
      mode 60
      "test/tdd/chapter06/01TypeLevelFuns.idr"
    it "TDD 06 - 02 DataStore" $ timed $ idrisWithStdin
      mode 60
      "test/tdd/chapter05/02_DataStore.idr"
      $ unlines
        [ "schema Int String"
        , "add 99 \"Red ballons\""
        , "add 76 \"Trombones\""
        , "schema String String Int"
        , "get 1"
        , "quit"
        ]
    it "TDD 07 - 01 Interfaces" $ timed $ idris
      mode 60
      "test/tdd/chapter07/01_Interfaces.idr"
    it "TDD 07 - 02 Expr" $ timed $ idris
      mode 60
      "test/tdd/chapter07/02_Expr.idr"
