module IdrisSpec where

import Control.Monad (forM_)
import Test.Hspec
import Test.Hspec.IdrisFrontend

spec :: Spec
spec = describe "Idris and Grin matches for:" $ forM_ [NonOptimised, Optimised] $ \mode ->
  describe (show mode) $ do
    it "TDD 01 - 01 Hello World" $ idris mode 60 "test/tdd/chapter01/01_HelloWorld.idr"
    it "TDD 01 - 02 CalcType" $ idris mode 60 "test/tdd/chapter01/02_CalcType.idr"
    it "TDD 02 - 01 Average" $ idris mode 60 "test/tdd/chapter02/01_Average.idr"
    it "TDD 02 - 02 Average" $ idrisWithStdin
      mode 60
      "test/tdd/chapter02/02_Average.idr"
      $ unlines
        [ "This is a test sentence."
        , "This is a another test sentence."
        ]
    it "TDD 02 - 03 Prelude" $ idris mode 60 "test/tdd/chapter02/03_Prelude.idr"
    it "TDD 03 - 01 Matrix" $ idris mode 60 "test/tdd/chapter03/01_Matrix.idr"
    it "TDD 04 - 01 Data Types" $ idris mode 120 "test/tdd/chapter04/01_DataTypes.idr"
    it "TDD 04 - 02 Data Store" $ idrisWithStdin
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
