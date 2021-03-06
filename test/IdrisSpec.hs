module IdrisSpec where

import Control.Monad (forM_, when)
import Test.Hspec
import Test.Hspec.IdrisGrinBackend
import System.Environment (lookupEnv)

{-
All the failing test cases are marked as pending. A reason is provided what goes wrong
when the test runs. In the long term these quests will be unlocked. :)
-}

modes = [NonOptimisedEval, OptimisedEval, Compiled]

spec :: Spec
spec = do

  modes <- runIO $ do
    env <- lookupEnv "IDRIS_GRIN_CI"
    pure $ maybe [NonOptimisedEval, OptimisedEval, Compiled] (const [NonOptimisedEval]) env
    -- pure [NonOptimisedEval]

  describe "In focus" $ do
    pure ()

  when True $ describe "Idris and Grin matches for:" $ forM_ modes $ \mode -> describe (show mode) $ do
    it "TDD 01 - 01 Hello World" $ timed $ idris mode 600 "test/tdd/chapter01/01_HelloWorld.idr"
    it "TDD 01 - 02 CalcType" $ timed $ idris mode 600 "test/tdd/chapter01/02_CalcType.idr"
    it "TDD 02 - 01 Average" $ timed $ idris mode 600 "test/tdd/chapter02/01_Average.idr"

    -- REASON: -nan is not computed on end of input
    xit "TDD 02 - 02 Average" $ timed $ idrisWithStdin
      mode 60
      "test/tdd/chapter02/02_Average.idr"
      $ unlines
        [ "This is a test sentence."
        , "This is a another test sentence."
        ]

    it "TDD 02 - 03 Prelude" $ timed $ idris mode 600 "test/tdd/chapter02/03_Prelude.idr"
    it "TDD 03 - 01 Matrix" $ timed $ idris mode 600 "test/tdd/chapter03/01_Matrix.idr"

    -- REASON: More than one default case error message is captured
    xit "TDD 04 - 01 Data Types" $ timed $ idris mode 600 "test/tdd/chapter04/01_DataTypes.idr"

    it "TDD 04 - 02 Data Store" $ timed $ idrisWithStdin
      mode 600
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
      mode 600
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
    it "TDD 06 - 01 Type Level Functions" $ timed $ idris mode 600 "test/tdd/chapter06/01TypeLevelFuns.idr"
    it "TDD 06 - 02 DataStore" $ timed $ idrisWithStdin
      mode 600
      "test/tdd/chapter06/02_DataStore.idr"
      $ unlines
        [ "schema Int String"
        , "add 99 \"Red ballons\""
        , "add 76 \"Trombones\""
        , "schema String String Int"
        , "get 1"
        , "quit"
        ]
    it "TDD 07 - 01 Interfaces" $ timed $ idris mode 600 "test/tdd/chapter07/01_Interfaces.idr"
    it "TDD 07 - 02 Expr" $ timed $ idris mode 600 "test/tdd/chapter07/02_Expr.idr"
    it "TDD 08 - 01 ExactLength" $ timed $ idris mode 600 "test/tdd/chapter08/01_ExactLength.idr"
    it "TDD 08 - 02 Reverse" $ timed $ idris mode 600 "test/tdd/chapter08/02_Reverse.idr"
    it "TDD 09 - 01 RemoveElem" $ timed $ idris mode 600 "test/tdd/chapter09/01_RemoveElem.idr"
    it "TDD 09 - 02 Game" $ timed $ idrisWithStdin
      mode 600
      "test/tdd/chapter09/02_Game.idr"
      $ unlines
        [ "Invalid"
        , "t"
        , "d"
        , "s"
        , "e"
        ]
    it "TDD 10 - 01 View" $ timed $ idris mode 600 "test/tdd/chapter10/01_View.idr"
    it "TDD 10 - 01 Exercises" $ timed $ idris mode 600 "test/tdd/chapter10/01_Exercises.idr"
    it "TDD 10 - 02 Recursive Views" $ timed $ idris mode 600 "test/tdd/chapter10/02_RecursiveViews.idr"
    it "TDD 10 - 02 Exercises" $ timed $ idris mode 600 "test/tdd/chapter10/02_Exercises.idr"

    -- REASON: Floating point differences
    xit "TDD 11 - 01 Exercises" $ timed $ idris mode 600 "test/tdd/chapter11/01_Exercises.idr"

    it "TDD 11 - 01 Inf" $ timed $ idris mode 600 "test/tdd/chapter11/01_Inf.idr"

    -- REASON: Shifting operation doesn't produce right values. Root cause: different Int representation.
    xit "TDD 11 - 01 Quiz" $ timed $ idrisWithStdin
      mode 600
      "test/tdd/chapter11/01_Quiz.idr"
      $ unlines
        [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"
        , "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"
        , "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"
        , "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"
        , "quit"
        ]
    it "TDD 11 - 01 SumSimple" $ timed $ idris mode 600 "test/tdd/chapter11/01_SumSimple.idr"

    -- REASON: Shifting operation doesn't produce right values. Root cause: different Int representation.
    xit "TDD 11 - 01 ShiftTest" $ timed $ idris mode 600 "test/tdd/chapter11/01_ShiftTest.idr"

    it "TDD 11 - 02 InfiniteProcesses" $ idrisWithStdin mode 600 "test/tdd/chapter11/02_InfiniteProcesses.idr" $
      unlines [ "1", "2", "3", "4", "5" ]

    -- REASON: Shifting operation doesn't produce right values. Root cause: different Int representation.
    xit "TDD 11 - 02 TotalArithQuiz" $ idrisWithStdin mode 600 "test/tdd/chapter11/02_TotalArithQuizTest.idr" $
      unlines [ "10", "11", "12", "23", "56", "78", "100" ]

    -- REASON: It throws a closed error handler and starts a process which
    -- runs into an infinite loop. Burning the CPU.
    xit "TDD 11 - 02 Exercise" $ idrisWithStdin mode 600 "test/tdd/chapter11/02_ExerciseTest.idr" $
      unlines [ "a", "b", "hello", "World" ]

    -- REASON: Shifting operation doesn't produce right values. Root cause: different Int representation.
    xit "TDD 11 - 03 ArithCmd" $ idrisWithStdin mode 600 "test/tdd/chapter11/03_ArithCmdTest.idr" $
      unlines [ "1", "2", "3", "4", "5" ]

    -- REASON: Random number generator based on shifting
    xit "TDD 11 - 03 ArithCmdDo" $ idrisWithStdin mode 600 "test/tdd/chapter11/03_ArithCmdDoTest.idr" $
      unlines [ "1", "2", "3", "4", "5" ]

    -- REASON: Random number generator based on shifting
    xit "TDD 11 - 03 Exercise" $ idrisWithStdin mode 600 "test/tdd/chapter11/03_ExerciseTest.idr" $
      unlines [ "1", "2", "3", "4", "5" ]

    -- REASON: uncaught exception: IOException of type IllegalOperation
    xit "TDD 11 - 03 Termination" $ idrisWithStdin mode 600 "test/tdd/chapter11/03_TerminationTest.idr" $
      unlines [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]

    it "TDD 12 - 01 TreeLabel"      $ idris mode 600 "test/tdd/chapter12/01_TreeLabel.idr"
    it "TDD 12 - 02 State"          $ idris mode 600 "test/tdd/chapter12/02_State.idr"
    it "TDD 12 - 03 TreeLabelWith"  $ idris mode 600 "test/tdd/chapter12/03_TreeLabelWith.idr"
    it "TDD 12 - 04 Exercise"       $ idris mode 600 "test/tdd/chapter12/04_Exercise01.idr"
    it "TDD 12 - 05 TreeLabelType"  $ idris mode 600 "test/tdd/chapter12/05_TreeLabelType.idr"

    -- REASON: Random number generator based on shifting
    xit "TDD 12 - 06 ArithState"     $ idrisWithStdin mode 600 "test/tdd/chapter12/06_ArithState.idr" $
      unlines  [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "quit" ]
