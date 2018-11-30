module IdrisSpec where

import Test.Hspec
import Test.Hspec.IdrisFrontend

spec :: Spec
spec = describe "Idris and Grin matches for:" $ do
  it "TDD 01 - 01 Hello World" $ idris "test/tdd/chapter01/01_HelloWorld.idr"
  it "TDD 01 - 02 CalcType" $ idris "test/tdd/chapter01/02_CalcType.idr"
  it "TDD 02 - 01 Average" $ idris "test/tdd/chapter02/01_Average.idr"
  it "TDD 02 - 03 Prelude" $ idris "test/tdd/chapter02/03_Prelude.idr"
  it "TDD 03 - 01 Matrix" $ idris "test/tdd/chapter03/01_Matrix.idr"
