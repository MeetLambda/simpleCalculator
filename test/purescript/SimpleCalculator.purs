module Test.SimpleCalculator where

import Control.Bind (discard)
import Control.Monad.Free (Free)
import Data.Eq ((==))
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Semiring ((+))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import SimpleCalculator as SimpleCalculator


applyKeys :: String -> String
applyKeys k = (foldl SimpleCalculator.handleKey SimpleCalculator.initialState (SimpleCalculator.keys k)).display

simpleCalculatorTestSuite :: Free TestF Unit
simpleCalculatorTestSuite =
    suite "Simple Calculator Test Suite" do
        test "trivial tests" do
            -- Assert.assert      "2 + 2 should be 4"    $ (2 + 2) == 4
            -- Assert.assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
            -- Assert.equal       4 (2 + 2)
            -- Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
            Assert.equal    "1" (applyKeys "1")
            Assert.equal    "0.3" (applyKeys ".3")
            Assert.equal    "5" (applyKeys "4.4+0.6=")
            -- Assert.equal    "20.5" (applyKeys "10.3+4.1+6.1=")
