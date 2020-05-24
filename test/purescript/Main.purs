module Test.Main where

import Data.Unit (Unit)
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.SimpleCalculator as Test.SimpleCalculator

main :: Effect Unit
main = runTest do
    Test.SimpleCalculator.simpleCalculatorTestSuite
