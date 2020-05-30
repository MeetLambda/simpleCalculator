module Test.SimpleCalculator where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Free (Free)
import Control.Semigroupoid ((<<<))
import Data.Array (last, snoc)
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Show (show)
import Data.Traversable (traverse_)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import SimpleCalculator as SimpleCalculator
import Test.Unit (suite, test, TestF)
import Test.Unit.Assert as Assert

-- todo :: forall a. a
-- todo = unsafeCoerce unit

-- foldl :: forall a b f. Foldable f => (b -> a -> b) -> b -> f a -> b
applyKeys :: String -> String
applyKeys k = (foldl SimpleCalculator.handleKey SimpleCalculator.initialState (SimpleCalculator.textToKeys k)).display

-- [{_, s0}, {k1, s1}, {k2, s2}, {k3, s4}]



-- traverse :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
-- sequence :: forall a m. Applicative m => t (m a) -> m (t a)
-- snoc :: forall a. Array a -> a -> Array a
type Accumulator = {key :: SimpleCalculator.Key, status :: SimpleCalculator.Status}

traceKeys :: String -> Aff String
traceKeys ks = do
    let xs = foldl traceKey [{status: SimpleCalculator.initialState, key: SimpleCalculator.NOOP}] (SimpleCalculator.textToKeys ks)
    traverse_ (liftEffect <<< log <<< show) xs
    pure $ maybe "" (\x -> x.status.display) (last xs)
    where
        traceKey :: Array (Accumulator) -> SimpleCalculator.Key -> Array (Accumulator)
        traceKey xs k = snoc xs {key: k, status: SimpleCalculator.handleKey s k}
            where 
                s = maybe SimpleCalculator.initialState (\l -> l.status) (last xs)
                

simpleCalculatorTestSuite :: Free TestF Unit
simpleCalculatorTestSuite =
    suite "Simple Calculator Test Suite" do
        test "trivial tests" do
            do
                result <- traceKeys "4.5 + .5 = + 1 ="
                Assert.equal "6" result
            Assert.equal    "1"     (applyKeys "1")
            Assert.equal    "0.3"   (applyKeys ".3")
            Assert.equal    "5.1"   (applyKeys "4.5 + 0.6 =")
            Assert.equal    "4.5"   (applyKeys "4.5 +")
            Assert.equal    "1"     (applyKeys "+1 =")
            Assert.equal    "-1"    (applyKeys "-1 =")
            Assert.equal    "0"     (applyKeys "/ 1 =")
            Assert.equal    "0"     (applyKeys "* 1 =")
            Assert.equal    "0"     (applyKeys "+=")
            Assert.equal    "0.6"   (applyKeys "4.5 + .6")
            Assert.equal    "2.1"   (applyKeys "4.0 / 2.1")
            Assert.equal    "2"     (applyKeys "4.0 / 2.0 =")
            Assert.equal    "8.4"   (applyKeys "4.0 * 2.1 =")
            Assert.equal    "8"     (applyKeys "4 * 2 =")
            Assert.equal    "3.9"   (applyKeys "4.5 - .6 =")
            Assert.equal    "3.9"   (applyKeys "4.5 - 0.6 =")
            Assert.equal    "4"     (applyKeys "4.5 - .6 C .5 =")
            Assert.equal    "5"     (applyKeys "4.5 - .6C + .5 =")
            Assert.equal    "0.5"   (applyKeys "4.5 - .6 # + .5 =")
            Assert.equal    "5"     (applyKeys "4.5 - .5 = + 1 =")
            Assert.equal    "4"     (applyKeys "4.5 - .5 = 1 + 3 =")
            Assert.equal    "20.5"  (applyKeys "10.3 + 4.1 + 6.1 =")
            Assert.equal    "1.33"  (applyKeys "1..3.3")
            Assert.equal    "2"     (applyKeys "2==")
            -- Assert.weird    "-7.9"  (applyKeys ".3 / 3 - 4 * 2 =")
            Assert.equal    "11" (applyKeys "2 + 3 = = =")

