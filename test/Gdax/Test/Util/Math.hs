module Gdax.Test.Util.Math where

import           Gdax.Util.Math

import           Test.Tasty.HUnit
import           Text.Printf

-- | Asserts that the specified actual value is roughly equal to the expected value.
assertRoughlyEqual ::
     Real a
  => String -- ^ The message format string (should contain 2 "%f", first = actual, second = expected)
  -> a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
assertRoughlyEqual =
  let tolerance = 1e-6
  in assertRoughlyEqualWithTolerance tolerance

assertRoughlyEqualWithTolerance ::
     Real a
  => Double -- ^ Tolerance as absolute value
  -> String -- ^ The message format string (should contain 2 "%f", first = actual, second = expected)
  -> a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
assertRoughlyEqualWithTolerance tolerance messageFmt expected actual =
  let msg =
        printf
          messageFmt
          (realToFrac expected :: Double)
          (realToFrac actual :: Double)
  in assertBool msg (roughlyEqualWithTolerance tolerance expected actual)
