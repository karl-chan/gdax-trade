module Gdax.Test.Util.Math where

import           Gdax.Util.Math

import           Test.Tasty.HUnit
import           Text.Printf

-- | Asserts that the specified actual value is roughly equal to the expected value.
assertRoughlyEqual ::
     Real a
  => String -- ^ The message format string (should contain 2 "%s", first = expected, second = actual)
  -> a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
assertRoughlyEqual messageFmt expected actual =
  let msg =
        printf
          messageFmt
          (realToFrac expected :: Double)
          (realToFrac actual :: Double)
  in assertBool msg (expected `roughlyEqual` actual)
