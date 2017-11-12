module Gdax.Util.Math.Test where

import           Gdax.Util.Math

import           Test.Tasty.HUnit

-- | Asserts that the specified actual value is roughly equal to the expected value.
assertRoughlyEqual ::
     Real a
  => String -- ^ The message prefix
  -> a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
assertRoughlyEqual preface expected actual =
  assertBool preface (expected `roughlyEqual` actual)
