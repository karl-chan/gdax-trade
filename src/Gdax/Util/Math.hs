{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Math where

import           Gdax.Types.Amount

import           Data.List
import           Data.Ord

-- For scientific instances as they will diverge unless converted to double
safeDiv :: (Real n1, Real n2, Fractional n) => n1 -> n2 -> n
safeDiv n1 n2 =
  realToFrac $ (realToFrac n1 :: Double) / (realToFrac n2 :: Double)

average :: (Real a, Fractional a) => [a] -> a
average xs = (sum xs) `safeDiv` (length $ xs)

percentile :: (Ord a) => [a] -> Double -> a
percentile = percentileBy id

percentileBy :: (Ord b) => (a -> b) -> [a] -> Double -> a
percentileBy fn xs pc =
  let sorted = sortBy (comparing fn) xs
      item = floor $ pc / 100 * (realToFrac $ length xs)
  in sorted !! item

-- For rough comparison due to rounding dp requirement by GDAX
roughlyEqual :: Real a => a -> a -> Bool
roughlyEqual =
  let tolerance = 1e-6
  in roughlyEqualWithTolerance tolerance

roughlyEqualWithTolerance :: (Real a) => Double -> a -> a -> Bool
roughlyEqualWithTolerance tolerance n1 n2 =
  abs (realToFrac n1 - realToFrac n2) < tolerance

roughlyEqualAmount :: Amount -> Amount -> Bool
roughlyEqualAmount a1 a2
  | AmountSize s1 <- a1
  , AmountSize s2 <- a2 = roughlyEqual s1 s2
  | AmountPrice p1 <- a1
  , AmountPrice p2 <- a2 = roughlyEqual p1 p2
  | otherwise = False
