{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Math where

import           Gdax.Types.Amount

import           Coinbase.Exchange.Types.Core

import           Data.List
import           Data.Scientific

-- For scientific instances as they will diverge unless converted to double
safeDiv :: (Real a, Fractional b) => a -> a -> b
safeDiv n1 n2 =
  realToFrac $ (realToFrac n1 :: Double) / (realToFrac n2 :: Double)

average :: (Real a, Fractional a) => [a] -> a
average xs = (sum xs) `safeDiv` (realToFrac . length $ xs)

percentile :: (Real a, Fractional a) => [a] -> Double -> a
percentile xs pc =
  let sorted = sort xs
      item = floor $ pc / 100 * (realToFrac $ length xs)
  in sorted !! item

-- For rounding down (floor) of scientific types
roundCoin :: Int -> CoinScientific -> CoinScientific
roundCoin dp CoinScientific {..} =
  CoinScientific $
  normalize $ scientific (floor $ (10 ^ dp) * unCoinScientific) (-dp)

roundPrice :: Int -> Price -> Price
roundPrice dp Price {..} = Price $ roundCoin dp unPrice

roundSize :: Int -> Size -> Size
roundSize dp Size {..} = Size $ roundCoin dp unSize

roundAmount :: Int -> Amount -> Amount
roundAmount dp amount =
  case amount of
    AmountPrice price -> AmountPrice $ roundPrice dp price
    AmountSize size   -> AmountSize $ roundSize dp size

-- For rough comparison due to rounding dp requirement by GDAX
roughlyEqual :: Real a => a -> a -> Bool
roughlyEqual n1 n2 =
  let tolerance = 1e-6 :: Double
  in abs (realToFrac n1 - realToFrac n2) < tolerance

roughlyEqualAmount :: Amount -> Amount -> Bool
roughlyEqualAmount a1 a2
  | AmountSize s1 <- a1
  , AmountSize s2 <- a2 = roughlyEqual s1 s2
  | AmountPrice p1 <- a1
  , AmountPrice p2 <- a2 = roughlyEqual p1 p2
  | otherwise = False
