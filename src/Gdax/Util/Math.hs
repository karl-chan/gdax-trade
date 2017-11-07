{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Math where

import           Gdax.Types.Amount            (Amount)
import qualified Gdax.Types.Amount            as Amount

import           Coinbase.Exchange.Types.Core

import           Data.Scientific

roundCoin :: Int -> CoinScientific -> CoinScientific
roundCoin dp CoinScientific {..} = CoinScientific $ normalize $ scientific (round $ (10 ^ dp) * unCoinScientific) (-dp)

roundPrice :: Int -> Price -> Price
roundPrice dp Price {..} = Price $ roundCoin dp unPrice

roundSize :: Int -> Size -> Size
roundSize dp Size {..} = Size $ roundCoin dp unSize

roundAmount :: Int -> Amount -> Amount
roundAmount dp amount =
    case amount of
        Amount.Price price -> Amount.Price $ roundPrice dp price
        Amount.Size size   -> Amount.Size $ roundSize dp size
