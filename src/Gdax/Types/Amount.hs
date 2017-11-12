{-# LANGUAGE DeriveAnyClass #-}

module Gdax.Types.Amount where

import           Coinbase.Exchange.Types.Core

data Amount
  = AmountSize Size
  | AmountPrice Price
  deriving (Eq, Show)
