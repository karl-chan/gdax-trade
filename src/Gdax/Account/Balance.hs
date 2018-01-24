{-# LANGUAGE RecordWildCards #-}

module Gdax.Account.Balance where

import           Gdax.Types.Currency

data Balance = Balance
  { currency  :: Currency
  , total     :: Double
  , available :: Double
  , held      :: Double
  } deriving (Show)
