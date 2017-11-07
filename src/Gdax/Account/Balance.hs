{-# LANGUAGE RecordWildCards #-}

module Gdax.Account.Balance where

import           Gdax.Types.Currency

data Balance = Balance
    { currency  :: Currency
    , total     :: Double
    , available :: Double
    , held      :: Double
    } deriving (Show)

data DeltaBalance = DeltaBalance
    { deltaTotal     :: Double
    , deltaAvailable :: Double
    , deltaHeld      :: Double
    } deriving (Show)

zero :: DeltaBalance
zero = DeltaBalance {deltaTotal = 0, deltaAvailable = 0, deltaHeld = 0}

add :: DeltaBalance -> Balance -> Balance
add DeltaBalance {..} Balance {..} =
    Balance
    { currency = currency
    , total = deltaTotal + total
    , available = deltaAvailable + available
    , held = deltaHeld + held
    }
