{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy where

import           Gdax.Algo.Signal.Direction
import           Gdax.Algo.Strategy.Scalping
import           Gdax.Algo.Strategy.SupportResistance
import           Gdax.Algo.Types
import           Gdax.Types.Bundle

import           Control.Monad.Reader
import           Prelude                              hiding (product)

strategy :: Strategy
strategy product = do
  bundle <- ask
  let productBundle = toProductBundle product bundle
  return $ runReader productStrategy productBundle

productStrategy :: Reader ProductBundle Proposal
productStrategy = do
  direction <- getDirection
  case direction of
    Up   -> supportResistance
    Down -> supportResistance
    None -> scalping
