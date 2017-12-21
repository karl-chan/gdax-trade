{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy.Momentum where

import           Gdax.Account.Balance
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.TimeSeries
import           Gdax.Types.TimeSeries.Util   hiding (head, last, product)
import qualified Gdax.Types.TimeSeries.Util   as TS
import           Gdax.Util.Config
import           Gdax.Util.Time

import           Coinbase.Exchange.Types.Core hiding (Market)

import           Control.Monad.Reader
import           Prelude                      hiding (product)

momentum :: Reader ProductBundle Proposal
momentum = do
  ProductBundle {..} <- ask
  direction <- identifyDirection
  let actions =
        case direction of
          Up ->
            [ NewAction $
              Market
              { side = Buy
              , product = product
              , amount = AmountPrice $ realToFrac $ total balance2
              }
            ]
          Down ->
            [ NewAction $
              Market
              { side = Sell
              , product = product
              , amount = AmountSize $ realToFrac $ total balance1
              }
            ]
          _ -> []
  return Proposal {actions = actions}

-- exceeds 1% over the last hour
identifyDirection :: Reader ProductBundle Direction
identifyDirection = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      considerPeriod = hour -- last hour
      lastHourSeries = lookback series considerPeriod
      openPrice = TS.open lastHourSeries
      closePrice = TS.close lastHourSeries
      determineDirection
        | closePrice > openPrice * (1 + realToFrac tolerance) = Up
        | closePrice < openPrice * (1 - realToFrac tolerance) = Down
        | otherwise = None
  return determineDirection
