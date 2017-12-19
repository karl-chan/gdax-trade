{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy where

import           Gdax.Algo.Strategy.SupportResistance
import           Gdax.Algo.Strategy.Trend
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.TimeSeries.Util           as TS

strategy :: Strategy
strategy [product] = do
  bundle <- ask
  productBundle <- toProductBundle product bundle
  let trend = identifyDirection series
      points = identifyTurningPoints series
      OrderBookSummary {..} = getSummary book
      [maybeResistance, maybeSupport] =
        map (nextTurningPoint midPrice points) [Up, Down]
      action =
        runReader
          (determineAction trend maybeResistance maybeSupport)
          productBundle
  return Proposal {actions = [action]}

determineAction ::
     Direction -> Maybe Price -> Maybe Price -> Reader ProductBundle NewAction
determineAction trend maybeResistance maybeSupport = do
  ProductBundle {..} <- ask
  return $
    case (trend, maybeResistance, maybeSupport) of
      (Up, Nothing, _) ->
        Market
        { side = Buy
        , product = product
        , amount = AmountPrice $ realToFrac $ total balance2
        }
      (Up, Just resistance, _) ->
        Limit
        { side = Sell
        , product = product
        , limitPrice = resistance
        , size = realToFrac $ total balance1
        }
      (Down, _, Nothing) ->
        Market
        { side = Sell
        , product = product
        , amount = AmountSize $ realToFrac $ total balance1
        }
      (Down, _, Just support) ->
        let lastHourStat =
              case combineStats . seriesToStats $ lookback series hour of
                Nothing   -> error "empty time series, unable to proceed"
                Just stat -> stat
            stopPrice = high lastHourStat * (1 - tolerance strategyConf)
            sellAmount = AmountSize $ realToFrac $ total balance1
        in sellNowOrLater currentPrice stopPrice product sellAmount
      (None, Just resistance, Just support) -> spread resistance support

sellNowOrLater :: Price -> Price -> Product -> Amount -> NewAction
sellNowOrLater currentPrice stopPrice product sellAmount =
  if currentPrice < stopPrice
    then Market {side = Sell, product = product, amount = sellAmount}
    else Stop
         { side = Sell
         , product = product
         , stopPrice = stopPrice
         , amount = sellAmount
         }
