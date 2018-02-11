{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Strategy.Scalping where

import           Gdax.Account.Balance
import           Gdax.Algo.Strategy.Util
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook         as Bk
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.Trades            as Trade
import           Gdax.Util.Config             as Config
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (Price, Size)

import           Control.Monad.Reader         hiding (asks)
import           Prelude                      hiding (product)

scalping :: Reader ProductBundle Proposal
scalping = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      OrderBookSummary {..} = getSummary book
      tradeSizes = map Trade.size trades
      targetSingleTradeSize = percentile tradeSizes scalpingPercentile
  (upperTargetPrice, lowerTargetPrice) <-
    targetScalpPrices targetSingleTradeSize
  let hasMoreBalance2 = total balance2 > total balance1 * realToFrac midPrice
  action <-
    if hasMoreBalance2
      then limitBuy upperTargetPrice
      else limitSell lowerTargetPrice
  return Proposal {actions = [action]}

targetScalpPrices :: Size -> Reader ProductBundle (Price, Price)
targetScalpPrices expectedSingleTradeSize = do
  ProductBundle {..} <- ask
  let asks = sortedAsks book
      bids = sortedBids book
      calcTarget remSize acc bookItems
        | remSize <= 0 = acc
        | [] <- bookItems = acc
        | (item:items) <- bookItems =
          calcTarget (remSize - Bk.size item) (Bk.price item) items
      f = calcTarget expectedSingleTradeSize 0
  return (f asks, f bids)
