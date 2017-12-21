{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Strategy.Scalping where

import           Gdax.Account.Balance
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook         as OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.TimeSeries
import qualified Gdax.Types.TimeSeries.Util   as TS
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Math
import           Gdax.Util.Time

import           Coinbase.Exchange.Types.Core (Price (..), Side (..), Size (..))

import           Control.Monad.Reader         hiding (asks)
import           Data.Time.Clock
import           Prelude                      hiding (product)

scalping :: Reader ProductBundle Proposal
scalping = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      OrderBookSummary {..} = getSummary book
      equivBalance2 = total balance1 * realToFrac midPrice
      lastStat = TS.last series
      interval = diffUTCTime (end lastStat) (start lastStat)
      volumePerMinute =
        (volume lastStat) `safeDiv` (realToFrac $ interval / minute)
  (upperTargetPrice, lowerTargetPrice) <- targetScalpPrices volumePerMinute
  let actions =
        if total balance2 > equivBalance2
          then [ NewAction $
                 Limit
                 { side = Buy
                 , product = product
                 , limitPrice = upperTargetPrice
                 , size = realToFrac $ total balance2
                 }
               ]
          else [ NewAction $
                 Limit
                 { side = Sell
                 , product = product
                 , limitPrice = lowerTargetPrice
                 , size = realToFrac $ total balance1
                 }
               ]
  return Proposal {actions = actions}

targetScalpPrices :: Size -> Reader ProductBundle (Price, Price)
targetScalpPrices expectedSingleTradeVolume = do
  ProductBundle {..} <- ask
  let asks = sortedAsks book
      bids = sortedBids book
      calcTarget remSize acc bookItems
        | remSize <= 0 = acc
        | [] <- bookItems = acc
        | (item:items) <- bookItems =
          calcTarget (remSize - OrderBook.size item) (price item) items
      f = calcTarget expectedSingleTradeVolume 0
  return (f asks, f bids)
-- spread :: Strategy
-- spread product = do
--   logDebug "Running strategy spread."
--   ProductBundle {..} <- extractProductBundle product
--   let bookSummary@OrderBookSummary {..} = getSummary book
--   logDebug $ "Book summary: " ++ show bookSummary
--   let hasMoreBalance1 =
--         (total balance1) * realToFrac midPrice > (total balance2)
--       newAction =
--         if hasMoreBalance1
--           then Limit
--                { side = Sell
--                , product = product
--                , limitPrice = bestAsk
--                , size = realToFrac $ total balance1
--                }
--           else Limit
--                { side = Buy
--                , product = product
--                , limitPrice = bestBid
--                , size =
--                    realToFrac $ (total balance2 / realToFrac bestBid :: Double)
--                }
--       profit = (bestAsk - bestBid) `safeDiv` midPrice
--       proposal =
--         StrategyProposal
--         {name = "Spread", freshPlan = newAction, profit = profit}
--   logDebug $ "Proposal: " ++ show proposal
--   return proposal
