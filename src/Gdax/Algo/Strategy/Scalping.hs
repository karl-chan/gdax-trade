{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Strategy.Scalping where

import           Gdax.Account.Balance
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import qualified Gdax.Types.TimeSeries.Util as TS
import           Gdax.Util.Logger
import Gdax.Util.Time
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (Side (..))

import Data.Time.Clock
import           Control.Monad.Reader
import           Prelude                      hiding (product)

spread :: Price -> Price -> Reader ProductBundle NewAction
spread resistance support = do
  ProductBundle {..} <- ask
  let profitMargin = (resistance - support) `safeDiv` support -- percentage double
      shouldGamble = profitMargin >= 0.01 -- 1% margin so that costs don't matter
  if shouldGamble
    then takeSides resistance support
    else scalp

takeSides :: Price -> Price -> Reader ProductBundle NewAction
takeSides resistance support = do
  ProductBundle {..} <- ask
  let OrderBookSummary {..} = getSummary book  
      distanceFromAbove = resistance - midPrice
      distanceFromBelow = midPrice - support
  if distanceFromBelow < distanceFromAbove
    then Market {
      side = Buy,
      product = product,
      amount = AmountPrice $ realToFrac $ total balance2
    }

scalping :: Reader ProductBundle NewAction
scalping = do
    ProductBundle {..} <- ask
    StrategyConf {..} <- reader strategyConfig
    let OrderBookSummary {..} = getSummary book
        equivBalance2 = balance1 * midPrice
        lastStat = TS.last series
        interval = diffUTC (end lastStat) (start lastStat)
        percentile (map size trades) 
        volumePerMinute = volume `safeDiv` (realToFrac $ interval / minute) :: Double
    (upperTargetPrice, lowerTargetPrice) <- targetScalpPrices volumePerMinute
    if balance2 > equivBalance2
        then Limit {
          side = Buy,
          product = product,
          limitPrice = upperTargetPrice,
          size = balance2
        } 
        else Limit {
            side = Sell,
            product = product,
            limitPrice = lowerTargetPrice,
            size = balance1
        }

targetScalpPrices :: Size -> Reader ProductBundle (Price, Price)
targetScalpPrices expectedSingleTradeVolume = do
    ProductBundle {..} <- ask
    asks <- sortedAsks book
    bids <- sortedBids book
    let calcTarget remValue acc bookItems
        | remValue <= 0 = acc
        | [] <- bookItems = acc
        | (item: items) <- bookItems = calcTarget items (remValue - size item) (price item)
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
