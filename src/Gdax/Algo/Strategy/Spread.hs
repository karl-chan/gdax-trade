{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Strategy.Spread where

import           Gdax.Account.Balance
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Algo.Util
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Util.Logger
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (Side (..))

import           Control.Monad.Reader
import           Prelude                      hiding (product)

spread :: Strategy
spread product = do
  logDebug "Running strategy spread."
  ProductBundle {..} <- extractProductBundle product
  let bookSummary@OrderBookSummary {..} = getSummary book
  logDebug $ "Book summary: " ++ show bookSummary
  let hasMoreBalance1 =
        (total balance1) * realToFrac midPrice > (total balance2)
      newAction =
        if hasMoreBalance1
          then Limit
               { side = Sell
               , product = product
               , limitPrice = bestAsk
               , size = realToFrac $ total balance1
               }
          else Limit
               { side = Buy
               , product = product
               , limitPrice = bestBid
               , size =
                   realToFrac $ (total balance2 / realToFrac bestBid :: Double)
               }
      profit = (bestAsk - bestBid) `safeDiv` midPrice
      proposal =
        StrategyProposal
        {name = "Spread", freshPlan = newAction, profit = profit}
  logDebug $ "Proposal: " ++ show proposal
  return proposal
