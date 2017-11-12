{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Strategy.Spread where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Algo.Util
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Logger

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
      action =
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
      profit =
        let numerator = bestAsk - bestBid
            denominator = midPrice
        in (realToFrac numerator :: Double) / (realToFrac denominator :: Double)
      proposal =
        StrategyProposal {name = "Spread", freshPlan = action, profit = profit}
  logDebug $ "Proposal: " ++ show proposal
  return proposal
