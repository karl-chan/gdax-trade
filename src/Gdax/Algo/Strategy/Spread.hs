{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gdax.Algo.Strategy.Spread where

import           Gdax.Account.MyAccount
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Data.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Bundle

import           Coinbase.Exchange.Types.Core (Side (..))

import           Control.Monad.Reader
import           Prelude                      hiding (product)

spread :: Strategy
spread product@(Pair baseCurrency quoteCurrency) = do
    bundle <- ask
    let (bestBid, midPrice, bestAsk) = orderBookSummary $ getOrderBook bundle product
        profit = realToFrac $ (bestAsk - bestBid) / midPrice
    baseBalance <- getBalance baseCurrency <$> reader account
    quoteBalance <- getBalance quoteCurrency <$> reader account
    let hasMoreQuoteCurrency = quoteBalance > baseBalance * realToFrac midPrice
        action =
            if hasMoreQuoteCurrency
                then Limit Buy product bestBid (realToFrac quoteBalance)
                else Limit Sell product bestAsk (realToFrac baseBalance)
    return ([action], profit)
