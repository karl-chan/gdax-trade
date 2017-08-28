module Gdax.Algo.Strategy.Spread where

import           Gdax.Algo.Strategy.Types
import           Gdax.Data.OrderBook
import           Gdax.Data.OrderBook.Types

import           Coinbase.Exchange.Types.Core (Side (..))

spread :: Strategy
spread series book =
    let asks = sortedAsks book
        bids = sortedBids book
        targetAsk = price . head $ asks
        targetBid = price . head $ bids
        midPrice = (targetAsk + targetBid) / 2
        expectedProfit = realToFrac $ (targetAsk - targetBid) / midPrice
    in [(Limit Sell targetAsk Nothing, expectedProfit), (Limit Buy targetBid Nothing, expectedProfit)]
