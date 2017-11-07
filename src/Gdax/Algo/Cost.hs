{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Cost where

import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook         as Book
import           Gdax.Types.OrderBook.Util
import           Gdax.Util.Config
import           Gdax.Util.Config.Fees

import           Coinbase.Exchange.Types.Core (Price, Side (Buy, Sell))

import           Control.Monad.Reader
import           Prelude                      hiding (product)

calculateCost :: CostCalculator
calculateCost action = do
    bundle <- ask
    feesConfig <- reader $ feesConf . config
    let book = orderBook (product action) bundle
    return $ platformCharge action feesConfig + spreadCost action book

-- Cost of currency conversion imposed by platform, as percentage
platformCharge :: Action -> FeesConf -> Cost
platformCharge action feesConfig =
    case action of
        Cancel {} -> 0
        Limit {}  -> takerFee (product action) feesConfig
        _         -> makerFee (product action) feesConfig

-- Cost of spread during transaction, as percentage
spreadCost :: Action -> OrderBook -> Cost
spreadCost Market {..} book =
    let (_, midPrice, _) = orderBookSummary book
        bookItems =
            case side of
                Buy  -> sortedAsks book
                Sell -> sortedBids book
    in realToFrac . abs $ (totalCost bookItems amount - midPrice) / midPrice
spreadCost _ _ = 0

-- Total price required to buy size from order book items
totalCost :: [OrderBookItem] -> Amount -> Price
totalCost = totalCost' 0

totalCost' :: Price -> [OrderBookItem] -> Amount -> Price
totalCost' cost [] _ = cost -- Base case
totalCost' acc (bkItem:items) remAmount =
    let canComplete =
            case remAmount of
                Size remSize -> Book.size bkItem >= remSize
                Price remFunds -> price bkItem * (realToFrac . Book.size) bkItem >= remFunds
    in if canComplete
           then let inc =
                        case remAmount of
                            Size remSize -> realToFrac remSize * price bkItem
                            Price remFunds -> remFunds
                    zero =
                        case remAmount of
                            Size {}  -> Size 0
                            Price {} -> Price 0
                in totalCost' (acc + inc) [] zero
           else let inc = (realToFrac . Book.size) bkItem * price bkItem
                    newRemAmount =
                        case remAmount of
                            Size remSize -> Size $ remSize - Book.size bkItem
                            Price remFunds -> Price $ remFunds - inc
                in totalCost' (acc + inc) items newRemAmount
