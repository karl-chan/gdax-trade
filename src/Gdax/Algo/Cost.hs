{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Cost where

import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Data.OrderBook.Types    as BK
import           Gdax.Data.OrderBook.Util
import           Gdax.Util.Bundle
import           Gdax.Util.Config
import           Gdax.Util.Config.Fees
import           Gdax.Util.Feed.OrderBook

import           Coinbase.Exchange.Types.Core (Price, Side (Buy, Sell), Size)

import           Control.Monad.Reader
import           Prelude                      hiding (product)

calculateCost :: CostCalculator
calculateCost action = do
    bundle <- ask
    feesConfig <- reader $ feesConf . config
    let book = getOrderBook bundle $ product action
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
  where
    totalCost' _ [] _ = error "Order book not large enough to handle order amount"
    totalCost' acc (x:xs) remAmount =
        let canComplete =
                case remAmount of
                    Left remSize -> BK.size x >= remSize
                    Right remFunds -> price x * (realToFrac . BK.size) x >= remFunds
        in if canComplete
               then acc +
                    case remAmount of
                        Left remSize   -> realToFrac remSize * price x
                        Right remFunds -> remFunds
               else let inc = (realToFrac . BK.size) x * price x
                        newRemAmount =
                            case remAmount of
                                Left remSize -> Left $ remSize - BK.size x
                                Right remFunds -> Right $ remFunds - inc
                    in totalCost' (acc + inc) xs newRemAmount
