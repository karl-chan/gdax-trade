{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Cost where

import           Gdax.Algo.Action             as A
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook         as Book
import           Gdax.Types.OrderBook.Util
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (Price, Side (Buy, Sell), Size)

import           Control.Monad.Reader
import           Data.HashMap.Strict          ((!))
import           Prelude                      hiding (product)

calculateCost :: CostCalculator
calculateCost actions = do
  logDebug "In calculate cost."
  case actions of
    [] -> return 0
    action:remainingActions -> do
      singleCost <- calculateSingleCost action
      remainingCost <- calculateCost remainingActions
      return $ singleCost + remainingCost

calculateSingleCost :: Action -> ReaderT Bundle IO Cost
calculateSingleCost action = do
  Bundle {..} <- ask
  case action of
    CancelAction {} -> return 0
    NewAction newAction -> do
      let book = books ! (A.product newAction)
          feesConfig = feesConf config
          pc = platformCharge newAction feesConfig
          sc = marketSpreadCost newAction book
      return $
        pureDebug ("Platform charge: " ++ show pc) pc +
        pureDebug ("Market spread cost: " ++ show sc) sc

-- Cost of currency conversion imposed by platform, as percentage
platformCharge :: NewAction -> FeesConf -> Cost
platformCharge newAction feesConfig =
  let useTakerFee = takerFee (A.product newAction) feesConfig
      useMakerFee = makerFee (A.product newAction) feesConfig
  in case newAction of
       Limit {}  -> useTakerFee
       Market {} -> useMakerFee
       Stop {}   -> useMakerFee

-- Cost of spread induced as market taker, as percentage
marketSpreadCost :: NewAction -> OrderBook -> Cost
marketSpreadCost action book =
  case action of
    Market {..} ->
      let OrderBookSummary {..} = getSummary book
          bookItems =
            case side of
              Buy  -> sortedAsks book
              Sell -> sortedBids book
      in case amount of
           AmountSize tradeSize ->
             let expectedPrice = realToFrac tradeSize * midPrice
                 priceDiff = actualPrice bookItems tradeSize - expectedPrice
             in abs $ priceDiff `safeDiv` expectedPrice
           AmountPrice tradePrice ->
             let expectedSize = tradePrice `safeDiv` midPrice
                 sizeDiff = actualSize bookItems tradePrice - expectedSize
             in abs $ sizeDiff `safeDiv` expectedSize
    _ -> 0

-- Actual size required to buy / sell price from order book items
actualSize :: [OrderBookItem] -> Price -> Size
actualSize items tradePrice = actualSize' items tradePrice 0
  where
    actualSize' :: [OrderBookItem] -> Price -> Size -> Size
    actualSize' bkItems remPrice accSize =
      case bkItems of
        [] -> accSize
        (OrderBookItem {..}:remItems) ->
          if remPrice <= realToFrac size * price
            then actualSize' [] 0 (accSize + remPrice `safeDiv` price)
            else actualSize'
                   remItems
                   (remPrice - realToFrac size * price)
                   (accSize + size)

-- Actual price required to buy / sell size from order book items
actualPrice :: [OrderBookItem] -> Size -> Price
actualPrice items tradeSize = actualPrice' items tradeSize 0
  where
    actualPrice' bkItems remSize accPrice =
      case bkItems of
        [] -> accPrice
        (OrderBookItem {..}:remItems) ->
          if remSize <= size
            then actualPrice' [] 0 (accPrice + realToFrac remSize * price)
            else actualPrice'
                   remItems
                   (remSize - size)
                   (accPrice + realToFrac size * price)
