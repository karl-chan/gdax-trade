{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gdax.Algo.Strategy.Spread where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Logger

import           Coinbase.Exchange.Types.Core (Side (..))

import           Control.Monad.Reader

spread :: Strategy
spread product@(Pair c1 c2) = do
    logDebug "Running strategy spread."
    bundle <- ask
    account <- reader account
    let book = orderBook product bundle
        (bestBid, midPrice, bestAsk) = orderBookSummary book
        profit =
            let numerator = bestAsk - bestBid
                denominator = midPrice
            in (realToFrac numerator :: Double) / (realToFrac denominator :: Double)
        balance1 = total $ getBalance c1 account
        balance2 = total $ getBalance c2 account
    logDebug $ "Book summary: " ++ show (bestBid, midPrice, bestAsk)
    let hasMoreBalance1 = balance1 * realToFrac midPrice > balance2
        action =
            if hasMoreBalance1
                then Limit Sell product bestAsk (realToFrac balance1)
                else Limit Buy product bestBid (realToFrac balance2)
        proposal =
            StrategyProposal {strategyName = "Spread", strategyActions = [action], strategyEstimatedProfit = profit}
    logDebug $ "Proposal: " ++ show proposal
    return proposal
