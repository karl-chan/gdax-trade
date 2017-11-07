{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Executor where

import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Math

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core (Size)
import qualified Coinbase.Exchange.Types.Core as CB

import           Control.Monad
import           Control.Monad.Reader
import           Gdax.Util.Logger
import           Prelude                      hiding (product)

execute :: Executor
execute Proposal {..} = mapM_ executeAction actions

executeAction :: Action -> ReaderT Bundle IO ()
executeAction action = do
    logDebug $ "About to execute action: " ++ show action
    conf <- reader (exchangeConf . config)
    maxDecimalPlaces <- reader (apiDecimalPlaces . config)
    let createNewOrder = void . createOrder . toNewOrder maxDecimalPlaces
        exchangeMethod =
            case action of
                Market {}      -> createNewOrder action
                Limit {}       -> createNewOrder action
                Stop {}        -> createNewOrder action
                Cancel orderId -> cancelOrder orderId
    execExchangeT conf exchangeMethod
    logDebug $ "Executed action: " ++ show action

toNewOrder :: Int -> Action -> NewOrder
toNewOrder maxDecimalPlaces action =
    let productId = toId . product $ action
        selfTrade = DecrementAndCancel
        roundSize' = roundSize maxDecimalPlaces
        roundPrice' = roundPrice maxDecimalPlaces
        roundAmount' = roundAmount maxDecimalPlaces
        newOrder =
            case action of
                Market {..} ->
                    NewMarketOrder
                    { noProductId = productId
                    , noSide = side
                    , noSelfTrade = selfTrade
                    , noSizeAndOrFunds = transformAmount . roundAmount' $ amount
                    , noClientOid = Nothing
                    }
                Limit {..} ->
                    NewLimitOrder
                    { noProductId = productId
                    , noSide = side
                    , noSelfTrade = selfTrade
                    , noPrice = roundPrice' limitPrice
                    , noSize = roundSize' size
                    , noTimeInForce = GoodTillCanceled
                    , noPostOnly = True
                    , noClientOid = Nothing
                    , noCancelAfter = Nothing
                    }
                Stop {..} ->
                    NewStopOrder
                    { noProductId = productId
                    , noSide = side
                    , noSelfTrade = selfTrade
                    , noPrice = roundPrice' stopPrice
                    , noSizeAndOrFunds = transformAmount . roundAmount' $ amount
                    , noClientOid = Nothing
                    }
                _ -> error $ "Cannot create new order from " ++ show action
    in pureDebug ("Will execute new order: " ++ show newOrder) newOrder

transformAmount :: Amount -> Either Size (Maybe Size, CB.Cost)
transformAmount amount =
    case amount of
        Size size   -> Left size
        Price price -> Right (Nothing, realToFrac price)
