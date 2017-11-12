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
  conf <- reader config
  let newOrder = runReader (toNewOrder action) conf
      createNewOrder = void . createOrder $ newOrder
      exchangeMethod =
        case action of
          Market {}      -> createNewOrder
          Limit {}       -> createNewOrder
          Stop {}        -> createNewOrder
          Cancel orderId -> cancelOrder orderId
  execExchangeT (exchangeConf conf) exchangeMethod
  logInfo $ "Executed action: " ++ show action

toNewOrder :: Action -> Reader Config NewOrder
toNewOrder action = do
  let productId = toId . product $ action
      selfTrade = DecrementAndCancel
      newOrder =
        case action of
          Market {..} ->
            NewMarketOrder
            { noProductId = productId
            , noSide = side
            , noSelfTrade = selfTrade
            , noSizeAndOrFunds = transformAmount amount
            , noClientOid = Nothing
            }
          Limit {..} ->
            NewLimitOrder
            { noProductId = productId
            , noSide = side
            , noSelfTrade = selfTrade
            , noPrice = limitPrice
            , noSize = size
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
            , noPrice = stopPrice
            , noSizeAndOrFunds = transformAmount amount
            , noClientOid = Nothing
            }
          _ -> error $ "Cannot create new order from " ++ show action
  return newOrder

transformAmount :: Amount -> Either Size (Maybe Size, CB.Cost)
transformAmount amount =
  case amount of
    AmountSize size   -> Left size
    AmountPrice price -> Right (Nothing, realToFrac price)
