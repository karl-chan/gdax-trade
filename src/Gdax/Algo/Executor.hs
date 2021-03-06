{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Executor where

import           Gdax.Algo.Action             as A
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Logger

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core (Size)
import qualified Coinbase.Exchange.Types.Core as CB

import           Control.Monad
import           Control.Monad.Reader
import           Prelude                      hiding (product)

execute :: Executor
execute Proposal {..} = mapM_ executeAction actions

executeAction :: Action -> ReaderT Bundle IO ()
executeAction action = do
  logDebug $ "About to execute action: " ++ show action
  conf <- reader config
  let exchangeMethod =
        case action of
          CancelAction cancelAction ->
            case cancelAction of
              Cancel orderId -> cancelOrder orderId
              CancelProduct product ->
                void $ cancelAllOrders (Just $ toId product)
              CancelAll -> void $ cancelAllOrders Nothing
          NewAction newAction ->
            let newOrder = runReader (toNewOrder newAction) conf
            in void $ createOrder newOrder
  execExchangeT (exchangeConf conf) exchangeMethod
  logInfo $ "Executed action: " ++ show action

toNewOrder :: NewAction -> Reader Config NewOrder
toNewOrder newAction = do
  let productId = toId . A.product $ newAction
      selfTrade = DecrementAndCancel
      newOrder =
        case newAction of
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
  return newOrder

transformAmount :: Amount -> Either Size (Maybe Size, CB.Cost)
transformAmount amount =
  case amount of
    AmountSize size   -> Left size
    AmountPrice price -> Right (Nothing, realToFrac price)
