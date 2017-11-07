{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.MyAccount.Internal.Order where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action

import           Coinbase.Exchange.Types.Core   hiding (Done, Open)
import           Coinbase.Exchange.Types.Socket

import           Control.Monad.Reader
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HM

--updateOrders :: ExchangeMessage -> Reader MyAccount (HashMap OrderId MyOrder)
--updateOrders msg = do
--    MyAccount {..} <- ask
--    let orderId = msgOrderId msg
--        product = fromId . msgProductId $ msg
--        Pair c1 c2 = product
--        newOrders = case msg of
--            Open {..} ->
--                let newLimitOrder = MyOrder {
--                    orderId = orderId,
--                    action = Limit {
--                        side = msgSide,
--                        product = product,
--                        limitPrice = msgPrice,
--                        size = msgRemainingSize
--                    }
--                }
--                in HM.insert orderId newLimitOrder orders
--            Match {..} ->
--                let maybeExistingOrder = findOrder account orderId
--                    case maybeExistingOrder of
--                        Nothing -> orders -- market order
--                        Just existingOrder -> -- limit / stop order on book
--
--                    newAction = case action existingOrder of
--                        Limit {..} ->
--
--                        Stop {..} ->
--                    updatedOrder = MyOrder {
--                        orderId = orderId,
--                        action = newAction }
--                in HM.insert orderId updatedOrder
--
--
--
--    return newOrders
