{-# LANGUAGE RecordWildCards #-}

module Gdax.Account.MyAccount where

import           Gdax.Account.Balance
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Types.Currency          as C
import           Gdax.Types.Product           as P

import           Coinbase.Exchange.Types.Core hiding (Limit, Price, Size)

import           Data.HashMap.Strict          (HashMap, (!))
import qualified Data.HashMap.Strict          as HM
import           Prelude                      hiding (product)

data MyAccount = MyAccount
  { balances :: HashMap Currency Balance
  , orders   :: HashMap OrderId MyOrder
  } deriving (Show)

getBalance :: Currency -> MyAccount -> Balance
getBalance currency account = balances account ! currency

findAllOrders :: MyAccount -> [MyOrder]
findAllOrders MyAccount {..} = HM.elems orders

findOrder :: MyAccount -> OrderId -> Maybe MyOrder
findOrder MyAccount {..} orderId = HM.lookup orderId orders

findOrdersForProduct :: MyAccount -> Product -> [MyOrder]
findOrdersForProduct MyAccount {..} p =
  let isProduct MyOrder {..} =
        case action of
          CancelAction {}     -> False
          NewAction newAction -> product newAction == p
  in filter isProduct $ HM.elems orders
