{-# LANGUAGE RecordWildCards #-}

module Gdax.Account.MyAccount where

import           Gdax.Account.Balance
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Types.Currency          as C
import           Gdax.Types.Product           as P
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Private    hiding (orderId)
import qualified Coinbase.Exchange.Private as CB
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core hiding (Limit)

import           Control.Monad.Reader
import           Data.HashMap.Strict          (HashMap, (!))
import qualified Data.HashMap.Strict          as HM

data MyAccount = MyAccount
    { balances :: HashMap Currency Balance
    , orders   :: HashMap OrderId MyOrder
    }

type MyAccountFeed = Feed MyAccount

type MyAccountFeedListener = FeedListener MyAccount

getBalance :: Currency -> MyAccount -> Double
getBalance currency account = total $ balances account ! currency

initAccount :: ReaderT Config IO MyAccount
initAccount = do
    balances <- initBalances
    orders <- initOrders
    return MyAccount {balances = balances, orders = orders}

initBalances :: ReaderT Config IO (HashMap Currency Balance)
initBalances = do
    conf <- reader exchangeConf
    accountList <- execExchangeT conf getAccountList
    let convert Account {..} =
            let currency = C.fromId accCurrency
            in ( currency
               , Balance
                 { currency = currency
                 , total = realToFrac accBalance
                 , available = realToFrac accAvailable
                 , held = realToFrac accHold
                 })
    return $ HM.fromList $ map convert accountList

initOrders :: ReaderT Config IO (HashMap OrderId MyOrder)
initOrders = do
    conf <- reader exchangeConf
    orderList <- execExchangeT conf $ getOrderList []
    let convert order =
            case order of
                LimitOrder {..} ->
                    MyOrder
                    { orderId = CB.orderId order
                    , action =
                          Limit
                          { side = orderSide
                          , product = P.fromId orderProductId
                          , limitPrice = orderPrice
                          , size = orderSize
                          }
                    }
                StopOrder {..} ->
                    let toAmount sizeOrFunds =
                            case sizeOrFunds of
                                Left size        -> Left size
                                Right (_, funds) -> Right $ realToFrac funds
                    in MyOrder
                       { orderId = CB.orderId order
                       , action =
                             Stop
                             { side = orderSide
                             , product = P.fromId orderProductId
                             , stopPrice = orderPrice
                             , amount = toAmount orderSizeAndOrFunds
                             }
                       }
        toPair order = (orderId order, order)
    return $ HM.fromList $ map (toPair . convert) orderList
