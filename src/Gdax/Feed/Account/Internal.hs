{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.Account.Internal where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Feed.Account.Types
import           Gdax.Types.Amount
import           Gdax.Types.Currency             (Currency)
import qualified Gdax.Types.Currency             as C
import qualified Gdax.Types.Product              as P
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Throttle

import           Coinbase.Exchange.Private       (getAccountList, getOrderList)
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core    (Cost, OrderId, Size)
import qualified Coinbase.Exchange.Types.Private as CB

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HM
import           Gdax.Util.Logger

streamAccount :: ReaderT Config IO MyAccountFeed
streamAccount = do
  config <- ask
  rate <- reader accountRefreshRate
  liftIO $ do
    accountFeed <- newFeed
    forkIO . forever $ do
      account <- runReaderT initAccount config
      logDebug $ "Initialised account."
      writeFeed accountFeed account
      sleep rate
    return accountFeed

initAccount :: ReaderT Config IO MyAccount
initAccount = do
  balances <- initBalances
  orders <- initOrders
  return MyAccount {balances = balances, orders = orders}

initBalances :: ReaderT Config IO (HashMap Currency Balance)
initBalances = do
  conf <- reader exchangeConf
  accountList <- execExchangeT conf getAccountList
  let convert CB.Account {..} =
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
          CB.LimitOrder {..} ->
            MyOrder
            { orderId = CB.orderId order
            , action =
                NewAction $
                Limit
                { side = orderSide
                , product = P.fromId orderProductId
                , limitPrice = orderPrice
                , size = orderSize
                }
            }
          CB.StopOrder {..} ->
            MyOrder
            { orderId = CB.orderId order
            , action =
                NewAction $
                Stop
                { side = orderSide
                , product = P.fromId orderProductId
                , stopPrice = orderPrice
                , amount = toAmount orderSizeAndOrFunds
                }
            }
          CB.MarketOrder {..} ->
            MyOrder
            { orderId = CB.orderId order
            , action =
                NewAction $
                Market
                { side = orderSide
                , product = P.fromId orderProductId
                , amount = toAmount orderSizeAndOrFunds
                }
            }
      toPair order = (orderId order, order)
  return $ HM.fromList $ map (toPair . convert) orderList

toAmount :: Either Size (Maybe Size, Cost) -> Amount
toAmount sizeOrFunds =
  case sizeOrFunds of
    Left size        -> AmountSize size
    Right (_, funds) -> AmountPrice $ realToFrac funds
