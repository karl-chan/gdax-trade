{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.MyAccount.Internal where

import Gdax.Account.Balance
import Gdax.Account.MyAccount
import Gdax.Account.MyOrder
import Gdax.Algo.Action
import Gdax.Feed.Gdax.Types
import Gdax.Feed.MyAccount.Internal.Balance
import Gdax.Feed.MyAccount.Types
import Gdax.Types.Currency hiding (fromId)
import Gdax.Types.Product
import Gdax.Util.Config
import Gdax.Util.Feed
import Gdax.Util.Throttle

import Coinbase.Exchange.Types.Core hiding (Done, Limit, Open)
import Coinbase.Exchange.Types.Socket

import Control.Concurrent
import Control.Monad.Reader
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Gdax.Util.Logger

streamAccount :: GdaxFeedListener -> ReaderT Config IO MyAccountFeed
streamAccount gdaxFeedListener = do
  config <- ask
  feesConfig <- reader feesConf
  liftIO $ do
    accountFeed <- newFeed
    forkIO . forever $ do
      account <- runReaderT initAccount config
      logDebug $ "Initialised account."
      writeFeed accountFeed account
      sleep 0.5
--        forkIO $ do
--            initialAccount <- runReaderT initAccount config
--            let loop account = do
--                    writeFeed accountFeed account
--                    exchangeMsg <- readFeed gdaxFeedListener
--                    loop $ updateAccount feesConfig exchangeMsg account
--            loop initialAccount
    return accountFeed
--isMine :: ExchangeMessage -> Bool
--isMine exchangeMsg = isJust $ msgUserId exchangeMsg
--updateAccount :: FeesConf -> ExchangeMessage -> MyAccount -> MyAccount
--updateAccount feesConf msg account =
  {-  if (not . isMine) msg
        then account
        else let newBalances = runReader (updateBalances msg feesConf) account
                 newOrders =
                     let orderId = msgOrderId msg
                     in case msg of
                            Open {..}        -> insertLimitOrder msg orders
                            ChangeLimit {..} -> insertLimitOrder msg orders
                            Match {..}       -> matchOrder msg orders
                            Done {..}        -> deleteOrder orderId orders
                            _                -> orders
             in trace
                    ("user_id: " ++ (show . msgUserId) msg ++ ", taker_user_id: " ++ (show . msgTakerUserId) msg)
                    account {balances = newBalances, orders = newOrders}-}
