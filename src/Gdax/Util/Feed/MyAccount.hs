{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Feed.MyAccount where

import           Gdax.Account.MyAccount
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax

import           Coinbase.Exchange.Types.Core   hiding (Done, Open, Limit)
import           Coinbase.Exchange.Types.Socket

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe
import           Prelude                        hiding (product)

newAccountFeed :: GdaxFeed -> ReaderT Config IO MyAccountFeed
newAccountFeed gdaxFeed = do
    gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
    streamAccount gdaxFeedListener

streamAccount :: GdaxFeedListener -> ReaderT Config IO MyAccountFeed
streamAccount gdaxFeedListener = do
    config <- ask
    refreshRate <- reader bundleRefreshRate
    liftIO $ do
        accountFeed <- newFeed
        forkIO $ do
            initialAccount <- runReaderT initAccount config
            let loop account = do
                    exchangeMsg <- readFeed gdaxFeedListener
                    loop $ updateAccount account exchangeMsg
            loop initialAccount
        return accountFeed

isMine :: ExchangeMessage -> Bool
isMine exchangeMsg = isJust $ msgUserId exchangeMsg

updateAccount :: MyAccount -> ExchangeMessage -> MyAccount
updateAccount account msg =
    if (not . isMine) msg
        then account
        else let currBalances = balances account
                 currOrders = orders account
                 newBalances = currBalances
                    -- TODO balances
                 newOrders =
                     let orderId = msgOrderId msg
                         product = fromId . msgProductId $ msg
                     in case msg of
                            Open {..} -> insertLimitOrder orderId product msgSide msgPrice msgRemainingSize currOrders
                            ChangeLimit {..} -> insertLimitOrder orderId product msgSide msgPrice msgNewSize currOrders
                            Done {..} -> HM.delete orderId currOrders
                            _ -> currOrders
             in account {balances = newBalances, orders = newOrders}

insertLimitOrder :: OrderId -> Product -> Side -> Price -> Size -> HashMap OrderId MyOrder -> HashMap OrderId MyOrder
insertLimitOrder orderId product side price size orders =
    let order =
            MyOrder
            {orderId = orderId, action = Limit {side = side, product = product, limitPrice = price, size = size}}
    in HM.insert orderId order orders
