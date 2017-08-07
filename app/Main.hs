{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gdax.Trade.Auth
import           Gdax.Trade.Feed
import           Gdax.Trade.OrderBook

import           BroadcastChan.Throw                (newBChanListener,
                                                     readBChan)
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket
import           Control.Concurrent                 (forkIO)
import           Control.Monad                      (forM_, forever, liftM)
import           Data.Text                          (Text)
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData, sendClose,
                                                     sendTextData)

currencyPair :: ProductId
currencyPair = "ETH-EUR"

runMode :: ApiType
runMode = Live

main :: IO ()
main = do
    conf <- getConf runMode
    feed <- newFeed currencyPair
    orderBookListener <- newBChanListener =<< livecastOrderBook currencyPair conf feed
    forever $ do
        book <- readBChan orderBookListener
        print $ bookSequence book
