{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gdax.Data.OrderBook
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Data.TimeSeries
import           Gdax.Data.TimeSeries.Types
import           Gdax.Util.Auth
import           Gdax.Util.Feed

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (forever, when)
import           Control.Monad.STM
import           Data.Maybe
import           Data.Time.Calendar           (fromGregorian)
import           Data.Time.Clock              (UTCTime(..))

currencyPair :: ProductId
currencyPair = "ETH-EUR"

runMode :: ApiType
runMode = Live

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2017 8 1) 0

granularity :: Granularity
granularity = 60 -- 1 minute

main :: IO ()
main = do
    conf <- getConf runMode
    productFeed <- newProductFeed currencyPair
--    bookFeed <- liveOrderBookFeed currencyPair conf productFeed
    tsFeed <- liveTSFeed startTime granularity currencyPair conf productFeed
    tsListener <- newFeedListener tsFeed
    forever $ do
        ts <- readFeed tsListener
        return ()
