{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gdax.Data.OrderBook
import           Gdax.Data.OrderBook.Types
import           Gdax.Util.Auth
import           Gdax.Util.Feed

import           BroadcastChan.Throw          (newBChanListener, readBChan)
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Control.Monad                (forever)

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
        putStrLn $ (show $ bookSequence book) ++ (show $ ask book) ++ (show $ bid book)
