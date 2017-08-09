{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gdax.Trade.Auth
import           Gdax.Trade.Feed
import           Gdax.Trade.OrderBook

import           BroadcastChan.Throw                (newBChanListener,
                                                     readBChan)
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Control.Monad                      (forever)
import qualified Data.HashMap as Map
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

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
        putStrLn $ (show $ bookSequence book) ++ (show $ minimumBy (comparing price) $ Map.elems $ bookAsks book)
