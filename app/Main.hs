{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Gdax.Trade.Auth
import           Gdax.Trade.OrderBook

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM.TChan   (newBroadcastTChanIO)
import           Control.Monad                  (forever, liftM)
import           Data.Aeson                     (eitherDecode)
import           Data.Text                      (Text)
import qualified Data.Text.IO                   as T
import           Network.WebSockets             (ClientApp, Connection,
                                                 receiveData, sendClose,
                                                 sendTextData)

import Data.Either

currencyPair :: ProductId
currencyPair = "ETH-EUR"

runMode :: ApiType
runMode = Live

main :: IO ()
main = do
    conf <- getConf runMode
    orderBookBroadcastChan <- newBroadcastTChanIO
    livecastOrderBook conf currencyPair orderBookBroadcastChan