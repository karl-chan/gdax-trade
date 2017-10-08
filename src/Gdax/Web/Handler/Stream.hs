{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Gdax.Web.Handler.Stream where

import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Web.Types

import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.String.Conversions
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Happstack.Server
import           Happstack.Server.WebSockets
import qualified Network.WebSockets             as WS
import           System.Log.Logger

streamHandler :: Handler
streamHandler = do
    conf <- reader exchangeConf
    runWebSocketsHappstack $ app conf
    ok $ toResponse ()

app :: ExchangeConf -> WS.ServerApp
app conf pendingConnection = do
    liftIO $ debugM __FILE__ "accepting connection..."
    conn <- WS.acceptRequest pendingConnection
    liftIO $ debugM __FILE__ "accepted."
    forever $ do
        req <- WS.receiveData conn :: IO Text
        debugM __FILE__ $ "Received request: " ++ cs req
        let productIds = map ProductId $ T.splitOn "," req :: [ProductId]
        subscribe conf Live productIds $ \gdaxConn ->
            forkIO . forever $ do
                gdaxResponse <- WS.receiveData gdaxConn :: IO Text
                WS.sendTextData conn gdaxResponse