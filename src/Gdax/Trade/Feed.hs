module Gdax.Trade.Feed where

import           BroadcastChan.Throw            (BroadcastChan, In, Out,
                                                 newBChanListener,
                                                 newBroadcastChan, readBChan,
                                                 writeBChan)
import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Core   (ProductId)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever, void)
import           Data.Aeson                     (eitherDecode)
import           Data.Either

import qualified Network.WebSockets             as WS

type Feed = BroadcastChan In ExchangeMessage

type FeedListener = BroadcastChan Out ExchangeMessage

newFeed :: ProductId -> IO Feed
newFeed productId = do
    feed <- newBroadcastChan
    forkIO $
        subscribe Live [productId] $ \conn -> do
            forever $ do
                ds <- WS.receiveData conn
                let res = eitherDecode ds
                case res :: Either String ExchangeMessage of
                    Left err  -> error err
                    Right msg -> writeBChan feed msg
    return feed

newFeedListener :: Feed -> IO FeedListener
newFeedListener = newBChanListener

waitUntilFeed :: FeedListener -> IO FeedListener
waitUntilFeed listener = do
    readBChan listener
    return listener
