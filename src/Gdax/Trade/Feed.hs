module Gdax.Trade.Feed
    ( newFeed
    , Feed
    ) where

import           BroadcastChan.Throw            (BroadcastChan, In,
                                                 newBroadcastChan, writeBChan)
import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Core   (ProductId)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage)
import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever)
import           Data.Aeson                     (eitherDecode)
import           Data.Either

import qualified Network.WebSockets             as WS

type Feed = BroadcastChan In ExchangeMessage

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
