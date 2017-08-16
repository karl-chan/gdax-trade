module Gdax.Data.Product where

import           Gdax.Util.Feed

import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Core   (ProductId, Sequence)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage, msgSequence)

import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever, void)
import           Data.Aeson                     (eitherDecode)
import           Data.Either
import qualified Data.PQueue.Prio.Min           as PQ
import qualified Network.WebSockets             as WS

type ProductFeed = Feed ExchangeMessage

type ProductFeedListener = FeedListener ExchangeMessage

newProductFeed :: [ProductId] -> IO ProductFeed
newProductFeed productIds = do
    productFeed <- newFeed
    forkIO $
        subscribe Live productIds $ \conn -> forever $ do
                ds <- WS.receiveData conn
                let res = eitherDecode ds
                case res :: Either String ExchangeMessage of
                    Left err  -> error err
                    Right msg -> writeFeed productFeed msg
    return productFeed