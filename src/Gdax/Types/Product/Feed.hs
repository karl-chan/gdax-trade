module Gdax.Types.Product.Feed where

import           Gdax.Types.Product
import           Gdax.Util.Feed

import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Core   (ProductId (..), Sequence)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage, msgSequence)

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Network.WebSockets             as WS

type ProductFeed = Feed ExchangeMessage

type ProductFeedListener = FeedListener ExchangeMessage

newProductFeed :: [Product] -> IO ProductFeed
newProductFeed products = do
    productFeed <- newFeed
    forkIO $
        subscribe Live (map toId products) $ \conn ->
            forever $ do
                ds <- WS.receiveData conn
                let res = eitherDecode ds
                case res :: Either String ExchangeMessage of
                    Left err  -> error err
                    Right msg -> writeFeed productFeed msg
    return productFeed
