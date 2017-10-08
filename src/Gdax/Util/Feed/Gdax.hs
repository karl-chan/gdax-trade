module Gdax.Util.Feed.Gdax where

import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Socket (ExchangeMessage)

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Network.WebSockets             as WS

type GdaxFeed = Feed ExchangeMessage

type GdaxFeedListener = FeedListener ExchangeMessage

newGdaxFeed :: [Product] -> ReaderT Config IO GdaxFeed
newGdaxFeed products = do
    conf <- reader exchangeConf
    liftIO $ do
        gdaxFeed <- newFeed
        forkIO $
            subscribe conf Live (map toId products) $ \conn ->
                forever $ do
                    ds <- WS.receiveData conn
                    let res = eitherDecode ds :: Either String ExchangeMessage
                    case res of
                        Left err  -> error err
                        Right msg -> writeFeed gdaxFeed msg
        return gdaxFeed
