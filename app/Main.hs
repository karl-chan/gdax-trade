module Main where

import           Gdax.Data.TimeSeries
import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Control.Monad           (forever)
import           Control.Monad.Reader    (runReaderT)
import           Data.Time.Calendar      (fromGregorian)
import           Data.Time.Clock         (UTCTime (..))

currencyPair :: Product
currencyPair = Pair ETH EUR

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2017 8 1) 0

main :: IO ()
main = do
    config <- getGlobalConfig
    productFeed <- newProductFeed [currencyPair]
--    bookFeed <- liveOrderBookFeed currencyPair conf productFeed
    tsFeed <- runReaderT (liveTSFeed startTime currencyPair productFeed) config
    tsListener <- newFeedListener tsFeed
    forever $ do
        ts <- readFeed tsListener
        return ()
