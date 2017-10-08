module Main where

import           Gdax.Types.Currency
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Feed.TimeSeries

import           Control.Monad             (forever)
import           Control.Monad.Reader      (runReaderT)
import           Data.Time.Calendar        (fromGregorian)
import           Data.Time.Clock           (UTCTime (..))

currencyPair :: Product
currencyPair = Pair ETH EUR

startTime :: UTCTime
startTime = UTCTime (fromGregorian 2017 8 1) 0

main :: IO ()
main = do
  config <- getGlobalConfig
  gdaxFeed <- runReaderT (newGdaxFeed [currencyPair]) config
--    bookFeed <- newOrderBookFeed currencyPair conf gdaxFeed
  tsFeed <-
    runReaderT (newTimeSeriesFeed gdaxFeed startTime currencyPair) config
  tsListener <- newFeedListener tsFeed
  forever $ do
    ts <- readFeed tsListener
    return ()
