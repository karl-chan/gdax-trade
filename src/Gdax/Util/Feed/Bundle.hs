module Gdax.Util.Feed.Bundle where

import           Gdax.Account.MyAccount
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.TimeSeries.Types
import           Gdax.Data.TimeSeries.Util
import           Gdax.Types.Product
import           Gdax.Util.Bundle
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Feed.OrderBook
import           Gdax.Util.Feed.TimeSeries
import           Gdax.Util.Throttle

import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Socket (ExchangeMessage)

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict            as HM
import qualified Network.WebSockets             as WS
import           Prelude                        hiding (product)

type BundleFeed = Feed Bundle

type BundleFeedListener = FeedListener Bundle

newBundleFeed :: [OrderBookFeed] -> [TimeSeriesFeed] -> MyAccountFeed -> ReaderT Config IO BundleFeed
newBundleFeed bookFeeds seriesFeeds accountFeed = do
    bookFeedListeners <- liftIO $ mapM newFeedListener bookFeeds
    seriesFeedListeners <- liftIO $ mapM newFeedListener seriesFeeds
    accountFeedListener <- liftIO $ newFeedListener accountFeed
    streamBundle bookFeedListeners seriesFeedListeners accountFeedListener

streamBundle ::
       [OrderBookFeedListener] -> [TimeSeriesFeedListener] -> MyAccountFeedListener -> ReaderT Config IO BundleFeed
streamBundle bookFeedListeners seriesFeedListeners accountFeedListener = do
    config <- ask
    refreshRate <- reader bundleRefreshRate
    liftIO $ do
        bundleFeed <- newFeed
        forkIO $ do
            initialBundle <- runReaderT (initBundle bookFeedListeners seriesFeedListeners) config
            booksTVar <- newTVarIO $ books initialBundle
            seriesTVar <- newTVarIO $ series initialBundle
            accountTVar <- newTVarIO $ account initialBundle
            forM_ bookFeedListeners $ \listener ->
                forkIO . forever $ do
                    book <- readFeed listener
                    let product = bookProduct book
                    atomically $ modifyTVar' booksTVar (HM.insert product book)
            forM_ seriesFeedListeners $ \listener ->
                forkIO . forever $ do
                    series <- readFeed listener
                    let product = getProduct series
                    atomically $ modifyTVar' seriesTVar (HM.insert product series)
            forkIO . forever $ do
                account <- readFeed accountFeedListener
                atomically $ swapTVar accountTVar account
            forever $ do
                sleep refreshRate
                books <- atomically . readTVar $ booksTVar
                series <- atomically . readTVar $ seriesTVar
                account <- atomically . readTVar $ accountTVar
                let bundle = Bundle {account = account, books = books, series = series, config = config}
                writeFeed bundleFeed bundle
        return bundleFeed

initBundle :: [OrderBookFeedListener] -> [TimeSeriesFeedListener] -> ReaderT Config IO Bundle
initBundle bookFeedListeners seriesFeedListeners = do
    config <- ask
    initialAccount <- initAccount
    initialBooks <- liftIO $ mapM readFeed bookFeedListeners
    initialSeries <- liftIO $ mapM readFeed seriesFeedListeners
    let initialBooksMap = HM.fromList $ map (\b -> (bookProduct b, b)) initialBooks
        initialSeriesMap = HM.fromList $ map (\s -> (getProduct s, s)) initialSeries
    return Bundle {account = initialAccount, books = initialBooksMap, series = initialSeriesMap, config = config}
