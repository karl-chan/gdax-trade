module Gdax.Feed.Bundle.Internal where

import           Gdax.Account.MyAccount
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.MyAccount.Types
import           Gdax.Feed.OrderBook.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.TimeSeries
import qualified Gdax.Types.TimeSeries.Util  as TS
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Logger
import           Gdax.Util.Throttle

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.HashMap.Strict         as HM

streamBundle ::
       [OrderBookFeedListener]
    -> [TimeSeriesFeedListener]
    -> MyAccountFeedListener
    -> ReaderT Config IO BundleFeed
streamBundle bookFeedListeners seriesFeedListeners accountFeedListener = do
    config <- ask
    refreshRate <- reader bundleRefreshRate
    liftIO $ do
        bundleFeed <- newFeed
        forkIO $ do
            initialBundle <- runReaderT (initBundle bookFeedListeners seriesFeedListeners) config
            logDebug "Initialised bundle."
            booksTVar <- newTVarIO $ books initialBundle
            seriesTVar <- newTVarIO $ series initialBundle
            accountTVar <- newTVarIO $ account initialBundle
            forM_ bookFeedListeners $ \listener ->
                forkIO . forever $ do
                    book <- readFeed listener
--                    logDebug $ "Received book in bundle: " ++ (show . bookSequence $ book)
                    let product = bookProduct book
                    atomically $ modifyTVar' booksTVar (HM.insert product book)
            forM_ seriesFeedListeners $ \listener ->
                forkIO . forever $ do
                    series <- readFeed listener
                    logDebug $ "Received series in bundle: " ++ (show . TS.range $ series)
                    let product = TS.getProduct series
                    atomically $ modifyTVar' seriesTVar (HM.insert product series)
            forkIO . forever $ do
                account <- readFeed accountFeedListener
                logDebug "Received account in bundle."
                atomically $ swapTVar accountTVar account
            forever $ do
                sleep refreshRate
                books <- atomically . readTVar $ booksTVar
                series <- atomically . readTVar $ seriesTVar
                account <- atomically . readTVar $ accountTVar
                let bundle = Bundle {account = account, books = books, series = series, config = config}
                writeFeed bundleFeed bundle
                logDebug "Wrote bundle to feed."
        return bundleFeed

initBundle :: [OrderBookFeedListener] -> [TimeSeriesFeedListener] -> ReaderT Config IO Bundle
initBundle bookFeedListeners seriesFeedListeners = do
    config <- ask
    initialAccount <- initAccount
    initialBooks <- liftIO $ mapM readFeed bookFeedListeners
    initialSeries <- liftIO $ mapM readFeed seriesFeedListeners
    let initialBooksMap = HM.fromList $ map (\b -> (bookProduct b, b)) initialBooks
        initialSeriesMap = HM.fromList $ map (\s -> (TS.getProduct s, s)) initialSeries
    return Bundle {account = initialAccount, books = initialBooksMap, series = initialSeriesMap, config = config}
