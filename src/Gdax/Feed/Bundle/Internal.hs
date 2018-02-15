module Gdax.Feed.Bundle.Internal where

import           Gdax.Feed.Account.Types
import           Gdax.Feed.Bundle.Types
import           Gdax.Feed.OrderBook.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.Product
import qualified Gdax.Types.TimeSeries.Util  as TS
import qualified Gdax.Types.Trades.Util      as Trades
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Logger
import           Gdax.Util.Throttle

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Prelude                     hiding (product)

streamBundle ::
     MyAccountFeedListener
  -> HashMap Product OrderBookFeedListener
  -> HashMap Product TimeSeriesFeedListener
  -> HashMap Product TradesFeedListener
  -> ReaderT Config IO BundleFeed
streamBundle accountFeedListener bookFeedListeners seriesFeedListeners tradesFeedListeners = do
  config <- ask
  refreshRate <- reader bundleRefreshRate
  liftIO $ do
    bundleFeed <- newFeed
    forkIO $ do
      initialBundle <-
        runReaderT
          (initBundle
             accountFeedListener
             bookFeedListeners
             seriesFeedListeners
             tradesFeedListeners)
          config
      logDebug "Initialised bundle."
      booksTVar <- newTVarIO $ books initialBundle
      multiSeriesTVar <- newTVarIO $ multiSeries initialBundle
      multiTradesTVar <- newTVarIO $ multiTrades initialBundle
      accountTVar <- newTVarIO $ account initialBundle
      forkIO . forever $ do
        account <- readFeed accountFeedListener
        logDebug "Received account in bundle."
        atomically $ swapTVar accountTVar account
      forM_ (HM.toList bookFeedListeners) $ \(product, listener) ->
        forkIO . forever $ do
          book <- readFeed listener
          logDebug $ "Received book in bundle: " ++ (show . bookSequence $ book)
          atomically $ modifyTVar' booksTVar (HM.insert product book)
      forM_ (HM.toList seriesFeedListeners) $ \(product, listener) ->
        forkIO . forever $ do
          series <- readFeed listener
          logDebug $
            "Received multi-series in bundle: " ++ (show . TS.range $ series)
          atomically $ modifyTVar' multiSeriesTVar (HM.insert product series)
      forM_ (HM.toList tradesFeedListeners) $ \(product, listener) ->
        forkIO . forever $ do
          trades <- readFeed listener
          logDebug $
            "Received multi-trades in bundle" ++ (show . Trades.range $ trades)
          atomically $ modifyTVar' multiTradesTVar (HM.insert product trades)
      forever $ do
        sleep refreshRate
        books <- atomically . readTVar $ booksTVar
        multiSeries <- atomically . readTVar $ multiSeriesTVar
        multiTrades <- atomically . readTVar $ multiTradesTVar
        account <- atomically . readTVar $ accountTVar
        let bundle =
              Bundle
              { account = account
              , books = books
              , multiSeries = multiSeries
              , multiTrades = multiTrades
              , config = config
              }
        writeFeed bundleFeed bundle
        logDebug "Wrote bundle to feed."
    return bundleFeed

initBundle ::
     MyAccountFeedListener
  -> HashMap Product OrderBookFeedListener
  -> HashMap Product TimeSeriesFeedListener
  -> HashMap Product TradesFeedListener
  -> ReaderT Config IO Bundle
initBundle accountFeedListener bookFeedListeners seriesFeedListeners tradesFeedListeners = do
  config <- ask
  initialAccount <- liftIO $ readFeed accountFeedListener
  initialBooksMap <- liftIO $ mapM readFeed bookFeedListeners
  initialSeriesMap <- liftIO $ mapM readFeed seriesFeedListeners
  initialTradesMap <- liftIO $ mapM readFeed tradesFeedListeners
  return
    Bundle
    { account = initialAccount
    , books = initialBooksMap
    , multiSeries = initialSeriesMap
    , multiTrades = initialTradesMap
    , config = config
    }
