{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gdax.Data.TimeSeries.Internal where

import           Gdax.Data.TimeSeries.Types
import qualified Gdax.Data.TimeSeries.Util          as TS
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Queue
import           Gdax.Util.Throttle

import           Coinbase.Exchange.MarketData       (getHistory)
import           Coinbase.Exchange.Types            (execExchange)
import           Coinbase.Exchange.Types.Core       (Price (..), Size (..))
import           Coinbase.Exchange.Types.MarketData (Candle (..))
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (Match),
                                                     msgMakerOrderId, msgPrice,
                                                     msgProductId, msgSequence,
                                                     msgSide, msgSize, msgTime)

import           Control.Concurrent                 (forkIO)
import           Control.Concurrent.MVar            (MVar, newMVar, readMVar,
                                                     swapMVar)
import           Control.Exception                  (SomeException, catch)
import           Control.Monad                      (void)
import           Control.Monad.Reader
import           Data.List                          (insert)
import qualified Data.Map                           as Map
import           Data.Maybe                         (maybe)
import           Data.Time.Clock                    (addUTCTime, diffUTCTime,
                                                     getCurrentTime)
import           Debug.Trace
import           Prelude                            hiding (product)

streamTimeSeries :: StartTime -> Product -> GdaxFeedListener -> ReaderT Config IO TimeSeriesFeed
streamTimeSeries startTime product gdaxFeedListener = do
    config <- ask
    granularity <- reader apiGranularity
    liftIO $ do
        tsFeed <- newFeed
        forkIO $ do
            now <- getCurrentTime
            initialTS <- runReaderT (initialSeries startTime now product) config
            let loop series = do
                    liftIO $ writeFeed tsFeed series
                    stat <- newStat granularity gdaxFeedListener
                    let newSeries = maybe series (TS.insert series) stat
                    traceIO $ "New stat: " ++ show stat ++ ". New time series range: " ++ showRange newSeries
                    loop newSeries
            loop initialTS
        return tsFeed

initialSeries :: StartTime -> EndTime -> Product -> ReaderT Config IO TimeSeries
initialSeries startTime endTime product = do
    config <- ask
    granularity <- reader apiGranularity
    concurrency <- reader apiThrottleConcurrency
    dataLimit <- reader apiThrottleDataLimit
    pauseGap <- reader apiThrottlePauseGap
    let intervalLength = granularity * fromIntegral dataLimit
        boundaries = insert endTime $ takeWhile (< endTime) $ iterate (addUTCTime $ realToFrac intervalLength) startTime
        intervals = zip boundaries $ tail boundaries
        restSeriesFromInterval (s, e) = runReaderT (restSeries s e product) config
        tasks = map restSeriesFromInterval intervals
    multiSeries <- liftIO $ throttle concurrency pauseGap tasks
    let series = TS.concat multiSeries
    traceM $ "All REST time series received: " ++ showRange series
    return series

restSeries :: StartTime -> EndTime -> Product -> ReaderT Config IO TimeSeries
restSeries startTime endTime product = do
    conf <- reader exchangeConf
    granularity <- reader apiGranularity
    retryGap <- reader apiThrottleRetryGap
    let productId = toId product
        tryRestSeries =
            catch
                (do candles <-
                        execExchange conf $
                        getHistory productId (Just startTime) (Just endTime) (Just $ floor granularity)
                    let series = fromCandles candles granularity product
                    traceIO $ "REST time series received: " ++ showRange series
                    return series)
                (\err -> do
                     traceIO $
                         "REST failed for search: " ++
                         showRangeTime startTime endTime ++
                         ", will retry after " ++ show retryGap ++ ".\n" ++ show (err :: SomeException)
                     sleep retryGap
                     tryRestSeries)
    liftIO tryRestSeries

newStat :: Granularity -> GdaxFeedListener -> IO (Maybe Stat)
newStat granularity gdaxFeedListener = do
    countdownTimer <- newMVar False :: IO (MVar Bool)
    forkIO $ do
        sleep granularity
        void $ swapMVar countdownTimer True
    let loop queue = do
            timeIsUp <- readMVar countdownTimer
            exchangeMsg <- readFeed gdaxFeedListener
            let newQueue = enqueue queue exchangeMsg
            if timeIsUp
                then return $ queueToStat queue
                else loop newQueue
    loop newExchangeMsgQueue

queueToStat :: ExchangeMsgQueue -> Maybe Stat
queueToStat queue =
    let matchQueue =
            queueFilter
                (\m ->
                     case m of
                         Match {..} -> True
                         _          -> False)
                queue
    in if queueNull matchQueue
           then Nothing
           else Just $ dequeue matchQueue updateStat $ (initialStat . queueHead) matchQueue

fromCandles :: [Candle] -> Granularity -> Product -> TimeSeries
fromCandles candles granularity product = Map.fromList $ map toKeyValue $ frontSeries ++ [lastSeries]
  where
    frontSeries = zipWith fromNeighbouringCandles candles $ tail candles
    lastSeries = fromCandle (last candles) granularity
    fromCandle (Candle s l h o c v) duration =
        Stat
        { start = s
        , end = addUTCTime duration s
        , low = Price $ realToFrac l
        , high = Price $ realToFrac h
        , open = Price $ realToFrac o
        , close = Price $ realToFrac c
        , volume = Size $ realToFrac v
        , product = product
        }
    fromNeighbouringCandles candle@(Candle s _ _ _ _ _) (Candle e _ _ _ _ _) = fromCandle candle $ diffUTCTime e s
    toKeyValue stat = (start stat, stat)

initialStat :: ExchangeMessage -> Stat
initialStat Match {..} =
    Stat
    { start = msgTime
    , end = msgTime
    , low = msgPrice
    , high = msgPrice
    , open = msgPrice
    , close = msgPrice
    , volume = 0
    , product = fromId msgProductId
    }

updateStat :: Stat -> ExchangeMessage -> Stat
updateStat stat@Stat {..} Match {..} =
    stat
    { start = min start msgTime
    , end = max end msgTime
    , low = min low msgPrice
    , high = max high msgPrice
    , open = open
    , close = msgPrice
    , volume = volume + msgSize
    }

showRange :: TimeSeries -> String
showRange = uncurry showRangeTime . TS.range

showRangeTime :: StartTime -> EndTime -> String
showRangeTime start end = show start ++ " - " ++ show end
