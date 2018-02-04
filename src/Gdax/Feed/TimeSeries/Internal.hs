{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.TimeSeries.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries              (Stat (..), TimeSeries)
import qualified Gdax.Types.TimeSeries.Util         as TS
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue
import           Gdax.Util.Throttle
import           Gdax.Util.Throttle.Api
import           Gdax.Util.Time

import           Coinbase.Exchange.MarketData       (getHistory)
import           Coinbase.Exchange.Types.Core       (Price (..), Size (..))
import           Coinbase.Exchange.Types.MarketData (Candle (..))
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (Match),
                                                     msgMakerOrderId, msgPrice,
                                                     msgProductId, msgSequence,
                                                     msgSide, msgSize, msgTime)

import           Control.Concurrent                 (forkIO)
import           Control.Concurrent.MVar            (MVar, newMVar, readMVar,
                                                     swapMVar)
import           Control.Monad                      (void)
import           Control.Monad.Reader
import           Data.List                          (insert)
import           Data.Maybe                         (maybe)
import           Data.Time.Clock                    (addUTCTime, diffUTCTime,
                                                     getCurrentTime)
import           Gdax.Util.Logger
import           Prelude                            hiding (product)

streamTimeSeries ::
     StartTime
  -> Product
  -> GdaxFeedListener
  -> ReaderT Config IO TimeSeriesFeed
streamTimeSeries startTime product gdaxFeedListener = do
  config <- ask
  granularity <- reader $ granularity . apiConf
  liftIO $ do
    tsFeed <- newFeed
    forkIO $ do
      now <- getCurrentTime
      initialTS <- runReaderT (initSeries startTime now product) config
      logDebug $ "Initialised time series."
      let loop series = do
            writeFeed tsFeed series
            stat <- newStat granularity gdaxFeedListener
            let newSeries = maybe series (\s -> TS.insert s series) stat
            logDebug $
              "New stat: " ++
              show stat ++ ". New time series range: " ++ showRange newSeries
            loop newSeries
      loop initialTS
    return tsFeed

initSeries :: StartTime -> EndTime -> Product -> ReaderT Config IO TimeSeries
initSeries startTime endTime product = do
  granularity <- reader $ granularity . apiConf
  dataLimit <- reader $ dataLimit . throttleConf . apiConf
  let intervalLength = granularity * fromIntegral dataLimit
      boundaries =
        insert endTime $
        takeWhile (< endTime) $
        iterate (addUTCTime $ realToFrac intervalLength) startTime
      intervals = zip boundaries $ tail boundaries
      requests =
        map
          (\(start, end) ->
             getHistory
               (toId product)
               (Just start)
               (Just end)
               (Just $ floor granularity))
          intervals
  candlesChunks <- throttleApi requests
  let candles = concat candlesChunks
      series = fromCandles candles granularity product
  logDebug $ "All REST time series received: " ++ showRange series
  return series

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
       else Just $
            dequeue matchQueue updateStat $ (initialStat . queueHead) matchQueue

fromCandles :: [Candle] -> Granularity -> Product -> TimeSeries
fromCandles candles granularity product =
  TS.statsToSeries $ frontSeries ++ [lastSeries]
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
    fromNeighbouringCandles candle@(Candle s _ _ _ _ _) (Candle e _ _ _ _ _) =
      fromCandle candle $ diffUTCTime e s

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
initialStat msg =
  error $ "initialStat only works for Match messages, not for: " ++ show msg

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
updateStat _ msg =
  error $ "updateStat only works for Match messages, not for: " ++ show msg

showRange :: TimeSeries -> String
showRange = uncurry showRangeTime . TS.range

showRangeTime :: StartTime -> EndTime -> String
showRangeTime start end = show start ++ " - " ++ show end
