{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.TimeSeries.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries              (Stat (..), TimeSeries)
import qualified Gdax.Types.TimeSeries.Util         as TS
import           Gdax.Util.Config
import           Gdax.Util.Feed
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
import           Control.Monad.Reader
import           Data.List                          (insert)
import           Data.Time.Clock                    (addUTCTime, diffUTCTime,
                                                     getCurrentTime)
import           Gdax.Util.Logger
import           Prelude                            hiding (product)

streamTimeSeries ::
     Product -> GdaxFeedListener -> ReaderT Config IO TimeSeriesFeed
streamTimeSeries product gdaxFeedListener = do
  config <- ask
  initialPeriod <- reader $ initialPeriod . timeSeriesConf
  liftIO $ do
    tsFeed <- newFeed
    forkIO $ do
      now <- getCurrentTime
      let startTime = addUTCTime (-initialPeriod) now
      initialSeries <-
        runReaderT (initialiseTimeSeries startTime now product) config
      logDebug $ "Initialised time series."
      let loop series = do
            writeFeed tsFeed series
            exchangeMsg <- readFeed gdaxFeedListener
            newSeries <- runReaderT (updateTimeSeries series exchangeMsg) config
            loop newSeries
      loop initialSeries
    return tsFeed

initialiseTimeSeries ::
     StartTime -> EndTime -> Product -> ReaderT Config IO TimeSeries
initialiseTimeSeries startTime endTime product = do
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
  now <- liftIO getCurrentTime
  let candles = concat $ candlesChunks
      series = TS.dropBefore startTime $ fromCandles candles now product
  logDebug $ "All REST time series received: " ++ TS.showRange series
  return series

updateTimeSeries ::
     TimeSeries -> ExchangeMessage -> ReaderT Config IO TimeSeries
updateTimeSeries series Match {..} = do
  granularity <- reader $ granularity . apiConf
  let lastStatTime = start . TS.last $ series
      elapsed = diffUTCTime msgTime lastStatTime
  return $
    if elapsed >= granularity
      then let elapsedMultiple = floor $ elapsed / granularity :: Int
               newBoundaryTime =
                 addUTCTime
                   (realToFrac elapsedMultiple * granularity)
                   lastStatTime
               series' =
                 TS.updateLastStat
                   (\stat -> stat {end = newBoundaryTime})
                   series
               newStat =
                 Stat
                 { start = newBoundaryTime
                 , end = msgTime
                 , low = msgPrice
                 , high = msgPrice
                 , open = msgPrice
                 , close = msgPrice
                 , volume = msgSize
                 , product = TS.product series
                 }
           in TS.insert newStat series'
      else TS.updateLastStat
             (\stat@Stat {..} ->
                stat
                { end = max end msgTime
                , low = min low msgPrice
                , high = max high msgPrice
                , close = msgPrice
                , volume = volume + msgSize
                })
             series
updateTimeSeries series msg =
  return $
  TS.updateLastStat
    (\stat@Stat {..} -> stat {end = max end $ msgTime msg})
    series

fromCandles :: [Candle] -> EndTime -> Product -> TimeSeries
fromCandles candles endTime product =
  TS.statsToSeries $ frontSeries ++ [lastSeries]
  where
    frontSeries = zipWith fromNeighbouringCandles candles $ tail candles
    lastSeries = fromCandle (last candles) endTime
    fromCandle (Candle s l h o c v) e =
      Stat
      { start = s
      , end = e
      , low = Price $ realToFrac l
      , high = Price $ realToFrac h
      , open = Price $ realToFrac o
      , close = Price $ realToFrac c
      , volume = Size $ realToFrac v
      , product = product
      }
    fromNeighbouringCandles candle@(Candle s1 _ _ _ _ _) (Candle s2 _ _ _ _ _) =
      fromCandle candle $ max s1 s2
