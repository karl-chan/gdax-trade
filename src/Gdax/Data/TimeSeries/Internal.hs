{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.TimeSeries.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Data.TimeSeries.Types
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue
import           Gdax.Util.Throttle

import           Coinbase.Exchange.MarketData       (getHistory)
import           Coinbase.Exchange.Types            (ExchangeConf, execExchange)
import           Coinbase.Exchange.Types.Core       (OrderId, Price (..),
                                                     ProductId, Sequence,
                                                     Side (Buy, Sell),
                                                     Size (..), unPrice, unSize)
import           Coinbase.Exchange.Types.MarketData (Book (Book),
                                                     BookItem (BookItem),
                                                     Candle (Candle), unClose,
                                                     unHigh, unLow, unOpen,
                                                     unVolume)
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (ChangeLimit, Done, Match, Open),
                                                     msgMakerOrderId,
                                                     msgMaybePrice, msgNewSize,
                                                     msgOrderId, msgPrice,
                                                     msgSequence, msgSide,
                                                     msgSize, msgTime)

import           BroadcastChan.Throw                (readBChan)
import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Concurrent.MVar            (MVar, newMVar, readMVar,
                                                     swapMVar)
import           Control.Exception                  (SomeException, catch)
import           Control.Monad                      (forever, void)
import           Data.List                          (head, insert, null)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (listToMaybe, maybe)
import           Data.Time.Clock                    (NominalDiffTime, UTCTime,
                                                     addUTCTime, diffUTCTime,
                                                     getCurrentTime)
import           Data.Time.Clock.POSIX              (posixSecondsToUTCTime)
import           Debug.Trace                        (traceIO)

processSeries :: StartTime -> Granularity -> ProductId -> ProductFeed -> TimeSeriesFeed -> Config -> IO ()
processSeries startTime granularity productId productFeed tsFeed config = do
    productFeedListener <- newFeedListener productFeed
    now <- getCurrentTime
    initialSeries <- restSeries startTime now granularity productId config
    let forever' series = do
            writeFeed tsFeed series
            newStat <- createStat granularity productFeedListener
            traceIO $ "New stat: " ++ show newStat
            let newSeries = maybe series (insertTS series) newStat
            traceIO $ "New time series range: " ++ showRange newSeries
            forever' newSeries
    forever' initialSeries

restSeries :: StartTime -> EndTime -> Granularity -> ProductId -> Config -> IO TimeSeries
restSeries startTime endTime granularity productId config = do
    let c = concurrency config
        d = dataLimit config
        p = pauseGap config
        r = retryGap config
        intervalLength = granularity * fromIntegral d
        boundaries = insert endTime $ takeWhile (< endTime) $ iterate (addUTCTime intervalLength) startTime
        intervals = zip boundaries $ tail boundaries
        getSeries s e = restSeries' s e granularity productId (exchangeConf config) r
    series <- fmap concatTS $ throttle c p $ map (uncurry getSeries) intervals
    traceIO $ "All REST time series received: " ++ showRange series
    return series

restSeries' :: StartTime -> EndTime -> Granularity -> ProductId -> ExchangeConf -> NominalDiffTime -> IO TimeSeries
restSeries' startTime endTime granularity productId exchangeConf retryGap = do
    let trial =
            catch
                (do candles <-
                        execExchange exchangeConf $
                        getHistory productId (Just startTime) (Just endTime) (Just $ (floor . toSeconds) granularity)
                    let series = fromCandles candles granularity
                    traceIO $ "REST time series received: " ++ showRange series
                    return series)
                (\err -> do
                     traceIO $
                         "REST failed for search: " ++
                         (show startTime) ++
                         " - " ++
                         (show endTime) ++
                         ", will retry after " ++ show retryGap ++ ".\n" ++ show (err :: SomeException)
                     sleep retryGap
                     trial)
    trial

createStat :: Granularity -> ProductFeedListener -> IO (Maybe Stat)
createStat granularity productFeedListener = do
    countdownTimer <- newMVar False :: IO (MVar Bool)
    forkIO $ do
        sleep granularity
        void $ swapMVar countdownTimer True
    let loop queue = do
            timeIsUp <- readMVar countdownTimer
            exchangeMsg <- readFeed productFeedListener
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
           else Just $ dequeue matchQueue updateStat $ (initStat . queueHead) matchQueue

fromCandles :: [Candle] -> Granularity -> TimeSeries
fromCandles candles granularity = Map.fromList $ map toKeyValue $ frontSeries ++ [lastSeries]
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
        }
    fromNeighbouringCandles candle@(Candle s _ _ _ _ _) (Candle e _ _ _ _ _) = fromCandle candle $ diffUTCTime e s
    toKeyValue stat = (start stat, stat)

initStat :: ExchangeMessage -> Stat
initStat Match {..} =
    Stat
    {start = msgTime, end = msgTime, low = msgPrice, high = msgPrice, open = msgPrice, close = msgPrice, volume = 0}

updateStat :: Stat -> ExchangeMessage -> Stat
updateStat Stat {..} msg@Match {..} =
    Stat
    { start = min start msgTime
    , end = max end msgTime
    , low = min low msgPrice
    , high = max high msgPrice
    , open = open
    , close = msgPrice
    , volume = volume + msgSize
    }

showRange :: TimeSeries -> String
showRange series =
    let (start, end) = rangeTS series
    in show start ++ " - " ++ show end
