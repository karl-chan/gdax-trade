{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.TimeSeries.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Data.TimeSeries.Types
import           Gdax.Util.Constants
import           Gdax.Util.Exchange
import           Gdax.Util.Feed
import           Gdax.Util.Throttle

import           Coinbase.Exchange.MarketData       (getHistory)
import           Coinbase.Exchange.Types            (ExchangeConf, runExchange)
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
import           Control.Exception                  (throw)
import           Control.Monad                      (forever, void)
import           Data.List                          (head, insert, null)
import           Data.Map                           (Map)
import qualified Data.Map as Map
import           Data.Maybe                         (listToMaybe)
import           Data.Time.Clock                    (NominalDiffTime, UTCTime,
                                                     addUTCTime, diffUTCTime,
                                                     getCurrentTime)
import           Data.Time.Clock.POSIX              (posixSecondsToUTCTime)
import           Debug.Trace                        (traceIO)

type EndTime = UTCTime

processSeries :: StartTime -> Granularity -> ProductId -> ExchangeConf -> ProductFeed -> TimeSeriesFeed -> IO ()
processSeries startTime granularity productId conf productFeed tsFeed = do
    productFeedListener <- newFeedListener productFeed
    now <- getCurrentTime
    initialSeries <- restSeries startTime now granularity productId conf
    let forever' series = do
            writeFeed tsFeed series
            newSeries <- updateSeries series granularity productFeedListener
            traceIO $ "New ts range: " ++ (show . fst . Map.findMin) newSeries ++ " - " ++ (show . fst . Map.findMax) newSeries
            forever' newSeries
    forever' initialSeries

restSeries :: StartTime -> EndTime -> Granularity -> ProductId -> ExchangeConf -> IO TimeSeries
restSeries startTime endTime granularity productId conf = do
    let intervalLength = granularity * fromIntegral maxRestDataPoints
        boundaries = insert endTime $ takeWhile (< endTime) $ iterate (addUTCTime intervalLength) startTime
        intervals = zip boundaries $ tail boundaries
        getSeries s e = restSeries' s e granularity productId conf
    ts <- fmap concatTS $ throttle maxRestRate 1 $ map (uncurry getSeries) intervals
    traceIO $ "All REST ts received: " ++ show startTime ++ " - " ++ show endTime
    return ts

updateSeries :: TimeSeries -> Granularity -> ProductFeedListener -> IO TimeSeries
updateSeries series granularity productFeedListener = do
    countdownTimer <- newMVar False :: IO (MVar Bool)
    forkIO $ do
        sleep granularity
        void $ swapMVar countdownTimer True
    let loop queue = do
            timeIsUp <- readMVar countdownTimer
            exchangeMsg <- readFeed productFeedListener
            let newQueue = enqueue queue exchangeMsg
            if timeIsUp
                then return $ updateWithExchangeMessages series queue
                else loop newQueue
    loop newExchangeMsgQueue

restSeries' :: StartTime -> EndTime -> Granularity -> ProductId -> ExchangeConf -> IO TimeSeries
restSeries' startTime endTime granularity productId conf = do
    res <-
        runExchange conf $ getHistory productId (Just startTime) (Just endTime) (Just $ (floor . toSeconds) granularity)
    traceIO $ "REST ts received: " ++ show startTime ++ " - " ++ show endTime
    case res of
        Left err      -> throw err
        Right candles -> return $ fromCandles candles granularity

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

updateWithExchangeMessages :: TimeSeries -> ExchangeMsgQueue -> TimeSeries
updateWithExchangeMessages series queue =
    let msgs = queueElems queue
        matchMsgs =
            filter
                (\m ->
                     case m of
                         Match {..} -> True
                         _          -> False)
                msgs
    in if null matchMsgs
           then series
           else let newStat = dequeue queue updateStat $ (initStat . head) matchMsgs
                in insertTS series newStat

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
