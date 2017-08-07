{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Gdax.Trade.OrderBook
    ( livecastOrderBook
    , OrderBook(..)
    ) where

import           Gdax.Trade.Feed                    (Feed)

import           Coinbase.Exchange.MarketData       hiding (Open, bookAsks,
                                                     bookBids, bookSequence)
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core       hiding (Done, Open)
import           Coinbase.Exchange.Types.MarketData hiding (Open, bookAsks,
                                                     bookBids, bookSequence)
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket

import           BroadcastChan.Throw                (BroadcastChan, In, Out,
                                                     newBChanListener,
                                                     newBroadcastChan,
                                                     readBChan, writeBChan)
import Debug.Trace (traceShowM, traceM)
import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Concurrent.MVar            (MVar, putMVar, tryReadMVar)
import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar)
import           Control.Concurrent.STM.TChan       (TChan, newTChanIO,
                                                     tryReadTChan, writeTChan)
import           Control.DeepSeq                    (NFData)
import           Control.Exception                  (throw)
import           Control.Monad                      (forever)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.ST                   (ST, runST)
import           Control.Monad.STM                  (atomically)
import           Data.Aeson                         (eitherDecode)
import           Data.Data

import           Data.HashMap                       (Map)
import qualified Data.HashMap                       as Map
import           Data.IORef                         (IORef, newIORef, readIORef,
                                                     writeIORef)
import           Data.List                          (sort)
import           Data.Maybe                         (fromJust, isJust,
                                                     isNothing, maybeToList)
import qualified Data.PQueue.Prio.Min               as PQ
import           Data.Typeable
import           GHC.Generics
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

data OrderBook = OrderBook
    { bookSequence :: Sequence
    , bookBids     :: OrderBookItems
    , bookAsks     :: OrderBookItems
    } deriving (Show, Data, Typeable, Generic, NFData)

type OrderBookItems = Map OrderId OrderBookItem

type OrderBookItem = BookItem OrderId

type OrderBookRef = IORef (Maybe OrderBook)

type OrderBookBroadcastChan = BroadcastChan In OrderBook

type FeedListener = BroadcastChan Out ExchangeMessage

type SyncSignal = TChan ()

type PlaybackFunc a = a -> ExchangeMessage -> a

type UpdateFunc = OrderBookItems -> ExchangeMessage -> OrderBookItems

type TransformFunc = OrderBookItem -> OrderBookItem

data UpdateOp
    = Insert
    | Replace
    | Delete

type ExchangeMsgQueue = PQ.MinPQueue Sequence ExchangeMessage

type ExchangeMsgQueueRef = IORef ExchangeMsgQueue

syncInterval = 60 * 1000 * 1000 -- sync order book every minute

livecastOrderBook :: ProductId -> ExchangeConf -> Feed -> IO OrderBookBroadcastChan
livecastOrderBook productId conf feed = do
    orderBookBroadcastChan <- newBroadcastChan
    feedListener <- newBChanListener feed

    -- Wait until feed is really available
    readBChan feedListener

    -- State variables
    queueRef <- newIORef PQ.empty :: IO (IORef ExchangeMsgQueue)
    bookRef <- newIORef Nothing :: IO (IORef (Maybe OrderBook))
    syncSignal <- newTChanIO :: IO SyncSignal
    -- Update logic
    forkIO $ periodicallySyncOrderBook syncSignal syncInterval
    forkIO $ processOrderBook bookRef queueRef productId conf feedListener syncSignal orderBookBroadcastChan
    return orderBookBroadcastChan

periodicallySyncOrderBook :: SyncSignal -> Int -> IO ()
periodicallySyncOrderBook signal period = do
    forever $ do
        atomically $ writeTChan signal ()
        threadDelay period
    return ()

processOrderBook ::
       OrderBookRef
    -> ExchangeMsgQueueRef
    -> ProductId
    -> ExchangeConf
    -> FeedListener
    -> SyncSignal
    -> OrderBookBroadcastChan
    -> IO ()
processOrderBook bookRef queueRef productId conf feedListener signal broadcastChan = do
    forever $ do
        res <- atomically $ tryReadTChan signal
        bookUpdated <-
            case res of
                Nothing -> tryIncrementOrderBook bookRef queueRef feedListener
                Just _ -> syncOrderBook bookRef queueRef productId conf feedListener
        if bookUpdated
            then readIORef bookRef >>= (writeBChan broadcastChan) . fromJust
            else return ()
    return ()

tryIncrementOrderBook :: OrderBookRef -> ExchangeMsgQueueRef -> FeedListener -> IO Bool
tryIncrementOrderBook bookRef queueRef feedListener = do
    exchangeMsg <- readBChan feedListener
    maybeBook <- readIORef bookRef
    queue <- readIORef queueRef
    let book = fromJust maybeBook
        shouldQueue = isNothing maybeBook || (not $ isSequential book queue exchangeMsg)
    if shouldQueue
        then do
            let newQueue = enqueue queue exchangeMsg
            writeIORef queueRef newQueue
            return False
        else do
            let book' = dequeue queue updateOrderBook book
                newBook = updateOrderBook book' exchangeMsg
            writeIORef queueRef PQ.empty
            writeIORef bookRef $ Just newBook
            return True

syncOrderBook :: OrderBookRef -> ExchangeMsgQueueRef -> ProductId -> ExchangeConf -> FeedListener -> IO Bool
syncOrderBook bookRef queueRef productId conf feed = do
    restSignal <- newEmptyMVar
    queue <- readIORef queueRef
    forkIO $ do restOrderBook productId conf >>= putMVar restSignal
    let loop q = do
            exchangeMsg <- readBChan feed
            restResult <- tryReadMVar restSignal
            case restResult of
                Nothing -> do
                    traceM $ "rest result is nothing " ++ (show $ msgSequence exchangeMsg)
                    let newQueue = enqueue q exchangeMsg
                    loop newQueue
                Just restBook -> do
                    traceM $ "rest book " ++ (show $ bookSequence restBook)
                    let outdated sequence _ = sequence <= bookSequence restBook
                        playbackQueue = PQ.dropWhileWithKey outdated q
                        book' = dequeue playbackQueue updateOrderBook restBook
                        newBook = updateOrderBook book' exchangeMsg
                    traceM $ "new book " ++ (show $ bookSequence newBook)
                    writeIORef queueRef PQ.empty
                    writeIORef bookRef $ Just newBook
                    return True
    loop queue

enqueue :: ExchangeMsgQueue -> ExchangeMessage -> ExchangeMsgQueue
enqueue queue exchangeMessage = PQ.insert (msgSequence exchangeMessage) exchangeMessage queue

dequeue :: ExchangeMsgQueue -> PlaybackFunc OrderBook -> OrderBook -> OrderBook
dequeue queue playbackFunc book = foldl playbackFunc book queue

restOrderBook :: ProductId -> ExchangeConf -> IO OrderBook
restOrderBook productId conf = do
    res <- runExchange conf $ getOrderBook productId
    case res of
        Left err      -> throw err
        Right rawBook -> return $ fromRawOrderBook rawBook

fromRawOrderBook :: Book OrderId -> OrderBook
fromRawOrderBook Book {..} =
    OrderBook
    {bookSequence = bookSequence, bookBids = fromRawOrderBookItems bookBids, bookAsks = fromRawOrderBookItems bookAsks}
  where
    fromRawOrderBookItems rawBookItems = Map.fromList $ map toPair rawBookItems
    toPair (BookItem price size orderId) = (orderId, BookItem price size orderId)

updateOrderBook :: OrderBook -> ExchangeMessage -> OrderBook
updateOrderBook book exchangeMessage =
    let book' = book {bookSequence = msgSequence exchangeMessage}
        func =
            case exchangeMessage of
                Open {..}        -> openOrder
                Match {..}       -> matchOrder
                Done {..}        -> doneOrder
                ChangeLimit {..} -> changeOrder
                _                -> const
    in case msgSide exchangeMessage of
           Sell -> book' {bookAsks = func (bookAsks book') exchangeMessage}
           Buy  -> book' {bookBids = func (bookBids book') exchangeMessage}

openOrder :: UpdateFunc
openOrder bookItems Open {..} =
    let newOrder = BookItem msgPrice msgRemainingSize msgOrderId
    in Map.insert msgOrderId newOrder bookItems

matchOrder :: UpdateFunc
matchOrder bookItems Match {..} =
    let matchFunc (BookItem price size orderId) = BookItem price (size - msgSize) orderId
    in transformOrder bookItems msgMakerOrderId matchFunc

changeOrder :: UpdateFunc
changeOrder bookItems ChangeLimit {..} =
    let changeFunc (BookItem price size orderId) = BookItem (maybe price id msgMaybePrice) msgNewSize orderId
    in transformOrder bookItems msgOrderId changeFunc

doneOrder :: UpdateFunc
doneOrder bookItems Done {..} = Map.delete msgOrderId bookItems

transformOrder :: OrderBookItems -> OrderId -> TransformFunc -> OrderBookItems
transformOrder bookItems orderId transformFunc = Map.adjust transformFunc orderId bookItems

isSequential :: OrderBook -> ExchangeMsgQueue -> ExchangeMessage -> Bool
isSequential prevBook queue exchangeMessage =
    let sequenceNums = (bookSequence prevBook) : (msgSequence exchangeMessage) : PQ.keysU queue
    in sort sequenceNums == [minimum sequenceNums .. maximum sequenceNums]
