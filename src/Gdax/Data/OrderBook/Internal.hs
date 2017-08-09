{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Util.Feed

import           Coinbase.Exchange.MarketData       (getOrderBook)
import           Coinbase.Exchange.Types            (ExchangeConf, runExchange)
import           Coinbase.Exchange.Types.Core       (OrderId, ProductId,
                                                     Sequence, Side (Buy, Sell))
import           Coinbase.Exchange.Types.MarketData (Book (Book),
                                                     BookItem (BookItem))
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (ChangeLimit, Done, Match, Open),
                                                     msgMakerOrderId,
                                                     msgMaybePrice, msgNewSize,
                                                     msgOrderId, msgPrice,
                                                     msgRemainingSize,
                                                     msgSequence, msgSide,
                                                     msgSize)

import           BroadcastChan.Throw                (BroadcastChan, In, Out,
                                                     newBChanListener,
                                                     newBroadcastChan,
                                                     readBChan, writeBChan)
import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar, tryReadMVar)
import           Control.Concurrent.STM.TChan       (TChan, newTChanIO,
                                                     tryReadTChan, writeTChan)
import           Control.Exception                  (throw)
import           Control.Monad                      (forever, void, when)
import           Control.Monad.STM                  (atomically)
import           Data.Aeson                         (eitherDecode)
import           Data.HashMap                       (Map)
import qualified Data.HashMap                       as Map
import           Data.List                          (sort)
import           Data.Maybe                         (fromJust, isJust,
                                                     isNothing, maybeToList)
import qualified Data.PQueue.Prio.Min               as PQ
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

type PlaybackFunc = OrderBook -> ExchangeMessage -> OrderBook

type UpdateFunc = OrderBookItems -> ExchangeMessage -> OrderBookItems

type TransformFunc = OrderBookItem -> OrderBookItem

type ExchangeMsgQueue = PQ.MinPQueue Sequence ExchangeMessage

-- | Sync order book if queue grows too long, probably due to dropped message
queueThreshold = 20

processOrderBook :: ProductId -> ExchangeConf -> FeedListener -> OrderBookBroadcastChan -> IO ()
processOrderBook productId conf feedListener broadcastChan = do
    let forever' maybeBook queue shouldSync = do
            (newMaybeBook, newQueue, bookUpdated) <-
                if shouldSync
                    then do
                        newBook <- syncOrderBook maybeBook queue productId conf feedListener
                        return (Just newBook, PQ.empty, True)
                    else tryIncrementOrderBook maybeBook queue feedListener
            when bookUpdated $ writeBChan broadcastChan $ (fromJust newMaybeBook)
            let newShouldSync = PQ.size newQueue >= queueThreshold
            forever' newMaybeBook newQueue newShouldSync
    forever' Nothing PQ.empty True

tryIncrementOrderBook ::
       Maybe OrderBook -> ExchangeMsgQueue -> FeedListener -> IO (Maybe OrderBook, ExchangeMsgQueue, Bool)
tryIncrementOrderBook maybeBook queue feedListener = do
    exchangeMsg <- readBChan feedListener
    let book = fromJust maybeBook
        shouldQueue = maybe True (\book -> not $ isSequential book queue exchangeMsg) maybeBook
    if shouldQueue
        then do
            let newQueue = enqueue queue exchangeMsg
            return (maybeBook, newQueue, False)
        else do
            let book' = dequeue queue updateOrderBook book
                newBook = updateOrderBook book' exchangeMsg
            return (Just newBook, PQ.empty, True)

syncOrderBook :: Maybe OrderBook -> ExchangeMsgQueue -> ProductId -> ExchangeConf -> FeedListener -> IO OrderBook
syncOrderBook maybeBook queue productId conf feed = do
    restArrivedSignal <- newEmptyMVar
    forkIO $ do restOrderBook productId conf >>= putMVar restArrivedSignal
    let loop q = do
            exchangeMsg <- readBChan feed
            restResult <- tryReadMVar restArrivedSignal
            case restResult of
                Nothing -> do
                    let newQueue = enqueue q exchangeMsg
                    loop newQueue
                Just restBook -> do
                    let outdated sequence _ = sequence <= bookSequence restBook
                        playbackQueue = PQ.dropWhileWithKey outdated q
                        book' = dequeue playbackQueue updateOrderBook restBook
                        newBook = updateOrderBook book' exchangeMsg
                    return newBook
    loop queue

enqueue :: ExchangeMsgQueue -> ExchangeMessage -> ExchangeMsgQueue
enqueue queue exchangeMessage = PQ.insert (msgSequence exchangeMessage) exchangeMessage queue

dequeue :: ExchangeMsgQueue -> PlaybackFunc -> OrderBook -> OrderBook
dequeue queue playbackFunc book = foldl playbackFunc book queue

restOrderBook :: ProductId -> ExchangeConf -> IO OrderBook
restOrderBook productId conf = do
    res <- runExchange conf $ getOrderBook productId
    case res of
        Left err      -> throw err
        Right rawBook -> return $ fromRawOrderBook rawBook

fromRawOrderBook :: Book OrderId -> OrderBook
fromRawOrderBook Book {..} =
    OrderBook {bookSequence = bookSequence, bookBids = fromRawBookItems bookBids, bookAsks = fromRawBookItems bookAsks}
  where
    fromRawBookItems rawBookItems = Map.fromList $ map toPair rawBookItems
    toPair (BookItem price size orderId) = (orderId, OrderBookItem price size orderId)

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
    let newOrder = OrderBookItem msgPrice msgRemainingSize msgOrderId
    in Map.insert msgOrderId newOrder bookItems

matchOrder :: UpdateFunc
matchOrder bookItems Match {..} =
    let matchFunc bookItem@OrderBookItem {size = oldSize} = bookItem {size = oldSize - msgSize}
    in transformOrder bookItems msgMakerOrderId matchFunc

changeOrder :: UpdateFunc
changeOrder bookItems ChangeLimit {..} =
    let changeFunc bookItem@OrderBookItem {price = oldPrice} =
            bookItem {price = maybe oldPrice id msgMaybePrice, size = msgNewSize}
    in transformOrder bookItems msgOrderId changeFunc

doneOrder :: UpdateFunc
doneOrder bookItems Done {..} = Map.delete msgOrderId bookItems

transformOrder :: OrderBookItems -> OrderId -> TransformFunc -> OrderBookItems
transformOrder bookItems orderId transformFunc = Map.adjust transformFunc orderId bookItems

isSequential :: OrderBook -> ExchangeMsgQueue -> ExchangeMessage -> Bool
isSequential prevBook queue exchangeMessage =
    let sequenceNums = (bookSequence prevBook) : (msgSequence exchangeMessage) : PQ.keysU queue
    in sort sequenceNums == [minimum sequenceNums .. maximum sequenceNums]
