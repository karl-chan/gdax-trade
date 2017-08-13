{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Util.Exchange
import           Gdax.Util.Feed

import           Coinbase.Exchange.MarketData       (getOrderBook)
import           Coinbase.Exchange.Types            (ExchangeConf, runExchange)
import           Coinbase.Exchange.Types.Core       (OrderId, ProductId,
                                                     Side (Buy, Sell))
import           Coinbase.Exchange.Types.MarketData (Book (Book),
                                                     BookItem (BookItem))
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (ChangeLimit, Done, Match, Open),
                                                     msgMakerOrderId,
                                                     msgMaybePrice, msgNewSize,
                                                     msgOrderId, msgPrice,
                                                     msgRemainingSize,
                                                     msgSequence, msgSide,
                                                     msgSize, msgTime)

import           Control.Concurrent                 (forkIO)
import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar, tryReadMVar)
import           Control.Exception                  (throw)
import           Control.Monad                      (forever, void, when)
import           Data.Aeson                         (eitherDecode)
import           Data.HashMap                       (Map)
import qualified Data.HashMap                       as Map
import           Data.List                          (sort)
import           Data.Maybe                         (fromJust, isJust,
                                                     isNothing, maybeToList, fromMaybe)
import           Data.Time.Clock                    (getCurrentTime)
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

type TransformFunc = OrderBookItem -> OrderBookItem

processOrderBook :: ProductId -> ExchangeConf -> ProductFeed -> OrderBookFeed -> IO ()
processOrderBook productId conf productFeed bookFeed = do
    productFeedListener <- newFeedListener productFeed >>= waitUntilFeed
    let forever' maybeBook queue shouldSync = do
            (newMaybeBook, newQueue, bookUpdated) <-
                if shouldSync
                    then do
                        newBook <- syncOrderBook maybeBook queue productId conf productFeedListener
                        return (Just newBook, newExchangeMsgQueue, True)
                    else tryIncrementOrderBook maybeBook queue productFeedListener
            when bookUpdated $ writeFeed bookFeed $ fromJust newMaybeBook
            let newShouldSync = queueSize newQueue >= queueThreshold
            forever' newMaybeBook newQueue newShouldSync
    forever' Nothing newExchangeMsgQueue True

tryIncrementOrderBook ::
       Maybe OrderBook -> ExchangeMsgQueue -> ProductFeedListener -> IO (Maybe OrderBook, ExchangeMsgQueue, Bool)
tryIncrementOrderBook maybeBook queue productFeedListener = do
    exchangeMsg <- readFeed productFeedListener
    let newQueue = enqueue queue exchangeMsg
        book = fromJust maybeBook
    return $
        if isJust maybeBook && canDequeue book newQueue
            then (Just $ dequeue newQueue updateOrderBook book, newExchangeMsgQueue, True)
            else (maybeBook, newQueue, False)

canDequeue :: OrderBook -> ExchangeMsgQueue -> Bool
canDequeue prevBook queue =
    let sequences = bookSequence prevBook : queueKeysU queue
    in sort sequences == [minimum sequences .. maximum sequences]

syncOrderBook :: Maybe OrderBook -> ExchangeMsgQueue -> ProductId -> ExchangeConf -> ProductFeedListener -> IO OrderBook
syncOrderBook maybeBook queue productId conf productFeedListener = do
    restArrivedSignal <- newEmptyMVar
    forkIO $ do
        book <- restOrderBook productId conf
        putMVar restArrivedSignal book
    let queueUntilSynced q = do
            exchangeMsg <- readFeed productFeedListener
            restResult <- tryReadMVar restArrivedSignal
            case restResult of
                Nothing -> do
                    let newQueue = enqueue q exchangeMsg
                    queueUntilSynced newQueue
                Just restBook -> do
                    let outdated sequence _ = sequence <= bookSequence restBook
                        validMsgQueue = queueDropWhileWithKey outdated q
                        book' = dequeue validMsgQueue updateOrderBook restBook
                        newBook = updateOrderBook book' exchangeMsg
                    return newBook
    queueUntilSynced queue

restOrderBook :: ProductId -> ExchangeConf -> IO OrderBook
restOrderBook productId conf = do
    res <- runExchange conf $ getOrderBook productId
    case res of
        Left err      -> throw err
        Right rawBook -> fromRawOrderBook rawBook

fromRawOrderBook :: Book OrderId -> IO OrderBook
fromRawOrderBook Book {..} = do
    now <- getCurrentTime
    return
        OrderBook
        { bookSequence = bookSequence
        , bookTime = now
        , bookBids = fromRawBookItems bookBids
        , bookAsks = fromRawBookItems bookAsks
        }
  where
    fromRawBookItems rawBookItems = Map.fromList $ map toKeyValue rawBookItems
    toKeyValue (BookItem price size orderId) = (orderId, OrderBookItem price size orderId)

updateOrderBook :: OrderBook -> ExchangeMessage -> OrderBook
updateOrderBook book exchangeMessage =
    let book' = book {bookSequence = msgSequence exchangeMessage, bookTime = msgTime exchangeMessage}
    in case msgSide exchangeMessage of
           Sell -> book' {bookAsks = updateOrder (bookAsks book') exchangeMessage}
           Buy -> book' {bookBids = updateOrder (bookBids book') exchangeMessage}

updateOrder :: OrderBookItems -> ExchangeMessage -> OrderBookItems
updateOrder bookItems msg =
    case msg of
        Open {..} ->
            let newOrder = OrderBookItem msgPrice msgRemainingSize msgOrderId
            in Map.insert msgOrderId newOrder bookItems
        Match {..} ->
            let transformFunc bookItem@OrderBookItem {size = oldSize} = bookItem {size = oldSize - msgSize}
            in transformOrder bookItems msgMakerOrderId transformFunc
        ChangeLimit {..} ->
            let transformFunc bookItem@OrderBookItem {price = oldPrice} =
                    bookItem {price = fromMaybe oldPrice msgMaybePrice, size = msgNewSize}
            in transformOrder bookItems msgOrderId transformFunc
        Done {..} -> Map.delete msgOrderId bookItems
        _ -> bookItems

transformOrder :: OrderBookItems -> OrderId -> TransformFunc -> OrderBookItems
transformOrder bookItems orderId transformFunc = Map.adjust transformFunc orderId bookItems
