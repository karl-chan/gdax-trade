{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue

import           Coinbase.Exchange.MarketData       (getOrderBook)
import           Coinbase.Exchange.Types            (ExchangeConf, execExchange,
                                                     execExchangeT)
import           Coinbase.Exchange.Types.Core       (OrderId, ProductId,
                                                     Side (Buy, Sell))
import           Coinbase.Exchange.Types.MarketData (Book (Book),
                                                     BookItem (BookItem))
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket     (ExchangeMessage (ChangeLimit, Done, Match, Open),
                                                     msgMakerOrderId,
                                                     msgMaybePrice, msgNewSize,
                                                     msgOrderId, msgPrice,
                                                     msgProductId,
                                                     msgRemainingSize,
                                                     msgSequence, msgSide,
                                                     msgSize, msgTime)

import           Control.Concurrent                 (forkIO)
import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar, tryReadMVar)
import           Control.Exception                  (throw)
import           Control.Monad                      (forever, void, when)
import           Control.Monad.Reader               (MonadReader, ReaderT, ask,
                                                     liftIO, reader, runReaderT)
import           Data.Aeson                         (eitherDecode)
import           Data.HashMap                       (Map)
import qualified Data.HashMap                       as Map
import           Data.List                          (sort)
import           Data.Maybe                         (fromJust, fromMaybe,
                                                     isJust, isNothing,
                                                     maybeToList)
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

type TransformFunc = OrderBookItem -> OrderBookItem

processOrderBook :: Product -> ProductFeedListener -> ReaderT Config IO OrderBookFeed
processOrderBook product productFeedListener = do
    config <- ask
    bookFeed <- liftIO newFeed
    liftIO . forkIO $ do
        initialBook <- runReaderT (initialiseOrderBook product productFeedListener) config
        let loop book = do
                newBook <- runReaderT (incrementOrderBook book product productFeedListener) config
                writeFeed bookFeed newBook
                loop newBook
        loop initialBook
    return bookFeed

initialiseOrderBook :: Product -> ProductFeedListener -> ReaderT Config IO OrderBook
initialiseOrderBook product productFeedListener = do
    config <- ask
    restBookRef <- liftIO newEmptyMVar :: ReaderT Config IO (MVar OrderBook)
    liftIO . forkIO $ do
        book <- runReaderT (restOrderBook product) config
        putMVar restBookRef book
    liftIO $ syncOrderBook restBookRef product productFeedListener

incrementOrderBook :: OrderBook -> Product -> ProductFeedListener -> ReaderT Config IO OrderBook
incrementOrderBook book product productFeedListener = do
    let initialQueue = newExchangeMsgQueue
        loop queue = do
            let shouldSync = queueSize queue >= queueThreshold
            if shouldSync
                then do
                    initialiseOrderBook product productFeedListener
                else do
                    exchangeMsg <- liftIO $ readFeed productFeedListener
                    let newQueue = safeAddToQueue queue exchangeMsg product
                    if canDequeue book newQueue
                        then return $ dequeue newQueue updateOrderBook book
                        else loop newQueue
    loop initialQueue

canDequeue :: OrderBook -> ExchangeMsgQueue -> Bool
canDequeue prevBook queue =
    let sequences = bookSequence prevBook : queueKeysU queue
    in sort sequences == [minimum sequences .. maximum sequences]

safeAddToQueue :: ExchangeMsgQueue -> ExchangeMessage -> Product -> ExchangeMsgQueue
safeAddToQueue queue exchangeMsg product =
    if msgProductId exchangeMsg == toId product
        then enqueue queue exchangeMsg
        else queue

replayMessages :: ExchangeMsgQueue -> OrderBook -> OrderBook
replayMessages queue book =
    let outdated sequence _ = sequence <= bookSequence book
        replayQueue = queueDropWhileWithKey outdated queue
    in dequeue replayQueue updateOrderBook book

syncOrderBook :: MVar OrderBook -> Product -> ProductFeedListener -> IO OrderBook
syncOrderBook restBookRef product productFeedListener = do
    waitUntilFeed productFeedListener
    let queueUntilSynced queue = do
            exchangeMsg <- readFeed productFeedListener
            let newQueue = safeAddToQueue queue exchangeMsg product
            maybeBook <- tryReadMVar restBookRef
            case maybeBook of
                Nothing -> queueUntilSynced newQueue
                Just restBook -> do
                    return $ replayMessages newQueue restBook
    queueUntilSynced newExchangeMsgQueue

restOrderBook :: Product -> ReaderT Config IO OrderBook
restOrderBook product = do
    conf <- reader exchangeConf
    rawBook <- execExchangeT conf $ getOrderBook (toId product)
    return $ fromRawOrderBook rawBook

fromRawOrderBook :: Book OrderId -> OrderBook
fromRawOrderBook Book {..} =
    OrderBook {bookSequence = bookSequence, bookBids = fromRawBookItems bookBids, bookAsks = fromRawBookItems bookAsks}
  where
    fromRawBookItems rawBookItems = Map.fromList $ map toKeyValue rawBookItems
    toKeyValue (BookItem price size orderId) = (orderId, OrderBookItem price size orderId)

updateOrderBook :: OrderBook -> ExchangeMessage -> OrderBook
updateOrderBook book exchangeMessage =
    let book' = book {bookSequence = msgSequence exchangeMessage}
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
