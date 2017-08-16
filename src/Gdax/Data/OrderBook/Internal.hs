{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue

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
import           Data.Time.Clock                    (getCurrentTime)
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

type TransformFunc = OrderBookItem -> OrderBookItem

processOrderBook :: ProductId -> ProductFeedListener -> ReaderT Config IO OrderBookFeed
processOrderBook productId productFeedListener = do
    config <- ask
    bookFeed <- liftIO newFeed
    liftIO . forkIO $ do
        initialBook <- runReaderT (initialiseOrderBook productId productFeedListener) config
        let loop book = do
                newBook <- runReaderT (incrementOrderBook book productId productFeedListener) config
                writeFeed bookFeed newBook
                loop newBook
        loop initialBook
    return bookFeed

initialiseOrderBook :: ProductId -> ProductFeedListener -> ReaderT Config IO OrderBook
initialiseOrderBook productId productFeedListener = do
    conf <- reader exchangeConf
    restBookRef <- liftIO newEmptyMVar :: ReaderT Config IO (MVar OrderBook)
    liftIO . forkIO $ do
        book <- restOrderBook productId conf
        putMVar restBookRef book
    liftIO $ syncOrderBook restBookRef productId productFeedListener

incrementOrderBook :: OrderBook -> ProductId -> ProductFeedListener -> ReaderT Config IO OrderBook
incrementOrderBook book productId productFeedListener = do
    let initialQueue = newExchangeMsgQueue
        loop queue = do
            let shouldSync = queueSize queue >= queueThreshold
            if shouldSync
                then do
                    initialiseOrderBook productId productFeedListener
                else do
                    exchangeMsg <- liftIO $ readFeed productFeedListener
                    let newQueue = safeAddToQueue queue exchangeMsg productId
                    if canDequeue book newQueue
                        then return $ dequeue newQueue updateOrderBook book
                        else loop newQueue
    loop initialQueue

canDequeue :: OrderBook -> ExchangeMsgQueue -> Bool
canDequeue prevBook queue =
    let sequences = bookSequence prevBook : queueKeysU queue
    in sort sequences == [minimum sequences .. maximum sequences]

safeAddToQueue :: ExchangeMsgQueue -> ExchangeMessage -> ProductId -> ExchangeMsgQueue
safeAddToQueue queue exchangeMsg productId =
    if msgProductId exchangeMsg == productId
        then enqueue queue exchangeMsg
        else queue

replayMessages :: ExchangeMsgQueue -> OrderBook -> OrderBook
replayMessages queue book =
    let outdated sequence _ = sequence <= bookSequence book
        replayQueue = queueDropWhileWithKey outdated queue
    in dequeue replayQueue updateOrderBook book

syncOrderBook :: MVar OrderBook -> ProductId -> ProductFeedListener -> IO OrderBook
syncOrderBook restBookRef productId productFeedListener = do
    waitUntilFeed productFeedListener
    let queueUntilSynced queue = do
            exchangeMsg <- readFeed productFeedListener
            let newQueue = safeAddToQueue queue exchangeMsg productId
            maybeBook <- tryReadMVar restBookRef
            case maybeBook of
                Nothing -> queueUntilSynced newQueue
                Just restBook -> do
                    return $ replayMessages newQueue restBook
    queueUntilSynced newExchangeMsgQueue

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
        , bookBids = fromRawBookItems bookBids
        , bookAsks = fromRawBookItems bookAsks
        }
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
