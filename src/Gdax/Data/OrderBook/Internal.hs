{-# LANGUAGE RecordWildCards #-}

module Gdax.Data.OrderBook.Internal where

import           Gdax.Data.OrderBook.Types
import           Gdax.Data.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Feed.Gdax
import           Gdax.Util.Queue

import           Coinbase.Exchange.Types.Core   (OrderId, Side (Buy, Sell))
import           Coinbase.Exchange.Types.Socket (ExchangeMessage (ChangeLimit, Done, Match, Open),
                                                 msgMakerOrderId, msgMaybePrice,
                                                 msgNewSize, msgOrderId,
                                                 msgPrice, msgProductId,
                                                 msgRemainingSize, msgSequence,
                                                 msgSide, msgSize, msgTime)

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (MVar, newEmptyMVar, putMVar,
                                                 tryReadMVar)
import           Control.Monad.Reader           (ReaderT, ask, liftIO,
                                                 runReaderT)
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (sort)
import           Data.Maybe                     (fromMaybe)
import           Prelude                        hiding (product, sequence)

type TransformFunc = OrderBookItem -> OrderBookItem

streamOrderBook :: Product -> GdaxFeedListener -> ReaderT Config IO OrderBookFeed
streamOrderBook product gdaxFeedListener = do
    config <- ask
    liftIO $ do
        bookFeed <- newFeed
        forkIO $ do
            initialBook <- runReaderT (initialiseOrderBook product gdaxFeedListener) config
            let loop book = do
                    newBook <- runReaderT (incrementOrderBook book product gdaxFeedListener) config
                    writeFeed bookFeed newBook
                    loop newBook
            loop initialBook
        return bookFeed

initialiseOrderBook :: Product -> GdaxFeedListener -> ReaderT Config IO OrderBook
initialiseOrderBook product gdaxFeedListener = do
    config <- ask
    liftIO $ do
        restBookRef <- newEmptyMVar
        forkIO $ do
            book <- runReaderT (restOrderBook product) config
            putMVar restBookRef book
        syncOrderBook restBookRef product gdaxFeedListener

incrementOrderBook :: OrderBook -> Product -> GdaxFeedListener -> ReaderT Config IO OrderBook
incrementOrderBook book product gdaxFeedListener = do
    let initialQueue = newExchangeMsgQueue
        loop queue = do
            let shouldSync = queueSize queue >= queueThreshold
            if shouldSync
                then initialiseOrderBook product gdaxFeedListener
                else do
                    exchangeMsg <- liftIO $ readFeed gdaxFeedListener
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

syncOrderBook :: MVar OrderBook -> Product -> GdaxFeedListener -> IO OrderBook
syncOrderBook restBookRef product gdaxFeedListener = do
    let queueUntilSynced queue = do
            exchangeMsg <- readFeed gdaxFeedListener
            let newQueue = safeAddToQueue queue exchangeMsg product
            maybeBook <- tryReadMVar restBookRef
            case maybeBook of
                Nothing       -> queueUntilSynced newQueue
                Just restBook -> return $ replayMessages newQueue restBook
    queueUntilSynced newExchangeMsgQueue

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
            in HM.insert msgOrderId newOrder bookItems
        Match {..} ->
            let transformFunc bookItem@OrderBookItem {size = oldSize} = bookItem {size = oldSize - msgSize}
            in transformOrder bookItems msgMakerOrderId transformFunc
        ChangeLimit {..} ->
            let transformFunc bookItem =
                    bookItem {price = msgPrice, size = msgNewSize}
            in transformOrder bookItems msgOrderId transformFunc
        Done {..} -> HM.delete msgOrderId bookItems
        _ -> bookItems

transformOrder :: OrderBookItems -> OrderId -> TransformFunc -> OrderBookItems
transformOrder bookItems orderId transformFunc = HM.adjust transformFunc orderId bookItems
