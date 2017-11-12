{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.OrderBook.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.OrderBook.Types
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue

import           Coinbase.Exchange.Types.Core   (Side (Buy, Sell))
import           Coinbase.Exchange.Types.Socket

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (MVar, newEmptyMVar, putMVar,
                                                 tryReadMVar)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (sort)
import           Gdax.Util.Logger
import           Prelude                        hiding (product, sequence)

streamOrderBook ::
     Product -> GdaxFeedListener -> ReaderT Config IO OrderBookFeed
streamOrderBook product gdaxFeedListener = do
  config <- ask
  liftIO $ do
    bookFeed <- newFeed
    forkIO $ do
      initialBook <-
        runReaderT (initialiseOrderBook product gdaxFeedListener) config
      logDebug "Initialised order book."
      let loop book = do
            writeFeed bookFeed book
            newBook <-
              runReaderT
                (incrementOrderBook book product gdaxFeedListener)
                config
            loop newBook
      loop initialBook
    return bookFeed

initialiseOrderBook ::
     Product -> GdaxFeedListener -> ReaderT Config IO OrderBook
initialiseOrderBook product gdaxFeedListener = do
  config <- ask
  liftIO $ do
    restBookRef <- newEmptyMVar
    forkIO $ do
      book <- runReaderT (restOrderBook product) config
      putMVar restBookRef book
    syncOrderBook restBookRef product gdaxFeedListener

incrementOrderBook ::
     OrderBook -> Product -> GdaxFeedListener -> ReaderT Config IO OrderBook
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

safeAddToQueue ::
     ExchangeMsgQueue -> ExchangeMessage -> Product -> ExchangeMsgQueue
safeAddToQueue queue exchangeMsg product =
  case exchangeMsg of
    Error {} -> queue
    Activate {} -> queue
    _ ->
      if msgProductId exchangeMsg == toId product
        then enqueue queue exchangeMsg
        else queue

replayMessages :: ExchangeMsgQueue -> OrderBook -> OrderBook
replayMessages queue book =
  let outdated bkSequence _ = bkSequence <= bookSequence book
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
       Sell ->
         book' {bookAsks = updateBookItems (bookAsks book') exchangeMessage}
       Buy ->
         book' {bookBids = updateBookItems (bookBids book') exchangeMessage}

updateBookItems :: OrderBookItems -> ExchangeMessage -> OrderBookItems
updateBookItems bookItems msg =
  case msg of
    Open {..} ->
      let newOrder = OrderBookItem msgPrice msgRemainingSize msgOrderId
      in HM.insert msgOrderId newOrder bookItems
    Match {..} ->
      let matchOp bookItem@OrderBookItem {size = oldSize} =
            bookItem {size = oldSize - msgSize}
      in HM.adjust matchOp msgMakerOrderId bookItems
    ChangeLimit {..} ->
      let changeOp bookItem = bookItem {price = msgPrice, size = msgNewSize}
      in HM.adjust changeOp msgOrderId bookItems
    Done {..} -> HM.delete msgOrderId bookItems
    _ -> bookItems
