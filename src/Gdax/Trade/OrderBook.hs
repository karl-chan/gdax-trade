module Gdax.Trade.OrderBook
    ( livecastOrderBook
    ) where

import           Coinbase.Exchange.MarketData
import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import           Coinbase.Exchange.Types.MarketData
import           Coinbase.Exchange.Types.Private
import           Coinbase.Exchange.Types.Socket

import           Control.Concurrent                 (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan       (TChan, newTChanIO,
                                                     readTChan, writeTChan)
import           Control.Exception                  (throw)
import           Control.Monad                      (forever)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.STM                  (atomically)
import           Data.Aeson                         (eitherDecode)
import           Data.IVar.Simple                   as IVar
import           Data.Maybe                         (fromJust, isNothing)
import qualified Data.PQueue.Prio.Min               as PQ
import           Network.WebSockets                 (ClientApp, Connection,
                                                     receiveData)

type OrderBook = Book OrderId

livecastOrderBook :: ExchangeConf -> ProductId -> TChan OrderBook -> IO ()
livecastOrderBook conf productId broadcastChan = do
    exchangeMessageChan <- newTChanIO
    startSequenceIVar <- IVar.new :: IO (IVar Sequence)
    forkIO $ subscribe Live [productId] $ socketApp exchangeMessageChan startSequenceIVar -- 1. Send a subscribe message for the product of interest.
    eitherBook <- runExchange conf $ getOrderBook productId -- 3. Make a REST request for the order book snapshot from the REST feed.
    case eitherBook of
        Left err -> throw err
        Right book -> do
            notifyStartSequence (bookSequence book) startSequenceIVar
            forever $ do
                exchangeMessage <- atomically $ readTChan exchangeMessageChan
                print exchangeMessage

socketApp :: TChan ExchangeMessage -> IVar Sequence -> ClientApp ()
socketApp chan ivar conn = do
    let queue = PQ.empty :: PQ.MinPQueue Sequence ExchangeMessage
        repeatProcess queue = do
            handleExchangeMessage conn $ \exchangeMessage -> do
                waitForStartSequence ivar (failureApp exchangeMessage) successApp
          where
            failureApp exchangeMessage = do
                let queue' = PQ.insert (msgSequence exchangeMessage) exchangeMessage queue -- 2. Queue any messages received over the websocket stream.
                repeatProcess queue
            successApp startSequence = do
                playbackQueue startSequence queue (atomically . writeTChan chan) -- 4. Playback queued messages, discarding sequence numbers before or equal to the snapshot sequence number.
                forever $ handleExchangeMessage conn $ atomically . writeTChan chan -- 6. After playback is complete, apply real-time stream messages as they arrive.
    repeatProcess queue

notifyStartSequence :: Sequence -> IVar Sequence -> IO ()
notifyStartSequence sequence ivar = IVar.write ivar sequence

waitForStartSequence :: IVar Sequence -> IO () -> (Sequence -> IO ()) -> IO ()
waitForStartSequence ivar failureApp successApp = do
    maybeStartSequence <- IVar.tryRead ivar
    if isNothing maybeStartSequence
        then failureApp
        else successApp $ fromJust maybeStartSequence

playbackQueue :: (Applicative f) => Sequence -> PQ.MinPQueue Sequence a -> (a -> f b) -> f (PQ.MinPQueue Sequence b)
playbackQueue startSequence queue func = do
    let remainingQueue = PQ.dropWhileWithKey (\k _ -> k <= startSequence) queue
    PQ.traverseWithKey (\_ msg -> func msg) remainingQueue

handleExchangeMessage :: Connection -> (ExchangeMessage -> IO a) -> IO a
handleExchangeMessage conn fn = do
    exchangeMessage <- receiveData conn
    case eitherDecode exchangeMessage of
        Left err              -> error err
        Right exchangeMessage -> fn exchangeMessage
