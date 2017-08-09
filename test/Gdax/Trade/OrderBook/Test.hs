module Gdax.Trade.OrderBook.Test where

import           Gdax.Trade.Feed
import           Gdax.Trade.OrderBook

import           Coinbase.Exchange.MarketData       hiding (Open, bookAsks,
                                                     bookBids, bookSequence)
import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core       hiding (Done, Open)
import           Coinbase.Exchange.Types.MarketData hiding (Open, bookAsks,
                                                     bookBids, bookSequence)
import qualified Coinbase.Exchange.Types.MarketData as CB
import           Coinbase.Exchange.Types.Socket
import           Control.Concurrent                 (forkIO)

import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar, readMVar,
                                                     tryReadMVar)
import qualified Data.HashMap                       as Map
import           Data.List
import           Data.Maybe                         (Maybe, fromJust,
                                                     listToMaybe)
import qualified Data.PQueue.Prio.Min               as PQ
import           Debug.Trace
import           Test.Tasty
import           Test.Tasty.HUnit

test :: ProductId -> ExchangeConf -> Feed -> TestTree
test productId conf feed =
    testGroup
        "Order Book"
        [testCase "Check that order book matches GDAX implementation" $ testImplementation productId conf feed]

testImplementation :: ProductId -> ExchangeConf -> Feed -> Assertion
testImplementation productId conf feed = do
    feedListener1 <- newFeedListener feed
    feedListener2 <- newFeedListener feed
    waitUntilFeed feedListener1
    waitUntilFeed feedListener2
    initialBook <- syncOrderBook Nothing PQ.empty productId conf feedListener2
    -- state variables
    bookByIncrementMVar <- newEmptyMVar :: IO (MVar OrderBook)
    bookBySyncMVar <- newEmptyMVar :: IO (MVar OrderBook)
    bookSequenceMVar <- newEmptyMVar :: IO (MVar Sequence)
    -- async update order book
    forkIO $ incrementBook bookByIncrementMVar bookSequenceMVar feedListener1 initialBook
    forkIO $ syncBook bookBySyncMVar bookSequenceMVar productId conf feedListener2
    -- compare order books
    bookByIncrement <- readMVar bookByIncrementMVar
    bookBySync <- readMVar bookBySyncMVar
    assertEqual (diffBooks bookByIncrement bookBySync) bookByIncrement bookBySync

incrementBook :: MVar OrderBook -> MVar Sequence -> FeedListener -> OrderBook -> IO ()
incrementBook bookMVar sequenceMVar feed initialBook = do
    let loop books queue = do
            maybeSequence <- tryReadMVar sequenceMVar
            case maybeSequence of
                Nothing -> do
                    (newMaybeBook, newQueue, bookUpdated) <- tryIncrementOrderBook (listToMaybe books) queue feed
                    let newBooks =
                            if bookUpdated
                                then (fromJust newMaybeBook) : books
                                else books
                    loop newBooks newQueue
                Just sequence -> do
                    putMVar bookMVar $ fromJust $ find ((== sequence) . bookSequence) books
    loop [initialBook] PQ.empty

syncBook :: MVar OrderBook -> MVar Sequence -> ProductId -> ExchangeConf -> FeedListener -> IO ()
syncBook bookMVar sequenceMVar productId conf feed = do
    book <- syncOrderBook Nothing PQ.empty productId conf feed
    putMVar sequenceMVar $ bookSequence book
    putMVar bookMVar book

diffBooks :: OrderBook -> OrderBook -> String
diffBooks book1 book2 =
    "Sequence: " ++
    (show $ bookSequence book1) ++
    " vs " ++
    (show $ bookSequence book2) ++
    "\nBids diff (1-2): " ++
    (diffItems bookBids book1 book2) ++
    "\nBids diff (2-1): " ++
    (diffItems bookBids book2 book1) ++
    "\nAsks diff (1-2): " ++
    (diffItems bookAsks book1 book2) ++ "\nAsks diff (2-1): " ++ (diffItems bookAsks book2 book1)
  where
    diffItems prop items1 items2 = show $ Map.difference (prop items1) (prop items2)
