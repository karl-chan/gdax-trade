module Gdax.Data.OrderBook.Test where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Util.Feed
import           Gdax.Data.Product

import           Coinbase.Exchange.Types      (ExchangeConf)
import           Coinbase.Exchange.Types.Core (ProductId, Sequence)

import           Control.Concurrent           (forkIO)

import           Control.Concurrent.MVar      (MVar, newEmptyMVar, putMVar,
                                               readMVar, tryReadMVar)
import qualified Data.HashMap                 as Map
import           Data.List
import           Data.Maybe                   (Maybe, fromJust, listToMaybe)
import qualified Data.PQueue.Prio.Min         as PQ
import           Test.Tasty
import           Test.Tasty.HUnit

test :: ProductId -> ExchangeConf -> ProductFeed -> TestTree
test productId conf feed =
    testGroup
        "Order Book"
        [testCase "Check that order book matches GDAX implementation" $ testImplementation productId conf feed]

testImplementation :: ProductId -> ExchangeConf -> ProductFeed -> Assertion
testImplementation productId conf feed = do
    feedListener1 <- newFeedListener feed >>= waitUntilFeed
    feedListener2 <- newFeedListener feed >>= waitUntilFeed
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

incrementBook :: MVar OrderBook -> MVar Sequence -> ProductFeedListener -> OrderBook -> IO ()
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

syncBook :: MVar OrderBook -> MVar Sequence -> ProductId -> ExchangeConf -> ProductFeedListener -> IO ()
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
