module Gdax.Data.OrderBook.Test where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Data.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Throttle

import           Coinbase.Exchange.Types.Core (ProductId, Sequence)

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import qualified Data.HashMap                 as Map
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit

syncDelay :: NominalDiffTime
syncDelay = 30 -- 30 seconds


test :: ProductId -> ProductFeed -> Config -> TestTree
test productId productFeed config = do
    testGroup
        "Order Book"
        [testCase "Check that order book matches GDAX implementation" $ testImplementation productId productFeed config]

testImplementation :: ProductId -> ProductFeed -> Config -> Assertion
testImplementation productId productFeed config = do
    feedListener <- newFeedListener productFeed >>= waitUntilFeed
    feedListener2 <- newFeedListener productFeed >>= waitUntilFeed
    initialBook <- runReaderT (initialiseOrderBook productId feedListener) config
    -- state variables
    sequenceSignal <- newEmptyMVar :: IO (MVar Sequence)
    -- async update order book
    bookByIncrementRef <- localBook initialBook sequenceSignal productId feedListener config
    bookBySyncRef <- serverBookWithDelay syncDelay sequenceSignal productId config
    -- compare order books
    bookByIncrement <- readMVar bookByIncrementRef
    bookBySync <- readMVar bookBySyncRef
    assertBool (diffBooks bookByIncrement bookBySync) (bookByIncrement == bookBySync)

--    forkIO . forever $ readFeed feedListener2 >>= print
localBook :: OrderBook -> MVar Sequence -> ProductId -> ProductFeedListener -> Config -> IO (MVar OrderBook)
localBook initialBook sequenceIn productId productFeedListener config = do
    bookRef <- newEmptyMVar
    forkIO $ do
        let loop books = do
                targetSeq <- tryReadMVar sequenceIn
                case targetSeq of
                    Nothing -> do
                        newBook <- runReaderT (incrementOrderBook (head books) productId productFeedListener) config
                        loop (newBook : books)
                    Just sequence -> putMVar bookRef $ fromJust $ find ((== sequence) . bookSequence) books
        loop [initialBook]
    return bookRef

serverBookWithDelay :: NominalDiffTime -> MVar Sequence -> ProductId -> Config -> IO (MVar OrderBook)
serverBookWithDelay delay sequenceOut productId config = do
    bookRef <- newEmptyMVar
    forkIO $ do
        sleep delay
        book <- restOrderBook productId (exchangeConf config)
        putMVar sequenceOut (bookSequence book)
        putMVar bookRef book
    return bookRef

diffBooks :: OrderBook -> OrderBook -> String
diffBooks book1 book2 =
    "Sequence: " ++
    (show . bookSequence) book1 ++
    " vs " ++
    (show . bookSequence) book2 ++
    "\nBids diff (1-2): " ++
    diffItems bookBids book1 book2 ++
    "\nBids diff (2-1): " ++
    diffItems bookBids book2 book1 ++
    "\nAsks diff (1-2): " ++ diffItems bookAsks book1 book2 ++ "\nAsks diff (2-1): " ++ diffItems bookAsks book2 book1
  where
    diffItems prop items1 items2 = show $ (Map.elems $ prop items1) \\ (Map.elems $ prop items2)
