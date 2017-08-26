module Gdax.Data.OrderBook.Test where

import           Gdax.Data.OrderBook.Internal
import           Gdax.Data.OrderBook.Types
import           Gdax.Types.Product
import           Gdax.Types.Product.Feed
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

test :: Product -> ProductFeed -> Config -> TestTree
test product productFeed config = do
    testGroup
        "Order Book"
        [testCase "Check that order book matches GDAX implementation" $ testImplementation product productFeed config]

testImplementation :: Product -> ProductFeed -> Config -> Assertion
testImplementation product productFeed config = do
    feedListener <- newFeedListener productFeed >>= waitUntilFeed
    feedListener2 <- newFeedListener productFeed >>= waitUntilFeed
    initialBook <- runReaderT (initialiseOrderBook product feedListener) config
    -- state variables
    sequenceSignal <- newEmptyMVar :: IO (MVar Sequence)
    -- async update order book
    bookByIncrementRef <- localBook initialBook sequenceSignal product feedListener config
    bookBySyncRef <- serverBookWithDelay syncDelay sequenceSignal product config
    -- compare order books
    bookByIncrement <- readMVar bookByIncrementRef
    bookBySync <- readMVar bookBySyncRef
    assertBool (diffBooks bookByIncrement bookBySync) (bookByIncrement == bookBySync)

--    forkIO . forever $ readFeed feedListener2 >>= print
localBook :: OrderBook -> MVar Sequence -> Product -> ProductFeedListener -> Config -> IO (MVar OrderBook)
localBook initialBook sequenceIn product productFeedListener config = do
    bookRef <- newEmptyMVar
    forkIO $ do
        let loop books = do
                targetSeq <- tryReadMVar sequenceIn
                case targetSeq of
                    Nothing -> do
                        newBook <- runReaderT (incrementOrderBook (head books) product productFeedListener) config
                        loop (newBook : books)
                    Just sequence -> putMVar bookRef $ fromJust $ find ((== sequence) . bookSequence) books
        loop [initialBook]
    return bookRef

serverBookWithDelay :: NominalDiffTime -> MVar Sequence -> Product -> Config -> IO (MVar OrderBook)
serverBookWithDelay delay sequenceOut product config = do
    bookRef <- newEmptyMVar
    forkIO $ do
        sleep delay
        book <- runReaderT (restOrderBook product) config
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
