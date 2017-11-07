module Gdax.Feed.OrderBook.Test where

import           Gdax.Feed.Gdax
import           Gdax.Feed.OrderBook
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Throttle

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import qualified Data.HashMap.Strict       as Map
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit

syncDelay :: NominalDiffTime
syncDelay = 30 -- 30 seconds

test :: Product -> GdaxFeed -> Config -> TestTree
test product gdaxFeed config = do
    testGroup
        "Order Book"
        [testCase "Check that order book matches GDAX implementation" $ testImplementation product gdaxFeed config]

testImplementation :: Product -> GdaxFeed -> Config -> Assertion
testImplementation product gdaxFeed config = do
    bookFeed <- runReaderT (newOrderBookFeed gdaxFeed product) config
    bookFeedListener <- newFeedListener bookFeed
    restBookRef <- newEmptyMVar
    forkIO $ do
        sleep syncDelay
        runReaderT (restOrderBook product) config >>= putMVar restBookRef
    let loop books = do
            book <- readFeed bookFeedListener
            maybeRestBook <- tryReadMVar restBookRef
            case maybeRestBook of
                Nothing -> loop (book : books)
                Just restBook -> do
                    let testBook = fromJust $ find (\b -> bookSequence b == bookSequence restBook) books
                    assertBool (diffBooks testBook restBook) (testBook == restBook)
    loop []

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
