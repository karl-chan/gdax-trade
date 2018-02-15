{-# LANGUAGE RecordWildCards #-}

module Gdax.Test.Integration.Feed.OrderBook where

import           Gdax.Test.Data

import           Gdax.Feed.OrderBook
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Util.Feed
import           Gdax.Util.Throttle
import           Gdax.Util.Time

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.Reader
import qualified Data.HashMap.Strict       as HM
import           Data.List                 hiding (product)
import           Data.Maybe
import           Prelude                   hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Order book" $ test

test :: Assertion
test = do
  let compareAgainAfter = 30 * second
  bookFeed <- runReaderT (newOrderBookFeed testProduct testGdaxFeed) testConfig
  bookFeedListener <- newFeedListener bookFeed
  restBookRef <- newEmptyMVar
  forkIO $ do
    sleep compareAgainAfter
    restBook <- runReaderT (restOrderBook testProduct) testConfig
    putMVar restBookRef restBook
  let loop books = do
        book <- readFeed bookFeedListener
        maybeRestBook <- tryReadMVar restBookRef
        case maybeRestBook of
          Nothing -> loop (book : books)
          Just restBook -> do
            let testBook =
                  fromJust $
                  find (\b -> bookSequence b == bookSequence restBook) books
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
  "\nAsks diff (1-2): " ++
  diffItems bookAsks book1 book2 ++
  "\nAsks diff (2-1): " ++ diffItems bookAsks book2 book1
  where
    diffItems prop items1 items2 =
      show $ (HM.elems $ prop items1) \\ (HM.elems $ prop items2)
