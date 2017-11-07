module Gdax.Feed.OrderBook.Types where

import           Gdax.Types.OrderBook
import           Gdax.Util.Feed

type OrderBookFeed = Feed OrderBook

type OrderBookFeedListener = FeedListener OrderBook
