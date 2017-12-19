module Gdax.Feed.Trades.Types where

import           Gdax.Types.Trades
import           Gdax.Util.Feed

type TradesFeed = Feed Trades

type TradesFeedListener = FeedListener Trades
