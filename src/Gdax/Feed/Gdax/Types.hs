module Gdax.Feed.Gdax.Types where

import           Gdax.Util.Feed

import           Coinbase.Exchange.Types.Socket

type GdaxFeed = Feed ExchangeMessage

type GdaxFeedListener = FeedListener ExchangeMessage
