module Gdax.Feed.Account.Types where

import           Gdax.Account.MyAccount
import           Gdax.Util.Feed

type MyAccountFeed = Feed MyAccount

type MyAccountFeedListener = FeedListener MyAccount
