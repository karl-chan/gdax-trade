module Gdax.Feed.MyAccount.Types where

import           Gdax.Account.MyAccount
import           Gdax.Util.Feed

type MyAccountFeed = Feed MyAccount

type MyAccountFeedListener = FeedListener MyAccount
