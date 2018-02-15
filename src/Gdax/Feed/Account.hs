module Gdax.Feed.Account
  ( module Gdax.Feed.Account.Types
  , module Gdax.Feed.Account
  ) where

import           Gdax.Feed.Account.Internal
import           Gdax.Feed.Account.Types
import           Gdax.Util.Config

import           Control.Monad.Reader

newAccountFeed :: ReaderT Config IO MyAccountFeed
newAccountFeed = streamAccount
