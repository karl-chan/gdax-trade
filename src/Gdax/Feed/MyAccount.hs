module Gdax.Feed.MyAccount
  ( module Gdax.Feed.MyAccount.Types
  , module Gdax.Feed.MyAccount
  ) where

import           Gdax.Feed.MyAccount.Internal
import           Gdax.Feed.MyAccount.Types
import           Gdax.Util.Config

import           Control.Monad.Reader

newAccountFeed :: ReaderT Config IO MyAccountFeed
newAccountFeed = streamAccount
