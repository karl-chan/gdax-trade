module Gdax.Feed.MyAccount
    ( module Gdax.Feed.MyAccount.Types
    , module Gdax.Feed.MyAccount
    ) where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.MyAccount.Internal
import           Gdax.Feed.MyAccount.Types
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Logger

import           Control.Monad.Reader

newAccountFeed :: GdaxFeed -> ReaderT Config IO MyAccountFeed
newAccountFeed gdaxFeed = do
    gdaxFeedListener <- liftIO . newFeedListener $ gdaxFeed
    streamAccount gdaxFeedListener
