module Gdax.Test.Data.Feed where

import           Gdax.Test.Data.Config
import           Gdax.Test.Data.Product

import           Gdax.Feed.Gdax

import           Control.Monad.Reader
import           System.IO.Unsafe

testGdaxFeed :: GdaxFeed
testGdaxFeed =
  unsafePerformIO $ runReaderT (newGdaxFeed [testProduct]) testConfig
