module Gdax.Test.Data.Config where

import           Gdax.Util.Config

import           System.IO.Unsafe

testConfig :: Config
testConfig = unsafePerformIO getGlobalConfig
