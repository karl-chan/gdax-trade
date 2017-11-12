module Gdax.Util.Config.Test where

import           Gdax.Util.Config

import           System.IO.Unsafe

testConfig :: Config
testConfig = unsafePerformIO getGlobalConfig
