module Gdax.Test.Integration where

import qualified Gdax.Test.Integration.Feed as Feed

import           Test.Tasty

tests :: TestTree
tests = do
  testGroup "Integration tests" [Feed.tests]
