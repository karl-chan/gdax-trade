module Gdax.Test.Unit where

import qualified Gdax.Test.Unit.Algo as Algo

import           Test.Tasty

tests :: TestTree
tests = testGroup "Unit tests" [Algo.tests]
