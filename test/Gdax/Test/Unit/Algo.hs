module Gdax.Test.Unit.Algo where

import qualified Gdax.Test.Unit.Algo.Cost as Cost

import           Test.Tasty

tests :: TestTree
tests = testGroup "Algo" [Cost.tests]
