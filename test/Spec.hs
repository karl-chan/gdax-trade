module Main where

import qualified Gdax.Algo.Cost.Test       as Cost
import qualified Gdax.Feed.OrderBook.Test  as OrderBookFeed
import qualified Gdax.Feed.TimeSeries.Test as TimeSeries
import           Gdax.Types.Bundle
import           Gdax.Types.Bundle.Test
import           Gdax.Types.TimeSeries

import           Control.Monad
import           Control.Monad.Reader
import           System.IO.Unsafe
import           Test.Tasty

granularity :: Granularity
granularity = 60 -- 60 seconds

main :: IO ()
main = defaultMain . unsafePerformIO $ runReaderT tests testBundle

tests :: ReaderT Bundle IO TestTree
tests =
  liftM2 testGroup (return "All tests") $
  sequence [Cost.tests, OrderBookFeed.tests]
--        TimeSeries.test granularity product gdaxFeed config]
