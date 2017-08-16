module Gdax.Util.Throttle where

import           Gdax.Data.TimeSeries.Types

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Monad              (replicateM)
import           Data.List                  (null, splitAt)
import           Data.Time.Clock            (NominalDiffTime)

throttle :: Int -> NominalDiffTime -> [IO a] -> IO [a]
throttle concurrency interval tasks =
    let loop remainingTasks acc =
            if null remainingTasks
                then return acc
                else do
                    let (immediateTasks, queueTasks) = splitAt concurrency remainingTasks
                    res <- mapConcurrently id immediateTasks
                    sleep interval
                    loop queueTasks $ acc ++ res
    in loop tasks []

sleep :: NominalDiffTime -> IO ()
sleep gap = threadDelay $ (floor . (* 1e6) . toSeconds) gap
