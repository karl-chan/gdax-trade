{-# LANGUAGE ScopedTypeVariables #-}

module Gdax.Util.Throttle where

import           Gdax.Util.Logger

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Data.List          (splitAt)
import           Data.Time.Clock

throttle :: [IO a] -> Int -> NominalDiffTime -> Maybe NominalDiffTime -> IO [a]
throttle tasks concurrency interval maybeRetryAfter =
  let loop [] results = return results
      loop remainingTasks acc = do
        let (immediateTasks, queueTasks) = splitAt concurrency remainingTasks
        results <- mapM (maybe id retry maybeRetryAfter) immediateTasks
        logDebug $
          "Task succeeded... sleeping for " ++
          show interval ++ " before resuming."
        sleep interval
        loop queueTasks $ acc ++ results
  in loop tasks []

retry :: NominalDiffTime -> IO a -> IO a
retry after task =
  catch task $ \(e :: SomeException) -> do
    logDebug $
      "Task failed because of: " ++
      show e ++ ". Retrying after: " ++ show after ++ "."
    sleep after
    retry after task

sleep :: NominalDiffTime -> IO ()
sleep seconds = threadDelay $ floor $ 1e6 * seconds
