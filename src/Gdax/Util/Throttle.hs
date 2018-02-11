{-# LANGUAGE ScopedTypeVariables #-}

module Gdax.Util.Throttle where

import           Gdax.Util.Logger

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Data.List          (splitAt)
import           Data.Time.Clock

throttle :: Int -> NominalDiffTime -> Maybe NominalDiffTime -> [IO a] -> IO [a]
throttle concurrency interval maybeRetryAfter tasks =
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
  catch task $ \(_ :: IOException) -> do
    logDebug $ "Task failed... retrying after " ++ show after ++ "s."
    sleep after
    retry after task

sleep :: NominalDiffTime -> IO ()
sleep seconds = threadDelay $ floor $ 1e6 * seconds
