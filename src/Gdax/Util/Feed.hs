module Gdax.Util.Feed where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM

type Feed a = TChan a

type FeedListener a = TChan a

newFeed :: IO (TChan a)
newFeed = newBroadcastTChanIO

newFeedListener :: Feed a -> IO (FeedListener a)
newFeedListener feed = do
    feedListener <- atomically . dupTChan $ feed
    atomically . peekTChan $ feedListener
    return feedListener

readFeed :: FeedListener a -> IO a
readFeed = atomically . readTChan

writeFeed :: Feed a -> a -> IO ()
writeFeed feed = atomically . writeTChan feed
