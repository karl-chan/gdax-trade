module Gdax.Util.Feed where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM

newtype Feed a = Feed
  { unFeed :: TChan a
  }

newtype FeedListener a = FeedListener
  { unFeedListener :: TChan a
  }

newFeed :: IO (Feed a)
newFeed = Feed <$> newBroadcastTChanIO

newFeedListener :: Feed a -> IO (FeedListener a)
newFeedListener (Feed feed) = do
  feedListener <- atomically . dupTChan $ feed
  atomically . peekTChan $ feedListener
  return (FeedListener feedListener)

readFeed :: FeedListener a -> IO a
readFeed = atomically . readTChan . unFeedListener

writeFeed :: Feed a -> a -> IO ()
writeFeed (Feed feed) = atomically . writeTChan feed
