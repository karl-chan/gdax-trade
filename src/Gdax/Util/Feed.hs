module Gdax.Util.Feed where

import           BroadcastChan.Throw            (BroadcastChan, In, Out,
                                                 newBChanListener,
                                                 newBroadcastChan, readBChan,
                                                 writeBChan)

type Feed a = BroadcastChan In a

type FeedListener a = BroadcastChan Out a

newFeed :: IO (Feed a)
newFeed = newBroadcastChan

newFeedListener :: Feed a -> IO (FeedListener a)
newFeedListener = newBChanListener

waitUntilFeed :: FeedListener a -> IO (FeedListener a)
waitUntilFeed listener = do
    readFeed listener
    return listener

readFeed :: FeedListener a -> IO a
readFeed = readBChan

writeFeed :: Feed a -> a -> IO ()
writeFeed = writeBChan
