module Gdax.Util.Queue where

import           Coinbase.Exchange.Types.Core   (Sequence)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage, msgSequence)

import qualified Data.PQueue.Prio.Min           as PQ

type ExchangeMsgQueue = PQ.MinPQueue Sequence ExchangeMessage

type DequeueFunc a = a -> ExchangeMessage -> a

-- | Sync order book if queue grows too long, probably due to dropped message
queueThreshold :: Int
queueThreshold = 20

newExchangeMsgQueue :: ExchangeMsgQueue
newExchangeMsgQueue = PQ.empty

queueDropWhileWithKey :: (Sequence -> ExchangeMessage -> Bool) -> ExchangeMsgQueue -> ExchangeMsgQueue
queueDropWhileWithKey = PQ.dropWhileWithKey

queueHead :: ExchangeMsgQueue -> ExchangeMessage
queueHead = snd . PQ.findMin

queueKeysU :: ExchangeMsgQueue -> [Sequence]
queueKeysU = PQ.keysU

queueElems :: ExchangeMsgQueue -> [ExchangeMessage]
queueElems = PQ.elemsU

queueFilter :: (ExchangeMessage -> Bool) -> ExchangeMsgQueue -> ExchangeMsgQueue
queueFilter = PQ.filter

queueNull :: ExchangeMsgQueue -> Bool
queueNull = PQ.null

queueSize :: ExchangeMsgQueue -> Int
queueSize = PQ.size

enqueue :: ExchangeMsgQueue -> ExchangeMessage -> ExchangeMsgQueue
enqueue queue exchangeMessage = PQ.insert (msgSequence exchangeMessage) exchangeMessage queue

dequeue :: ExchangeMsgQueue -> DequeueFunc a -> a -> a
dequeue queue playbackFunc target = foldl playbackFunc target queue
