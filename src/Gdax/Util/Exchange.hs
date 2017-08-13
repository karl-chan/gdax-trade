module Gdax.Util.Exchange where

import           Gdax.Util.Feed

import           Coinbase.Exchange.Socket       (subscribe)
import           Coinbase.Exchange.Types        (ApiType (Live))
import           Coinbase.Exchange.Types.Core   (ProductId, Sequence)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage, msgSequence)

import           Control.Concurrent             (forkIO)
import           Control.Monad                  (forever, void)
import           Data.Aeson                     (eitherDecode)
import           Data.Either
import qualified Data.PQueue.Prio.Min           as PQ
import qualified Network.WebSockets             as WS

type ExchangeMsgQueue = PQ.MinPQueue Sequence ExchangeMessage

type DequeueFunc a = a -> ExchangeMessage -> a

-- | Sync order book if queue grows too long, probably due to dropped message
queueThreshold = 20 :: Int

newExchangeMsgQueue :: ExchangeMsgQueue
newExchangeMsgQueue = PQ.empty

queueDropWhileWithKey :: (Sequence -> ExchangeMessage -> Bool) -> ExchangeMsgQueue -> ExchangeMsgQueue
queueDropWhileWithKey = PQ.dropWhileWithKey

queueKeysU :: ExchangeMsgQueue -> [Sequence]
queueKeysU = PQ.keysU

queueElems :: ExchangeMsgQueue -> [ExchangeMessage]
queueElems = PQ.elemsU

queueSize :: ExchangeMsgQueue -> Int
queueSize = PQ.size

enqueue :: ExchangeMsgQueue -> ExchangeMessage -> ExchangeMsgQueue
enqueue queue exchangeMessage = PQ.insert (msgSequence exchangeMessage) exchangeMessage queue

dequeue :: ExchangeMsgQueue -> DequeueFunc a -> a -> a
dequeue queue playbackFunc target = foldl playbackFunc target queue