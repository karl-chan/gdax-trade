{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.Trades.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.TimeSeries.Types
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Product
import           Gdax.Types.TimeSeries          (EndTime, Granularity,
                                                 StartTime, Stat (..),
                                                 TimeSeries)
import qualified Gdax.Types.TimeSeries.Util     as TS
import           Gdax.Types.Trades
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Queue
import           Gdax.Util.Throttle

import           Coinbase.Exchange.MarketData   (getTrades, tradePrice,
                                                 tradeSide, tradeSize,
                                                 tradeTime)
import qualified Coinbase.Exchange.MarketData   as CB
import           Coinbase.Exchange.Types        (execExchangeT)
import           Coinbase.Exchange.Types.Core   (Price (..), Size (..))
import           Coinbase.Exchange.Types.Socket (ExchangeMessage (Match),
                                                 msgMakerOrderId, msgPrice,
                                                 msgProductId, msgSequence,
                                                 msgSide, msgSize, msgTime)

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (MVar, newMVar, readMVar,
                                                 swapMVar)
import           Control.Exception              (SomeException, catch)
import           Control.Monad                  (void)
import           Control.Monad.Reader
import           Data.List                      (insert)
import           Data.Maybe                     (maybe)
import           Data.Time.Clock                (addUTCTime, diffUTCTime,
                                                 getCurrentTime)
import           Gdax.Util.Logger
import           Prelude                        hiding (product)

streamTrades ::
     StartTime -> Product -> GdaxFeedListener -> ReaderT Config IO TradesFeed
streamTrades product gdaxFeedListener = do
  config <- ask
  granularity <- reader apiGranularity
  liftIO $ do
    tradesFeed <- newFeed
    forkIO $ do
      initialTrades <- runReaderT (initialTrades product) config
      logDebug $ "Initialised trades."
      let loop trades = do
            writeFeed tradeFeed trades
            exchangeMsg <- readFeed gdaxFeedListener
            let newTrades = updateTrades trades exchangeMsg
            loop newTrades
      loop initialTrades
    return tsFeed

initialTrades :: Product -> ReaderT Config IO Trades
initialTrades product = do
  config <- ask
  granularity <- reader apiGranularity
  conf <- reader exchangeConf
  rawTrades <- execExchangeT conf $ getTrades $ toId product
  let trades = map (fromRawTrade product) rawTrades
  logDebug $ "Received REST trades: " ++ show trades
  return trades

updateTrades :: Trades -> ExchangeMsg -> Trades
updateTrades trades Match {..} = 
    let newTrade = Trade {
        time = msgTime,
        product = fromId msgProductId,
        side = msgSide,
        size = msgSize,
        price = msgPrice
    }
        
updateTrades trades _ = trades
fromRawTrade :: Product -> CB.Trade -> Trade
fromRawTrade product CB.Trade {..} =
  Trade
  { time = tradeTime
  , product = product
  , side = tradeSide
  , size = tradeSize
  , price = tradePrice
  }

updateStat :: Trades -> ExchangeMessage -> Stat
updateStat stat@Stat {..} Match {..} =
  stat
  { start = min start msgTime
  , end = max end msgTime
  , low = min low msgPrice
  , high = max high msgPrice
  , open = open
  , close = msgPrice
  , volume = volume + msgSize
  }
updateStat _ msg =
  error $ "updateStat only works for Match messages, not for: " ++ show msg

showRange :: TimeSeries -> String
showRange = uncurry showRangeTime . TS.range

showRangeTime :: StartTime -> EndTime -> String
showRangeTime start end = show start ++ " - " ++ show end
