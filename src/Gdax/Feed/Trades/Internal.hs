{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.Trades.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Product
import           Gdax.Types.Trades
import qualified Gdax.Types.Trades.Util         as Trades
import           Gdax.Util.Config
import           Gdax.Util.Feed
import           Gdax.Util.Logger
import           Gdax.Util.Throttle.Api
import           Gdax.Util.Time

import           Coinbase.Exchange.MarketData   (getTradesPaginated, tradePrice,
                                                 tradeSide, tradeSize,
                                                 tradeTime)
import qualified Coinbase.Exchange.MarketData   as CB
import           Coinbase.Exchange.Types.Socket (ExchangeMessage (Match),
                                                 msgMakerOrderId, msgPrice,
                                                 msgProductId, msgSequence,
                                                 msgSide, msgSize, msgTime)

import           Control.Concurrent             (forkIO)
import           Control.Monad.Reader
import           Data.Time.Clock
import           Prelude                        hiding (product)

streamTrades :: Product -> GdaxFeedListener -> ReaderT Config IO TradesFeed
streamTrades product gdaxFeedListener = do
  config <- ask
  window <- reader $ rollingWindow . tradesConf
  now <- liftIO getCurrentTime
  liftIO $ do
    tradesFeed <- newFeed
    forkIO $ do
      let startTime = addUTCTime (-window) now
      initialTrades <- runReaderT (initTrades startTime product) config
      logDebug $ "Initialised trades."
      let loop trades = do
            writeFeed tradesFeed trades
            exchangeMsg <- readFeed gdaxFeedListener
            newTrades <- runReaderT (updateTrades trades exchangeMsg) config
            loop newTrades
      loop initialTrades
    return tradesFeed

initTrades :: StartTime -> Product -> ReaderT Config IO Trades
initTrades startTime product = do
  let productId = toId product
      terminateCondition rawTrades =
        null rawTrades || tradeTime (last rawTrades) <= startTime
  allRawTrades <-
    throttlePaginatedApi (getTradesPaginated productId) terminateCondition
  let rawTrades = concat allRawTrades
  logDebug $ "Received all REST trades: " ++ show rawTrades
  let trades = Trades.listToTrades $ map (fromRawTrade product) rawTrades
  return trades

updateTrades :: Trades -> ExchangeMessage -> ReaderT Config IO Trades
updateTrades trades Match {..} = do
  window <- reader $ rollingWindow . tradesConf
  now <- liftIO getCurrentTime
  let cutoff = addUTCTime (-window) now
      newTrade =
        Trade
        { time = msgTime
        , product = fromId msgProductId
        , side = msgSide
        , size = msgSize
        , price = msgPrice
        }
      recentTrades = Trades.dropBefore cutoff trades
  return $ Trades.insert newTrade recentTrades
updateTrades trades _ = return trades

fromRawTrade :: Product -> CB.Trade -> Trade
fromRawTrade product CB.Trade {..} =
  Trade
  { time = tradeTime
  , product = product
  , side = tradeSide
  , size = tradeSize
  , price = tradePrice
  }
