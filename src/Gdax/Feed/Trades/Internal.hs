{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.Trades.Internal where

import           Gdax.Feed.Gdax.Types
import           Gdax.Feed.Trades.Types
import           Gdax.Types.Product
import           Gdax.Types.Trades
import           Gdax.Util.Config
import           Gdax.Util.Feed

import           Coinbase.Exchange.MarketData   (getTrades, tradePrice,
                                                 tradeSide, tradeSize,
                                                 tradeTime)
import qualified Coinbase.Exchange.MarketData   as CB
import           Coinbase.Exchange.Types        (execExchangeT)
import           Coinbase.Exchange.Types.Socket (ExchangeMessage (Match),
                                                 msgMakerOrderId, msgPrice,
                                                 msgProductId, msgSequence,
                                                 msgSide, msgSize, msgTime)

import           Control.Concurrent             (forkIO)
import           Control.Monad.Reader
import           Gdax.Util.Logger
import           Prelude                        hiding (product)

streamTrades :: Product -> GdaxFeedListener -> ReaderT Config IO TradesFeed
streamTrades product gdaxFeedListener = do
  config <- ask
  liftIO $ do
    tradesFeed <- newFeed
    forkIO $ do
      initialTrades <- runReaderT (initTrades product) config
      logDebug $ "Initialised trades."
      let loop trades = do
            writeFeed tradesFeed trades
            exchangeMsg <- readFeed gdaxFeedListener
            let newTrades = updateTrades trades exchangeMsg
            loop newTrades
      loop initialTrades
    return tradesFeed

initTrades :: Product -> ReaderT Config IO Trades
initTrades product = do
  conf <- reader exchangeConf
  rawTrades <- execExchangeT conf $ getTrades $ toId product
  let trades = map (fromRawTrade product) rawTrades
  logDebug $ "Received REST trades: " ++ show trades
  return trades

updateTrades :: Trades -> ExchangeMessage -> Trades
updateTrades trades Match {..} =
  let newTrade =
        Trade
        { time = msgTime
        , product = fromId msgProductId
        , side = msgSide
        , size = msgSize
        , price = msgPrice
        }
  in trades ++ [newTrade]
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
