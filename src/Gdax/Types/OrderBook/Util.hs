{-# LANGUAGE RecordWildCards #-}

module Gdax.Types.OrderBook.Util where

import           Gdax.Types.OrderBook
import           Gdax.Types.Product
import           Gdax.Util.Config

import           Coinbase.Exchange.MarketData       hiding (Product)
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import qualified Coinbase.Exchange.Types.MarketData as CB hiding (Product)

import           Control.Monad.Reader
import qualified Data.HashMap.Strict                as HM
import           Data.List                          (sortOn)

-- Asks sorted in ascending order of price
sortedAsks :: OrderBook -> [OrderBookItem]
sortedAsks OrderBook {..} = sortOn price $ HM.elems bookAsks

-- Bids sorted in descending order of price
sortedBids :: OrderBook -> [OrderBookItem]
sortedBids OrderBook {..} = sortOn (negate . price) $ HM.elems bookBids

orderBookSummary :: OrderBook -> (Price, Price, Price)
orderBookSummary book =
    let bestBid = price . head . sortedBids $ book
        bestAsk = price . head . sortedAsks $ book
        midPrice = (bestBid + bestAsk) / 2
    in (bestBid, midPrice, bestAsk)

-- Amount of spread to incur given purchase of size
--marketSpread :: Side -> Size -> OrderBook -> Scientific
--marketSpread side size OrderBook {..} =
--    case side of
--        Buy  -> bookBids
--        Sell -> bookAsks
orderBookItemsCost :: [OrderBookItem] -> Bool -> Price
orderBookItemsCost bookItems inQuoteCurrency =
    sum
        [ let priceItem =
                  if inQuoteCurrency
                      then price item
                      else 1
          in priceItem * (realToFrac . size) item
        | item <- bookItems
        ]



restOrderBook :: Product -> ReaderT Config IO OrderBook
restOrderBook product = do
    conf <- reader exchangeConf
    rawBook <- execExchangeT conf $ getOrderBook (toId product)
    return $ fromRawOrderBook rawBook product

fromRawOrderBook :: CB.Book OrderId -> Product -> OrderBook
fromRawOrderBook CB.Book {..} product =
    OrderBook
    { bookSequence = bookSequence
    , bookBids = fromRawBookItems bookBids
    , bookAsks = fromRawBookItems bookAsks
    , bookProduct = product
    }
  where
    fromRawBookItems rawBookItems = HM.fromList $ map toKeyValue rawBookItems
    toKeyValue (CB.BookItem price size orderId) = (orderId, OrderBookItem price size orderId)
