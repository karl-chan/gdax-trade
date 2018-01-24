{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy.Util where

import           Gdax.Account.Balance
import           Gdax.Algo.Action
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (Price, Side (Buy, Sell))

import           Control.Monad.Reader
import           Prelude                      hiding (product)

marketBuy :: Reader ProductBundle Action
marketBuy = marketAll Buy

marketSell :: Reader ProductBundle Action
marketSell = marketAll Sell

limitBuy :: Price -> Reader ProductBundle Action
limitBuy = limitAll Buy

limitSell :: Price -> Reader ProductBundle Action
limitSell = limitAll Sell

marketAll :: Side -> Reader ProductBundle Action
marketAll side = do
  ProductBundle {..} <- ask
  let OrderBookSummary {..} = getSummary book
  return $
    NewAction $
    Market
    { side = side
    , product = product
    , amount =
        case side of
          Buy  -> AmountPrice $ realToFrac . total $ balance2
          Sell -> AmountSize $ realToFrac . total $ balance1
    }

limitAll :: Side -> Price -> Reader ProductBundle Action
limitAll side price = do
  ProductBundle {..} <- ask
  let OrderBookSummary {..} = getSummary book
  return $
    NewAction $
    Limit
    { side = side
    , product = product
    , limitPrice = price
    , size =
        case side of
          Buy  -> (total balance2) `safeDiv` price
          Sell -> realToFrac . total $ balance1
    }
