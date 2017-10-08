{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Executor where

import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Product
import           Gdax.Util.Bundle
import           Gdax.Util.Config

import           Coinbase.Exchange.Private
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core (Size)
import qualified Coinbase.Exchange.Types.Core as CB

import           Control.Monad
import           Control.Monad.Reader
import           Prelude                      hiding (product)

execute :: Executor
execute action = do
    conf <- reader (exchangeConf . config)
    let method =
            case action of
                Cancel orderId -> cancelOrder orderId
                _ -> void . createOrder . transformAction $ action
    execExchangeT conf method

transformAction :: Action -> NewOrder
transformAction action =
    let productId = toId . product $ action
        selfTrade = DecrementAndCancel
    in case action of
           Market {..} ->
               NewMarketOrder
               { noProductId = productId
               , noSide = side
               , noSelfTrade = selfTrade
               , noSizeAndOrFunds = transformAmount amount
               }
           Limit {..} ->
               NewLimitOrder
               { noProductId = productId
               , noSide = side
               , noSelfTrade = selfTrade
               , noPrice = limitPrice
               , noSize = size
               , noTimeInForce = GoodTillCanceled
               , noPostOnly = True
               }
           Stop {..} ->
               NewStopOrder
               { noProductId = productId
               , noSide = side
               , noSelfTrade = selfTrade
               , noPrice = stopPrice
               , noSizeAndOrFunds = transformAmount amount
               }
           _ -> error "Only new orders can be transformed"

transformAmount :: Amount -> Either Size (Maybe Size, CB.Cost)
transformAmount (Left size) = Left size
transformAmount (Right funds) = Right (Nothing, realToFrac funds)
