{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Optimiser where

import           Gdax.Account.MyAccount
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Math

import           Control.Monad.Reader
import           Data.List              hiding (product)
import           Prelude                hiding (product)

optimise :: Optimiser
optimise freshPlan = do
  Bundle {..} <- ask
  let p = product freshPlan
      openOrders = findOrdersForProduct account p
      alreadySatisfied =
        length openOrders == 1 && action (head openOrders) == freshPlan
      cancelOrders = map (Cancel . orderId) openOrders
      actions =
        if alreadySatisfied
          then []
          else cancelOrders ++ [freshPlan]
  mapM roundAction actions

-- Round action to acceptable decimal places to prevent rejection by GDAX
roundAction :: Action -> ReaderT Bundle IO Action
roundAction action = do
  dp <- reader $ apiDecimalPlaces . config
  let roundedAction =
        case action of
          Market {..} -> action {amount = roundAmount dp amount}
          Limit {..} ->
            action
            {limitPrice = roundPrice dp limitPrice, size = roundSize dp size}
          Stop {..} ->
            action
            { stopPrice = roundPrice dp stopPrice
            , amount = roundAmount dp amount
            }
          Cancel {} -> action
  logDebug $ "Rounded action to: " ++ show roundedAction
  return roundedAction
