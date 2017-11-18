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
  skip <- alreadySatisfied freshPlan
  let actions =
        if skip
          then []
          else let cancelOpenOrders =
                     [CancelAction (CancelProduct $ product freshPlan)]
               in cancelOpenOrders ++ [NewAction freshPlan]
  mapM roundAction actions

-- Skip update if fresh plan is identical to unique open order
alreadySatisfied :: FreshPlan -> ReaderT Bundle IO Bool
alreadySatisfied freshPlan = do
  Bundle {..} <- ask
  let openOrders = findAllOrders account
  case openOrders of
    [MyOrder {action = NewAction newAction}] -> return $ newAction == freshPlan
    _ -> return False

roundAction :: Action -> ReaderT Bundle IO Action
roundAction action = do
  case action of
    CancelAction {}     -> return action
    NewAction newAction -> NewAction <$> roundNewAction newAction

-- Round action to acceptable decimal places to prevent rejection by GDAX
roundNewAction :: NewAction -> ReaderT Bundle IO NewAction
roundNewAction newAction = do
  dp <- reader $ apiDecimalPlaces . config
  let roundedNewAction =
        case newAction of
          Market {..} -> newAction {amount = roundAmount dp amount}
          Limit {..} ->
            newAction
            {limitPrice = roundPrice dp limitPrice, size = roundSize dp size}
          Stop {..} ->
            newAction
            { stopPrice = roundPrice dp stopPrice
            , amount = roundAmount dp amount
            }
  logDebug $ "Rounded action to: " ++ show roundedNewAction
  return roundedNewAction
