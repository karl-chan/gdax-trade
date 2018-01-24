{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Gdax.Algo.Optimiser where

import           Gdax.Account.MyAccount
import           Gdax.Account.MyOrder
import           Gdax.Algo.Action
import           Gdax.Algo.Types
import           Gdax.Types.Amount
import           Gdax.Types.Bundle
import           Gdax.Util.Config
import           Gdax.Util.Logger

import           Coinbase.Exchange.Types.Core (CoinScientific (..), Price (..),
                                               Size (..))

import           Control.Monad.Reader
import           Data.List                    hiding (product)
import           Data.Scientific
import           Prelude                      hiding (product)

optimise :: Optimiser
optimise proposal = do
  Bundle {..} <- ask
  skip <- alreadySatisfied proposal
  revisedActions <-
    if skip
      then return []
      else do
        roundedActions <- mapM roundAction $ actions proposal
        return $ [CancelAction CancelAll] ++ roundedActions
  return proposal {actions = revisedActions}

-- Skip update if fresh plan is identical to unique open order
alreadySatisfied :: Proposal -> ReaderT Bundle IO Bool
alreadySatisfied proposal = do
  Bundle {..} <- ask
  case actions proposal of
    [NewAction newAction] -> do
      let openOrders = findAllOrders account
      case openOrders of
        [MyOrder {action = NewAction existingNewAction}] ->
          return $ existingNewAction == newAction
        _ -> return False
    _ -> return False

roundAction :: Action -> ReaderT Bundle IO Action
roundAction action = do
  case action of
    CancelAction {}     -> return action
    NewAction newAction -> NewAction <$> roundNewAction newAction

-- Round action to acceptable decimal places to prevent rejection by GDAX
roundNewAction :: NewAction -> ReaderT Bundle IO NewAction
roundNewAction newAction = do
  dp <- reader $ decimalPlaces . apiConf . config
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

roundPrice :: Int -> Price -> Price
roundPrice dp Price {..} = Price $ roundCoin dp unPrice

roundSize :: Int -> Size -> Size
roundSize dp Size {..} = Size $ roundCoin dp unSize

roundAmount :: Int -> Amount -> Amount
roundAmount dp amount =
  case amount of
    AmountPrice price -> AmountPrice $ roundPrice dp price
    AmountSize size   -> AmountSize $ roundSize dp size

-- For rounding down (floor) of scientific types
roundCoin :: Int -> CoinScientific -> CoinScientific
roundCoin dp CoinScientific {..} =
  CoinScientific $
  normalize $ scientific (floor $ (10 ^ dp) * unCoinScientific) (-dp)
