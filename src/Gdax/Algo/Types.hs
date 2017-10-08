module Gdax.Algo.Types where

import           Gdax.Account.MyAccount
import           Gdax.Algo.Action
import           Gdax.Types.Product
import           Gdax.Util.Bundle

import           Coinbase.Exchange.Types.Core (OrderId, Price, Side, Size)

import           Control.Monad.Reader

-- List of possible actions with corresponding predicted returns
type Strategy = Product -> ReaderT Bundle IO Proposal

-- Calculates cost of executing action
type CostCalculator = Action -> ReaderT Bundle IO Cost

-- Execute each action on gdax exchange
type Executor = Action -> ReaderT Bundle IO ()

type Proposal = ([Action], Return)

type Return = Double -- As percentage

type Cost = Double -- As percentage
