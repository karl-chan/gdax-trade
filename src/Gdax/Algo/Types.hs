{-# LANGUAGE DuplicateRecordFields #-}

module Gdax.Algo.Types where

import           Gdax.Algo.Action
import           Gdax.Types.Bundle
import           Gdax.Types.Product

import           Control.Monad.Reader

-- List of possible actions with corresponding predicted returns
type Strategy = Product -> ReaderT Bundle IO StrategyProposal

-- Optimise fresh plan into sequence of actions (taking into account of open orders)
type Optimiser = FreshPlan -> ReaderT Bundle IO [Action]

-- Calculates cost of executing action
type CostCalculator = [Action] -> ReaderT Bundle IO Cost

-- Execute a proposal on gdax exchange
type Executor = Proposal -> ReaderT Bundle IO ()

-- As if there are no open orders
type FreshPlan = NewAction

data StrategyProposal = StrategyProposal
  { name      :: String
  , freshPlan :: FreshPlan
  , profit    :: Profit
  } deriving (Show)

data Proposal = Proposal
  { description :: String
  , actions     :: [Action]
  , profit      :: Profit
  , cost        :: Cost
  , netProfit   :: Profit
  } deriving (Show)

type Profit = Double -- As percentage

type Cost = Double -- As percentage
