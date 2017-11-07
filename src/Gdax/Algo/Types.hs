module Gdax.Algo.Types where

import           Gdax.Algo.Action
import           Gdax.Types.Bundle
import           Gdax.Types.Product

import           Control.Monad.Reader

-- List of possible actions with corresponding predicted returns
type Strategy = Product -> ReaderT Bundle IO StrategyProposal

-- Calculates cost of executing action
type CostCalculator = Action -> ReaderT Bundle IO Cost

-- Execute a proposal on gdax exchange
type Executor = Proposal -> ReaderT Bundle IO ()

data StrategyProposal = StrategyProposal
    { strategyName            :: String
    , strategyActions         :: [Action]
    , strategyEstimatedProfit :: Profit
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
