{-# LANGUAGE DuplicateRecordFields #-}

module Gdax.Algo.Types where

import           Gdax.Algo.Action
import           Gdax.Types.Bundle
import           Gdax.Types.Product

import           Control.Monad.Reader

-- List of possible actions with corresponding predicted returns
type Strategy = Product -> ReaderT Bundle IO Proposal

-- Optimise proposal sequence of actions (taking into account of open orders and skipping unnecessary actions)
type Optimiser = Proposal -> ReaderT Bundle IO Proposal

-- Calculates cost of executing action
type CostCalculator = [Action] -> ReaderT Bundle IO Cost

-- Execute a proposal on gdax exchange
type Executor = Proposal -> ReaderT Bundle IO ()

data Proposal = Proposal
  { actions :: [Action]
  -- , netProfit :: Double -- As percentage
  } deriving (Show)

type Profit = Double -- As percentage

type Cost = Double -- As percentage
