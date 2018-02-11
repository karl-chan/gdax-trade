{-# LANGUAGE RecordWildCards #-}

module Gdax.Types.Trades.Util where

import           Gdax.Types.Trades

import           Data.List         hiding (product)
import           Data.Ord
import           Data.Time.Clock
import           Prelude           hiding (product)

insert :: Trade -> Trades -> Trades
insert = insertBy (comparing tradeId)

range :: Trades -> (UTCTime, UTCTime)
range trades = (time . head $ trades, time . last $ trades)

dropBefore :: UTCTime -> Trades -> Trades
dropBefore cutoffTime = dropWhile (\Trade {..} -> time < cutoffTime)

showRange :: Trades -> String
showRange series =
  let (startTime, endTime) = range series
  in show startTime ++ " - " ++ show endTime
