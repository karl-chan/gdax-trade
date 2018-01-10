{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy.SupportResistance where

import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.TimeSeries
import           Gdax.Types.TimeSeries.Util  as TS hiding (product)
import           Gdax.Util.Config
import           Gdax.Util.Time

import           Coinbase.Exchange.Types.Core

import           Control.Monad.Reader
import           Data.List                    (find)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Prelude                      hiding (product)
import Control.Monad

supportResistance :: Reader ProductBundle Proposal
supportResistance = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      turningPoints = identifyTurningPoints series
      direction = statDirection $ TS.last series
  [maybeResistance, maybeSupport] <- mapM nextTurningPoint [Up, Down]
  let actions = 
                case (direction, maybeResistance, maybeSupport) of
                      (Up, Just resistance, Just support) ->
                            -- TODO  determine closer to resistance or support
                      (Up, Nothing, _) ->

                      (Down, Just resistance, Just support) ->
                        
                      (Down, _, Nothing) -> 
                          [NewAction $ Market {
                              side = Sell,
                              product = product,
                              amount = AmountSize $ realToFrac $ total balance1
                          }]
  return $ Proposal actions


identifyTurningPoints :: TimeSeries -> Set Price
identifyTurningPoints series =
  let hourlySeries = downscale series hour
      hourlyStats = seriesToStats hourlySeries
      points = identifyTurningPointsFromStats hourlyStats
  in Set.fromList points

-- assume continuous, output points preserve input series temporal ordering
identifyTurningPointsFromStats :: [Stat] -> [Price]
identifyTurningPointsFromStats (x:y:ys) =
  let maybeTurningPoint =
        case (statDirection x, statDirection y) of
          (Up, Down) -> Just $ high x
          (Down, Up) -> Just $ low x
          _          -> Nothing
      rest = identifyTurningPointsFromStats (y : ys)
  in case maybeTurningPoint of
       Nothing           -> rest
       Just turningPoint -> turningPoint : rest
identifyTurningPointsFromStats _ = []

-- Next target based on turning points
nextTurningPoint :: Direction -> Reader ProductBundle (Maybe Price)
nextTurningPoint direction = do
  ProductBundle {..} <- ask
  let OrderBookSummary {midPrice = currentPrice} = getSummary book
      StrategyConf {..} = strategyConfig
      turningPoints = identifyTurningPoints series
  return $
    case direction of
      Up ->
        find (\pt -> pt > currentPrice * realToFrac (1 + tolerance)) $
        Set.toAscList turningPoints
      Down ->
        find (\pt -> pt < currentPrice * realToFrac (1 - tolerance)) $
        Set.toDescList turningPoints
      None -> Nothing
