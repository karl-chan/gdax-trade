{-# LANGUAGE RecordWildCards #-}

module Gdax.Algo.Strategy.SupportResistance where

import           Gdax.Algo.Signal.Direction
import           Gdax.Algo.Strategy.Util
import           Gdax.Algo.Types
import           Gdax.Types.Bundle
import           Gdax.Types.OrderBook
import           Gdax.Types.OrderBook.Util
import           Gdax.Types.TimeSeries
import           Gdax.Types.TimeSeries.Util   as TS hiding (product)
import           Gdax.Util.Config
import           Gdax.Util.Time

import           Coinbase.Exchange.Types.Core

import           Control.Monad
import           Control.Monad.Reader
import           Data.List                    (find)
import           Data.Maybe                   (maybeToList)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Prelude                      hiding (product)

supportResistance :: Reader ProductBundle Proposal
supportResistance = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      OrderBookSummary {..} = getSummary book
  direction <- getDirection
  [maybeResistance, maybeSupport] <- mapM nextTurningPoint [Up, Down]
  maybeAction <-
    case (direction, maybeResistance, maybeSupport) of
      (Up, Nothing, _) -> Just <$> marketBuy -- Skyrocket
      (Up, Just resistance, Just support) -> do
        let d = resistance - support
            supportZone = support + d / 3
        case () of
          _
            | midPrice < supportZone -> Just <$> marketBuy
            | otherwise -> Just <$> limitSell resistance
      (Down, _, Nothing) -> Just <$> marketSell -- Plummet
      (Down, Just resistance, Just support) -> do
        let d = resistance - support
            resistanceZone = resistance - d / 3
            supportZone = support + d / 3
        case () of
          _
            | midPrice > resistanceZone -> Just <$> limitSell resistanceZone
            | midPrice < supportZone -> Just <$> limitSell supportZone
            | otherwise -> Just <$> marketSell
      _ -> return Nothing
  return $ Proposal $ maybeToList maybeAction

identifyTurningPoints :: TimeSeries -> Set Price
identifyTurningPoints series =
  let hourlySeries = downscale series hour
      hourlyStats = seriesToStats hourlySeries
      points = identifyTurningPointsFromStats hourlyStats
  in Set.fromList points

-- assume continuous, output points preserve input series temporal ordering
identifyTurningPointsFromStats :: [Stat] -> [Price]
identifyTurningPointsFromStats (x:y:ys) =
  let statDirection Stat {..} =
        case compare start end of
          LT -> Up
          GT -> Down
          EQ -> None
      maybeTurningPoint =
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
