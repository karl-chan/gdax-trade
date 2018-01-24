{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Gdax.Algo.Signal.Direction where

import           Gdax.Types.Bundle
import qualified Gdax.Types.TimeSeries.Util as TS
import           Gdax.Util.Config
import           Gdax.Util.Time

import           Control.DeepSeq            (NFData)
import           Control.Monad.Reader
import           Data.Data                  (Data)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Prelude                    hiding (product)

data Direction
  = Up
  | Down
  | None
  deriving (Eq, Show, Read, Data, Typeable, Generic, NFData)

-- exceeds 1% over the last hour
getDirection :: Reader ProductBundle Direction
getDirection = do
  ProductBundle {..} <- ask
  let StrategyConf {..} = strategyConfig
      lastHourSeries = TS.lookback series hour
      openPrice = TS.open lastHourSeries
      closePrice = TS.close lastHourSeries
      direction =
        case () of
          _
            | closePrice > openPrice * (1 + realToFrac tolerance) -> Up
            | closePrice < openPrice * (1 - realToFrac tolerance) -> Down
            | otherwise -> None
  return direction
