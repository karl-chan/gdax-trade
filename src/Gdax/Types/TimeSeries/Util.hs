module Gdax.Types.TimeSeries.Util where

import           Gdax.Types.Product
import           Gdax.Types.TimeSeries

import qualified Data.Map              as Map
import           Data.Time.Clock
import           Prelude               hiding (max, min, product)

insert :: TimeSeries -> Stat -> TimeSeries
insert series stat = Map.insert (start stat) stat series

concat :: [TimeSeries] -> TimeSeries
concat = Map.unions

between :: TimeSeries -> StartTime -> EndTime -> TimeSeries
between series startTime endTime = Map.takeWhileAntitone (<= endTime) . Map.dropWhileAntitone (< startTime) $ series

lookback :: TimeSeries -> NominalDiffTime -> TimeSeries
lookback series duration =
    let (_, endTime) = range series
        startTime = addUTCTime (negate duration) endTime
    in between series startTime endTime

getProduct :: TimeSeries -> Product
getProduct series = product . snd $ Map.elemAt 0 series

min :: TimeSeries -> Stat
min = snd . Map.findMin

max :: TimeSeries -> Stat
max = snd . Map.findMax

range :: TimeSeries -> (StartTime, EndTime)
range series = ((start . min) series, (end . max) series)
