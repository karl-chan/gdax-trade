module Gdax.Data.TimeSeries.Util where

import           Coinbase.Exchange.Types.Core (Price (..), Size (..))

import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Gdax.Data.TimeSeries.Types
import           Prelude                      hiding (max, min)

insert :: TimeSeries -> Stat -> TimeSeries
insert series stat = Map.insert (start stat) stat series

concat :: [TimeSeries] -> TimeSeries
concat = Map.unions

between :: TimeSeries -> StartTime -> EndTime -> TimeSeries
between series startTime endTime = Map.takeWhileAntitone (<= endTime) . Map.dropWhileAntitone (< startTime) $ series

min :: TimeSeries -> Stat
min = snd . Map.findMin

max :: TimeSeries -> Stat
max = snd . Map.findMax

range :: TimeSeries -> (StartTime, EndTime)
range series = ((start . min) series, (end . max) series)
