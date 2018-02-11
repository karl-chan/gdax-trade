{-# LANGUAGE RecordWildCards #-}

module Gdax.Types.TimeSeries.Util where

import           Gdax.Types.Product
import           Gdax.Types.TimeSeries        as TS
import           Gdax.Util.Time

import           Coinbase.Exchange.Types.Core (Price (..))

import           Data.List.Split
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Time.Clock
import           Prelude                      hiding (head, last, product)
import qualified Prelude                      as Prelude

null :: TimeSeries -> Bool
null = Map.null

insert :: Stat -> TimeSeries -> TimeSeries
insert stat series = Map.insert (start stat) stat series

concat :: [TimeSeries] -> TimeSeries
concat = Map.unions

between :: StartTime -> EndTime -> TimeSeries -> TimeSeries
between startTime endTime series =
  Map.takeWhileAntitone (<= endTime) . Map.dropWhileAntitone (< startTime) $
  series

lookback :: NominalDiffTime -> TimeSeries -> TimeSeries
lookback duration series =
  let (_, endTime) = range series
      startTime = addUTCTime (negate duration) endTime
  in between startTime endTime series

dropBefore :: UTCTime -> TimeSeries -> TimeSeries
dropBefore startTime = Map.dropWhileAntitone (< startTime)

product :: TimeSeries -> Product
product series = TS.product . snd $ Map.elemAt 0 series

head :: TimeSeries -> Stat
head = snd . Map.findMin

last :: TimeSeries -> Stat
last = snd . Map.findMax

open :: TimeSeries -> Price
open = TS.open . head

close :: TimeSeries -> Price
close = TS.close . last

find :: UTCTime -> TimeSeries -> Maybe Stat
find time series = snd <$> Map.lookupGE time series

range :: TimeSeries -> (StartTime, EndTime)
range series = ((start . head) series, (end . last) series)

-- Works only if new granularity is a multiple of original granularity
downscale :: TimeSeries -> Granularity -> TimeSeries
downscale series granularity =
  case Map.lookupMin series of
    Nothing -> series
    Just (_, Stat {..}) ->
      let interval = diffUTCTime end start
          multiplier = floor $ granularity / interval
          groupedStats = chunksOf multiplier $ Map.elems series
          newStats = mapMaybe concatStats groupedStats
      in statsToSeries newStats

statsToSeries :: [Stat] -> TimeSeries
statsToSeries stats = Map.fromList $ map (\s -> (start s, s)) stats

seriesToStats :: TimeSeries -> [Stat]
seriesToStats = Map.elems

concatStats :: [Stat] -> Maybe Stat
concatStats [] = Nothing
concatStats stats =
  Just
    Stat
    { start = minimum $ map start stats
    , end = maximum $ map end stats
    , low = minimum $ map low stats
    , high = maximum $ map high stats
    , open = TS.open $ Prelude.head stats
    , close = TS.close $ Prelude.last stats
    , volume = sum $ map volume stats
    , product = TS.product $ Prelude.head stats
    }

updateLastStat :: (Stat -> Stat) -> TimeSeries -> TimeSeries
updateLastStat f = Map.updateMax (Just . f)

showRange :: TimeSeries -> String
showRange series =
  let (startTime, endTime) = range series
  in show startTime ++ " - " ++ show endTime
