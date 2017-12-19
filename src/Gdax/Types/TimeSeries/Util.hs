{-# LANGUAGE RecordWildCards #-}

module Gdax.Types.TimeSeries.Util where

import           Gdax.Types.Product
import           Gdax.Types.TimeSeries        as TS

import           Coinbase.Exchange.Types.Core (Price (..), Size (..))

import           Data.List.Split
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Ord                     (Ordering (EQ, GT, LT), compare)
import           Data.Time.Clock
import           Prelude                      hiding (head, last, product)
import qualified Prelude                      as Prelude

insert :: TimeSeries -> Stat -> TimeSeries
insert series stat = Map.insert (start stat) stat series

concat :: [TimeSeries] -> TimeSeries
concat = Map.unions

between :: TimeSeries -> StartTime -> EndTime -> TimeSeries
between series startTime endTime =
  Map.takeWhileAntitone (<= endTime) . Map.dropWhileAntitone (< startTime) $
  series

lookback :: TimeSeries -> NominalDiffTime -> TimeSeries
lookback series duration =
  let (_, endTime) = range series
      startTime = addUTCTime (negate duration) endTime
  in between series startTime endTime

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
          newStats = mapMaybe combineStats groupedStats
      in statsToSeries newStats

statsToSeries :: [Stat] -> TimeSeries
statsToSeries stats = Map.fromList $ map (\s -> (start s, s)) stats

seriesToStats :: TimeSeries -> [Stat]
seriesToStats = Map.elems

statDirection :: Stat -> Direction
statDirection Stat {..} =
  case compare start end of
    LT -> Up
    GT -> Down
    EQ -> None

combineStats :: [Stat] -> Maybe Stat
combineStats [] = Nothing
combineStats stats =
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
