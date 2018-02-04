module Gdax.Types.Trades.Util where

import           Gdax.Types.Trades

import qualified Data.Map.Strict   as Map
import           Data.Time.Clock

insert :: Trade -> Trades -> Trades
insert trade trades = Map.insert (time trade) trade trades

range :: Trades -> (UTCTime, UTCTime)
range trades = (fst . Map.findMin $ trades, fst . Map.findMax $ trades)

listToTrades :: [Trade] -> Trades
listToTrades list = Map.fromList $ map (\trade -> (time trade, trade)) list

tradesToList :: Trades -> [Trade]
tradesToList = Map.elems

dropBefore :: UTCTime -> Trades -> Trades
dropBefore cutoffTime = Map.dropWhileAntitone (< cutoffTime)
