module Gdax.Algo.Strategy.TurningPoints
where

import Gdax.Data.TimeSeries.Types
import Gdax.Data.OrderBook.Types



import Coinbase.Exchange.Types.Private


turningPoints :: TimeSeries -> OrderBook -> MyAccount -> Decision
turningPoints series book account =


