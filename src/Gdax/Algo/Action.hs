module Gdax.Algo.Action where

import           Gdax.Types.Amount
import           Gdax.Types.Product

import           Coinbase.Exchange.Types.Core (OrderId, Price, Side, Size)

data Action
    = Market { side    :: Side
             , product :: Product
             , amount  :: Amount }
    | Limit { side       :: Side
            , product    :: Product
            , limitPrice :: Price
            , size       :: Size }
    | Stop { side      :: Side
           , product   :: Product
           , stopPrice :: Price
           , amount    :: Amount }
    | Cancel OrderId
    deriving (Show)
