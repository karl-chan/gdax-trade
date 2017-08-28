module Gdax.Account.Balance where

import           Coinbase.Exchange.Types.Core (Size)
import           Gdax.Types.Currency

data Balance = Balance
    { currency  :: Currency
    , total     :: Size
    , available :: Size
    , held      :: Size
    }
