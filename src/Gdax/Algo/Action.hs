module Gdax.Algo.Action where

import           Gdax.Types.Amount
import           Gdax.Types.Product
import           Gdax.Util.Math

import           Coinbase.Exchange.Types.Core (OrderId, Price, Side, Size)

import           Prelude                      hiding (product)

data Action
  = NewAction NewAction
  | CancelAction CancelAction
  deriving (Eq, Show)

data NewAction
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
  deriving (Show)

data CancelAction
  = Cancel OrderId
  | CancelProduct Product
  | CancelAll
  deriving (Eq, Show)

-- Allow tolerance - compare price / size only up to certain decimal place
instance Eq NewAction where
  (==) (Market s1 p1 a1) (Market s2 p2 a2) =
    s1 == s2 && p1 == p2 && a1 `roughlyEqualAmount` a2
  (==) (Limit s1 p1 lp1 sz1) (Limit s2 p2 lp2 sz2) =
    s1 == s2 && p1 == p2 && lp1 `roughlyEqual` lp2 && sz1 `roughlyEqual` sz2
  (==) (Stop s1 p1 sp1 a1) (Stop s2 p2 sp2 a2) =
    s1 == s2 && p1 == p2 && sp1 `roughlyEqual` sp2 && a1 `roughlyEqualAmount` a2
  (==) _ _ = False
