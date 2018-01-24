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
  (==) (Market side1 product1 amount1) (Market side2 product2 amount2) =
    side1 == side2 &&
    product1 == product2 && amount1 `roughlyEqualAmount` amount2
  (==) (Limit side1 product1 limitPrice1 size1) (Limit side2 product2 limitPrice2 size2) =
    side1 == side2 &&
    product1 == product2 &&
    limitPrice1 `roughlyEqual` limitPrice2 && size1 `roughlyEqual` size2
  (==) (Stop side1 product1 stopPrice1 amount1) (Stop side2 product2 stopPrice2 amount2) =
    side1 == side2 &&
    product1 == product2 &&
    stopPrice1 `roughlyEqual` stopPrice2 && amount1 `roughlyEqualAmount` amount2
  (==) _ _ = False
