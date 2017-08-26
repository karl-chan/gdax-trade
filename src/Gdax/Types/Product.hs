module Gdax.Types.Product where

import           Gdax.Types.Currency

import           Coinbase.Exchange.Types.Core (ProductId (..))

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String.Conversions

data Product =
    Pair Currency
         Currency
    deriving (Eq)

instance Show Product where
    show (Pair c1 c2) = show c1 ++ "-" ++ show c2

availableProducts :: [Product]
availableProducts =
    [ Pair BTC USD
    , Pair BTC GBP
    , Pair BTC EUR
    , Pair ETH USD
    , Pair ETH BTC
    , Pair ETH EUR
    , Pair LTC USD
    , Pair LTC BTC
    , Pair LTC EUR
    ]

mkProduct :: Currency -> Currency -> Maybe Product
mkProduct c1 c2 =
    let match (Pair x y) = c1 == x && c2 == y
    in find match availableProducts

fromId :: ProductId -> Product
fromId productId =
    let productIdStr = (cs . unProductId) productId
        [c1, c2] = splitOn "-" productIdStr
    in fromJust $ mkProduct (read c1) (read c2)

toId :: Product -> ProductId
toId product = ProductId $ cs . show $ product
