{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Gdax.Types.Product where

import           Gdax.Types.Currency

import           Coinbase.Exchange.Types.Core (ProductId (..))

import           Data.Char
import           Data.Hashable
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String.Conversions
import           GHC.Generics

data Product =
    Pair Currency
         Currency
    deriving (Eq, Ord, Generic, Hashable)

instance Show Product where
    show (Pair c1 c2) = show c1 ++ "-" ++ show c2

instance Read Product where
    readsPrec _ str =
        let [c1, c2] = splitOn "-" str
            product = Pair (read $ map toUpper c1) (read $ map toUpper c2)
        in [(product, "")]

fromId :: ProductId -> Product
fromId = read . show

toId :: Product -> ProductId
toId product = ProductId $ cs . show $ product
