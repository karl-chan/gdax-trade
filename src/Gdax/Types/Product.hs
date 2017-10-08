{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gdax.Types.Product where

import           Gdax.Types.Currency

import           Coinbase.Exchange.Types.Core (ProductId (..))

import           Control.DeepSeq              (NFData)
import           Data.Char
import           Data.Data                    (Data)
import           Data.Hashable
import           Data.List.Split
import           Data.String.Conversions
import           GHC.Generics
import           Prelude                      hiding (product)

allProducts :: [Product]
allProducts = [Pair BTC EUR, Pair ETH BTC, Pair ETH EUR, Pair LTC BTC, Pair LTC EUR]

data Product =
    Pair Currency
         Currency
    deriving (Eq, Ord, Generic, Hashable, Data, NFData)

instance Show Product where
    show (Pair c1 c2) = show c1 ++ "-" ++ show c2

instance Read Product where
    readsPrec _ str =
        let [c1, c2] = splitOn "-" $ map toUpper str
        in [(mkProduct (read c1) (read c2), "")]

mkProduct :: Currency -> Currency -> Product
mkProduct c1 c2 =
    let candidate = Pair c1 c2
    in if candidate `elem` allProducts
           then candidate
           else error $ show c1 ++ "-" ++ show c2 ++ " is not a valid product"

fromId :: ProductId -> Product
fromId = read . show

toId :: Product -> ProductId
toId product = ProductId $ cs . show $ product
