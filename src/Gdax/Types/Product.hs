{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Gdax.Types.Product where

import           Gdax.Types.Currency          hiding (safeRead)

import           Coinbase.Exchange.Types.Core (ProductId (..))

import           Control.DeepSeq              (NFData)
import           Data.Aeson
import           Data.Char
import           Data.Data                    (Data)
import           Data.Hashable
import           Data.List.Split
import           Data.Monoid                  ((<>))
import           Data.String.Conversions
import           GHC.Generics
import           Prelude                      hiding (product)
import           Text.Read                    (readMaybe)

allProducts :: [Product]
allProducts =
  [ Pair BTC USD
  , Pair BTC GBP
  , Pair BTC EUR
  , Pair BCH USD
  , Pair BCH BTC
  , Pair ETH USD
  , Pair ETH BTC
  , Pair ETH EUR
  , Pair LTC USD
  , Pair LTC BTC
  , Pair LTC EUR
  ]

data Product =
  Pair Currency
       Currency
  deriving (Eq, Ord, Generic, Hashable, Data, NFData)

instance ToJSON Product where
  toJSON (Pair c1 c2) = String $ (cs . show $ c1) <> "-" <> (cs . show $ c2)

instance Show Product where
  show (Pair c1 c2) = show c1 ++ "-" ++ show c2

instance Read Product where
  readsPrec _ str =
    case splitOn "-" $ map toUpper str of
      [s1, s2] ->
        case readMaybe s1 of
          Nothing -> []
          Just c1 ->
            case readMaybe s2 of
              Nothing -> []
              Just c2 ->
                case mkProduct c1 c2 of
                  Left _        -> []
                  Right product -> [(product, "")]
      _ -> []

mkProduct :: Currency -> Currency -> Either String Product
mkProduct c1 c2 =
  let candidate = Pair c1 c2
  in if candidate `elem` allProducts
       then Right candidate
       else Left $ show c1 ++ "-" ++ show c2 ++ " is not a valid product"

fromId :: ProductId -> Product
fromId ProductId {..} =
  case safeRead . cs $ unProductId of
    Left err      -> error err
    Right product -> product

toId :: Product -> ProductId
toId product = ProductId $ cs . show $ product

safeRead :: String -> Either String Product
safeRead arg =
  case readMaybe arg of
    Nothing      -> Left $ arg ++ " could not be parsed as a product."
    Just product -> Right product

safeReads :: String -> Either String [Product]
safeReads commaSeparatedArgs =
  let args = splitOn "," commaSeparatedArgs
      readProducts' products [] = Right $ reverse products
      readProducts' acc (arg:rest) =
        case safeRead arg of
          Left err      -> Left err
          Right product -> readProducts' (product : acc) rest
  in readProducts' [] args
