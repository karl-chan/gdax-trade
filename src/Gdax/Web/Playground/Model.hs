module Gdax.Web.Playground.Model where

import           Gdax.Types.Product
import           Gdax.Web.Types

import           Data.ByteString.Lazy (ByteString)

data PlaygroundModel
    = None
    | Rest { method          :: SearchMethod
           , url             :: String
           , pagination      :: PaginationOptions
           , sandboxMode     :: Bool
           , responseBody    :: ByteString
           , timeTakenMillis :: Double }
    | Stream { products       :: [Product]
             , url            :: String
             , initialMessage :: ByteString
             , sandboxMode    :: Bool }
    | Error { method      :: SearchMethod
            , args        :: SearchArgs
            , msg         :: String
            , sandboxMode :: Bool }

null :: PlaygroundModel -> Bool
null None = True
null _    = False

isSandbox :: PlaygroundModel -> Bool
isSandbox None  = False
isSandbox model = sandboxMode model
