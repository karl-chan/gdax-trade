module Gdax.Web.Types.RestMethod where

import           Network.HTTP.Types.Method

allRestMethods :: [Method]
allRestMethods = [methodGet, methodPost, methodDelete]
