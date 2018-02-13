{-# LANGUAGE RecordWildCards #-}

module Gdax.Util.Throttle.Api where

import           Gdax.Util.Config
import           Gdax.Util.Logger
import           Gdax.Util.Throttle

import           Coinbase.Exchange.Types

import           Control.Monad.Reader

throttleApi :: [Exchange a] -> ReaderT Config IO [a]
throttleApi requests = do
  conf <- reader exchangeConf
  ThrottleConf {..} <- reader $ throttleConf . apiConf
  let tasks = map (execExchange conf) requests
  liftIO $ throttle tasks parallelism interval (Just retryGap)

throttlePaginatedApi ::
     (Pagination -> Exchange (a, Pagination))
  -> (a -> Bool)
  -> ReaderT Config IO [a]
throttlePaginatedApi request terminatingCondition = do
  conf <- reader exchangeConf
  ThrottleConf {..} <- reader $ throttleConf . apiConf
  let loop pagination acc = do
        (result, paginationResult) <-
          retry retryGap $ execExchange conf $ request pagination
        if terminatingCondition result
          then return (result : acc)
          else do
            sleep interval
            let nextPagination =
                  Pagination {before = Nothing, after = after paginationResult}
            logDebug $ "Next pagination: " ++ (show $ after nextPagination)
            loop nextPagination (result : acc)
  liftIO $ loop nullPagination []
