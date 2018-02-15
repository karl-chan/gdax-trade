{-# LANGUAGE RecordWildCards #-}

module Gdax.Test.Integration.Feed.Account where

import           Gdax.Test.Data

import           Gdax.Account.MyAccount
import           Gdax.Feed.Account
import           Gdax.Types.Currency
import           Gdax.Util.Feed

import           Control.Monad.Reader
import qualified Data.HashMap.Strict    as HM
import           Data.List              hiding (product)
import           Prelude                hiding (product)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Account" $ test

test :: Assertion
test = do
  accountFeed <- runReaderT (newAccountFeed) testConfig
  accountFeedListener <- newFeedListener accountFeed
  initialAccount <- readFeed accountFeedListener
   -- Check that account contains all currenecies
  forM_ [(minBound :: Currency) ..] $ \currency -> do
    assertBool ("account should have currency: " ++ show currency) $
      HM.member currency $ balances initialAccount
