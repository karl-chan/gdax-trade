{-# LANGUAGE RecordWildCards #-}

module Gdax.Feed.MyAccount.Internal.Balance where

import           Gdax.Account.Balance
import           Gdax.Account.MyAccount
import           Gdax.Algo.Action
import           Gdax.Types.Currency            hiding (fromId)
import           Gdax.Types.Product
import           Gdax.Util.Config
import           Gdax.Util.Config.Fees

import           Coinbase.Exchange.Socket
import           Coinbase.Exchange.Types.Core   hiding (Done, Limit, Open)
import           Coinbase.Exchange.Types.Socket

import           Control.Monad.Reader
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe

updateBalances ::
     ExchangeMessage -> FeesConf -> Reader MyAccount (HashMap Currency Balance)
updateBalances msg feesConf = do
  account@MyAccount {..} <- ask
  let product = fromId . msgProductId $ msg
      Pair c1 c2 = product
      (deltaBalance1, deltaBalance2) =
        case msg of
          Open {..} ->
            case msgSide of
              Buy ->
                let holdAmount =
                      realToFrac msgPrice * realToFrac msgRemainingSize
                in ( zero
                   , DeltaBalance
                     { deltaTotal = 0
                     , deltaAvailable = -holdAmount
                     , deltaHeld = holdAmount
                     })
              Sell ->
                let holdAmount = realToFrac msgRemainingSize
                in ( DeltaBalance
                     { deltaTotal = 0
                     , deltaAvailable = -holdAmount
                     , deltaHeld = holdAmount
                     }
                   , zero)
          Match {..} -> do
            let isMaker = isNothing msgTakerUserId
                isTaker = not isMaker
                percentFee =
                  if isMaker
                    then makerFee product feesConf
                    else takerFee product feesConf
            if msgSide == Buy && isMaker || msgSide == Sell && isTaker
              then let creditAmount = realToFrac msgSize
                       debitAmount =
                         realToFrac msgPrice * realToFrac msgSize *
                         (1 + percentFee)
                       releaseAmount = realToFrac msgPrice * realToFrac msgSize
                   in ( DeltaBalance
                        { deltaTotal = creditAmount
                        , deltaAvailable = creditAmount
                        , deltaHeld = 0
                        }
                      , DeltaBalance
                        { deltaTotal = -debitAmount
                        , deltaAvailable = -debitAmount
                        , deltaHeld = releaseAmount
                        })
              else let creditAmount = realToFrac msgPrice * realToFrac msgSize
                       debitAmount = realToFrac msgSize * (1 + percentFee)
                       releaseAmount = realToFrac msgSize
                   in ( DeltaBalance
                        { deltaTotal = -debitAmount
                        , deltaAvailable = -debitAmount
                        , deltaHeld = releaseAmount
                        }
                      , DeltaBalance
                        { deltaTotal = creditAmount
                        , deltaAvailable = creditAmount
                        , deltaHeld = 0
                        })
          ChangeLimit {..} ->
            case msgSide of
              Buy ->
                let holdAmount =
                      realToFrac msgPrice * realToFrac (msgNewSize - msgOldSize)
                in ( zero
                   , DeltaBalance
                     { deltaTotal = 0
                     , deltaAvailable = -holdAmount
                     , deltaHeld = holdAmount
                     })
              Sell ->
                let holdAmount = realToFrac (msgNewSize - msgOldSize)
                in ( DeltaBalance
                     { deltaTotal = 0
                     , deltaAvailable = -holdAmount
                     , deltaHeld = holdAmount
                     }
                   , zero)
--                Done {..} ->
--                    case msgMaybePriceSize of
--                        Nothing -> (zero, zero) -- market order
--                        Just (msgPrice, msgRemainingSize) -- limit order
--                         ->
--                            case findOrder account msgOrderId of
--                                Nothing -> error $ "Order: " ++ show msgOrderId ++ " not found in MyAccount!"
--                                Just order -> do
--                                    let Limit {..} = action order
--                                    case msgSide of
--                                        Buy ->
--                                            let releaseAmount =
--                                                    realToFrac limitPrice * realToFrac size -
--                                                    realToFrac msgPrice * realToFrac msgRemainingSize
--                                            in ( zero
--                                               , DeltaBalance
--                                                 { deltaTotal = 0
--                                                 , deltaAvailable = releaseAmount
--                                                 , deltaHeld = -releaseAmount
--                                                 })
--                                        Sell ->
--                                            let releaseAmount = realToFrac (size - msgRemainingSize)
--                                            in ( DeltaBalance
--                                                 { deltaTotal = 0
--                                                 , deltaAvailable = releaseAmount
--                                                 , deltaHeld = -releaseAmount
--                                                 }
--                                               , zero)
          _ -> (zero, zero)
      balances' = HM.adjust (add deltaBalance1) c1 balances
      newBalances = HM.adjust (add deltaBalance2) c2 balances'
  return newBalances
