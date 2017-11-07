module Gdax.Util.Config.Fees where

import           Data.HashMap.Strict (HashMap, (!))
import           Gdax.Types.Product

type Fee = Double

type MakerFee = Fee

type TakerFee = Fee

type FeesConf = HashMap Product (MakerFee, TakerFee)

makerFee :: Product -> FeesConf -> MakerFee
makerFee product config = fst $ config ! product

takerFee :: Product -> FeesConf -> TakerFee
takerFee product config = snd $ config ! product
