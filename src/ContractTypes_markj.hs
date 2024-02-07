{-# LANGUAGE TemplateHaskell #-}

module ContractTypes_markj (SimpleSale (..), PSimpleSale (..), MarketRedeemer (..)) where

import Plutarch.Api.V2 (PAddress)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Address)
import PlutusTx qualified

data SimpleSale = SimpleSale
  { sellerAddress :: Address, -- The main seller Note that we are using address
    priceOfAsset :: Integer -- cost of the value in it
  }
  deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''SimpleSale [('SimpleSale, 0)]

-- Constr 0 [Constr 0 [Constr 0 [B "\255\255"],Constr 1 []],I 10]
-- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Typeclasses/PlutusType,%20PCon,%20and%20PMatch.md#implementing-plutustype-for-your-own-types-data-encoding
data PSimpleSale (s :: S)
  = PSimpleSale
      ( Term
          s
          ( PDataRecord
              '[ "sellerAddress" ':= PAddress,
                 "priceOfAsset" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

-- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Usage/Deriving%20for%20newtypes.md
instance DerivePlutusType PSimpleSale where type DPTStrat _ = PlutusTypeData -- Data Encoding

-- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift
instance PUnsafeLiftDecl PSimpleSale where type PLifted PSimpleSale = SimpleSale -- plift Plutarch -> PlutusTx
-- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Typeclasses/PConstant%20and%20PLift.md#implementing-pconstant--plift
deriving via (DerivePConstantViaData SimpleSale PSimpleSale) instance (PConstantDecl SimpleSale) -- pconstant PlutusTx -> Plutarch

--PlutusTx
data MarketRedeemer = Buy | Withdraw
  deriving stock (Generic, Show, Prelude.Eq)

PlutusTx.makeIsDataIndexed ''MarketRedeemer [('Buy, 0), ('Withdraw, 1)]

-- Plutarch representation

data PMarketRedeemer (s :: S)
  = PBuy (Term s (PDataRecord '[]))
  | PWithdraw (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMarketRedeemer where type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMarketRedeemer