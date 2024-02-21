{-# LANGUAGE TemplateHaskell #-}

module ContractTypes_matthias (SimpleSale (..), PSimpleSale (..), MarketRedeemer (..), PMarketRedeemer (..)) where

import Plutarch.Api.V2 (PAddress)
import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData), PDataFields)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Address)
import PlutusTx qualified

data SimpleSale = SimpleSale
  { sellerAddress :: Address -- The main seller Note that we are using address
  , priceOfAsset :: Integer -- cost of the value in it
  }
  deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''SimpleSale [('SimpleSale, 0)]

-- Constr 0 [Constr 0 [Constr 0 [B "\255\255"],Constr 1 []],I 10]
data PSimpleSale (s :: S)
  = PSimpleSale
      ( Term
          s
          ( PDataRecord
              '[ "sellerAddress" ':= PAddress
               , "priceOfAsset" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PSimpleSale where type DPTStrat _ = PlutusTypeData -- Data Encoding

instance PUnsafeLiftDecl PSimpleSale where type PLifted PSimpleSale = SimpleSale -- plift Plutarch -> PlutusTx

deriving via (DerivePConstantViaData SimpleSale PSimpleSale) instance (PConstantDecl SimpleSale) -- pconstant PlutusTx -> Plutarch

-- PlutusTx
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

-- lift
instance PUnsafeLiftDecl PMarketRedeemer where type PLifted PMarketRedeemer = MarketRedeemer

-- pconstant
deriving via (DerivePConstantViaData MarketRedeemer PMarketRedeemer) instance (PConstantDecl MarketRedeemer)
