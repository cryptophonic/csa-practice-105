module ContractTypes_davidp (SimpleSale (..), PSimpleSale (..), MarketRedeemer (..)) where

import Plutarch.Api.V2
import Plutarch.Prelude

data SimpleSale (s :: S) 
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

instance DerivePlutusType PSimpleSale where type DPTStrat _ = PlutusTypeData

data MarketRedeemer (s :: S)
    = PMarketRedeemer
        ( Term s )