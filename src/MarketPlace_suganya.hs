module MarketPlace_suganya (psellerPkh, marketPlace) where

import ContractTypes_suganya (PMarketRedeemer (..), PSimpleSale (PSimpleSale), SimpleSale (priceOfAsset))
import Conversions (pconvert)
import Plutarch.Api.V1 (PAddress, PCredential (PPubKeyCredential, PScriptCredential), PPubKeyHash, PValidator)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

-- sellerPkh= case sellerAddress of { Address cre m_sc -> case cre of
--                                                            PubKeyCredential pkh -> Just pkh
--                                                            ScriptCredential vh -> Nothing  }

psellerPkh :: Term s (PAddress :--> PMaybe (PPubKeyHash))
psellerPkh = phoistAcyclic $ plam $ \add -> pmatch (pfield @"credential" # add) $ \case
  PPubKeyCredential ((pfield @"_0" #) -> pkh) -> pcon $ PJust pkh
  PScriptCredential _ -> pcon PNothing

-- Spending Validator
marketPlace :: ClosedTerm PValidator
marketPlace = plam $ \datum' redeemer' ctx' -> P.do
  let redeemer = pconvert @PMarketRedeemer redeemer'
      datum = pconvert @PSimpleSale datum'
  datumF <- pletFields @["sellerAddress", "priceO-fAsset"] datum
  ctxF <- pletFields @'["txInfo"] ctx'
  pmatch (psellerPkh # datumF.sellerAddress) $ \case
    PNothing -> perror
    PJust _ -> pmatch redeemer $ \case
      PBuy _ -> undefined
      PWithdraw _ -> undefined

{-
{-# INLINABLE allScriptInputsCount #-}
allScriptInputsCount:: ScriptContext ->Integer
allScriptInputsCount ctx@(ScriptContext info purpose)=
    foldl (\c txOutTx-> c + countTxOut txOutTx) 0 (txInfoInputs  info)
  where
  countTxOut (TxInInfo _ (TxOut addr _ _ _)) = case addr of { Address cre m_sc -> case cre of
                                                              PubKeyCredential pkh -> 0
                                                              ScriptCredential vh -> 1  }

-}

{-
  pmatch psellerPkh $ \case
    PNothing -> perror
    PJust _ -> pmatch redeemer $\case
                PBuy -> undefined
                PWithdraw -> undefined

  -- pattern match psellerPkh
  -- nothing -> error
  -- just pkh -> continue validation
  --            pattern match redeemer
  --                  pbuy -> continue validation
  --                  pwithdraw -> continue validation

  undefined

-}
