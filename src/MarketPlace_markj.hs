module MarketPlace_markj (psellerPkh, marketPlace) where

import ContractTypes (PMarketRedeemer, PSimpleSale (PSimpleSale), SimpleSale (priceOfAsset))
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

allScriptInputsCount :: Term s (PScriptContext :--> PInteger)
allScriptInputsCount = plam $ \ctx -> P.do
  ctxF <- pletFields @'["txInfo"] ctx
  infoF <- pletFields @'["inputs"] ctxF.txInfo

-- Spending Validator
marketPlace :: ClosedTerm PValidator
marketPlace = plam $ \datum' redeemer' ctx' -> P.do
  let redeemer = pconvert @PMarketRedeemer redeemer'
      datum = pconvert @PSimpleSale datum'
  datumF <- pletFields @["sellerAddress", "priceO-fAsset"] datum
  -- ctxF <- pletFields @'["txInfo"] ctx'
  pmatch $ psellerPkh # datumF.sellerAddress $ \case
    PNothing -> pTraceError "Invalid seller address"
    PJust pkh -> case redeemer of
      PBuy -> (plift $ allScriptInputsCount # ctx') == 1

  -- pattern match psellerPkh
  -- nothing -> error
  -- just pkh -> continue validation
  --            pattern match redeemer
  --                  pbuy -> continue validation
  --                  pwithdraw -> continue validation

  undefined
