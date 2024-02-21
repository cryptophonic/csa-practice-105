module MarketPlace_matthias (psellerPkh, marketPlace) where

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

-- Spending Validator
marketPlace :: ClosedTerm PValidator
marketPlace = plam $ \datum' redeemer' ctx' -> P.do
  let redeemer = pconvert @PMarketRedeemer redeemer'
      datum = pconvert @PSimpleSale datum'
  datumF <- pletFields @["sellerAddress", "priceO-fAsset"] datum
  ctxF <- pletFields @'["txInfo"] ctx'
  -- pattern match psellerPkh

  -- nothing -> error
  -- just pkh -> continue validation
  --            pattern match redeemer
  --                  pbuy -> continue validation
  --                  pwithdraw -> continue validation

  undefined
