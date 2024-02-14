module MarketPlace (psellerPkh, marketPlace) where
import Plutarch.Api.V1 (PAddress, PPubKeyHash, PCredential (PPubKeyCredential, PScriptCredential), PValidator)
import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Conversions (pconvert)
import ContractTypes (PMarketRedeemer, PSimpleSale)

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
  let _redeemer = pconvert @PMarketRedeemer redeemer'
      datum = pconvert @PSimpleSale datum'
  _datumF <- pletFields @["sellerAddress", "priceO-fAsset"] datum
  _ctxF <- pletFields @'["txInfo"] ctx'
  -- pattern match psellerPkh
  -- nothing -> error
  -- just pkh -> continue validation
  --            pattern match redeemer
  --                  pbuy -> continue validation
  --                  pwithdraw -> continue validation
  popaque $ pconstant ()


