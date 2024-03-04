module MarketPlace_markj (addressToPkh, marketPlace) where

import ContractTypes (PMarketRedeemer (PBuy, PWithdraw), PSimpleSale)
import Conversions (pconvert)
import Plutarch.Api.V1 (PAddress, PCredential (PPubKeyCredential, PScriptCredential), PPubKeyHash)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Api.V2 (PTxInfo, PTxOut, PScriptContext)
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.Api.V2 (PTxInInfo, PValidator)
import Plutarch.Extra.AssetClass (passetClassData)
import Plutarch.Extra.Value (passetClassDataValueOf)
import "liqwid-plutarch-extra" Plutarch.Extra.List (pmapMaybe)

addressToPkh :: Term s (PAddress :--> PMaybe PPubKeyHash)
addressToPkh = phoistAcyclic $ plam $ \add -> pmatch (pfield @"credential" # add) $ \case
  PPubKeyCredential ((pfield @"_0" #) -> pkh) -> pcon $ PJust pkh
  PScriptCredential _ -> pcon PNothing

addressToScriptCred :: Term s (PAddress :--> PMaybe PScriptHash)
addressToScriptCred = phoistAcyclic $ plam $ \add -> pmatch (pfield @"credential" # add) $ \case
  PPubKeyCredential _ -> pcon PNothing
  PScriptCredential ((pfield @"_0" #) -> sc) -> pcon $ PJust sc

countTxOut :: Term s (PInteger :--> PTxInInfo :--> PInteger)
countTxOut = phoistAcyclic $ plam $ \x txInInfo -> P.do
  inInfo' <- pletFields @'["resolved"] txInInfo
  resolved' <- pletFields @'["address"] inInfo'.resolved
  let scriptCred' = addressToScriptCred # resolved'.address
  pmatch scriptCred' $ \case
    PNothing -> x
    PJust _ -> x + 1

pallScriptInputsCount :: Term s (PScriptContext :--> PInteger)
pallScriptInputsCount = phoistAcyclic $ plam $ \ctx -> P.do
  ctx' <- pletFields @'["txInfo"] ctx
  info' <- pletFields @'["inputs"] ctx'.txInfo
  pfoldl # countTxOut # pconstant 0 # info'.inputs

ppubKeyOutputsAt :: Term s (PPubKeyHash :--> PTxInfo :--> PBuiltinList _)
ppubKeyOutputsAt = phoistAcyclic $ plam $ \userPKH txinfo -> P.do
  let outputs = pfield @"outputs" # txinfo
  let funct :: Term _ (PTxOut :--> _)
      funct = plam $ \output -> P.do
        outputF <- pletFields @["address", "value"] output
        PPubKeyCredential ((pfield @"_0" #) -> pkh) <- pmatch (pfield @"credential" # outputF.address)
        pif
          (userPKH #== pkh)
          (pcon $ PJust $ outputF.value)
          (pcon $ PNothing)
  pmapMaybe # funct # outputs

mkAdaValue :: Term s (PInteger :--> PValue 'Sorted 'NonZero)
mkAdaValue = plam $ \amount -> Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # amount

pvaluePaidTo :: Term s (PPubKeyHash :--> (PTxInfo :--> PValue 'Sorted 'NonZero))
pvaluePaidTo = plam $ \pkh txinfo ->
  pfoldl
    # (plam $ \acc cur -> acc <> (pforgetPositive $ pfromData cur))
    # (mkAdaValue # 0) -- Value, only ADA
    # (ppubKeyOutputsAt # pkh # txinfo) -- [PValue]

ptxSignedByPkh :: Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

-- Spending Validator
marketPlace :: ClosedTerm PValidator
marketPlace = plam $ \dat red ctx -> P.do
  let redeemer = pconvert @PMarketRedeemer red
      datum = pconvert @PSimpleSale dat
  datum' <- pletFields @'["sellerAddress", "priceOfAsset"] datum
  ctx' <- pletFields @'["txInfo"] ctx
  let sellerPkh = addressToPkh # datum'.sellerAddress
      adaAsset = passetClassData # padaSymbol # padaToken
  pmatch sellerPkh $ \case
    PNothing -> ptraceError "Script address in seller"
    PJust pkh -> pmatch redeemer $ \case
      PBuy _ -> P.do
        let valid =
              (ptraceIfFalse "Multiple script inputs" $ pallScriptInputsCount # ctx #== 1)
                #&& ( ptraceIfFalse "Seller not paid" $
                        datum'.priceOfAsset
                          #<= (passetClassDataValueOf # adaAsset # (pvaluePaidTo # pkh # ctx'.txInfo))
                    )
        popaque $ pif valid (pconstant ()) perror
      PWithdraw _ -> P.do
        let valid = ptraceIfFalse "Seller signature missing" $ ptxSignedByPkh # pdata pkh # (pfield @"signatories" # ctx'.txInfo)
        popaque $ pif valid (pconstant ()) perror