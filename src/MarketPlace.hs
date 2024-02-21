module MarketPlace (psellerPkh, marketPlace, pallScriptInputsCount, ppubKeyOutputsAt) where

import ContractTypes (PMarketRedeemer (PBuy, PWithdraw), PSimpleSale)
import Conversions (pconvert)
import Plutarch.Api.V1 (PAddress, PCredential (PPubKeyCredential, PScriptCredential), PPubKeyHash)
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pforgetPositive, psingleton)
import Plutarch.Api.V2 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PScriptContext, PTxInfo, PTxOut, PValidator, PValue)
import Plutarch.Extra.AssetClass (passetClassData)
import Plutarch.Extra.Value (passetClassDataValueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import "liqwid-plutarch-extra" Plutarch.Extra.List (pmapMaybe)

pallScriptInputsCount :: Term s (PScriptContext :--> PInteger)
pallScriptInputsCount =
  phoistAcyclic $ plam $ \sc ->
    pfoldl # (plam $ \count input -> count + pcountTxOut # input) # 0 # (pfromData $ pfield @"inputs" # (pfield @"txInfo" # sc))
  where
    pcountTxOut = plam $ \x -> pmatch (pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))) $ \case
      PScriptCredential _ -> 1
      _ -> 0

ppubKeyOutputsAt :: forall (s :: S). Term s (PPubKeyHash :--> PTxInfo :--> PBuiltinList _)
ppubKeyOutputsAt = phoistAcyclic $ plam $ \userPKH txinfo -> P.do
  let outputs = pfield @"outputs" # txinfo
  let funct :: Term _ (PTxOut :--> _)
      funct = plam $ \output -> P.do
        outputF <- pletFields @["address", "value"] output
        PPubKeyCredential ((pfield @"_0" #) -> pkh) <- pmatch (pfield @"credential" # outputF.address)
        pif
          (userPKH #== pkh) -- predicate
          (pcon $ PJust $ outputF.value)
          -- then
          (pcon $ PNothing) -- else
  pmapMaybe # funct # outputs

ptxSignedByPkh :: Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

mkAdaValue :: forall {s :: S}. Term s (PInteger :--> PValue 'Sorted 'NonZero)
mkAdaValue = plam $ \amount -> Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # amount

pvaluePaidTo :: forall {s :: S}. Term s (PPubKeyHash :--> (PTxInfo :--> PValue 'Sorted 'NonZero))
pvaluePaidTo = plam $ \pkh txinfo ->
  pfoldl
    # (plam $ \acc curr -> acc <> (pforgetPositive $ pfromData curr))
    # (mkAdaValue # 0) -- Value , only ADA
    # (ppubKeyOutputsAt # pkh # txinfo) -- [PValue]

psellerPkh :: Term s (PAsData PAddress :--> PMaybe (PAsData PPubKeyHash))
psellerPkh = phoistAcyclic $ plam $ \add -> pmatch (pfield @"credential" # add) $ \case
  PPubKeyCredential ((pfield @"_0" #) -> pkh) -> pcon $ PJust pkh
  PScriptCredential _ -> pcon PNothing

-- Spending Validator
marketPlace :: ClosedTerm PValidator
marketPlace = plam $ \datum' redeemer' ctx' -> P.do
  let redeemer = pconvert @PMarketRedeemer redeemer'
      datum = pconvert @PSimpleSale datum'
      adaAsset = passetClassData # padaSymbol # padaToken -- CS , TN == assetclass
  datumF <- pletFields @["sellerAddress", "priceOfAsset"] datum
  ctxF <- pletFields @'["txInfo"] ctx'
  pmatch (psellerPkh # (pfield @"sellerAddress" # datum)) $ \case
    PNothing -> perror
    PJust pkh -> pmatch redeemer $ \case
      PBuy _ -> P.do
        let valid =
              (ptraceIfFalse "Multiple script inputs" $ pallScriptInputsCount # ctx' #== 1)
                #&& ( ptraceIfFalse "Seller not paid" $
                        datumF.priceOfAsset #<= (passetClassDataValueOf # adaAsset # (pvaluePaidTo # (pfromData pkh) # ctxF.txInfo))
                    )

        popaque $
          pif
            valid
            (pconstant ()) -- then
            perror -- else
      PWithdraw _ -> P.do
        let valid = ptraceIfFalse "Seller Signature Missing" $ ptxSignedByPkh # pkh # (pfield @"signatories" # ctxF.txInfo)
        popaque $
          pif
            valid
            (pconstant ())
            perror

-- pandList = foldl1 ((#) . (pand' #))
