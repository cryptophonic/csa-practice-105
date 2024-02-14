{-# INLINABLE mkMarket #-}
mkMarket ::  SimpleSale   -> MarketRedeemer -> ScriptContext    -> Bool
mkMarket  ds@SimpleSale{sellerAddress,priceOfAsset}  action ctx =
  case sellerPkh of 
    Nothing -> traceError "Script Address in seller"
    Just pkh -> case  action of
        Buy       -> traceIfFalse "Multiple script inputs" (allScriptInputsCount  ctx == 1)  && 
                     traceIfFalse "Seller not paid" (assetClassValueOf   (valuePaidTo info pkh) adaAsset >= priceOfAsset)
        Withdraw -> traceIfFalse "Seller Signature Missing" $ txSignedBy info pkh

    where
      sellerPkh= case sellerAddress of { Address cre m_sc -> case cre of
                                                           PubKeyCredential pkh -> Just pkh
                                                           ScriptCredential vh -> Nothing  }
      info  =  scriptContextTxInfo ctx
      adaAsset=AssetClass (adaSymbol,adaToken )

-- Homework Plutarch
{-# INLINABLE allScriptInputsCount #-}
allScriptInputsCount:: ScriptContext ->Integer
allScriptInputsCount ctx@(ScriptContext info purpose)=
    foldl (\c txOutTx-> c + countTxOut txOutTx) 0 (txInfoInputs  info)
  where
  countTxOut (TxInInfo _ (TxOut addr _ _ _)) = case addr of { Address cre m_sc -> case cre of
                                                              PubKeyCredential pkh -> 0
                                                              ScriptCredential vh -> 1  } 