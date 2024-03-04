{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spec.NFTMarketProperty_markj (testPByteString, runpropPlutarch, genHashByteString, genPubKeyHash, genUserCredential, genScriptCredential, genCredential, genAddress, genPrettyByteString, genAssetClass, genValue, genSingletonValue, genScriptContext, genSellerPKH, propertySellerPKH) where

import Control.Applicative (Applicative (liftA2))
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Hash (sha2_256)
import Plutarch.Context (buildSpending, input, credential, withValue, checkPhase1)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (fromPFun)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue, currencySymbol, tokenName)
import PlutusLedgerApi.V2 (Address (..), Credential (PubKeyCredential, ScriptCredential), PubKeyHash (..), ScriptHash (ScriptHash), Value, ScriptContext )
import PlutusTx.Prelude (toBuiltin)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, chooseAny, elements, forAll, listOf1, oneof, vectorOf, Testable (property))
import Test.QuickCheck.Property (Property)
import PlutusTx.Builtins (BuiltinByteString)
import Plutarch.Extra.Maybe (pjust, pfromMaybe)

import MarketPlace_markj (addressToPkh)
import Plutarch.Maybe (pfromJust)
import Plutarch.Api.V2 (PAddress)
import Plutarch.Api.V1 (PPubKeyHash)
import Plutarch.Api.V2 (PTuple)

instance Arbitrary ByteString where
  arbitrary :: Gen ByteString
  arbitrary = pack <$> arbitrary

-- instance Arbitrary BuiltinByteString where
--   arbitrary :: Gen BuiltinByteString
--   -- arbitrary = toBuiltin . pack <$> arbitrary
--   arbitrary = toBuiltin <$> (arbitrary :: Gen ByteString)

genHashByteString :: Gen ByteString
genHashByteString = sha2_256 . pack . show <$> (chooseAny :: Gen Integer)

-- | Random bytestring but only with alphabets for better legibility.
genPrettyByteString :: Gen ByteString
genPrettyByteString = pack <$> listOf1 (elements ['a' .. 'z'])

genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = PubKeyHash . toBuiltin <$> genHashByteString

-- | Random user credential.
genUserCredential :: Gen Credential
genUserCredential = PubKeyCredential <$> genPubKeyHash

-- | Random script credential.
genScriptCredential :: Gen Credential
genScriptCredential = ScriptCredential . ScriptHash . toBuiltin <$> genHashByteString

-- | Random credential: combination of user and script credential generators.
genCredential :: Gen Credential
genCredential = oneof [genUserCredential, genScriptCredential]

genAddress :: Gen Address
genAddress = flip Address Nothing <$> genCredential

genAssetClass :: Gen AssetClass
genAssetClass =
  AssetClass
    <$> liftA2
      (,)
      (currencySymbol <$> genHashByteString)
      (tokenName <$> genPrettyByteString)

genValue :: AssetClass -> Gen Value
genValue ac = assetClassValue ac . abs <$> (arbitrary :: Gen Integer)

genSingletonValue :: Gen Value
genSingletonValue = genAssetClass >>= genValue

-- Homework
genScriptContext :: Gen ScriptContext
genScriptContext = do
   cred <- genCredential
   val <- genSingletonValue
   return $
     buildSpending checkPhase1 $
       input $
         mconcat
           [ credential cred,
             withValue val
           ]

testPByteString :: Gen ([ByteString])
testPByteString = do
  -- n <- sha2_256 <$> arbitrary
  l <- vectorOf 10 arbitrary
  pure l

genSellerPKH :: Gen (Address, PubKeyHash)
genSellerPKH = do
  pkh <- genPubKeyHash
  return $ (Address (PubKeyCredential pkh) $ Nothing, pkh)

propertySellerPKH :: Term s (PTuple PAddress PPubKeyHash :--> PBool)
propertySellerPKH = plam $ \tuple -> P.do
  let addr = (pfield @"_0" #) tuple
      pkh = (pfield @"_1" #) tuple
      maybePkh = addressToPkh # addr
      computedPkh = pfromJust # maybePkh
  computedPkh #== pkh

-- Random Integer
--functionprop :: Term s (PInteger :--> PBool)
--functionprop = plam $ \x -> x #< x + 1

runpropPlutarch :: Property
runpropPlutarch = forAll arbitrary $ fromPFun propertySellerPKH

-- arbitrary(Random Integer) -> functionprop

-- Homework turn psellerPkh to property based test
-- 1. Generator of (Address, PubKeyHash)
-- 2. Property to test -> Address with PubKeyHash == Just PubKeyHash
-- 3. Generator of (Address contains ScriptHash)
-- 4. Property to test -> Address with ScriptHash == Nothing

--propPubKeyHash :: Term s ()

--propScriptHash

--runpropSellerPkh :: Property
--runpropSellerPkh = forAll arbitrary $ fromPFun psellerPkh
