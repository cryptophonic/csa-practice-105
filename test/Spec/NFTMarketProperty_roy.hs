{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spec.NFTMarketProperty (testPByteString, functionprop, runpropPlutarch, genHashByteString, genPubKeyHash, genUserCredential, genScriptCredential, genCredential, genAddress, genPrettyByteString, genAssetClass, genValue, genSingletonValue) where

import Control.Applicative (Applicative (liftA2))
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Hash (sha2_256)
import MarketPlace (psellerPkh)
import Plutarch.Prelude
import Plutarch.Test.QuickCheck (fromPFun)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValue, currencySymbol, tokenName)
import PlutusLedgerApi.V2 (Address (..), Credential (PubKeyCredential, ScriptCredential), PubKeyHash (..), ScriptHash (ScriptHash), Value)
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude (toBuiltin)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, chooseAny, elements, forAll, listOf1, oneof, vectorOf)
import Test.QuickCheck.Property (Property)

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
-- genInput :: Gen ScriptContext
-- genInput = do
--   cred <- genCredential
--   val <- genSingletonValue
--   return $
--     buildSpending $
--       input $
--         mconcat
--           [ credential cred,
--             withValue val
--           ]

testPByteString :: Gen ([ByteString])
testPByteString = do
  -- n <- sha2_256 <$> arbitrary
  l <- vectorOf 10 arbitrary
  pure l

{-
Address
addressCredential :: Credential
addressStakingCredential :: Maybe StakingCredential
-}

gensellerPKH :: Gen ((Address, PubKeyHash))
gensellerPKH = do
  add <- genAddress
  pkh <- genPubKeyHash
  return (add, pkh)

propertySellerPKH :: forall {s :: S}. PubKeyHash -> Address -> Term s PBool
propertySellerPKH pkh addr =
  (psellerPkh # (pconstantData addr)) #== (pcon $ PJust (pconstantData pkh))

runpropSellerPKH :: Property
runpropSellerPKH = forAll gensellerPKH $ (\(addr, pkh) ->  fromPFun $ propertySellerPKH pkh addr)

-- Random Integer
functionprop :: Term s (PInteger :--> PBool)
functionprop = plam $ \x -> x #< x + 1

runpropPlutarch :: Property
runpropPlutarch = forAll arbitrary $ fromPFun functionprop

-- arbitrary(Random Integer) -> functionprop

-- psellerPkh :: Term s (PAsData PAddress :--> PMaybe (PAsData PPubKeyHash))
-- psellerPkh = phoistAcyclic $ plam $ \add -> pmatch (pfield @"credential" # add) $ \case
--   PPubKeyCredential ((pfield @"_0" #) -> pkh) -> pcon $ PJust pkh -- wallet address
--   PScriptCredential _ -> pcon PNothing -- script address

-- Homework turn psellerPkh to property based test
-- 1. Generator of (Address, PubKeyHash)
-- 2. Property to test -> Address with PubKeyHash == Just PubKeyHash
-- 3. Generator of (Address contains ScriptHash)
-- 4. Property to test -> Address with ScriptHash == Nothing