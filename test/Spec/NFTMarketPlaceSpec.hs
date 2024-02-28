module Spec.NFTMarketPlaceSpec (sampleScriptHash, samplePubKeyHash1, aliceAddress, clayNFT, goodCtx1, samplePubKeyHash2, sampleTest) where

import ContractTypes (MarketRedeemer (Buy), SimpleSale (SimpleSale))
import MarketPlace qualified
import Plutarch.Context (Builder, SpendingBuilder, address, buildSpending, checkPhase1, fee, input, output, script, signedWith, txId, withRedeemer, withRefIndex, withRefTxId, withSpendingOutRefIdx, withValue)
import Plutarch.Test.Precompiled (Expectation (..), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (Address (Address), Credential (PubKeyCredential), CurrencySymbol (CurrencySymbol), PubKeyHash, ScriptContext, ScriptHash, TokenName (TokenName), Value, adaSymbol, adaToken, singleton)
import PlutusTx qualified
import Test.Tasty (TestTree)

sampleScriptHash :: ScriptHash
sampleScriptHash = "395e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

samplePubKeyHash1 :: PubKeyHash
samplePubKeyHash1 = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

samplePubKeyHash2 :: PubKeyHash
samplePubKeyHash2 = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb"

aliceAddress :: Address
aliceAddress = Address (PubKeyCredential samplePubKeyHash1) Nothing

bobAddress :: Address
bobAddress = Address (PubKeyCredential samplePubKeyHash2) Nothing

clayNFT :: Value
clayNFT = singleton (CurrencySymbol "currency-symbol-one") (TokenName "clayNFT") 1

inputScript :: SpendingBuilder
inputScript =
  input $
    mconcat
      [ script sampleScriptHash
      , withValue (singleton adaSymbol adaToken 2)
      , withValue clayNFT
      , withRefTxId "759d3795c282bc2680d5541faa409f789cde81df369d03d057e4b58954ed865b"
      , withRefIndex 1
      , withRedeemer (PlutusTx.toData Buy)
      ]

inputBob :: (Builder a) => a
inputBob =
  input $
    mconcat
      [ address bobAddress
      , withValue (singleton adaSymbol adaToken 102)
      , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
      , withRefIndex 2
      ]

outputToAlice :: (Builder a) => a
outputToAlice =
  output $
    mconcat
      [ address aliceAddress
      , withValue
          (singleton adaSymbol adaToken 102 <> clayNFT)
      ]

outputToAliceBuilder :: (Builder a) => Integer -> Address -> Value -> a
outputToAliceBuilder amount add nft =
  output $
    mconcat
      [ address add
      , withValue
          (singleton adaSymbol adaToken amount <> nft)
      ]

commonPurpose :: SpendingBuilder
commonPurpose = withSpendingOutRefIdx 1

goodCtx1 :: ScriptContext
goodCtx1 =
  buildSpending checkPhase1 $
    mconcat
      [ inputScript
      , inputBob
      , outputToAlice
      , signedWith samplePubKeyHash1
      , fee (singleton adaSymbol adaToken 2)
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

badCtx1 :: ScriptContext
badCtx1 =
  buildSpending checkPhase1 $
    mconcat
      [ inputScript
      , inputBob
      , outputToAliceBuilder 98 aliceAddress clayNFT
      , signedWith samplePubKeyHash1
      , fee (singleton adaSymbol adaToken 6)
      , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
      , commonPurpose
      ]

sampleTest :: TestTree
sampleTest = tryFromPTerm "My Test" MarketPlace.marketPlace $ do
  testEvalCase
    "this test passed"
    Success
    [ PlutusTx.toData (SimpleSale aliceAddress 100)
    , PlutusTx.toData (Buy)
    , PlutusTx.toData goodCtx1
    ]
  testEvalCase
    "this test passed"
    Failure
    [ PlutusTx.toData (SimpleSale aliceAddress 100)
    , PlutusTx.toData (Buy)
    , PlutusTx.toData badCtx1
    ]
