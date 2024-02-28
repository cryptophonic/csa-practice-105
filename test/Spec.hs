module Main (main) where

import Spec.NFTMarketPlaceSpec (sampleTest)
import Spec.NFTMarketProperty (runpropPlutarch)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "all"
      [ unitTests
      , propertyTests
      ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit test"
    ([sampleTest])

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Test"
    ([testProperty "Plutarch Property Test" runpropPlutarch])
