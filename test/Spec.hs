module Main (main) where

import Spec.NFTMarketPlaceSpec (sampleTest)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $ testGroup "all" [(testGroup "Unit test" ([sampleTest]))]