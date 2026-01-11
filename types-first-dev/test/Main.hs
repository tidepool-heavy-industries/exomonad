-- | Test suite (placeholder).
--
-- Tests will be added as handlers are implemented.
module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TypesFirstDev" $ do
    it "placeholder test" $ do
      True `shouldBe` True
