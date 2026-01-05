module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "tidepool-platform" $ do
    it "placeholder - tests needed" $ do
      True `shouldBe` True
