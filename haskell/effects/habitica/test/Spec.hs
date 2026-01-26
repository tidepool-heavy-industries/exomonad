module Main (main) where

import Test.Hspec

import qualified ResponseSpec

main :: IO ()
main = hspec $ do
  describe "exomonad-habitica" $ do
    describe "Response" ResponseSpec.spec
