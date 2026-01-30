module Main (main) where

import ResponseSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "exomonad-habitica" $ do
    describe "Response" ResponseSpec.spec
