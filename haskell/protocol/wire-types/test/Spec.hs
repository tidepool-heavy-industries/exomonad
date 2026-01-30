module Main (main) where

import ExoMonad.Wire.TypesSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ExoMonad.Wire.Types" ExoMonad.Wire.TypesSpec.spec
