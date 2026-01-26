module Main (main) where

import Test.Hspec
import qualified ExoMonad.Wire.TypesSpec

main :: IO ()
main = hspec $ do
  describe "ExoMonad.Wire.Types" ExoMonad.Wire.TypesSpec.spec
