module Main (main) where

import Test.Hspec

import qualified WireTypesSpec
import qualified TestGraphSpec

main :: IO ()
main = hspec $ do
  describe "tidepool-wasm" $ do
    describe "WireTypes" WireTypesSpec.spec
    describe "TestGraph" TestGraphSpec.spec
