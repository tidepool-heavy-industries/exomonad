module Main (main) where

import Test.Hspec

import qualified WireTypesSpec
import qualified TestGraphSpec
import qualified LlmTestGraphSpec
import qualified ProtocolConformanceSpec
import qualified RunnerSpec
import qualified FfiSpec

main :: IO ()
main = hspec $ do
  describe "tidepool-wasm" $ do
    describe "WireTypes" WireTypesSpec.spec
    describe "TestGraph" TestGraphSpec.spec
    describe "LlmTestGraph" LlmTestGraphSpec.spec
    describe "ProtocolConformance" ProtocolConformanceSpec.spec
    describe "Runner" RunnerSpec.spec
    describe "FFI" FfiSpec.spec
