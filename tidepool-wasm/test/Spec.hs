module Main (main) where

import Test.Hspec

import qualified WireTypesSpec
import qualified TestGraphSpec
import qualified ExampleGraphSpec
import qualified LlmTestGraphSpec
import qualified ProtocolConformanceSpec
import qualified RunnerSpec
import qualified FfiSpec
import qualified E2ESpec

main :: IO ()
main = hspec $ do
  describe "tidepool-wasm" $ do
    describe "WireTypes" WireTypesSpec.spec
    describe "TestGraph" TestGraphSpec.spec
    describe "ExampleGraph" ExampleGraphSpec.spec
    describe "LlmTestGraph" LlmTestGraphSpec.spec
    describe "ProtocolConformance" ProtocolConformanceSpec.spec
    describe "Runner" RunnerSpec.spec
    describe "FFI" FfiSpec.spec
    describe "E2E" E2ESpec.spec
