module Main (main) where

import Test.Hspec

import qualified WireTypesSpec
import qualified TestGraphSpec
import qualified ExampleGraphSpec
import qualified ProtocolConformanceSpec
import qualified RunnerSpec
import qualified FfiSpec

main :: IO ()
main = hspec $ do
  describe "tidepool-wasm" $ do
    describe "WireTypes" WireTypesSpec.spec
    describe "TestGraph" TestGraphSpec.spec
    describe "ExampleGraph" ExampleGraphSpec.spec
    describe "ProtocolConformance" ProtocolConformanceSpec.spec
    describe "Runner" RunnerSpec.spec
    describe "FFI" FfiSpec.spec
