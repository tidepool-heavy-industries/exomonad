module Main (main) where

import Test.Hspec

import qualified WireTypesSpec
import qualified TestGraphSpec
import qualified ExampleGraphSpec
import qualified LlmTestGraphSpec
import qualified ProtocolConformanceSpec
import qualified ProtocolPropertySpec
import qualified RunnerSpec
import qualified FfiSpec
import qualified E2ESpec
import qualified ExecutorSpec
import qualified CodegenSyncSpec
import qualified RoundtripSpec
import qualified RegistrySpec

main :: IO ()
main = hspec $ do
  describe "tidepool-wasm" $ do
    describe "WireTypes" WireTypesSpec.spec
    describe "TestGraph" TestGraphSpec.spec
    describe "ExampleGraph" ExampleGraphSpec.spec
    describe "LlmTestGraph" LlmTestGraphSpec.spec
    describe "ProtocolConformance" ProtocolConformanceSpec.spec
    describe "ProtocolProperty" ProtocolPropertySpec.spec
    describe "Runner" RunnerSpec.spec
    describe "FFI" FfiSpec.spec
    describe "E2E" E2ESpec.spec
    describe "Graph Executor E2E" ExecutorSpec.spec
    describe "Codegen Sync" CodegenSyncSpec.spec
    describe "Roundtrip" RoundtripSpec.spec
    describe "Registry" RegistrySpec.spec
