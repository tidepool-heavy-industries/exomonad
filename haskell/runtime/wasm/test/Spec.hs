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
import qualified InterpreterSpec
import qualified CodegenSyncSpec
import qualified RoundtripSpec
import qualified RegistrySpec
import qualified TypedToolSpec
import qualified ConversionSpec
import qualified ToolResultOutcomeSpec

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
    describe "Graph Interpreter E2E" InterpreterSpec.spec
    describe "Codegen Sync" CodegenSyncSpec.spec
    describe "Roundtrip" RoundtripSpec.spec
    describe "Registry" RegistrySpec.spec
    describe "TypedTool" TypedToolSpec.spec
    describe "Conversion" ConversionSpec.spec
    describe "ToolResultOutcome" ToolResultOutcomeSpec.spec
