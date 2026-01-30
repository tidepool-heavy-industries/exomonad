module Main (main) where

import CodegenSyncSpec qualified
import ConversionSpec qualified
import E2ESpec qualified
import ExampleGraphSpec qualified
import FfiSpec qualified
import InterpreterSpec qualified
import LlmTestGraphSpec qualified
import ProtocolConformanceSpec qualified
import ProtocolPropertySpec qualified
import RegistrySpec qualified
import RoundtripSpec qualified
import RunnerSpec qualified
import Test.Hspec
import TestGraphSpec qualified
import ToolResultOutcomeSpec qualified
import TypedToolSpec qualified
import WireTypesSpec qualified

main :: IO ()
main = hspec $ do
  describe "exomonad-wasm" $ do
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
