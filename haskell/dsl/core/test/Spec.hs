module Main (main) where

import CLIGraphSpec qualified
import CLISpec qualified
import CallHandlerSpec qualified
import ClaudeAPIGoldenSpec qualified
import ConvertTransitionHintSpec qualified
import DecisionSpec qualified
import DecisionToolsSpec qualified
import DispatchGotoSpec qualified
import GotoChoiceSpec qualified
import GraphValidationSpec qualified
import InjectTargetSpec qualified
import LLMNodeInterpretSpec qualified
import MemorySerializationSpec qualified
import MermaidSpec qualified
import OneOfSpec qualified
import SchemaDerivationSpec qualified
import StructuredOutputSpec qualified
import TUIWireFormatSpec qualified
import Test.Hspec
import ToolRecordTHSpec qualified
import ToolSchemaBugSpec qualified
import ToolTransitionIntegrationSpec qualified

main :: IO ()
main = hspec $ do
  describe "Tool Schema Bug" $ do
    ToolSchemaBugSpec.spec

  describe "Schema Derivation" $ do
    SchemaDerivationSpec.spec

  describe "CLI Derivation" $ do
    CLISpec.spec

  describe "CLI Graph E2E" $ do
    CLIGraphSpec.spec

  describe "Graph DSL" $ do
    GraphValidationSpec.spec

  describe "Claude API Golden" $ do
    ClaudeAPIGoldenSpec.spec

  describe "LLM Node Execution" $ do
    LLMNodeInterpretSpec.spec

  describe "Memory Serialization" $ do
    MemorySerializationSpec.spec

  describe "Mermaid Generation" $ do
    MermaidSpec.spec

  describe "StructuredOutput" $ do
    StructuredOutputSpec.spec

  describe "OneOf GADT" $ do
    OneOfSpec.spec

  describe "InjectTarget" $ do
    InjectTargetSpec.spec

  describe "GotoChoice" $ do
    GotoChoiceSpec.spec

  describe "CallHandler" $ do
    CallHandlerSpec.spec

  describe "DispatchGoto" $ do
    DispatchGotoSpec.spec

  describe "ConvertTransitionHint" $ do
    ConvertTransitionHintSpec.spec

  describe "Tool Transition Integration" $ do
    ToolTransitionIntegrationSpec.spec

  describe "DecisionTools" $ do
    DecisionToolsSpec.spec

  describe "Decision" $ do
    DecisionSpec.spec

  describe "TUI Wire Format" $ do
    TUIWireFormatSpec.spec

  describe "ToolRecord TH" $ do
    ToolRecordTHSpec.spec
