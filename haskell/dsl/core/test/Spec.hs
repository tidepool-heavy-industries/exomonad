module Main (main) where

import Test.Hspec

import qualified CLISpec
import qualified CLIGraphSpec
import qualified GraphValidationSpec
import qualified LLMNodeInterpretSpec
import qualified MemorySerializationSpec
import qualified MermaidSpec
import qualified StructuredOutputSpec

import qualified OneOfSpec
import qualified InjectTargetSpec
import qualified GotoChoiceSpec
import qualified CallHandlerSpec
import qualified DispatchGotoSpec
import qualified ConvertTransitionHintSpec
import qualified ToolTransitionIntegrationSpec
import qualified DecisionToolsSpec
import qualified DecisionSpec

main :: IO ()
main = hspec $ do
  describe "CLI Derivation" $ do
    CLISpec.spec

  describe "CLI Graph E2E" $ do
    CLIGraphSpec.spec

  describe "Graph DSL" $ do
    GraphValidationSpec.spec

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
