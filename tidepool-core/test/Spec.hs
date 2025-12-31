module Main (main) where

import Test.Hspec

import qualified GraphValidationSpec
import qualified LLMNodeExecuteSpec
import qualified MemorySerializationSpec
import qualified MermaidSpec

import qualified OneOfSpec
import qualified InjectTargetSpec
import qualified GotoChoiceSpec
import qualified CallHandlerSpec
import qualified DispatchGotoSpec

main :: IO ()
main = hspec $ do
  describe "Graph DSL" $ do
    GraphValidationSpec.spec

  describe "LLM Node Execution" $ do
    LLMNodeExecuteSpec.spec

  describe "Memory Serialization" $ do
    MemorySerializationSpec.spec

  describe "Mermaid Generation" $ do
    MermaidSpec.spec
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
