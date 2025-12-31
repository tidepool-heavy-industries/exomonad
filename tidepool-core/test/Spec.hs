module Main (main) where

import Test.Hspec

import qualified GraphValidationSpec
import qualified LLMNodeExecuteSpec
import qualified MemorySerializationSpec

main :: IO ()
main = hspec $ do
  describe "Graph DSL" $ do
    GraphValidationSpec.spec

  describe "LLM Node Execution" $ do
    LLMNodeExecuteSpec.spec

  describe "Memory Serialization" $ do
    MemorySerializationSpec.spec
