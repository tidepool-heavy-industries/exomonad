module Main (main) where

import Test.Hspec

import qualified GraphValidationSpec
import qualified MemorySerializationSpec

main :: IO ()
main = hspec $ do
  describe "Graph DSL" $ do
    GraphValidationSpec.spec

  describe "Memory Serialization" $ do
    MemorySerializationSpec.spec
